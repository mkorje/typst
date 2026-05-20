use typst_library::diag::SourceResult;
use typst_library::foundations::{Resolve, StyleChain};
use typst_library::layout::{Abs, Em, Frame, FrameItem, Point, Rel, Size};
use typst_library::math::ir::{
    Body, GlyphData, MathChild, MultilineView, TableChild, collect,
};
use typst_library::math::{AugmentOffsets, style_for_denominator};
use typst_library::text::TextElem;
use typst_library::visualize::{FillRule, FixedStroke, Geometry, LineCap, Shape};
use typst_syntax::Span;

use super::{MathContext, MathProperties};
use super::fragment::{FrameFragment, GlyphFragment};
use super::run::{MathRun, RowLayout, measure_row, stack_rows};

const DEFAULT_STROKE_THICKNESS: Em = Em::new(0.05);

/// Layout a [`TableChild`].
#[typst_macros::time(name = "math table layout", span = props.span)]
pub fn layout_table<'a>(
    item: &TableChild<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    props: &MathProperties,
) -> SourceResult<()> {
    let rows = &item.cells;
    let nrows = rows.len();
    let ncols = rows.first().map_or(0, |row| row.len());

    if ncols == 0 || nrows == 0 {
        let empty = Frame::soft(Size::zero());
        let wrapped = wrap_with_delimiters(
            empty,
            item.open.as_ref(),
            item.close.as_ref(),
            styles,
            ctx,
        )?;
        ctx.push(FrameFragment::new(props, styles, wrapped));
        return Ok(());
    }

    let gap = item.gap.zip_map(ctx.region.size, Rel::relative_to);
    let half_gap = gap * 0.5;

    // We provide a default stroke thickness that scales
    // with font size to ensure that augmentation lines
    // look correct by default at all matrix sizes.
    // The line cap is also set to square because it looks more "correct".
    let default_stroke_thickness = DEFAULT_STROKE_THICKNESS.resolve(styles);
    let default_stroke = FixedStroke {
        thickness: default_stroke_thickness,
        paint: styles.get_ref(TextElem::fill).as_decoration(),
        cap: LineCap::Square,
        ..Default::default()
    };

    let (mut hline, mut vline, stroke) = match &item.augment {
        Some(augment) => {
            // We need to get stroke here for ownership.
            let stroke =
                augment.stroke.clone().unwrap_or_default().unwrap_or(default_stroke);
            (augment.hline.clone(), augment.vline.clone(), stroke)
        }
        _ => (AugmentOffsets::default(), AugmentOffsets::default(), default_stroke),
    };

    // Before the full matrix body can be laid out, the
    // individual cells must first be independently laid out
    // so we can ensure alignment across rows and columns.
    let mut cols: Vec<Vec<CellLayout>> =
        (0..ncols).map(|_| Vec::with_capacity(nrows)).collect();

    // This variable stores the maximum ascent and descent for each row.
    let mut heights = vec![(Abs::zero(), Abs::zero()); nrows];

    let denom_style = style_for_denominator(styles);
    // We pad ascent and descent with the ascent and descent of the paren
    // to ensure that normal matrices are aligned with others unless they are
    // way too big.
    let (ascent, descent) =
        GlyphFragment::new_char(ctx, styles.chain(&denom_style), '(', Span::detached())
            .map(|glyph| (glyph.ascent(), glyph.descent()))
            .unwrap_or((Abs::zero(), Abs::zero()));

    for (r, row) in rows.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            let cell = layout_cell(cell, ctx, styles)?;

            heights[r].0.set_max(cell.height.0.max(ascent));
            heights[r].1.set_max(cell.height.1.max(descent));

            cols[c].push(cell);
        }
    }

    for line in hline.0.iter_mut() {
        if *line < 0 {
            *line += nrows as isize;
        }
    }

    for line in vline.0.iter_mut() {
        if *line < 0 {
            *line += ncols as isize;
        }
    }

    // For each row, combine maximum ascent and descent into a row height.
    // Sum the row heights, then add the total height of the gaps between rows.
    let mut total_height =
        heights.iter().map(|&(a, b)| a + b).sum::<Abs>() + gap.y * (nrows - 1) as f64;

    if hline.0.contains(&0) {
        total_height += gap.y;
    }

    if hline.0.contains(&(nrows as isize)) {
        total_height += gap.y;
    }

    // Width starts at zero because it can't be calculated until later
    let mut frame = Frame::soft(Size::new(Abs::zero(), total_height));

    let mut x = Abs::zero();

    if vline.0.contains(&0) {
        frame.push(
            Point::with_x(x + half_gap.x),
            line_item(total_height, true, stroke.clone(), props.span),
        );
        x += gap.x;
    }

    for (index, col) in cols.into_iter().enumerate() {
        let sub_widths = compute_sub_column_widths(&col);
        let rows = col.into_iter().enumerate().map(|(row, cell_layout)| RowLayout {
            cells: cell_layout.sub_columns,
            frame_height: cell_layout.height,
            row_height: Some(heights[row]),
        });

        let builder = stack_rows(
            rows,
            &sub_widths,
            item.alternator,
            item.align,
            gap.y,
            if hline.0.contains(&0) { gap.y } else { Abs::zero() },
        );

        for (cell, mut pos) in builder.frames {
            pos.x += x;
            frame.push_frame(pos, cell);
        }

        // Advance to the end of the column
        x += builder.size.x;

        // If a vertical line should be inserted after this column
        if vline.0.contains(&(index as isize + 1)) {
            frame.push(
                Point::with_x(x + half_gap.x),
                line_item(total_height, true, stroke.clone(), props.span),
            );
        }

        // Advance to the start of the next column
        x += gap.x;
    }

    let total_width = if !(vline.0.contains(&(ncols as isize))) { x - gap.x } else { x };

    // This allows the horizontal lines to be laid out
    for line in hline.0 {
        let offset = if line == 0 {
            gap.y
        } else {
            (heights[0..line as usize].iter().map(|&(a, b)| a + b).sum::<Abs>()
                + gap.y * (line - 1) as f64)
                + half_gap.y
        };

        frame.push(
            Point::with_y(offset),
            line_item(total_width, false, stroke.clone(), props.span),
        );
    }

    frame.size_mut().x = total_width;

    let axis = ctx.font().math().axis_height.resolve(styles);
    let height = frame.height();
    frame.set_baseline(height / 2.0 + axis);

    // Wrap with the table's own delimiters.
    let wrapped = wrap_with_delimiters(
        frame,
        item.open.as_ref(),
        item.close.as_ref(),
        styles,
        ctx,
    )?;
    ctx.push(FrameFragment::new(props, styles, wrapped));
    Ok(())
}

struct CellLayout {
    sub_columns: Vec<MathRun>,
    height: (Abs, Abs),
}

/// Layout one cell, split at alignment points into sub-columns.
fn layout_cell<'a>(
    cell: Body<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
) -> SourceResult<CellLayout> {
    let span = cell.content.span();
    let loc = ctx.locator.next(&span);
    let children =
        collect(cell.content, cell.styles, ctx.engine, loc, ctx.arenas)?;

    // Cells may contain alignment points (`Align`) producing sub-columns;
    // linebreaks within a cell are warned about + ignored at collect
    // time so we only consult the first row.
    let view = MultilineView::new(&children);
    let row: &[&[MathChild]] = view.rows.first().map(Vec::as_slice).unwrap_or(&[]);

    let mut sub_columns: Vec<MathRun> = Vec::with_capacity(row.len().max(1));
    if row.is_empty() {
        sub_columns.push(Vec::new());
    } else {
        for col in row {
            sub_columns.push(ctx.layout_children(col, cell.styles)?);
        }
    }

    let height = measure_row(&sub_columns);
    let _ = styles;
    Ok(CellLayout { sub_columns, height })
}

/// Wrap a body frame with the table's pre-built delimiter glyphs.
fn wrap_with_delimiters<'a>(
    body: Frame,
    open: Option<&GlyphData>,
    close: Option<&GlyphData>,
    styles: StyleChain<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
) -> SourceResult<Frame> {
    if open.is_none() && close.is_none() {
        return Ok(body);
    }

    let body_height = body.height();
    let mut layout_delim = |g: &GlyphData| -> SourceResult<Frame> {
        g.set_stretch_relative_to(body_height, typst_library::layout::Axis::Y);
        let child = MathChild::Glyph(g.clone());
        let frag = ctx
            .layout_children(std::slice::from_ref(&child), styles)?
            .into_iter()
            .next()
            .expect("delimiter glyph layout should yield a fragment");
        Ok(frag.into_frame())
    };

    let open_frame = open.map(&mut layout_delim).transpose()?;
    let close_frame = close.map(&mut layout_delim).transpose()?;

    let ow = open_frame.as_ref().map_or(Abs::zero(), |f| f.width());
    let cw = close_frame.as_ref().map_or(Abs::zero(), |f| f.width());
    let bw = body.width();
    let bh = body.height();
    let h = bh
        .max(open_frame.as_ref().map_or(Abs::zero(), |f| f.height()))
        .max(close_frame.as_ref().map_or(Abs::zero(), |f| f.height()));

    let mut frame = Frame::soft(Size::new(ow + bw + cw, h));
    frame.set_baseline(body.baseline() + (h - bh) / 2.0);

    let by = (h - bh) / 2.0;
    let mut x = Abs::zero();
    if let Some(f) = open_frame {
        let y = (h - f.height()) / 2.0;
        frame.push_frame(Point::new(x, y), f);
        x += ow;
    }
    frame.push_frame(Point::new(x, by), body);
    x += bw;
    if let Some(f) = close_frame {
        let y = (h - f.height()) / 2.0;
        frame.push_frame(Point::new(x, y), f);
    }

    Ok(frame)
}

/// Compute max sub-column widths across a table column.
fn compute_sub_column_widths(col: &[CellLayout]) -> Vec<Abs> {
    let len = col.iter().map(|cell| cell.sub_columns.len()).max().unwrap_or(1);
    let mut sub_widths = vec![Abs::zero(); len];
    for cell in col {
        for (i, sub_column) in cell.sub_columns.iter().enumerate() {
            let width = sub_column.iter().map(|f| f.width()).sum();
            sub_widths[i].set_max(width);
        }
    }
    sub_widths
}

fn line_item(length: Abs, vertical: bool, stroke: FixedStroke, span: Span) -> FrameItem {
    let line_geom = if vertical {
        Geometry::Line(Point::with_y(length))
    } else {
        Geometry::Line(Point::with_x(length))
    };

    FrameItem::Shape(
        Shape {
            geometry: line_geom,
            fill: None,
            fill_rule: FillRule::default(),
            stroke: Some(stroke),
        },
        span,
    )
}
