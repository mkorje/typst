use typst_library::diag::SourceResult;
use typst_library::foundations::{Resolve, StyleChain};
use typst_library::layout::{Abs, Em, Frame, FrameItem, Point, Rel, Size};
use typst_library::math::ir::{MathItem, MathProperties, TableItem};
use typst_library::math::{AugmentOffsets, style_for_denominator};
use typst_library::text::TextElem;
use typst_library::visualize::{FillRule, FixedStroke, Geometry, LineCap, Shape};
use typst_syntax::Span;

use super::MathContext;
use super::fragment::{FrameFragment, GlyphFragment, MathFragment};
use super::run::{
    cumulative_alignment_points, measure_sub_columns, sub_columns_into_line_frame,
};

const DEFAULT_STROKE_THICKNESS: Em = Em::new(0.05);

type SubColumnFragments = Vec<Vec<MathFragment>>;
type RowHeights = Vec<(Abs, Abs)>;
type ColumnLayouts = Vec<Vec<CellLayout>>;
type PreparedTableLayout = (ColumnLayouts, RowHeights);

struct CellLayout {
    sub_columns: SubColumnFragments,
    dims: (Abs, Abs),
}

/// Layout a [`TableItem`].
#[typst_macros::time(name = "math table layout", span = props.span)]
pub fn layout_table(
    item: &TableItem,
    ctx: &mut MathContext,
    styles: StyleChain,
    props: &MathProperties,
) -> SourceResult<()> {
    let rows = &item.cells;
    let nrows = rows.len();
    let ncols = rows.first().map_or(0, |row| row.len());

    if ncols == 0 || nrows == 0 {
        ctx.push(FrameFragment::new(props, styles, Frame::soft(Size::zero())));
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

    let denom_style = style_for_denominator(styles);
    // We pad ascent and descent with the ascent and descent of the paren
    // to ensure that normal matrices are aligned with others unless they are
    // way too big.
    // This will never panic as a paren will never shape into nothing.
    let paren =
        GlyphFragment::new_char(ctx, styles.chain(&denom_style), '(', Span::detached())
            .unwrap();

    let (cols, heights) = collect_columns_and_row_heights(
        item,
        ctx,
        styles,
        paren.ascent(),
        paren.descent(),
    )?;

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

        // Compute cumulative alignment points from sub-column widths.
        let (points, rcol) = cumulative_alignment_points(&sub_widths);
        let has_alignment = !points.is_empty();

        let mut y = if hline.0.contains(&0) { gap.y } else { Abs::zero() };

        for (cell_layout, &(ascent, descent)) in col.into_iter().zip(&heights) {
            let cell = sub_columns_into_line_frame(
                cell_layout.sub_columns,
                &points,
                item.alternator,
                Some(cell_layout.dims),
            );
            let pos = Point::new(
                if !has_alignment {
                    x + item.align.position(rcol - cell.width())
                } else {
                    x
                },
                y + ascent - cell.ascent(),
            );

            frame.push_frame(pos, cell);

            y += ascent + descent + gap.y;
        }

        // Advance to the end of the column
        x += rcol;

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

    ctx.push(FrameFragment::new(props, styles, frame));
    Ok(())
}

fn layout_cell_sub_columns(
    cell: &[MathItem],
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<SubColumnFragments> {
    let mut sub_columns = Vec::with_capacity(cell.len());
    for sub_col_item in cell {
        sub_columns.push(ctx.layout_into_fragments(sub_col_item, styles)?);
    }
    Ok(sub_columns)
}

fn collect_columns_and_row_heights(
    item: &TableItem,
    ctx: &mut MathContext,
    styles: StyleChain,
    min_ascent: Abs,
    min_descent: Abs,
) -> SourceResult<PreparedTableLayout> {
    let rows = &item.cells;
    let nrows = rows.len();
    let ncols = rows.first().map_or(0, |row| row.len());
    let mut cols: ColumnLayouts = (0..ncols).map(|_| Vec::with_capacity(nrows)).collect();
    let mut heights: RowHeights = vec![(Abs::zero(), Abs::zero()); nrows];

    for (r, row) in rows.iter().enumerate() {
        for (c, cell) in row.iter().enumerate() {
            let sub_columns = layout_cell_sub_columns(cell, ctx, styles)?;
            let dims = measure_sub_columns(&sub_columns);

            heights[r].0.set_max(dims.0.max(min_ascent));
            heights[r].1.set_max(dims.1.max(min_descent));

            cols[c].push(CellLayout { sub_columns, dims });
        }
    }

    Ok((cols, heights))
}

fn compute_sub_column_widths(col: &[CellLayout]) -> Vec<Abs> {
    // Compute max sub-column widths across all rows in this table column.
    // Rows without alignment (1 sub-column) have their total width tracked
    // as `pending_width`, which is incorporated as a minimum when new
    // sub-column width entries are first established.
    let max_nsub = col.iter().map(|cell| cell.sub_columns.len()).max().unwrap_or(1);
    let mut sub_widths = Vec::<Abs>::with_capacity(max_nsub);
    let mut pending_width = Abs::zero();

    for cell in col {
        let is_aligned = cell.sub_columns.len() > 1;
        if !is_aligned && max_nsub > 1 {
            // Non-aligned row in an aligned column: accumulate as pending
            // or into the first sub-column.
            let width: Abs = cell.sub_columns[0].iter().map(|f| f.width()).sum();
            if sub_widths.is_empty() {
                pending_width.set_max(width);
            } else {
                sub_widths[0].set_max(width);
            }
        } else {
            for (i, sub_column) in cell.sub_columns.iter().enumerate() {
                let width: Abs = sub_column.iter().map(|f| f.width()).sum();
                if i < sub_widths.len() {
                    sub_widths[i].set_max(width);
                } else if i == 0 {
                    sub_widths.push(width.max(pending_width));
                } else {
                    sub_widths.push(width);
                }
            }
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
