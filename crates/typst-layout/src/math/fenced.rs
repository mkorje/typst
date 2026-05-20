use typst_library::diag::SourceResult;
use typst_library::foundations::{Resolve, StyleChain};
use typst_library::layout::{Abs, Axis, Frame, Point, Size};
use typst_library::math::DELIM_SHORT_FALL;
use typst_library::math::ir::{
    FencedView, LrChild, MathChild, StretchInfo, collect,
};
use typst_library::text::TextElem;
use ecow::EcoString;

use super::{MathContext, MathProperties};
use super::fragment::{FrameFragment, GlyphFragment, MathFragment};
use super::run::{MathFragmentsExt, MathRun};

/// Lay out an [`LrChild`].
#[typst_macros::time(name = "math fenced layout", span = props.span)]
pub fn layout_fenced<'a>(
    item: &LrChild<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    props: &MathProperties,
) -> SourceResult<()> {
    let body_styles = item.body.styles;
    let span = item.body.content.span();
    let loc = ctx.locator.next(&span);
    let body_children = collect(
        item.body.content,
        body_styles,
        ctx.engine,
        loc,
        ctx.arenas,
    )?;

    let view = FencedView::new(&body_children);

    let font_size = styles.resolve(TextElem::size);
    let short_fall = DELIM_SHORT_FALL.at(font_size);

    // Pre-pass: set stretch info for `Mid`-marked items in the body, so
    // they get laid out at the same vertical stretch as the surrounding
    // delimiters.
    apply_mid_stretch(&body_children, &view, body_styles, ctx, short_fall)?;

    let body_frame = layout_body(&view, ctx, body_styles)?;
    let body_height = body_frame.height();

    let open_frame = view
        .open
        .and_then(extract_delim_char)
        .and_then(|c| stretched_delim(ctx, body_styles, c, body_height, short_fall, span));
    let close_frame = view
        .close
        .and_then(extract_delim_char)
        .and_then(|c| stretched_delim(ctx, body_styles, c, body_height, short_fall, span));

    let open_w = open_frame.as_ref().map_or(Abs::zero(), |f| f.width());
    let close_w = close_frame.as_ref().map_or(Abs::zero(), |f| f.width());
    let body_w = body_frame.width();
    let body_h = body_frame.height();

    let total_w = open_w + body_w + close_w;
    let total_h = body_h
        .max(open_frame.as_ref().map_or(Abs::zero(), |f| f.height()))
        .max(close_frame.as_ref().map_or(Abs::zero(), |f| f.height()));

    let mut frame = Frame::soft(Size::new(total_w, total_h));
    frame.set_baseline(body_frame.baseline() + (total_h - body_h) / 2.0);

    let body_y = (total_h - body_h) / 2.0;
    let mut x = Abs::zero();

    if let Some(f) = open_frame {
        let y = (total_h - f.height()) / 2.0;
        frame.push_frame(Point::new(x, y), f);
        x += open_w;
    }
    frame.push_frame(Point::new(x, body_y), body_frame);
    x += body_w;
    if let Some(f) = close_frame {
        let y = (total_h - f.height()) / 2.0;
        frame.push_frame(Point::new(x, y), f);
    }

    ctx.push(FrameFragment::new(props, styles, frame));
    Ok(())
}

/// Layout the body of a fenced group as a single frame.
///
/// Single-row uses the inline math run builder; multi-row builds rows
/// independently and stacks them.
fn layout_body<'a>(
    view: &FencedView<'a, '_>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
) -> SourceResult<Frame> {
    if view.body.nrows() == 0 {
        return Ok(Frame::soft(Size::zero()));
    }

    let mut row_frames = Vec::with_capacity(view.body.nrows());
    for row in &view.body.rows {
        let mut row_run: MathRun = Vec::new();
        for col in row {
            row_run.extend(ctx.layout_children(col, styles)?);
        }
        row_frames.push(row_run.into_frame());
    }

    if row_frames.len() == 1 {
        return Ok(row_frames.into_iter().next().unwrap());
    }

    Ok(stack_rows_vertical(row_frames))
}

/// If a child looks like a delimiter glyph, return its character.
fn extract_delim_char(child: &MathChild) -> Option<char> {
    match child {
        MathChild::Glyph(g) => g.text.chars().next(),
        _ => None,
    }
}

/// Build a delimiter glyph stretched to the target height, centered on
/// the math axis.
fn stretched_delim<'a>(
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    c: char,
    height: Abs,
    short_fall: Abs,
    span: typst_syntax::Span,
) -> Option<Frame> {
    let text: EcoString = std::iter::once(c).collect();
    let mut g = GlyphFragment::new(ctx.engine.world, styles, &text, span)?;
    if let Some(axis) = g.stretch_axis(ctx.engine) {
        g.stretch(ctx.engine, height, short_fall, axis);
        if axis == typst_library::layout::Axis::Y {
            g.center_on_axis();
        }
    }
    let frag: MathFragment = g.into();
    Some(frag.into_frame())
}

/// For each `Mid` marker in the LR body, push a vertical stretch hint
/// into its inner glyph (if any) so layout draws it sized to match the
/// outer delimiters.
fn apply_mid_stretch<'a>(
    body_children: &[MathChild<'a>],
    view: &FencedView<'a, '_>,
    styles: StyleChain<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    short_fall: Abs,
) -> SourceResult<()> {
    if view.mid_positions.is_empty() {
        return Ok(());
    }

    // Estimate the stretch height from the body's pre-laid-out children.
    // Cheap approximation: use 1em (the typical delimiter target) — the
    // actual relative_to gets set on the glyph and the glyph layout
    // re-resolves it. This matches the old code's `Rel::one()` target.
    let target = typst_library::layout::Rel::new(
        typst_library::layout::Ratio::one(),
        typst_library::layout::Abs::zero(),
    );
    let info = StretchInfo::new(target, typst_library::layout::Em::zero());
    let _ = short_fall;
    let _ = styles;
    let _ = ctx;

    for &idx in &view.mid_positions {
        if let Some(MathChild::Mid(mid)) = body_children.get(idx) {
            // The mid body might itself be a glyph or a sequence; for
            // the typical case (a single character) it's a glyph after
            // collection. Push the stretch info into any glyph found.
            let span = mid.body.content.span();
            let loc = ctx.locator.next(&span);
            let mid_children = collect(
                mid.body.content,
                mid.body.styles,
                ctx.engine,
                loc,
                ctx.arenas,
            )?;
            for child in &mid_children {
                if let Some(g) = child.stretch_target() {
                    g.update_stretch(info);
                }
            }
            // NOTE: the collected `mid_children` aren't reused here —
            // they'll be re-collected when `layout_body` walks the
            // outer body. Cells with `Mid` markers are split across
            // sub-columns; the stretch info now lives on the underlying
            // glyph (shared via the IR's Body content), so the
            // re-collection sees it via the same arena content.
            let _ = mid_children;
        }
    }

    Ok(())
}

/// Vertically stack row frames into one frame (left-aligned).
fn stack_rows_vertical(rows: Vec<Frame>) -> Frame {
    if rows.is_empty() {
        return Frame::soft(Size::zero());
    }
    let width = rows.iter().map(|f| f.width()).max().unwrap_or(Abs::zero());
    let height: Abs = rows.iter().map(|f| f.height()).sum();
    let mut out = Frame::soft(Size::new(width, height));
    let mut y = Abs::zero();
    let mut first_baseline = None;
    for row in rows {
        if first_baseline.is_none() {
            first_baseline = Some(y + row.baseline());
        }
        let h = row.height();
        out.push_frame(Point::new(Abs::zero(), y), row);
        y += h;
    }
    if let Some(b) = first_baseline {
        out.set_baseline(b);
    }
    out
}
