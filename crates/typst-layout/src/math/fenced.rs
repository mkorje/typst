use typst_library::diag::SourceResult;
use typst_library::foundations::StyleChain;
use typst_library::layout::{Abs, Axis};
use typst_library::math::{FencePart, FencedItem, MathItem, MathProperties};

use super::MathContext;

/// Lays out an [`FencedItem`].
#[typst_macros::time(name = "math fenced layout", span = props.span)]
pub fn layout_fenced(
    item: &FencedItem,
    ctx: &mut MathContext,
    styles: StyleChain,
    props: &MathProperties,
) -> SourceResult<()> {
    // Layout the FULL body to compute relative_to for delimiter sizing.
    // This is important for split fences: we use the full body height
    // even when only rendering a portion.
    let body = ctx.layout_into_fragments(&item.body, styles)?;
    let relative_to = if item.balanced {
        let mut max_extent = Abs::zero();
        for fragment in body.iter() {
            let (font, size) = fragment.font(ctx, item.body.styles().unwrap_or(styles));
            let axis = font.math().axis_height.at(size);
            let extent = (fragment.ascent() - axis).max(fragment.descent() + axis);
            max_extent = max_extent.max(extent);
        }
        2.0 * max_extent
    } else {
        body.iter().map(|f| f.height()).max().unwrap_or_default()
    };

    // Determine which parts to render based on split status.
    let (render_open, render_close, visible_items): (bool, bool, &[MathItem]) =
        match &item.split {
            Some(split) => (
                split.part == FencePart::Start,
                split.part == FencePart::End,
                split.visible_items,
            ),
            None => (true, true, item.body.as_slice()),
        };

    // Set stretch info for stretched mid items in the FULL body.
    // We need to do this even for split fences to ensure consistent sizing.
    let mut has_mid_stretched = false;
    for body_item in item.body.as_slice() {
        if body_item.mid_stretched().is_some_and(|x| x) {
            has_mid_stretched = true;
            body_item.set_stretch_relative_to(relative_to, Axis::Y);
        }
    }

    // Layout the opening delimiter if present and this is a Start part (or not split).
    if render_open && let Some(open) = &item.open {
        open.set_stretch_relative_to(relative_to, Axis::Y);
        let open = ctx.layout_into_fragment(open, styles)?;
        ctx.push(open);
    }

    // Layout the visible portion of the body.
    // For non-split fences, this is the full body.
    // For split fences, this is just the visible_items slice.
    if item.split.is_some() || has_mid_stretched {
        // Need to re-layout (either split fence or mid-stretched items updated)
        let visible_body = ctx.layout_items_into_fragments(visible_items, styles)?;
        ctx.extend(visible_body);
    } else {
        // Non-split, no mid-stretched: use already-laid-out body
        ctx.extend(body);
    }

    // Layout the closing delimiter if present and this is an End part (or not split).
    if render_close && let Some(close) = &item.close {
        close.set_stretch_relative_to(relative_to, Axis::Y);
        let close = ctx.layout_into_fragment(close, styles)?;
        ctx.push(close);
    }

    Ok(())
}
