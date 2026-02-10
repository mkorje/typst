use typst_library::diag::SourceResult;
use typst_library::foundations::StyleChain;
use typst_library::layout::{Abs, Axis};
use typst_library::math::ir::{FenceSizing, FencedItem, MathProperties};

use super::{MathContext, fragment::MathFragment};

/// Lays out a [`FencedItem`].
#[typst_macros::time(name = "math fenced layout", span = props.span)]
pub fn layout_fenced(
    item: &FencedItem,
    ctx: &mut MathContext,
    styles: StyleChain,
    props: &MathProperties,
) -> SourceResult<()> {
    let body_frags = ctx.layout_into_fragments(&item.body, styles)?;
    let body_styles = item.body.styles().unwrap_or(styles);
    let relative_to = if let Some(sizing) = item.sizing.get() {
        if let Some(cached) = sizing.cached_relative_to.get() {
            cached
        } else {
            let computed = relative_to_from_sizing(sizing, ctx, styles, item.balanced)?;
            sizing.cached_relative_to.set(Some(computed));
            computed
        }
    } else {
        relative_to_from_fragments(&body_frags, ctx, body_styles, item.balanced)
    };

    // Set stretch info for stretched mid items.
    let mut has_mid_stretched = false;
    for body_item in item.body.as_slice() {
        if body_item.mid_stretched().is_some_and(|x| x) {
            has_mid_stretched = true;
            body_item.set_stretch_relative_to(relative_to, Axis::Y);
        }
    }

    // Layout the opening delimiter if present.
    if let Some(open) = &item.open {
        open.set_stretch_relative_to(relative_to, Axis::Y);
        let open = ctx.layout_into_fragment(open, styles)?;
        ctx.push(open);
    }

    // Layout the actual body. If stretch info was updated or if the sizing
    // body differs, re-layout.
    if has_mid_stretched {
        let body = ctx.layout_into_fragments(&item.body, styles)?;
        ctx.extend(body);
    } else {
        ctx.extend(body_frags);
    }

    // Layout the closing delimiter if present.
    if let Some(close) = &item.close {
        close.set_stretch_relative_to(relative_to, Axis::Y);
        let close = ctx.layout_into_fragment(close, styles)?;
        ctx.push(close);
    }

    Ok(())
}

fn relative_to_from_sizing(
    sizing: &FenceSizing,
    ctx: &mut MathContext,
    styles: StyleChain,
    balanced: bool,
) -> SourceResult<Abs> {
    let mut max_metric = Abs::zero();
    for item in sizing.items.iter() {
        let fragments = ctx.layout_into_fragments(item, styles)?;
        let item_styles = item.styles().unwrap_or(styles);
        let metric = if balanced {
            max_extent_from_fragments(&fragments, ctx, item_styles)
        } else {
            max_height_from_fragments(&fragments)
        };
        max_metric = max_metric.max(metric);
    }
    Ok(if balanced { 2.0 * max_metric } else { max_metric })
}

fn relative_to_from_fragments(
    fragments: &[MathFragment],
    ctx: &MathContext,
    styles: StyleChain,
    balanced: bool,
) -> Abs {
    if balanced {
        2.0 * max_extent_from_fragments(fragments, ctx, styles)
    } else {
        max_height_from_fragments(fragments)
    }
}

fn max_extent_from_fragments(
    fragments: &[MathFragment],
    ctx: &MathContext,
    styles: StyleChain,
) -> Abs {
    let mut max_extent = Abs::zero();
    for fragment in fragments {
        let (font, size) = fragment.font(ctx, styles);
        let axis = font.math().axis_height.at(size);
        let extent = (fragment.ascent() - axis).max(fragment.descent() + axis);
        max_extent = max_extent.max(extent);
    }
    max_extent
}

fn max_height_from_fragments(fragments: &[MathFragment]) -> Abs {
    fragments.iter().map(|f| f.height()).max().unwrap_or_default()
}
