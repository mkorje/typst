use typst_library::diag::{warning, SourceResult};
use typst_library::foundations::{Packed, StyleChain};
use typst_library::layout::{Abs, Axis, Em, Rel};
use typst_library::math::StretchElem;
use typst_library::text::Glyph;
use typst_utils::Get;

use super::{delimiter_alignment, stretch_axes, MathContext, MathFragment};

/// Lays out a [`StretchElem`].
#[typst_macros::time(name = "math.stretch", span = elem.span())]
pub fn layout_stretch(
    elem: &Packed<StretchElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let mut fragment = ctx.layout_into_fragment(&elem.body, styles)?;
    stretch_fragment(
        ctx,
        styles,
        &mut fragment,
        None,
        None,
        elem.size(styles),
        Abs::zero(),
    );
    ctx.push(fragment);
    Ok(())
}

/// Attempts to stretch the given fragment by/to the amount given in stretch.
pub fn stretch_fragment(
    ctx: &mut MathContext,
    styles: StyleChain,
    fragment: &mut MathFragment,
    axis: Option<Axis>,
    relative_to: Option<Abs>,
    stretch: Rel<Abs>,
    short_fall: Abs,
) {
    let size = fragment.size();

    let MathFragment::Glyph(ref mut glyph) = fragment else { return };

    // Return if we attempt to stretch along an axis which isn't stretchable,
    // so that the original fragment isn't modified.
    let axes = stretch_axes(&glyph.text.font, glyph.base_id);
    let stretch_axis = if let Some(axis) = axis {
        if !axes.get(axis) {
            return;
        }
        axis
    } else {
        match axes.get_only() {
            Some(x) => x,
            None => {
                if axes.all(|f| *f) {
                    // As far as we know, there aren't any glyphs that have both
                    // vertical and horizontal constructions. So for the time being, we
                    // will assume that a glyph cannot have both.
                    ctx.engine.sink.warn(warning!(
                       glyph.text.glyphs[0].span.0,
                       "glyph has both vertical and horizontal constructions";
                       hint: "this is probably a font bug";
                       hint: "please file an issue at https://github.com/typst/typst/issues"
                    ));
                }
                return;
            }
        }
    };

    // TODO: Glyph may have already been stretched, so need to go back to unstretched state somehow.
    if glyph.text.glyphs.len() > 1 && glyph.text.glyphs[0].x_advance == Em::zero() {
        glyph.shift -= glyph.descent;
    }
    glyph.text.glyphs = vec![Glyph {
        id: glyph.base_id.0,
        x_advance: glyph.text.font.advance(glyph.base_id.0).unwrap_or_default(),
        ..glyph.text.glyphs[0].clone()
    }];
    glyph.update_glyph();

    let relative_to_size = relative_to.unwrap_or_else(|| size.get(stretch_axis));

    glyph.stretch(stretch.relative_to(relative_to_size), short_fall, stretch_axis);

    if stretch_axis == Axis::Y {
        glyph.center_on_axis();
    }
}
