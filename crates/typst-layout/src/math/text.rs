use std::f64::consts::SQRT_2;

use codex::styling::{to_style, MathStyle};
use ecow::EcoString;
use typst_library::diag::SourceResult;
use typst_library::foundations::{Packed, StyleChain, SymbolElem};
use typst_library::layout::{Abs, Size};
use typst_library::math::{EquationElem, MathSize, MathVariant};
use typst_library::text::{
    BottomEdge, BottomEdgeMetric, TextElem, TopEdge, TopEdgeMetric,
};
use typst_syntax::{is_newline, Span};
use unicode_math_class::MathClass;
use unicode_segmentation::UnicodeSegmentation;

use super::{FrameFragment, GlyphFragment, MathContext, MathFragment, MathRun};

/// Lays out a [`TextElem`].
pub fn layout_text(
    elem: &Packed<TextElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let text = &elem.text;
    let span = elem.span();
    let fragment = if text.contains(is_newline) {
        layout_text_lines(text.split(is_newline), span, ctx, styles)?
    } else {
        layout_inline_text(text, span, ctx, styles)?
    };
    ctx.push(fragment);
    Ok(())
}

/// Layout multiple lines of text.
fn layout_text_lines<'a>(
    lines: impl Iterator<Item = &'a str>,
    span: Span,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<FrameFragment> {
    let mut fragments = vec![];
    for (i, line) in lines.enumerate() {
        if i != 0 {
            fragments.push(MathFragment::Linebreak);
        }
        if !line.is_empty() {
            fragments.push(layout_inline_text(line, span, ctx, styles)?.into());
        }
    }
    let mut frame = MathRun::new(fragments).into_frame(styles);
    let axis = scaled!(ctx, styles, axis_height);
    frame.set_baseline(frame.height() / 2.0 + axis);
    Ok(FrameFragment::new(styles, frame))
}

/// Layout the given text string into a [`FrameFragment`] after styling all
/// characters for the math font (without auto-italics).
fn layout_inline_text(
    text: &str,
    span: Span,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<FrameFragment> {
    if text.chars().all(|c| c.is_ascii_digit() || c == '.') {
        // Small optimization for numbers. Note that this lays out slightly
        // differently to normal text and is worth re-evaluating in the future.
        let mut fragments = vec![];
        let is_single = text.chars().count() == 1;
        for unstyled_c in text.chars() {
            let style = char_style(styles, unstyled_c, false);
            let c = to_style(unstyled_c, style).next().unwrap();
            let mut glyph = GlyphFragment::new(ctx, styles, c, span);
            if is_single {
                // Duplicate what `layout_glyph` does exactly even if it's
                // probably incorrect here.
                match EquationElem::size_in(styles) {
                    MathSize::Script => glyph.make_script_size(ctx),
                    MathSize::ScriptScript => glyph.make_script_script_size(ctx),
                    _ => {}
                }
            }
            fragments.push(glyph.into());
        }
        let frame = MathRun::new(fragments).into_frame(styles);
        Ok(FrameFragment::new(styles, frame).with_text_like(true))
    } else {
        let local = [
            TextElem::set_top_edge(TopEdge::Metric(TopEdgeMetric::Bounds)),
            TextElem::set_bottom_edge(BottomEdge::Metric(BottomEdgeMetric::Bounds)),
        ]
        .map(|p| p.wrap());

        let styles = styles.chain(&local);
        let styled_text: EcoString = text
            .chars()
            .flat_map(|c| to_style(c, char_style(styles, c, false)))
            .collect();
        let spaced = styled_text.graphemes(true).nth(1).is_some();
        let elem = TextElem::packed(styled_text).spanned(span);

        // There isn't a natural width for a paragraph in a math environment;
        // because it will be placed somewhere probably not at the left margin
        // it will overflow. So emulate an `hbox` instead and allow the
        // paragraph to extend as far as needed.
        let frame = crate::inline::layout_inline(
            ctx.engine,
            &[(&elem, styles)],
            &mut ctx.locator.next(&span).split(),
            styles,
            Size::splat(Abs::inf()),
            false,
        )?
        .into_frame();

        Ok(FrameFragment::new(styles, frame)
            .with_class(MathClass::Alphabetic)
            .with_text_like(true)
            .with_spaced(spaced))
    }
}

/// Layout a single character in the math font with the correct styling applied
/// (includes auto-italics).
pub fn layout_symbol(
    elem: &Packed<SymbolElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    // Switch dotless char to normal when we have the dtls OpenType feature.
    // This should happen before the main styling pass.
    let (unstyled_c, dtls) = match try_dotless(elem.text) {
        Some(c) if ctx.dtls_table.is_some() => (c, true),
        _ => (elem.text, false),
    };
    let style = char_style(styles, unstyled_c, true);
    let c = to_style(unstyled_c, style).next().unwrap();
    let fragment = match GlyphFragment::try_new(ctx, styles, c, elem.span()) {
        Some(glyph) => layout_glyph(glyph, dtls, ctx, styles),
        None => {
            // Not in the math font, fallback to normal inline text layout.
            layout_inline_text(c.encode_utf8(&mut [0; 4]), elem.span(), ctx, styles)?
                .into()
        }
    };
    ctx.push(fragment);
    Ok(())
}

/// Layout a [`GlyphFragment`].
fn layout_glyph(
    mut glyph: GlyphFragment,
    dtls: bool,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> MathFragment {
    if dtls {
        glyph.make_dotless_form(ctx);
    }
    let math_size = EquationElem::size_in(styles);
    match math_size {
        MathSize::Script => glyph.make_script_size(ctx),
        MathSize::ScriptScript => glyph.make_script_script_size(ctx),
        _ => {}
    }

    if glyph.class == MathClass::Large {
        let mut variant = if math_size == MathSize::Display {
            let height = scaled!(ctx, styles, display_operator_min_height)
                .max(SQRT_2 * glyph.height());
            glyph.stretch_vertical(ctx, height, Abs::zero())
        } else {
            glyph.into_variant()
        };
        // TeXbook p 155. Large operators are always vertically centered on the
        // axis.
        variant.center_on_axis(ctx);
        variant.into()
    } else {
        glyph.into()
    }
}

/// Style the character by selecting the unicode codepoint for italic, bold,
/// caligraphic, etc.
///
/// <https://www.w3.org/TR/mathml-core/#new-text-transform-mappings>
/// <https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols>
fn char_style(styles: StyleChain, c: char, auto_italic: bool) -> MathStyle {
    use MathStyle::*;
    use MathVariant::*;

    let variant = EquationElem::variant_in(styles);
    let bold = EquationElem::bold_in(styles);
    let italic = EquationElem::italic_in(styles).unwrap_or(
        auto_italic
            && matches!(
                c,
                'a'..='z' | 'ħ' | 'ı' | 'ȷ' | 'A'..='Z' |
                'α'..='ω' | '∂' | 'ϵ' | 'ϑ' | 'ϰ' | 'ϕ' | 'ϱ' | 'ϖ'
            )
            && matches!(variant, Sans | Serif),
    );

    match (variant, bold, italic) {
        (Serif, false, false) => Plain,
        (Serif, true, false) => Bold,
        (Serif, false, true) => Italic,
        (Serif, true, true) => BoldItalic,
        (Sans, false, false) => Sansserif,
        (Sans, true, false) => SansserifBold,
        (Sans, false, true) => SansserifItalic,
        (Sans, true, true) => SansserifBoldItalic,
        (Cal, false, _) => Script,
        (Cal, true, _) => BoldScript,
        (Frak, false, _) => Fraktur,
        (Frak, true, _) => BoldFraktur,
        (Mono, _, _) => Monospace,
        (Bb, _, false) => Doublestruck,
        (Bb, _, true) => DoublestruckItalic,
    }
}

/// The non-dotless version of a dotless character that can be used with the
/// `dtls` OpenType feature.
pub fn try_dotless(c: char) -> Option<char> {
    match c {
        'ı' => Some('i'),
        'ȷ' => Some('j'),
        _ => None,
    }
}
