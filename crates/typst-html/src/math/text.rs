use codex::styling::{MathStyle, to_style};
use ecow::EcoString;
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{
    Content, NativeElement, Packed, StyleChain, SymbolElem,
};
use typst_library::introspection::SplitLocator;
use typst_library::math::EquationElem;
use typst_library::text::TextElem;
use typst_utils::default_math_class;
use unicode_math_class::MathClass;

use super::show_equation;
use crate::{HtmlAttrs, HtmlElem, attr::mathml as attr, tag::mathml as tag};

pub fn show_symbol(
    elem: &Packed<SymbolElem>,
    _: &mut Engine,
    _: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    // Can't check if the font has the dtls feature...

    let variant = styles.get(EquationElem::variant);
    let bold = styles.get(EquationElem::bold);
    let italic = styles.get(EquationElem::italic);

    let style = MathStyle::select(elem.text, variant, bold, italic);
    let text: EcoString = to_style(elem.text, style).collect();

    let class = styles
        .get(EquationElem::class)
        .or_else(|| default_math_class(text.chars().next().unwrap()))
        .unwrap_or(MathClass::Normal);

    let mut attrs = HtmlAttrs::new();

    // Only add this when necessary, i.e. to ensure browsers don't perform an
    // italic mapping. See https://www.w3.org/TR/mathml-core/#math-auto-transform.
    let mut chars = text.chars();
    if matches!(chars.next().unwrap(), 'A'..='Z' | 'a'..='z' | 'ı' | 'ȷ' | 'Α'..='Ρ' | 'ϴ' | 'Σ'..='Ω' | '∇' | 'α'..='ω' | '∂' | 'ϵ' | 'ϑ' | 'ϰ' | 'ϕ' | 'ϱ' | 'ϖ')
        && chars.next().is_none()
    {
        attrs.push(attr::mathvariant, "normal");
    }

    let mut fence = false;
    let mut separator = false;

    // TODO: need to process spaces to assign vary math class things...
    let tag = match class {
        MathClass::Normal
        | MathClass::Alphabetic
        | MathClass::Special
        | MathClass::GlyphPart
        | MathClass::Space => tag::mi,
        MathClass::Vary | MathClass::Relation | MathClass::Diacritic => tag::mo,
        MathClass::Binary => {
            attrs.push(attr::form, "infix");
            tag::mo
        }
        MathClass::Unary => {
            attrs.push(attr::form, "prefix");
            tag::mo
        }
        MathClass::Punctuation => {
            separator = true;
            tag::mo
        }
        MathClass::Fence => {
            fence = true;
            tag::mo
        }
        MathClass::Large => {
            attrs.push(attr::largeop, "true");
            tag::mo
        }
        MathClass::Opening => {
            fence = true;
            attrs.push(attr::form, "prefix");
            tag::mo
        }
        MathClass::Closing => {
            fence = true;
            attrs.push(attr::form, "postfix");
            tag::mo
        }
    };

    let mut chars = text.chars();
    if let Some(c) = chars.next()
        && chars.next().is_none()
    {
        if separator && !operator_tables::special::is_separator(c) {
            attrs.push(attr::separator, "true");
        }
        if fence && !operator_tables::special::is_fence(c) {
            attrs.push(attr::fence, "true");
        }
    }

    Ok(HtmlElem::new(tag)
        .with_attrs(attrs)
        .with_body(Some(TextElem::packed(text)))
        .pack())
}

// Generate this automatically in typst-assets?
mod operator_tables {
    pub mod special {
        pub fn is_fence(c: char) -> bool {
            matches!(c, '\u{0028}'..='\u{0029}' | '\u{005B}' | '\u{005D}' | '\u{007B}'..='\u{007D}' | '\u{0331}' | '\u{2016}' | '\u{2018}'..='\u{2019}' | '\u{201C}'..='\u{201D}' | '\u{2308}'..='\u{230B}' | '\u{2329}'..='\u{232A}' | '\u{2772}'..='\u{2773}' | '\u{27E6}'..='\u{27EF}' | '\u{2980}' | '\u{2983}'..='\u{2999}' | '\u{29D8}'..='\u{29DB}' | '\u{29FC}'..='\u{29FD}')
        }

        pub fn is_separator(c: char) -> bool {
            matches!(c, '\u{002C}' | '\u{003B}' | '\u{2063}')
        }
    }
}

pub fn show_text(
    elem: &Packed<TextElem>,
    _: &mut Engine,
    _: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let variant = styles.get(EquationElem::variant);
    let bold = styles.get(EquationElem::bold);
    // Disable auto-italic.
    let italic = styles.get(EquationElem::italic).or(Some(false));

    let styled_text: EcoString = elem
        .text
        .chars()
        .flat_map(|c| to_style(c, MathStyle::select(c, variant, bold, italic)))
        .collect();

    let tag = if styled_text.chars().all(|c| c.is_ascii_digit() || c == '.') {
        tag::mn
    } else {
        tag::mtext
    };

    Ok(HtmlElem::new(tag)
        .with_body(Some(TextElem::packed(styled_text)))
        .pack()
        .spanned(elem.span()))
}
