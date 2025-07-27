use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, StyleChain};
use typst_library::introspection::SplitLocator;
use typst_library::math::{AccentElem, style_cramped};
use typst_library::text::TextElem;

use super::{group, show_equation};
use crate::{HtmlElem, attr::mathml as attr, tag::mathml as tag};

pub fn show_accent(
    elem: &Packed<AccentElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let accent = elem.accent;
    let (tag, attr) = if accent.is_bottom() {
        (tag::munder, attr::accentunder)
    } else {
        (tag::mover, attr::accent)
    };

    // TODO: What to do about dtls OpenType feature?
    let cramped = style_cramped();
    let base = group(show_equation(&elem.base, engine, locator, styles.chain(&cramped))?);

    let accent = HtmlElem::new(tag::mo)
        .with_body(Some(TextElem::packed(accent.0)))
        .pack();

    Ok(HtmlElem::new(tag)
        .with_body(Some(base + accent))
        .with_attr(attr, "true")
        .pack()
        .spanned(elem.span()))
}
