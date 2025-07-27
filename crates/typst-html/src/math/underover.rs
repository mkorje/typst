use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, StyleChain};
use typst_library::introspection::SplitLocator;
use typst_library::math::{
    OverbraceElem, OverbracketElem, OverparenElem, OvershellElem, Position,
    UnderbraceElem, UnderbracketElem, UnderparenElem, UndershellElem,
    style_for_subscript, style_for_superscript,
};
use typst_library::text::TextElem;
use typst_syntax::Span;

use super::{group, show_equation};
use crate::{HtmlElem, attr::mathml as attr, tag::mathml as tag};

pub fn show_underbrace(
    elem: &Packed<UnderbraceElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏟',
        Position::Under,
        elem.span(),
    )
}

pub fn show_overbrace(
    elem: &Packed<OverbraceElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏞',
        Position::Over,
        elem.span(),
    )
}

pub fn show_underbracket(
    elem: &Packed<UnderbracketElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⎵',
        Position::Under,
        elem.span(),
    )
}

pub fn show_overbracket(
    elem: &Packed<OverbracketElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⎴',
        Position::Over,
        elem.span(),
    )
}

pub fn show_underparen(
    elem: &Packed<UnderparenElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏝',
        Position::Under,
        elem.span(),
    )
}

pub fn show_overparen(
    elem: &Packed<OverparenElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏜',
        Position::Over,
        elem.span(),
    )
}

pub fn show_undershell(
    elem: &Packed<UndershellElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏡',
        Position::Under,
        elem.span(),
    )
}

pub fn show_overshell(
    elem: &Packed<OvershellElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_underover(
        engine,
        locator,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏠',
        Position::Over,
        elem.span(),
    )
}

#[allow(clippy::too_many_arguments)]
fn show_underover(
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
    base: &Content,
    annotation: &Option<Content>,
    c: char,
    position: Position,
    span: Span,
) -> SourceResult<Content> {
    let base = group(show_equation(base, engine, locator, styles)?);
    let glyph = HtmlElem::new(tag::mo).with_body(Some(TextElem::packed(c))).pack();

    let (tag, attr) = match position {
        Position::Under => (tag::munder, attr::accentunder),
        Position::Over => (tag::mover, attr::accent),
    };

    let underover = HtmlElem::new(tag)
        .with_body(Some(base + glyph))
        .with_attr(attr, "true")
        .pack();

    let Some(annotation) = annotation else {
        return Ok(underover.spanned(span));
    };

    let under_style = style_for_subscript(styles);
    let over_style = style_for_superscript(styles);
    let styles = match position {
        Position::Under => styles.chain(&under_style),
        Position::Over => styles.chain(&over_style),
    };

    let annotation = group(show_equation(annotation, engine, locator, styles)?);
    Ok(HtmlElem::new(tag)
        .with_body(Some(underover + annotation))
        .pack()
        .spanned(span))
}
