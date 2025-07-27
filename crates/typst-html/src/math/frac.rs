use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{
    Content, NativeElement, Packed, StyleChain, SymbolElem,
};
use typst_library::introspection::SplitLocator;
use typst_library::math::{
    BinomElem, FracElem, style_for_denominator, style_for_numerator,
};
use typst_library::text::TextElem;
use typst_syntax::Span;

use super::{group, show_equation};
use crate::{HtmlElem, attr::mathml as attr, tag::mathml as tag};

pub fn show_frac(
    elem: &Packed<FracElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_frac_like(
        engine,
        locator,
        styles,
        &elem.num,
        std::slice::from_ref(&elem.denom),
        false,
        elem.span(),
    )
}

pub fn show_binom(
    elem: &Packed<BinomElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    show_frac_like(engine, locator, styles, &elem.upper, &elem.lower, true, elem.span())
}

pub fn show_frac_like(
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
    num: &Content,
    denom: &[Content],
    binom: bool,
    span: Span,
) -> SourceResult<Content> {
    let num_style = style_for_numerator(styles);
    let num = group(show_equation(num, engine, locator, styles.chain(&num_style))?);

    let denom_style = style_for_denominator(styles);
    let denom = group(show_equation(
        &Content::sequence(
            denom
                .iter()
                .flat_map(|a| [SymbolElem::packed(','), a.clone()])
                .skip(1),
        ),
        engine,
        locator,
        styles.chain(&denom_style),
    )?);

    let frac = HtmlElem::new(tag::mfrac)
        .with_body(Some(num + denom))
        .with_optional_attr(attr::linethickness, binom.then_some("0"))
        .pack();

    if !binom {
        return Ok(frac.spanned(span));
    }

    let open = HtmlElem::new(tag::mo).with_body(Some(TextElem::packed('('))).pack();
    let close = HtmlElem::new(tag::mo).with_body(Some(TextElem::packed(')'))).pack();
    Ok(HtmlElem::new(tag::mrow)
        .with_body(Some(open + frac + close))
        .pack()
        .spanned(span))
}
