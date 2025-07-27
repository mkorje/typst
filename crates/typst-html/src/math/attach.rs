use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, StyleChain};
use typst_library::introspection::SplitLocator;
use typst_library::math::{
    AttachElem, LimitsElem, PrimesElem, ScriptsElem, style_for_denominator,
};
use typst_library::text::TextElem;

use super::show_equation;
use crate::{HtmlElem, tag::mathml as tag};

pub fn show_attach(
    elem: &Packed<AttachElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    todo!();

    /*let base = show_equation(&elem.base, engine, styles)?;

    let tl = elem
        .tl
        .get_ref(styles)
        .as_ref()
        .map(|tl| show_equation(&tl, engine, styles))
        .transpose()?;
    let t = elem
        .t
        .get_ref(styles)
        .as_ref()
        .map(|t| show_equation(&t, engine, styles))
        .transpose()?;
    let tr = elem
        .tr
        .get_ref(styles)
        .as_ref()
        .map(|tr| show_equation(&tr, engine, styles))
        .transpose()?;
    let bl = elem
        .bl
        .get_ref(styles)
        .as_ref()
        .map(|bl| show_equation(&bl, engine, styles))
        .transpose()?;
    let b = elem
        .b
        .get_ref(styles)
        .as_ref()
        .map(|b| show_equation(&b, engine, styles))
        .transpose()?;
    let br = elem
        .br
        .get_ref(styles)
        .as_ref()
        .map(|br| show_equation(&br, engine, styles))
        .transpose()?;

    let (t, tr) = (None::<Content>, t);
    let (b, br) = (None::<Content>, b);

    match (tl, t, tr, bl, b, br) {
        (None, None, None, None, None, Some(br)) => Ok(HtmlElem::new(tag::msub)
            .with_body(Some(base + br))
            .pack()
            .spanned(elem.span())),
        (None, None, Some(tr), None, None, None) => Ok(HtmlElem::new(tag::msup)
            .with_body(Some(base + tr))
            .pack()
            .spanned(elem.span())),
        (None, None, Some(tr), None, None, Some(br)) => Ok(HtmlElem::new(tag::msubsup)
            .with_body(Some(base + br + tr))
            .pack()
            .spanned(elem.span())),
        _ => Ok(base),
    }*/
}

pub fn show_primes(
    elem: &Packed<PrimesElem>,
    _: &mut Engine,
    _: &mut SplitLocator,
    _: StyleChain,
) -> SourceResult<Content> {
    let body = match elem.count {
        count @ 1..=4 => {
            let c = match count {
                1 => '′',
                2 => '″',
                3 => '‴',
                4 => '⁗',
                _ => unreachable!(),
            };
            TextElem::packed(c)
        }
        count => {
            // TODO: Should this be one <mo> or multiple?
            TextElem::packed("′".repeat(count))
        }
    };
    Ok(HtmlElem::new(tag::mo)
        .with_body(Some(body))
        .pack()
        .spanned(elem.span()))
}

pub fn show_scripts(
    elem: &Packed<ScriptsElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    todo!()
}

pub fn show_limits(
    elem: &Packed<LimitsElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    todo!()
}
