use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, StyleChain};
use typst_library::introspection::SplitLocator;
use typst_library::math::{EquationElem, MathSize, RootElem, style_cramped};

use super::{group, show_equation};
use crate::{HtmlElem, tag::mathml as tag};

pub fn show_root(
    elem: &Packed<RootElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let cramped = style_cramped();
    let radicand =
        show_equation(&elem.radicand, engine, locator, styles.chain(&cramped))?;

    let Some(index) = elem.index.get_ref(styles) else {
        return Ok(HtmlElem::new(tag::msqrt)
            .with_body(Some(radicand))
            .pack()
            .spanned(elem.span()));
    };

    let sscript = EquationElem::size.set(MathSize::ScriptScript).wrap();
    let index = group(show_equation(index, engine, locator, styles.chain(&sscript))?);
    Ok(HtmlElem::new(tag::mroot)
        .with_body(Some(group(radicand) + index))
        .pack()
        .spanned(elem.span()))
}
