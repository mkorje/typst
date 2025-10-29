use typst_syntax::Span;

use crate::diag::SourceResult;
use crate::foundations::{Content, NativeElement, Packed, StyleChain, elem, func};
use crate::math::{
    EquationElem, MathContext, MathSize, Mathy, RadicalItem, style_cramped,
};

/// A square root.
///
/// ```example
/// $ sqrt(3 - 2 sqrt(2)) = sqrt(2) - 1 $
/// ```
#[func(title = "Square Root")]
pub fn sqrt(
    span: Span,
    /// The expression to take the square root of.
    radicand: Content,
) -> Content {
    RootElem::new(radicand).pack().spanned(span)
}

/// A general root.
///
/// ```example
/// $ root(3, x) $
/// ```
#[elem(Mathy)]
pub struct RootElem {
    /// Which root of the radicand to take.
    #[positional]
    pub index: Option<Content>,

    /// The expression to take the root of.
    #[required]
    pub radicand: Content,
}

pub fn resolve_root(
    elem: &Packed<RootElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let radicand = {
        let cramped = style_cramped();
        let styles = styles.chain(&cramped);
        ctx.resolve_into_run(&elem.radicand, styles)?
    };
    let index = {
        let sscript = EquationElem::size.set(MathSize::ScriptScript).wrap();
        let styles = styles.chain(&sscript);
        elem.index
            .get_ref(styles)
            .as_ref()
            .map(|elem| ctx.resolve_into_run(elem, styles))
            .transpose()?
    };
    ctx.push(RadicalItem::new(radicand, index, styles));
    Ok(())
}
