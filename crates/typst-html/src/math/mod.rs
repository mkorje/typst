mod accent;
mod attach;
mod frac;
mod mat;
mod root;
mod text;
mod underover;

use self::accent::*;
use self::attach::*;
use self::frac::*;
use self::mat::*;
use self::root::*;
use self::text::*;
use self::underover::*;

use typst_library::diag::{SourceResult, warning};
use typst_library::engine::Engine;
use typst_library::foundations::{
    Content, NativeElement, Packed, SequenceElem, StyleChain, SymbolElem,
};
use typst_library::introspection::{SplitLocator, TagElem};
use typst_library::layout::{HElem, Spacing};
use typst_library::math::*;
use typst_library::routines::{Arenas, RealizationKind};
use typst_library::text::{LinebreakElem, SpaceElem, TextElem};

use crate::{HtmlElem, css, tag::mathml as tag};

/// Produce MathML nodes from content.
pub fn show_equation(
    content: &Content,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let arenas = Arenas::default();
    let mut children = (engine.routines.realize)(
        RealizationKind::Math,
        engine,
        locator,
        &arenas,
        content,
        styles,
    )?;

    // Perform some preprocessing on the children.
    prepare(&arenas, &mut children);

    let mut output = Vec::new();
    for (child, styles) in children {
        handle(child, engine, locator, styles, &mut output)?;
    }

    Ok(Content::sequence(output))
}

/// Convert one element into HTML node(s).
fn handle(
    child: &Content,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
    output: &mut Vec<Content>,
) -> SourceResult<()> {
    if child.is::<TagElem>() {
        output.push(child.clone());
    } else if let Some(elem) = child.to_packed::<HElem>() {
        output.push(show_h(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<SpaceElem>() {
    } else if let Some(elem) = child.to_packed::<LinebreakElem>() {
    } else if let Some(elem) = child.to_packed::<SymbolElem>() {
        output.push(show_symbol(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<TextElem>() {
        output.push(show_text(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<OpElem>() {
        // Ideally this should use the text font, but the spec recommends mi...
        // But we want mo so that we can use movablelimits!
        // Quite the pickle
        // let text = group(mathml_fragment(
        //     engine,
        //     &elem.text,
        //     locator.next(&elem.text.span()),
        //     styles,
        // )?);
        // let limits = elem.limits.get(styles);
        // output.push(
        //     HtmlElement::new(tag::mo)
        //         .with_children(vec![text])
        //         .with_attr(attr::movablelimits, eco_format!("{}", limits))
        //         .with_attr(attr::lspace, "0em")
        //         .with_attr(attr::rspace, "0em")
        //         .spanned(elem.span())
        //         .into(),
        // );
    } else if let Some(elem) = child.to_packed::<StretchElem>() {
    } else if let Some(elem) = child.to_packed::<LrElem>() {
        // Need to set class of opening and closing...
        // output.push(
        //     HtmlElement::new(tag::mrow)
        //         .with_children(mathml_fragment(
        //             engine,
        //             &elem.body,
        //             locator.next(&elem.span()),
        //             styles,
        //         )?)
        //         .spanned(elem.span())
        //         .into(),
        // );
    } else if let Some(elem) = child.to_packed::<MidElem>() {
        // <mo fence="true" form="infix" stretchy="true"></mo>
    } else if let Some(elem) = child.to_packed::<AttachElem>() {
        output.push(show_attach(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<PrimesElem>() {
        output.push(show_primes(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<ScriptsElem>() {
        output.push(show_scripts(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<LimitsElem>() {
        output.push(show_limits(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<AccentElem>() {
        output.push(show_accent(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<RootElem>() {
        output.push(show_root(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<FracElem>() {
        output.push(show_frac(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<BinomElem>() {
        output.push(show_binom(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<UnderbraceElem>() {
        output.push(show_underbrace(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<OverbraceElem>() {
        output.push(show_overbrace(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<UnderbracketElem>() {
        output.push(show_underbracket(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<OverbracketElem>() {
        output.push(show_overbracket(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<UnderparenElem>() {
        output.push(show_underparen(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<OverparenElem>() {
        output.push(show_overparen(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<UndershellElem>() {
        output.push(show_undershell(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<OvershellElem>() {
        output.push(show_overshell(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<MatElem>() {
        output.push(show_mat(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<VecElem>() {
        output.push(show_vec(elem, engine, locator, styles)?);
    } else if let Some(elem) = child.to_packed::<CasesElem>() {
        output.push(show_cases(elem, engine, locator, styles)?);
    } else if child.can::<dyn Mathy>() {
        // CancelElem, UnderlineElem, OverlineElem, AlignPointElem, ClassElem
        engine.sink.warn(warning!(
            child.span(),
            "{} was ignored during MathML export",
            child.elem().name()
        ));
    } else {
        // Arbitrary content is not allowed, as per the HTML spec.
        // Only MathML token elements (mi, mo, mn, ms, and mtext),
        // when descendants of HTML elements, may contain
        // [phrasing content] from the HTML namespace.
        // https://html.spec.whatwg.org/#phrasing-content-2
        //
        // In MathML 3, nothing is allowed except MathML elements.
        // Further, nesting root-level math elements is disallowed.
        //
        // Since the math element is considered phrasing content,
        // it can be nested in MathML Core (as long as it is itself
        // within a MathML token element).
        engine.sink.warn(warning!(
            child.span(),
            "{} was ignored during MathML export",
            child.elem().name()
        ));
    }
    Ok(())
}

pub fn group(content: Content) -> Content {
    if let Some(sequence) = content.to_packed::<SequenceElem>()
        && sequence.children.len() > 1
    {
        let span = content.span();
        HtmlElem::new(tag::mrow).with_body(Some(content)).pack().spanned(span)
    } else {
        content
    }
}

fn show_h(
    elem: &Packed<HElem>,
    _: &mut Engine,
    _: &mut SplitLocator,
    _: StyleChain,
) -> SourceResult<Content> {
    let mut inline = css::Properties::new();
    match elem.amount {
        Spacing::Rel(rel) => inline.push("width", css::rel(rel)),
        Spacing::Fr(_) => {}
    }
    Ok(HtmlElem::new(tag::mspace)
        .with_styles(inline)
        .pack()
        .spanned(elem.span()))
}
