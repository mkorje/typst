use codex::styling::{to_style, MathStyle};
use comemo::{Track, Tracked, TrackedMut};
use ecow::{eco_format, EcoString};
use typst_library::diag::{warning, At, SourceResult};
use typst_library::engine::{Engine, Route, Sink, Traced};
use typst_library::foundations::{
    Content, NativeElement, Packed, StyleChain, SymbolElem,
};
use typst_library::introspection::{
    Introspector, Locator, LocatorLink, SplitLocator, TagElem,
};
use typst_library::layout::{HElem, Spacing};
use typst_library::math::*;
use typst_library::routines::{Arenas, Pair, RealizationKind, Routines};
use typst_library::text::{LinebreakElem, SpaceElem, TextElem};
use typst_library::World;
use typst_syntax::Span;
use typst_utils::default_math_class;
use unicode_math_class::MathClass;

use crate::fragment::html_fragment;
use crate::{
    attr::mathml as attr, css, tag::mathml as tag, HtmlAttrs, HtmlElem, HtmlElement,
    HtmlNode,
};

/// Produce MathML nodes from content.
#[typst_macros::time(name = "mathml fragment")]
pub fn mathml_fragment(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
) -> SourceResult<Vec<HtmlNode>> {
    mathml_fragment_impl(
        engine.routines,
        engine.world,
        engine.introspector,
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        content,
        locator.track(),
        styles,
    )
}

/// The cached, internal implementation of [`mathml_fragment`].
#[comemo::memoize]
#[allow(clippy::too_many_arguments)]
fn mathml_fragment_impl(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    introspector: Tracked<Introspector>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
) -> SourceResult<Vec<HtmlNode>> {
    let link = LocatorLink::new(locator);
    let mut locator = Locator::link(&link).split();
    let mut engine = Engine {
        routines,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    engine.route.check_html_depth().at(content.span())?;

    let arenas = Arenas::default();
    let children = (engine.routines.realize)(
        RealizationKind::Math,
        &mut engine,
        &mut locator,
        &arenas,
        content,
        styles,
    )?;

    convert_to_nodes(&mut engine, &mut locator, children.iter().copied())
}

/// Converts realized content into MathML nodes.
fn convert_to_nodes<'a>(
    engine: &mut Engine,
    locator: &mut SplitLocator,
    children: impl IntoIterator<Item = Pair<'a>>,
) -> SourceResult<Vec<HtmlNode>> {
    let mut output = Vec::new();
    for (child, styles) in children {
        handle(engine, child, locator, styles, &mut output)?;
    }
    Ok(output)
}

/// Convert one element into HTML node(s).
fn handle(
    engine: &mut Engine,
    child: &Content,
    locator: &mut SplitLocator,
    styles: StyleChain,
    output: &mut Vec<HtmlNode>,
) -> SourceResult<()> {
    if let Some(elem) = child.to_packed::<TagElem>() {
        output.push(HtmlNode::Tag(elem.tag.clone()));
    } else if let Some(elem) = child.to_packed::<HElem>() {
        // show_h(elem, engine, styles)
    } else if let Some(elem) = child.to_packed::<SpaceElem>() {
    } else if let Some(elem) = child.to_packed::<LinebreakElem>() {
    } else if let Some(elem) = child.to_packed::<SymbolElem>() {
        output.push(show_symbol(elem, styles).into());
    } else if let Some(elem) = child.to_packed::<TextElem>() {
        // show_text(elem, engine, styles)
    } else if let Some(elem) = child.to_packed::<OpElem>() {
        // Ideally this should use the text font, but the spec recommends mi...
        // But we want mo so that we can use movablelimits!
        // Quite the pickle
        let text = group(mathml_fragment(
            engine,
            &elem.text,
            locator.next(&elem.text.span()),
            styles,
        )?);
        let limits = elem.limits.get(styles);
        output.push(
            HtmlElement::new(tag::mo)
                .with_children(vec![text])
                .with_attr(attr::movablelimits, eco_format!("{}", limits))
                .with_attr(attr::lspace, "0em")
                .with_attr(attr::rspace, "0em")
                .spanned(elem.span())
                .into(),
        );
    } else if let Some(elem) = child.to_packed::<RootElem>() {
        let radicand = mathml_fragment(
            engine,
            &elem.radicand,
            locator.next(&elem.radicand.span()),
            styles,
        )?;
        output.push(
            if let Some(index) = elem.index.get_ref(styles) {
                let index = group(mathml_fragment(
                    engine,
                    index,
                    locator.next(&index.span()),
                    styles,
                )?);
                HtmlElement::new(tag::mroot).with_children(vec![group(radicand), index])
            } else {
                HtmlElement::new(tag::msqrt).with_children(radicand)
            }
            .spanned(elem.span())
            .into(),
        );
    } else if let Some(elem) = child.to_packed::<LrElem>() {
        // Need to set class of opening and closing...
        output.push(
            HtmlElement::new(tag::mrow)
                .with_children(mathml_fragment(
                    engine,
                    &elem.body,
                    locator.next(&elem.span()),
                    styles,
                )?)
                .spanned(elem.span())
                .into(),
        );
    } else if let Some(elem) = child.to_packed::<MidElem>() {
        // <mo fence="true" form="infix" stretchy="true"></mo>
    } else if let Some(elem) = child.to_packed::<FracElem>() {
        let num = group(mathml_fragment(
            engine,
            &elem.num,
            locator.next(&elem.num.span()),
            styles,
        )?);
        let denom = group(mathml_fragment(
            engine,
            &elem.denom,
            locator.next(&elem.denom.span()),
            styles,
        )?);
        output.push(
            HtmlElement::new(tag::mfrac)
                .with_children(vec![num, denom])
                .spanned(elem.span())
                .into(),
        )
    } else if let Some(elem) = child.to_packed::<BinomElem>() {
        // let upper = group(mathml_fragment(
        //     engine,
        //     &elem.upper,
        //     locator.next(&elem.upper.span()),
        //     styles,
        // )?);
        // let lower = group(mathml_fragment(
        //     engine,
        //     &elem.lower,
        //     locator.next(&elem.lower.span()),
        //     styles,
        // )?);
        // output.push(
        //     HtmlElement::new(tag::mfrac)
        //         .with_children(vec![upper, lower])
        //         .with_attr(attr::linethickness, "0")
        //         .into(),
        // );
        // let lower = show_equation(
        //     &Content::sequence(
        //         elem.lower
        //             .iter()
        //             .flat_map(|a| [SymbolElem::packed(','), a.clone()])
        //             .skip(1),
        //     ),
        //     engine,
        //     styles,
        // )?;
    } else if let Some(elem) = child.to_packed::<AccentElem>() {
        let accent = elem.accent;
        let (tag, attr) = if accent.is_bottom() {
            (tag::munder, attr::accentunder)
        } else {
            (tag::mover, attr::accent)
        };
        let base = group(mathml_fragment(
            engine,
            &elem.base,
            locator.next(&elem.base.span()),
            styles,
        )?);
        let accent = HtmlElement::new(tag::mo)
            .with_children(vec![HtmlNode::text(accent.0, elem.span())])
            .into();
        output.push(
            HtmlElement::new(tag)
                .with_attr(attr, "true")
                .with_children(vec![base, accent])
                .spanned(elem.span())
                .into(),
        );
    } else if let Some(elem) = child.to_packed::<AttachElem>() {
        // } else if let Some(elem) = child.to_packed::<PrimesElem>() {
        // } else if let Some(elem) = child.to_packed::<ScriptsElem>() {
        // } else if let Some(elem) = child.to_packed::<LimitsElem>() {
        // } else if let Some(elem) = child.to_packed::<StretchElem>() {
    } else if let Some(elem) = child.to_packed::<UnderbraceElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⏟',
        //     Position::Under,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<OverbraceElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⏞',
        //     Position::Over,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<UnderbracketElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⎵',
        //     Position::Under,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<OverbracketElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⎴',
        //     Position::Over,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<UnderparenElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⏝',
        //     Position::Under,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<OverparenElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⏜',
        //     Position::Over,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<UndershellElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⏡',
        //     Position::Under,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<OvershellElem>() {
        // show_underover(
        //     engine,
        //     styles,
        //     &elem.body,
        //     elem.annotation.get_ref(styles),
        //     '⏠',
        //     Position::Over,
        //     elem.span(),
        // )
    } else if let Some(elem) = child.to_packed::<MatElem>() {
        // show_mat(elem, engine, styles)
    } else if let Some(elem) = child.to_packed::<VecElem>() {
        // show_vec(elem, engine, styles)
    } else if let Some(elem) = child.to_packed::<CasesElem>() {
        // show_cases(elem, engine, styles)
        // } else if let Some(elem) = child.to_packed::<BoxElem>() {
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
        // output.extend(html_fragment(engine, child, locator.next(&child.span()), styles)?);
    }
    Ok(())
}

fn group(mut output: Vec<HtmlNode>) -> HtmlNode {
    if output.len() != 1 {
        HtmlElement::new(tag::mrow).with_children(output).into()
    } else {
        output.remove(0)
    }
}

fn show_h(elem: &Packed<HElem>, _: &mut Engine, _: StyleChain) -> SourceResult<Content> {
    let mut inline = css::Properties::new();
    match elem.amount {
        Spacing::Rel(rel) => inline.push("width", css::rel(rel)),
        Spacing::Fr(_) => {}
    }
    Ok(HtmlElem::new(tag::mspace).with_styles(inline).pack())
}

fn show_symbol(elem: &Packed<SymbolElem>, styles: StyleChain) -> HtmlElement {
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
            attrs.push(attr::separator, "true");
            tag::mo
        }
        MathClass::Fence => {
            attrs.push(attr::fence, "true");
            tag::mo
        }
        MathClass::Large => {
            attrs.push(attr::largeop, "true");
            tag::mo
        }
        MathClass::Opening => {
            attrs.push(attr::fence, "true");
            attrs.push(attr::form, "prefix");
            tag::mo
        }
        MathClass::Closing => {
            attrs.push(attr::fence, "true");
            attrs.push(attr::form, "postfix");
            tag::mo
        }
    };

    HtmlElement {
        tag,
        attrs,
        children: vec![HtmlNode::text(text, elem.span())],
        span: elem.span(),
    }
}

fn show_text(
    elem: &Packed<TextElem>,
    _: &mut Engine,
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

/*fn show_attach(
    elem: &Packed<AttachElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let merged = elem.merge_base();
    let elem = merged.as_ref().unwrap_or(elem);

    let base = show_equation(&elem.base, engine, styles)?;

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
    }
}

fn show_underover(
    engine: &mut Engine,
    styles: StyleChain,
    base: &Content,
    annotation: &Option<Content>,
    c: char,
    position: Position,
    span: Span,
) -> SourceResult<Content> {
    let base = show_equation(base, engine, styles)?;
    // Don't need to specify stretchy, all 10 characters are listed as stretchy
    // in the operator dictionary.
    // But they don't have the correct math class to end up as mo, so we do it manually here...
    let glyph = HtmlElem::new(tag::mo).with_body(Some(TextElem::packed(c))).pack();

    //TODO: should these have accent=true or false?
    let tag = match position {
        Position::Under => tag::munder,
        Position::Over => tag::mover,
    };

    let underover = if let Some(annotation) = annotation {
        let annotation = show_equation(annotation, engine, styles)?;
        HtmlElem::new(tag).with_body(Some(glyph + annotation)).pack()
    } else {
        glyph
    };

    Ok(HtmlElem::new(tag)
        .with_body(Some(base + underover))
        .pack()
        .spanned(span))
}

fn show_mat(
    elem: &Packed<MatElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let delim = elem.delim.get(styles);
    let mut row = vec![];

    if let Some(open_c) = delim.open() {
        let open_styles = EquationElem::class.set(Some(MathClass::Opening)).wrap();
        row.push(show_equation(
            &SymbolElem::packed(open_c),
            engine,
            styles.chain(&open_styles),
        )?);
    }

    row.push(
        HtmlElem::new(tag::mtable)
            .with_body(Some(
                elem.rows
                    .iter()
                    .map(|row| {
                        Ok(HtmlElem::new(tag::mtr)
                            .with_body(Some(
                                row.into_iter()
                                    .map(|cell| {
                                        Ok(HtmlElem::new(tag::mtd)
                                            .with_body(Some(show_equation(
                                                &cell, engine, styles,
                                            )?))
                                            .pack())
                                    })
                                    .collect::<SourceResult<_>>()?,
                            ))
                            .pack())
                    })
                    .collect::<SourceResult<_>>()?,
            ))
            .pack(),
    );

    if let Some(close_c) = delim.close() {
        let close_styles = EquationElem::class.set(Some(MathClass::Closing)).wrap();
        row.push(show_equation(
            &SymbolElem::packed(close_c),
            engine,
            styles.chain(&close_styles),
        )?);
    }

    Ok(HtmlElem::new(tag::mrow)
        .with_body(Some(Content::sequence(row)))
        .pack()
        .spanned(elem.span()))
}

fn show_vec(
    elem: &Packed<VecElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let delim = elem.delim.get(styles);
    let mut row = vec![];

    if let Some(open_c) = delim.open() {
        let open_styles = EquationElem::class.set(Some(MathClass::Opening)).wrap();
        row.push(show_equation(
            &SymbolElem::packed(open_c),
            engine,
            styles.chain(&open_styles),
        )?);
    }

    row.push(
        HtmlElem::new(tag::mtable)
            .with_body(Some(
                elem.children
                    .iter()
                    .map(|cell| {
                        Ok(HtmlElem::new(tag::mtr)
                            .with_body(Some(
                                HtmlElem::new(tag::mtd)
                                    .with_body(Some(show_equation(
                                        &cell, engine, styles,
                                    )?))
                                    .pack(),
                            ))
                            .pack())
                    })
                    .collect::<SourceResult<_>>()?,
            ))
            .pack(),
    );

    if let Some(close_c) = delim.close() {
        let close_styles = EquationElem::class.set(Some(MathClass::Closing)).wrap();
        row.push(show_equation(
            &SymbolElem::packed(close_c),
            engine,
            styles.chain(&close_styles),
        )?);
    }

    Ok(HtmlElem::new(tag::mrow)
        .with_body(Some(Content::sequence(row)))
        .pack()
        .spanned(elem.span()))
}

fn show_cases(
    elem: &Packed<CasesElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let delim = elem.delim.get(styles);
    let mut row = vec![];

    if !elem.reverse.get(styles) {
        if let Some(open_c) = delim.open() {
            let open_styles = EquationElem::class.set(Some(MathClass::Opening)).wrap();
            row.push(show_equation(
                &SymbolElem::packed(open_c),
                engine,
                styles.chain(&open_styles),
            )?);
        }
    }

    row.push(
        HtmlElem::new(tag::mtable)
            .with_body(Some(
                elem.children
                    .iter()
                    .map(|cell| {
                        Ok(HtmlElem::new(tag::mtr)
                            .with_body(Some(
                                HtmlElem::new(tag::mtd)
                                    .with_body(Some(show_equation(
                                        &cell, engine, styles,
                                    )?))
                                    .pack(),
                            ))
                            .pack())
                    })
                    .collect::<SourceResult<_>>()?,
            ))
            .pack(),
    );

    if elem.reverse.get(styles) {
        if let Some(close_c) = delim.close() {
            let close_styles = EquationElem::class.set(Some(MathClass::Closing)).wrap();
            row.push(show_equation(
                &SymbolElem::packed(close_c),
                engine,
                styles.chain(&close_styles),
            )?);
        }
    }

    Ok(HtmlElem::new(tag::mrow)
        .with_body(Some(Content::sequence(row)))
        .pack()
        .spanned(elem.span()))
}
*/
