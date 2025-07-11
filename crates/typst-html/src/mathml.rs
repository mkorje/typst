use codex::styling::{to_style, MathStyle};
use ecow::{eco_format, EcoString};
use typst_library::diag::{warning, SourceResult};
use typst_library::engine::Engine;
use typst_library::foundations::{
    Content, NativeElement, Packed, StyleChain, SymbolElem,
};
use typst_library::introspection::Locator;
use typst_library::layout::{HElem, Spacing};
use typst_library::math::*;
use typst_library::routines::{Arenas, RealizationKind};
use typst_library::text::{LinebreakElem, SpaceElem, TextElem};
use typst_syntax::Span;
use typst_utils::default_math_class;
use unicode_math_class::MathClass;

use crate::{attr::mathml as attr, css, tag::mathml as tag, HtmlAttrs, HtmlElem};

pub fn show_equation(
    content: &Content,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    // TODO: Probably better to pass around a mutable vec instead of recursively returning content.
    // TODO: Maybe should be chaining sup/sub/cramped etc. to stylechain as in
    // typst-layout for math, so that introspection and all will work...?
    // TODO: There is a lot of overlap with typst-layout for math. Maybe it
    // could work to "merge" the two somewhat, and have a central interface in
    // a new crate typst-math? No idea if this will turn out great...
    // Could then try use "normal" show rules to display the elements, so that
    // it gets done during realization automatically and better integrates with
    // everything else (non-math stuff).

    // The locator is not used by HTML export, so we can just fabricate one.
    let mut locator = Locator::root().split();
    let arenas = Arenas::default();
    let pairs = (engine.routines.realize)(
        RealizationKind::Math,
        engine,
        &mut locator,
        &arenas,
        content,
        styles,
    )?;

    if let [(elem, styles)] = pairs[..] {
        show_realized(elem, engine, styles)
    } else {
        let mut elems = vec![];
        for (elem, styles) in pairs {
            elems.push(show_realized(elem, engine, styles)?)
        }
        println!("{:?}\n", elems);
        Ok(HtmlElem::new(tag::mrow)
            .with_body(Some(Content::sequence(elems)))
            .pack())
    }
}

fn show_realized(
    elem: &Content,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    if let Some(elem) = elem.to_packed::<HElem>() {
        show_h(elem, engine, styles)
    // } else if let Some(elem) = elem.to_packed::<TagElem>() {
    } else if let Some(elem) = elem.to_packed::<SpaceElem>() {
        Ok(Content::empty())
    // } else if let Some(elem) = elem.to_packed::<LinebreakElem>() {
    // } else if let Some(elem) = elem.to_packed::<BoxElem>() {
    // } else if let Some(elem) = elem.to_packed::<AlignPointElem>() {
    // } else if let Some(elem) = elem.to_packed::<ClassElem>() {
    } else if let Some(elem) = elem.to_packed::<SymbolElem>() {
        show_symbol(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<TextElem>() {
        show_text(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<OpElem>() {
        show_op(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<RootElem>() {
        show_root(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<LrElem>() {
        show_lr(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<MidElem>() {
        show_mid(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<FracElem>() {
        show_frac(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<BinomElem>() {
        show_binom(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<AccentElem>() {
        show_accent(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<AttachElem>() {
        show_attach(elem, engine, styles)
    // } else if let Some(elem) = elem.to_packed::<PrimesElem>() {
    // } else if let Some(elem) = elem.to_packed::<ScriptsElem>() {
    // } else if let Some(elem) = elem.to_packed::<LimitsElem>() {
    // } else if let Some(elem) = elem.to_packed::<StretchElem>() {
    } else if let Some(elem) = elem.to_packed::<UnderbraceElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⏟',
            Position::Under,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<OverbraceElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⏞',
            Position::Over,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<UnderbracketElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⎵',
            Position::Under,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<OverbracketElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⎴',
            Position::Over,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<UnderparenElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⏝',
            Position::Under,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<OverparenElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⏜',
            Position::Over,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<UndershellElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⏡',
            Position::Under,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<OvershellElem>() {
        show_underover(
            engine,
            styles,
            &elem.body,
            elem.annotation.get_ref(styles),
            '⏠',
            Position::Over,
            elem.span(),
        )
    } else if let Some(elem) = elem.to_packed::<MatElem>() {
        show_mat(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<VecElem>() {
        show_vec(elem, engine, styles)
    } else if let Some(elem) = elem.to_packed::<CasesElem>() {
        show_cases(elem, engine, styles)
    } else if elem.can::<dyn Mathy>() {
        // CancelElem, UnderlineElem, OverlineElem
        engine.sink.warn(warning!(
            elem.span(),
            "{} was ignored during HTML export",
            elem.elem().name()
        ));
        Ok(Content::empty())
    } else {
        println!("external: {:?}\n", elem.elem().name());
        Ok(Content::empty())
        // Ok(elem.clone())
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

fn show_symbol(
    elem: &Packed<SymbolElem>,
    _: &mut Engine,
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

    Ok(HtmlElem::new(tag)
        .with_body(Some(TextElem::packed(text)))
        .with_attrs(attrs)
        .pack()
        .spanned(elem.span()))
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

fn show_op(
    elem: &Packed<OpElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    // Ideally this should use the text font, but the spec recommends mi...
    // But we want mo so that we can use movablelimits!
    // Quite the pickle
    let text = show_equation(&elem.text, engine, styles)?;
    let limits = elem.limits.get(styles);
    Ok(HtmlElem::new(tag::mo)
        .with_body(Some(text))
        .with_attr(attr::movablelimits, eco_format!("{}", limits))
        .with_attr(attr::lspace, "0em")
        .with_attr(attr::rspace, "0em")
        .pack()
        .spanned(elem.span()))
}

fn show_root(
    elem: &Packed<RootElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let radicand = show_equation(&elem.radicand, engine, styles)?;
    let index = elem.index.get_ref(styles);
    if let Some(index) = index {
        let index = show_equation(index, engine, styles)?;
        Ok(HtmlElem::new(tag::mroot)
            .with_body(Some(radicand + index))
            .pack()
            .spanned(elem.span()))
    } else {
        Ok(HtmlElem::new(tag::msqrt)
            .with_body(Some(radicand))
            .pack()
            .spanned(elem.span()))
    }
}

fn show_lr(
    elem: &Packed<LrElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    // Need to set class of opening and closing...
    show_equation(&elem.body, engine, styles)
}

fn show_mid(
    elem: &Packed<MidElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    // <mo fence="true" form="infix" stretchy="true"></mo>
    show_equation(&elem.body, engine, styles)
}

fn show_frac(
    elem: &Packed<FracElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let num = show_equation(&elem.num, engine, styles)?;
    let denom = show_equation(&elem.denom, engine, styles)?;
    Ok(HtmlElem::new(tag::mfrac)
        .with_body(Some(num + denom))
        .pack()
        .spanned(elem.span()))
}

fn show_binom(
    elem: &Packed<BinomElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let upper = show_equation(&elem.upper, engine, styles)?;
    let lower = show_equation(
        &Content::sequence(
            elem.lower
                .iter()
                .flat_map(|a| [SymbolElem::packed(','), a.clone()])
                .skip(1),
        ),
        engine,
        styles,
    )?;

    let mut seq = vec![];
    seq.push(show_equation(&SymbolElem::packed('('), engine, styles)?);
    seq.push(
        HtmlElem::new(tag::mfrac)
            .with_body(Some(upper + lower))
            .with_attr(attr::linethickness, "0")
            .pack(),
    );
    seq.push(show_equation(&SymbolElem::packed(')'), engine, styles)?);

    Ok(HtmlElem::new(tag::mrow)
        .with_body(Some(Content::sequence(seq)))
        .pack()
        .spanned(elem.span()))
}

fn show_accent(
    elem: &Packed<AccentElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    let accent = elem.accent;
    let (tag, attr) = if accent.is_bottom() {
        (tag::munder, attr::accentunder)
    } else {
        (tag::mover, attr::accent)
    };

    let base = show_equation(&elem.base, engine, styles)?;
    let accent = HtmlElem::new(tag::mo)
        .with_body(Some(TextElem::packed(accent.0)))
        .pack();

    Ok(HtmlElem::new(tag)
        .with_body(Some(base + accent))
        .with_attr(attr, "true")
        .pack()
        .spanned(elem.span()))
}

fn show_primes(
    elem: &Packed<PrimesElem>,
    engine: &mut Engine,
    styles: StyleChain,
) -> SourceResult<Content> {
    // <mo></mo>
    todo!()
}

fn show_attach(
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
