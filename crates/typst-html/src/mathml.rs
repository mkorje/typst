use ecow::{EcoVec, eco_format, eco_vec};
use typst_assets::mathml::*;
use typst_library::{
    diag::SourceResult,
    math::{GlyphItem, MathComponent, MathItem, MathRun, ScriptsItem},
};
use unicode_math_class::MathClass;

use crate::tag::mathml as tag;
use crate::{HtmlElement, HtmlNode};
use crate::{attr::mathml as attr, css};

pub fn handle_math(run: &MathRun) -> SourceResult<Vec<HtmlNode>> {
    if run.row_count() == 1 {
        convert_math_to_nodes(run)
    } else {
        // https://github.com/w3c/mathml-core/issues/156
        // style="justify-items: start/end; padding-inline: 0;"
        // text-align: center; for firefox
        let (cells, ncols) = run.alignments();
        let empty_cell: HtmlNode = HtmlElement::new(tag::mtd).into();
        let cells = cells
            .into_iter()
            .map(|row| {
                let mut new_row: Vec<HtmlNode> = row
                    .iter()
                    .enumerate()
                    .map(|(i, cell)| {
                        let side = if i % 2 == 0 { "end" } else { "start" };
                        let props = css::Properties::new()
                            .with("padding-inline", "0")
                            .with("justify-items", side)
                            .with("text-align", side);
                        Ok(HtmlElement::new(tag::mtd)
                            .with_children(convert_math_to_nodes(cell)?)
                            .with_styles(props)
                            .into())
                    })
                    .collect::<SourceResult<Vec<HtmlNode>>>()?;

                new_row.resize(ncols, empty_cell.clone());

                Ok(HtmlElement::new(tag::mtr).with_children(new_row).into())
            })
            .collect::<SourceResult<Vec<HtmlNode>>>()?;

        // old: no alignment points.
        // let rows = run
        //     .rows()
        //     .iter()
        //     .map(convert_math_to_nodes)
        //     .map(|nodes_result| {
        //         nodes_result.map(|nodes| {
        //             HtmlElement::new(tag::mtr)
        //                 .with_children(eco_vec![
        //                     HtmlElement::new(tag::mtd).with_children(nodes).into()
        //                 ])
        //                 .into()
        //         })
        //     })
        //     .collect::<SourceResult<EcoVec<HtmlNode>>>()?;
        let props = css::Properties::new().with("math-style", "normal");
        Ok(vec![
            HtmlElement::new(tag::mtable)
                .with_children(cells)
                .with_styles(props)
                .into(),
        ])
    }
}

fn convert_math_to_nodes(run: &MathRun) -> SourceResult<Vec<HtmlNode>> {
    let mut nodes = vec![];

    let mut items = run.iter().enumerate().peekable();
    while let Some((i, item)) = items.next() {
        match item {
            MathItem::Tag(tag) => nodes.push(HtmlNode::Tag(tag.clone())),
            MathItem::Space | MathItem::Linebreak => {}
            MathItem::Glyph(glyph) => {
                // TODO: keep track of current Form.
                // https://www.w3.org/TR/mathml-core/#dfn-algorithm-for-determining-the-form-of-an-embellished-operator
                let form = if i == 0 && items.peek().is_some() {
                    Form::Prefix
                } else if i != 0 && items.peek().is_none() {
                    Form::Postfix
                } else {
                    Form::Infix
                };

                nodes.push(handle_glyph(glyph, form));
            }
            MathItem::Group(group) => {
                let children = handle_math(&group.items)?;
                nodes.push(HtmlElement::new(tag::mrow).with_children(children).into());
            }

            MathItem::Radical(radical) => {
                let radicand = handle_math(&radical.radicand)?;
                let index =
                    radical.index.as_ref().map(handle_math).transpose()?.map(group);

                let (tag, children) = if let Some(index) = index {
                    (tag::mroot, vec![group(radicand), index])
                } else {
                    (tag::msqrt, radicand)
                };

                nodes.push(HtmlElement::new(tag).with_children(children).into());
            }

            MathItem::Fraction(fraction) => {
                let numerator = group(handle_math(&fraction.numerator)?);
                let denominator = group(handle_math(&fraction.denominator)?);
                let line = (!fraction.line).then_some("0");

                nodes.push(
                    HtmlElement::new(tag::mfrac)
                        .with_children(vec![numerator, denominator])
                        .with_optional_attr(attr::linethickness, line)
                        .into(),
                );
            }

            MathItem::Text(text) => {
                let tag = if text.text.chars().all(|c| c.is_ascii_digit() || c == '.') {
                    tag::mn
                } else {
                    tag::mtext
                };

                nodes.push(
                    HtmlElement::new(tag)
                        .with_children(eco_vec![HtmlNode::Text(
                            text.text.clone(),
                            text.span
                        )])
                        .into(),
                );
            }

            MathItem::Accent(accent) => {
                let (tag, attr) = if accent.is_bottom {
                    (tag::munder, attr::accentunder)
                } else {
                    (tag::mover, attr::accent)
                };

                let base = group(handle_math(&accent.base)?);

                // TODO: maybe only add this if the base is an i or j, or only add when disabling.
                // let dtls = if elem.dotless.get(styles) { "on" } else { "off" };
                // if !accent.is_bottom() {
                //     base.to_packed_mut::<HtmlElem>().unwrap().push_attr(
                //         crate::attr::style,
                //         eco_format!("font-feature-settings: 'dtls' {};", dtls),
                //     );
                // }

                // TODO: convert accent char to non-combining, then lookup with postfix (or infix?) form for stretchy
                // Should surpress "Text run starts with a composing character" warnings from validator.
                let accent = group(handle_math(&accent.accent)?);

                nodes.push(
                    HtmlElement::new(tag)
                        .with_children(vec![base, accent])
                        .with_attr(attr, "true")
                        .into(),
                );
            }

            MathItem::Table(table) => {
                let cells = table
                    .cells
                    .iter()
                    .map(|row| {
                        let cell_nodes = row
                            .iter()
                            .map(|cell| {
                                // let props =
                                //     css::Properties::new().with("padding", "0em 0.2em");
                                Ok(HtmlElement::new(tag::mtd)
                                    .with_children(handle_math(cell)?)
                                    // .with_styles(props)
                                    .into())
                            })
                            .collect::<SourceResult<EcoVec<HtmlNode>>>();

                        cell_nodes.map(|nodes| {
                            HtmlElement::new(tag::mtr).with_children(nodes).into()
                        })
                    })
                    .collect::<SourceResult<EcoVec<HtmlNode>>>()?;
                nodes.push(HtmlElement::new(tag::mtable).with_children(cells).into());
            }

            MathItem::Scripts(scripts) => nodes.push(handle_scripts(scripts)?),

            MathItem::Spacing(amount, _) => {
                nodes.push(
                    HtmlElement::new(tag::mspace)
                        .with_attr(attr::width, eco_format!("{}", css::length(*amount)))
                        .into(),
                );
            }

            // Ignored
            // TODO: align should be unreachable
            MathItem::SkewedFraction(_)
            | MathItem::External(_)
            | MathItem::Box(_)
            | MathItem::Cancel(_)
            | MathItem::Line(_)
            | MathItem::Primes(_)
            | MathItem::Align
            | _ => {}
        }
    }

    Ok(nodes)
}

fn group(mut nodes: Vec<HtmlNode>) -> HtmlNode {
    if nodes.len() == 1 {
        nodes.pop().unwrap()
    } else {
        HtmlElement::new(tag::mrow).with_children(nodes).into()
    }
}

fn handle_glyph(glyph: &MathComponent<GlyphItem>, initial_form: Form) -> HtmlNode {
    let mut form = None;
    let mut fence = false;
    let mut separator = false;
    let mut largeop = false;
    // TODO: lspace, rspace
    let tag = match glyph.props.class() {
        MathClass::Normal
        | MathClass::Alphabetic
        | MathClass::Special
        | MathClass::GlyphPart
        | MathClass::Space => tag::mi,
        MathClass::Relation => tag::mo,
        MathClass::Diacritic => {
            form = Some(Form::Postfix);
            tag::mo
        }
        MathClass::Binary => {
            form = Some(Form::Infix);
            tag::mo
        }
        MathClass::Vary | MathClass::Unary => {
            form = Some(Form::Prefix);
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
            largeop = true;
            tag::mo
        }
        MathClass::Opening => {
            fence = true;
            form = Some(Form::Prefix);
            tag::mo
        }
        MathClass::Closing => {
            fence = true;
            form = Some(Form::Postfix);
            tag::mo
        }
    };

    let info = get_operator_info(&glyph.text, form.unwrap_or(initial_form));
    let form = form.filter(|f| *f != initial_form).map(|f| match f {
        Form::Prefix => "prefix",
        Form::Infix => "infix",
        Form::Postfix => "postfix",
    });

    let fence = (fence != is_fence(&glyph.text)).then(|| eco_format!("{}", fence));
    let separator =
        (separator != is_separator(&glyph.text)).then(|| eco_format!("{}", separator));

    let largeop = (largeop != info.properties.contains(Properties::LARGEOP))
        .then(|| eco_format!("{}", largeop));
    // We don't use movablelimits as we handle the positioning ourselves.
    let movablelimits = (info.properties.contains(Properties::MOVABLELIMITS))
        .then(|| eco_format!("false"));

    // TODO: symmetric, minsize, maxsize
    let stretchy = glyph.stretch.is_some();
    let stretchy = (stretchy != info.properties.contains(Properties::STRETCHY))
        .then(|| eco_format!("{}", stretchy));

    let mathvariant = will_auto_transform(&glyph.text).then_some("normal");

    HtmlElement::new(tag)
        .with_children(eco_vec![HtmlNode::text(glyph.text.clone(), glyph.span)])
        .with_optional_attr(attr::form, form)
        .with_optional_attr(attr::fence, fence)
        .with_optional_attr(attr::separator, separator)
        .with_optional_attr(attr::largeop, largeop)
        .with_optional_attr(attr::movablelimits, movablelimits)
        .with_optional_attr(attr::stretchy, stretchy)
        .with_optional_attr(attr::mathvariant, mathvariant)
        .into()
}

fn handle_scripts(scripts: &ScriptsItem) -> SourceResult<HtmlNode> {
    let mut base = group(handle_math(&scripts.base)?);

    macro_rules! layout {
        ($content:ident) => {
            scripts
                .$content
                .as_ref()
                .map(|x| handle_math(&x))
                .transpose()?
                .map(group)
        };
    }

    let t = layout!(top);
    let tr = layout!(top_right);
    let tl = layout!(top_left);
    let b = layout!(bottom);
    let br = layout!(bottom_right);
    let bl = layout!(bottom_left);

    if let Some((tag, other_children)) = match (tl, tr, bl, br) {
        (None, None, None, None) => None,
        (None, None, None, Some(br)) => Some((tag::msub, vec![br])),
        (None, Some(tr), None, None) => Some((tag::msup, vec![tr])),
        (None, Some(tr), None, Some(br)) => Some((tag::msubsup, vec![br, tr])),
        (tl, tr, bl, br) => {
            let unwrap = |node: Option<HtmlNode>| {
                node.unwrap_or(HtmlElement::new(tag::mrow).into())
            };
            Some((
                tag::mmultiscripts,
                vec![
                    unwrap(br),
                    unwrap(tr),
                    HtmlElement::new(tag::mprescripts).into(),
                    unwrap(bl),
                    unwrap(tl),
                ],
            ))
        }
    } {
        let mut children = vec![base];
        children.extend(other_children);
        base = HtmlElement::new(tag).with_children(children).into();
    }

    if let Some((tag, other_children)) = match (t, b) {
        (None, None) => None,
        (Some(t), None) => Some((tag::mover, vec![t])),
        (None, Some(b)) => Some((tag::munder, vec![b])),
        (Some(t), Some(b)) => Some((tag::munderover, vec![b, t])),
    } {
        let mut children = vec![base];
        children.extend(other_children);
        base = HtmlElement::new(tag).with_children(children).into();
    }

    Ok(base)
}
