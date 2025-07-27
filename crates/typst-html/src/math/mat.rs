use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Content, NativeElement, Packed, StyleChain};
use typst_library::introspection::SplitLocator;
use typst_library::math::{CasesElem, MatElem, VecElem, style_for_denominator};
use typst_library::text::TextElem;
use typst_syntax::Span;

use super::show_equation;
use crate::{HtmlElem, tag::mathml as tag};

pub fn show_mat(
    elem: &Packed<MatElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let rows = elem.rows.iter().map(|i| i.iter().collect()).collect();
    let body = show_body(&rows, engine, locator, styles)?;
    let delim = elem.delim.get(styles);
    Ok(add_delimiters(body, delim.open(), delim.close(), elem.span()))
}

pub fn show_vec(
    elem: &Packed<VecElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let rows = elem.children.iter().map(|x| vec![x]).collect();
    let body = show_body(&rows, engine, locator, styles)?;
    let delim = elem.delim.get(styles);
    Ok(add_delimiters(body, delim.open(), delim.close(), elem.span()))
}

pub fn show_cases(
    elem: &Packed<CasesElem>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let rows = elem.children.iter().map(|x| vec![x]).collect();
    let body = show_body(&rows, engine, locator, styles)?;
    let delim = elem.delim.get(styles);
    let (open, close) = if elem.reverse.get(styles) {
        (None, delim.close())
    } else {
        (delim.open(), None)
    };
    Ok(add_delimiters(body, open, close, elem.span()))
}

fn show_body(
    rows: &Vec<Vec<&Content>>,
    engine: &mut Engine,
    locator: &mut SplitLocator,
    styles: StyleChain,
) -> SourceResult<Content> {
    let denom_style = style_for_denominator(styles);
    let styles = styles.chain(&denom_style);
    Ok(HtmlElem::new(tag::mtable)
        .with_body(Some(
            rows.iter()
                .map(|row| {
                    Ok(HtmlElem::new(tag::mtr)
                        .with_body(Some(
                            row.iter()
                                .map(|cell| {
                                    // TODO: Check for linebreaks and emit a
                                    // warning.
                                    Ok(HtmlElem::new(tag::mtd)
                                        .with_body(Some(show_equation(
                                            cell, engine, locator, styles,
                                        )?))
                                        .pack())
                                })
                                .collect::<SourceResult<_>>()?,
                        ))
                        .pack())
                })
                .collect::<SourceResult<_>>()?,
        ))
        .pack())
}

fn add_delimiters(
    body: Content,
    open: Option<char>,
    close: Option<char>,
    span: Span,
) -> Content {
    if open.is_none() && close.is_none() {
        return body.spanned(span);
    }

    let mut row = vec![];

    if let Some(open_c) = open {
        row.push(
            HtmlElem::new(tag::mo)
                .with_body(Some(TextElem::packed(open_c)))
                .pack(),
        );
    }

    row.push(body);

    if let Some(close_c) = close {
        row.push(
            HtmlElem::new(tag::mo)
                .with_body(Some(TextElem::packed(close_c)))
                .pack(),
        );
    }

    HtmlElem::new(tag::mrow)
        .with_body(Some(Content::sequence(row)))
        .pack()
        .spanned(span)
}
