use typst_syntax::Span;

use crate::diag::SourceResult;
use crate::foundations::{Content, NativeElement, Packed, StyleChain, elem};
use crate::math::{Accent, AccentElem, AttachElem, LimitsElem, MathContext, Mathy};

/// A horizontal line under content.
///
/// ```example
/// $ underline(1 + 2 + ... + 5) $
/// ```
#[elem(Mathy)]
pub struct UnderlineElem {
    /// The content above the line.
    #[required]
    pub body: Content,
}

pub fn resolve_underline(
    _elem: &Packed<UnderlineElem>,
    _ctx: &mut MathContext,
    _styles: StyleChain,
) -> SourceResult<()> {
    Ok(())
}

/// A horizontal line over content.
///
/// ```example
/// $ overline(1 + 2 + ... + 5) $
/// ```
#[elem(Mathy)]
pub struct OverlineElem {
    /// The content below the line.
    #[required]
    pub body: Content,
}

pub fn resolve_overline(
    _elem: &Packed<OverlineElem>,
    _ctx: &mut MathContext,
    _styles: StyleChain,
) -> SourceResult<()> {
    Ok(())
}

/// A horizontal brace under content, with an optional annotation below.
///
/// ```example
/// $ underbrace(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct UnderbraceElem {
    /// The content above the brace.
    #[required]
    pub body: Content,

    /// The optional content below the brace.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_underbrace(
    elem: &Packed<UnderbraceElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏟',
        Position::Under,
        elem.span(),
    )
}

/// A horizontal brace over content, with an optional annotation above.
///
/// ```example
/// $ overbrace(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct OverbraceElem {
    /// The content below the brace.
    #[required]
    pub body: Content,

    /// The optional content above the brace.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_overbrace(
    elem: &Packed<OverbraceElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏞',
        Position::Over,
        elem.span(),
    )
}

/// A horizontal bracket under content, with an optional annotation below.
///
/// ```example
/// $ underbracket(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct UnderbracketElem {
    /// The content above the bracket.
    #[required]
    pub body: Content,

    /// The optional content below the bracket.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_underbracket(
    elem: &Packed<UnderbracketElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⎵',
        Position::Under,
        elem.span(),
    )
}

/// A horizontal bracket over content, with an optional annotation above.
///
/// ```example
/// $ overbracket(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct OverbracketElem {
    /// The content below the bracket.
    #[required]
    pub body: Content,

    /// The optional content above the bracket.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_overbracket(
    elem: &Packed<OverbracketElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⎴',
        Position::Over,
        elem.span(),
    )
}

/// A horizontal parenthesis under content, with an optional annotation below.
///
/// ```example
/// $ underparen(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct UnderparenElem {
    /// The content above the parenthesis.
    #[required]
    pub body: Content,

    /// The optional content below the parenthesis.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_underparen(
    elem: &Packed<UnderparenElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏝',
        Position::Under,
        elem.span(),
    )
}

/// A horizontal parenthesis over content, with an optional annotation above.
///
/// ```example
/// $ overparen(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct OverparenElem {
    /// The content below the parenthesis.
    #[required]
    pub body: Content,

    /// The optional content above the parenthesis.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_overparen(
    elem: &Packed<OverparenElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏜',
        Position::Over,
        elem.span(),
    )
}

/// A horizontal tortoise shell bracket under content, with an optional
/// annotation below.
///
/// ```example
/// $ undershell(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct UndershellElem {
    /// The content above the tortoise shell bracket.
    #[required]
    pub body: Content,

    /// The optional content below the tortoise shell bracket.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_undershell(
    elem: &Packed<UndershellElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏡',
        Position::Under,
        elem.span(),
    )
}

/// A horizontal tortoise shell bracket over content, with an optional
/// annotation above.
///
/// ```example
/// $ overshell(0 + 1 + dots.c + n, n + 1 "numbers") $
/// ```
#[elem(Mathy)]
pub struct OvershellElem {
    /// The content below the tortoise shell bracket.
    #[required]
    pub body: Content,

    /// The optional content above the tortoise shell bracket.
    #[positional]
    pub annotation: Option<Content>,
}

pub fn resolve_overshell(
    elem: &Packed<OvershellElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_underoverspreader(
        ctx,
        styles,
        &elem.body,
        elem.annotation.get_ref(styles),
        '⏠',
        Position::Over,
        elem.span(),
    )
}

/// Resolve an over- or underbrace-like object.
fn resolve_underoverspreader(
    ctx: &mut MathContext,
    styles: StyleChain,
    body: &Content,
    annotation: &Option<Content>,
    c: char,
    position: Position,
    span: Span,
) -> SourceResult<()> {
    // TODO: preserve body's class
    let mut base = LimitsElem::new(
        AccentElem::new(body.clone(), Accent::new(c))
            .with_dotless(false)
            .pack()
            .spanned(span),
    )
    .with_inline(true)
    .pack()
    .spanned(span);

    if annotation.is_some() {
        base = match position {
            Position::Under => AttachElem::new(base).with_b(annotation.clone()),
            Position::Over => AttachElem::new(base).with_t(annotation.clone()),
        }
        .pack()
        .spanned(span);
    }

    let item = ctx.resolve_into_item(&base, styles)?;
    ctx.push(item);
    Ok(())
}

/// A marker to distinguish under- and overlines.
pub enum Position {
    Under,
    Over,
}
