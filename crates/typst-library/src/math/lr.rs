use typst_utils::SliceExt;
use unicode_math_class::MathClass;

use crate::diag::SourceResult;
use crate::foundations::{
    Content, NativeElement, Packed, StyleChain, SymbolElem, elem, func,
};
use crate::layout::{Length, Rel};
use crate::math::{EquationElem, GroupItem, MathContext, MathItem, Mathy};

/// Scales delimiters.
///
/// While matched delimiters scale by default, this can be used to scale
/// unmatched delimiters and to control the delimiter scaling more precisely.
#[elem(title = "Left/Right", Mathy)]
pub struct LrElem {
    /// The size of the brackets, relative to the height of the wrapped content.
    #[default(Rel::one())]
    pub size: Rel<Length>,

    /// The delimited content, including the delimiters.
    #[required]
    #[parse(
        let mut arguments = args.all::<Content>()?.into_iter();
        let mut body = arguments.next().unwrap_or_default();
        arguments.for_each(|arg| body += SymbolElem::packed(',') + arg);
        body
    )]
    pub body: Content,
}

pub fn resolve_lr(
    elem: &Packed<LrElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    // Extract from an EquationElem.
    let mut body = &elem.body;
    if let Some(equation) = body.to_packed::<EquationElem>() {
        body = &equation.body;
    }

    // Extract implicit LrElem.
    if let Some(lr) = body.to_packed::<LrElem>()
        && lr.size.get(styles).is_one()
    {
        body = &lr.body;
    }

    let mut items = ctx.resolve_into_items(body, styles)?;

    // Ignore leading and trailing ignorant items.
    let (mut start_idx, end_idx) = items.split_prefix_suffix(|f| f.is_ignorant());
    let inner_items = &mut items[start_idx..end_idx];

    let height = elem.size.resolve(styles);

    let scale_if_delimiter = |item: &mut MathItem, apply: Option<MathClass>| {
        if matches!(
            item.class(),
            MathClass::Opening | MathClass::Closing | MathClass::Fence
        ) {
            if let MathItem::Glyph(glyph) = item {
                glyph.stretch = Some((height, true));
            }

            if let Some(class) = apply {
                item.set_class(class);
            }
        }
    };

    // Scale up items at both ends.
    match inner_items {
        [one] => scale_if_delimiter(one, None),
        [first, .., last] => {
            scale_if_delimiter(first, Some(MathClass::Opening));
            scale_if_delimiter(last, Some(MathClass::Closing));
        }
        [] => {}
    }

    // Handle MathItem::Glyph items that should be scaled up.
    for item in inner_items.iter_mut() {
        if let MathItem::Glyph(glyph) = item
            && glyph.mid_stretched == Some(false)
        {
            glyph.mid_stretched = Some(true);
            glyph.stretch = Some((height, true));
        }
    }

    let mut inner_items = items.drain(start_idx..end_idx).collect::<Vec<_>>();

    // Remove weak Spacing immediately after the opening or immediately
    // before the closing.
    let mut index = 0;
    let len = inner_items.len();
    let opening_exists =
        inner_items.first().is_some_and(|f| f.class() == MathClass::Opening);
    let closing_exists =
        inner_items.last().is_some_and(|f| f.class() == MathClass::Closing);
    inner_items.retain(|item| {
        let discard = (index == 1 && opening_exists
            || index + 2 == len && closing_exists)
            && matches!(item, MathItem::Spacing(_, true));
        index += 1;
        !discard
    });

    if opening_exists {
        items.insert(start_idx, inner_items.remove(0));
        start_idx += 1;
    }

    if closing_exists {
        items.insert(start_idx, inner_items.pop().unwrap());
    }

    let grouped_item: MathItem = GroupItem::new(inner_items, styles).into();
    items.insert(start_idx, grouped_item);
    ctx.push(GroupItem::new(items, styles));
    Ok(())
}

/// Scales delimiters vertically to the nearest surrounding `{lr()}` group.
///
/// ```example
/// $ { x mid(|) sum_(i=1)^n w_i|f_i (x)| < 1 } $
/// ```
#[elem(Mathy)]
pub struct MidElem {
    /// The content to be scaled.
    #[required]
    pub body: Content,
}

pub fn resolve_mid(
    elem: &Packed<MidElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let mut items = ctx.resolve_into_items(&elem.body, styles)?;
    for item in &mut items {
        if let MathItem::Glyph(glyph) = item {
            glyph.props.set_class(MathClass::Relation);
            glyph.mid_stretched = Some(false);
        }
    }
    ctx.extend(items);
    Ok(())
}

/// Floors an expression.
///
/// ```example
/// $ floor(x/2) $
/// ```
#[func]
pub fn floor(
    /// The size of the brackets, relative to the height of the wrapped content.
    #[named]
    size: Option<Rel<Length>>,
    /// The expression to floor.
    body: Content,
) -> Content {
    delimited(body, '⌊', '⌋', size)
}

/// Ceils an expression.
///
/// ```example
/// $ ceil(x/2) $
/// ```
#[func]
pub fn ceil(
    /// The size of the brackets, relative to the height of the wrapped content.
    #[named]
    size: Option<Rel<Length>>,
    /// The expression to ceil.
    body: Content,
) -> Content {
    delimited(body, '⌈', '⌉', size)
}

/// Rounds an expression.
///
/// ```example
/// $ round(x/2) $
/// ```
#[func]
pub fn round(
    /// The size of the brackets, relative to the height of the wrapped content.
    #[named]
    size: Option<Rel<Length>>,
    /// The expression to round.
    body: Content,
) -> Content {
    delimited(body, '⌊', '⌉', size)
}

/// Takes the absolute value of an expression.
///
/// ```example
/// $ abs(x/2) $
/// ```
#[func]
pub fn abs(
    /// The size of the brackets, relative to the height of the wrapped content.
    #[named]
    size: Option<Rel<Length>>,
    /// The expression to take the absolute value of.
    body: Content,
) -> Content {
    delimited(body, '|', '|', size)
}

/// Takes the norm of an expression.
///
/// ```example
/// $ norm(x/2) $
/// ```
#[func]
pub fn norm(
    /// The size of the brackets, relative to the height of the wrapped content.
    #[named]
    size: Option<Rel<Length>>,
    /// The expression to take the norm of.
    body: Content,
) -> Content {
    delimited(body, '‖', '‖', size)
}

fn delimited(
    body: Content,
    left: char,
    right: char,
    size: Option<Rel<Length>>,
) -> Content {
    let span = body.span();
    let mut elem = LrElem::new(Content::sequence([
        SymbolElem::packed(left),
        body,
        SymbolElem::packed(right),
    ]));
    // Push size only if size is provided
    if let Some(size) = size {
        elem.size.set(size);
    }
    elem.pack().spanned(span)
}
