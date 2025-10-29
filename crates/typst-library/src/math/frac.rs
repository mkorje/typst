use typst_syntax::{Span, Spanned};

use crate::diag::{SourceResult, bail};
use crate::foundations::{
    Cast, Content, NativeElement, Packed, StyleChain, SymbolElem, Value, elem,
};
use crate::layout::Rel;
use crate::math::{
    FractionItem, GroupItem, LrElem, MathContext, MathItem, Mathy, SkewedFractionItem,
    style_for_denominator, style_for_numerator,
};

/// A mathematical fraction.
///
/// # Example
/// ```example
/// $ 1/2 < (x+1)/2 $
/// $ ((x+1)) / 2 = frac(a, b) $
/// ```
///
/// # Syntax
/// This function also has dedicated syntax: Use a slash to turn neighbouring
/// expressions into a fraction. Multiple atoms can be grouped into a single
/// expression using round grouping parentheses. Such parentheses are removed
/// from the output, but you can nest multiple to force them.
#[elem(title = "Fraction", Mathy)]
pub struct FracElem {
    /// The fraction's numerator.
    #[required]
    pub num: Content,

    /// The fraction's denominator.
    #[required]
    pub denom: Content,

    /// How the fraction should be laid out.
    ///
    /// ```example:"Styles"
    /// $ frac(x, y, style: "vertical") $
    /// $ frac(x, y, style: "skewed") $
    /// $ frac(x, y, style: "horizontal") $
    /// ```
    ///
    /// ```example:"Setting the default"
    /// #set math.frac(style: "skewed")
    /// $ a / b $
    /// ```
    ///
    /// ```example:"Handling of grouping parentheses"
    /// // Grouping parentheses are removed.
    /// #set math.frac(style: "vertical")
    /// $ (a + b) / b $
    ///
    /// // Grouping parentheses are removed.
    /// #set math.frac(style: "skewed")
    /// $ (a + b) / b $
    ///
    /// // Grouping parentheses are retained.
    /// #set math.frac(style: "horizontal")
    /// $ (a + b) / b $
    /// ```
    ///
    /// ```example:"Different styles in inline vs block equations"
    /// // This changes the style for inline equations only.
    /// #show math.equation.where(block: false): set math.frac(style: "horizontal")
    ///
    /// This $(x-y)/z = 3$ is inline math, and this is block math:
    /// $ (x-y)/z = 3 $
    /// ```
    #[default(FracStyle::Vertical)]
    pub style: FracStyle,

    /// Whether the numerator was originally surrounded by parentheses
    /// that were stripped by the parser.
    #[internal]
    #[parse(None)]
    #[default(false)]
    pub num_deparenthesized: bool,

    /// Whether the denominator was originally surrounded by parentheses
    /// that were stripped by the parser.
    #[internal]
    #[parse(None)]
    #[default(false)]
    pub denom_deparenthesized: bool,
}

pub fn resolve_frac(
    elem: &Packed<FracElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    match elem.style.get(styles) {
        FracStyle::Skewed => resolve_skewed_frac(ctx, styles, &elem.num, &elem.denom),
        FracStyle::Horizontal => resolve_horizontal_frac(
            ctx,
            styles,
            &elem.num,
            &elem.denom,
            elem.span(),
            elem.num_deparenthesized.get(styles),
            elem.denom_deparenthesized.get(styles),
        ),
        FracStyle::Vertical => resolve_vertical_frac_like(
            ctx,
            styles,
            &elem.num,
            std::slice::from_ref(&elem.denom),
            false,
            elem.span(),
        ),
    }
}

/// Fraction style
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash, Cast)]
pub enum FracStyle {
    /// Stacked numerator and denominator with a bar.
    #[default]
    Vertical,
    /// Numerator and denominator separated by a slash.
    Skewed,
    /// Numerator and denominator placed inline and parentheses are not
    /// absorbed.
    Horizontal,
}

/// A binomial expression.
///
/// # Example
/// ```example
/// $ binom(n, k) $
/// $ binom(n, k_1, k_2, k_3, ..., k_m) $
/// ```
#[elem(title = "Binomial", Mathy)]
pub struct BinomElem {
    /// The binomial's upper index.
    #[required]
    pub upper: Content,

    /// The binomial's lower index.
    #[required]
    #[variadic]
    #[parse(
        let values = args.all::<Spanned<Value>>()?;
        if values.is_empty() {
            // Prevents one element binomials
            bail!(args.span, "missing argument: lower");
        }
        values.into_iter().map(|spanned| spanned.v.display()).collect()
    )]
    pub lower: Vec<Content>,
}

pub fn resolve_binom(
    elem: &Packed<BinomElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    resolve_vertical_frac_like(ctx, styles, &elem.upper, &elem.lower, true, elem.span())
}

/// Resolve a vertical fraction or binomial.
fn resolve_vertical_frac_like(
    ctx: &mut MathContext,
    styles: StyleChain,
    num: &Content,
    denom: &[Content],
    binom: bool,
    span: Span,
) -> SourceResult<()> {
    let num_style = style_for_numerator(styles);
    let numerator = ctx.resolve_into_run(num, styles.chain(&num_style))?;

    let denom_style = style_for_denominator(styles);
    let denominator = ctx.resolve_into_run(
        &Content::sequence(
            // Add a comma between each element.
            denom
                .iter()
                .flat_map(|a| [SymbolElem::packed(',').spanned(span), a.clone()])
                .skip(1),
        ),
        styles.chain(&denom_style),
    )?;

    let frac = FractionItem::new(numerator, denominator, !binom, styles);

    if binom {
        let mut left =
            ctx.resolve_into_item(&SymbolElem::packed('(').spanned(span), styles)?;
        if let MathItem::Glyph(ref mut glyph) = left {
            glyph.stretch = Some((Rel::one(), true));
        }

        let mut right =
            ctx.resolve_into_item(&SymbolElem::packed(')').spanned(span), styles)?;
        if let MathItem::Glyph(ref mut glyph) = right {
            glyph.stretch = Some((Rel::one(), true));
        }

        ctx.push(GroupItem::new(vec![left, frac.into(), right], styles));
    } else {
        ctx.push(frac);
    }

    Ok(())
}

// Resolve a horizontal fraction
fn resolve_horizontal_frac(
    ctx: &mut MathContext,
    styles: StyleChain,
    num: &Content,
    denom: &Content,
    span: Span,
    num_deparen: bool,
    denom_deparen: bool,
) -> SourceResult<()> {
    let num = if num_deparen {
        &LrElem::new(Content::sequence(vec![
            SymbolElem::packed('('),
            num.clone(),
            SymbolElem::packed(')'),
        ]))
        .pack()
    } else {
        num
    };
    let num = ctx.resolve_into_item(num, styles)?;
    ctx.push(num);

    let slash = ctx.resolve_into_item(&SymbolElem::packed('/').spanned(span), styles)?;
    ctx.push(slash);

    let denom = if denom_deparen {
        &LrElem::new(Content::sequence(vec![
            SymbolElem::packed('('),
            denom.clone(),
            SymbolElem::packed(')'),
        ]))
        .pack()
    } else {
        denom
    };
    let denom = ctx.resolve_into_item(denom, styles)?;
    ctx.push(denom);

    Ok(())
}

/// Resolve a skewed fraction.
fn resolve_skewed_frac(
    ctx: &mut MathContext,
    styles: StyleChain,
    num: &Content,
    denom: &Content,
) -> SourceResult<()> {
    let num_style = style_for_numerator(styles);
    let numerator = ctx.resolve_into_run(num, styles.chain(&num_style))?;
    let denom_style = style_for_denominator(styles);
    let denominator = ctx.resolve_into_run(denom, styles.chain(&denom_style))?;

    ctx.push(SkewedFractionItem::new(numerator, denominator, styles));

    Ok(())
}
