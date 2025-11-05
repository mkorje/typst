use std::ops::{Deref, DerefMut};

use codex::styling::{MathStyle, to_style};
use ecow::EcoString;
use typst_syntax::{Span, is_newline};
use typst_utils::{SliceExt, default_math_class};
use unicode_segmentation::UnicodeSegmentation;

use crate::diag::{SourceResult, bail, warning};
use crate::engine::Engine;
use crate::foundations::{
    Content, Packed, Resolve, Smart, StyleChain, Styles, SymbolElem,
};
use crate::introspection::{SplitLocator, Tag, TagElem};
use crate::layout::{
    Abs, Axes, Axis, BoxElem, FixedAlignment, HElem, PlaceElem, Ratio, Rel, Spacing,
};
use crate::routines::{Arenas, RealizationKind};
use crate::text::{FontFeatures, LinebreakElem, SpaceElem, TextElem};

/// The math IR builder.
pub struct MathBuilder<'a, 'v, 'e> {
    // External.
    engine: &'v mut Engine<'e>,
    locator: &'v mut SplitLocator<'a>,
    arenas: &'a Arenas,
    // Mutable.
    items: Vec<MathItem<'a>>,
}

impl<'a, 'v, 'e> MathBuilder<'a, 'v, 'e> {
    /// Create a new math builder.
    pub fn new(
        engine: &'v mut Engine<'e>,
        locator: &'v mut SplitLocator<'a>,
        arenas: &'a Arenas,
    ) -> Self {
        Self { engine, locator, arenas, items: vec![] }
    }

    pub fn build(
        &mut self,
        elem: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<MathRun<'a>> {
        self.resolve_into_run(elem, styles)
    }

    /// Lifetime-extends some styles.
    fn store_styles(&self, styles: impl Into<Styles>) -> &'a Styles {
        self.arenas.styles.alloc(styles.into())
    }

    /// Lifetime-extends some content.
    fn store(&self, content: Content) -> &'a Content {
        self.arenas.content.alloc(content)
    }

    /// Push a item.
    fn push(&mut self, item: impl Into<MathItem<'a>>) {
        self.items.push(item.into());
    }

    /// Push multiple items.
    fn extend(&mut self, items: impl IntoIterator<Item = MathItem<'a>>) {
        self.items.extend(items);
    }

    /// Resolve the given element and return the resulting [`MathItem`]s.
    fn resolve_into_items(
        &mut self,
        elem: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<Vec<MathItem<'a>>> {
        // The element's resolve_math() changes the items held in this
        // MathContext object, but for convenience this function shouldn't change
        // them, so we restore the MathContext's items after obtaining the
        // resolve result.
        let prev = std::mem::take(&mut self.items);
        self.resolve_into_self(elem, styles)?;
        Ok(std::mem::replace(&mut self.items, prev))
    }

    /// Resolve the given element and return the result as a [`MathRun`].
    pub fn resolve_into_run(
        &mut self,
        elem: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<MathRun<'a>> {
        Ok(MathRun::new(self.resolve_into_items(elem, styles)?))
    }

    fn resolve_into_item(
        &mut self,
        elem: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<MathItem<'a>> {
        let mut items = self.resolve_into_run(elem, styles)?.0;
        if items.len() == 1 {
            Ok(items.pop().unwrap())
        } else {
            Ok(GroupItem::new(items, styles).into())
        }
    }

    /// Resolve arbitrary content.
    fn resolve_into_self(
        &mut self,
        content: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let pairs = (self.engine.routines.realize)(
            RealizationKind::Math,
            self.engine,
            self.locator,
            &self.arenas,
            content,
            styles,
        )?;

        for (elem, styles) in pairs {
            resolve_realized(elem, self, styles)?;
        }

        Ok(())
    }
}

/// Resolves a leaf element resulting from realization.
fn resolve_realized<'a, 'v, 'e>(
    elem: &'a Content,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    if let Some(elem) = elem.to_packed::<TagElem>() {
        ctx.push(MathItem::Tag(elem.tag.clone()));
    } else if elem.is::<LinebreakElem>() {
        ctx.push(MathItem::Linebreak);
    } else if elem.is::<SpaceElem>() {
        ctx.push(MathItem::Space);
    } else if let Some(elem) = elem.to_packed::<HElem>() {
        resolve_h(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<TextElem>() {
        resolve_text(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<SymbolElem>() {
        resolve_symbol(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BoxElem>() {
        resolve_box(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AccentElem>() {
        resolve_accent(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AttachElem>() {
        resolve_attach(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<PrimesElem>() {
        resolve_primes(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<ScriptsElem>() {
        resolve_scripts(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LimitsElem>() {
        resolve_limits(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<StretchElem>() {
        resolve_stretch(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<CancelElem>() {
        resolve_cancel(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<FracElem>() {
        resolve_frac(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BinomElem>() {
        resolve_binom(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LrElem>() {
        resolve_lr(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<MidElem>() {
        resolve_mid(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<VecElem>() {
        resolve_vec(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<MatElem>() {
        resolve_mat(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<CasesElem>() {
        resolve_cases(elem, ctx, styles)?;
    } else if elem.is::<AlignPointElem>() {
        ctx.push(MathItem::Align);
    } else if let Some(elem) = elem.to_packed::<ClassElem>() {
        resolve_class(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<OpElem>() {
        resolve_op(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<RootElem>() {
        resolve_root(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<UnderlineElem>() {
        resolve_underline(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<OverlineElem>() {
        resolve_overline(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<UnderbraceElem>() {
        resolve_underbrace(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<OverbraceElem>() {
        resolve_overbrace(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<UnderbracketElem>() {
        resolve_underbracket(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<OverbracketElem>() {
        resolve_overbracket(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<UnderparenElem>() {
        resolve_underparen(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<OverparenElem>() {
        resolve_overparen(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<UndershellElem>() {
        resolve_undershell(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<OvershellElem>() {
        resolve_overshell(elem, ctx, styles)?;
    } else {
        ctx.push(ExternalItem::new(elem, styles));
    }
    Ok(())
}

fn resolve_h(
    elem: &Packed<HElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    if let Spacing::Rel(rel) = elem.amount
        && rel.rel.is_zero()
    {
        ctx.push(MathItem::Spacing(rel.abs.resolve(styles), elem.weak.get(styles)));
    }
    Ok(())
}

fn resolve_text<'a, 'v, 'e>(
    elem: &'a Packed<TextElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let variant = styles.get(EquationElem::variant);
    let bold = styles.get(EquationElem::bold);
    // Disable auto-italic.
    let italic = styles.get(EquationElem::italic).or(Some(false));

    let num = elem.text.chars().all(|c| c.is_ascii_digit() || c == '.');
    let multiline = elem.text.contains(is_newline);

    let styled_text: EcoString = elem
        .text
        .chars()
        .flat_map(|c| to_style(c, MathStyle::select(c, variant, bold, italic)))
        .collect();

    let mut item: MathItem = TextItem::new(styled_text, styles, elem.span()).into();
    if !multiline {
        item.set_text_like(true);
        if !num {
            item.set_class(MathClass::Alphabetic);
            item.set_spaced(true);
        }
    }

    ctx.push(item);
    Ok(())
}

fn resolve_symbol<'a, 'v, 'e>(
    elem: &'a Packed<SymbolElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let variant = styles.get(EquationElem::variant);
    let bold = styles.get(EquationElem::bold);
    let italic = styles.get(EquationElem::italic);
    for cluster in elem.text.graphemes(true) {
        let text: EcoString = cluster
            .chars()
            .flat_map(|c| to_style(c, MathStyle::select(c, variant, bold, italic)))
            .collect();
        ctx.push(GlyphItem::new(text, styles, elem.span()));
    }
    Ok(())
}

fn resolve_box<'a, 'v, 'e>(
    elem: &'a Packed<BoxElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    ctx.push(BoxItem::new(elem, styles));
    Ok(())
}

fn resolve_accent<'a, 'v, 'e>(
    elem: &'a Packed<AccentElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let accent = elem.accent;
    let top_accent = !accent.is_bottom();

    let mut new_styles = Styles::new();
    new_styles.apply(style_cramped());
    // Try to replace the base glyph with its dotless variant.
    if top_accent && elem.dotless.get(styles) {
        new_styles.apply(style_dtls());
    }
    let new_styles = ctx.store_styles(new_styles);

    let base_styles = ctx.arenas.bump.alloc(styles).chain(new_styles);
    let base = ctx.resolve_into_run(&elem.base, base_styles)?;

    let accent = ctx.store(SymbolElem::packed(accent.0).spanned(elem.span()));
    let mut accent = ctx.resolve_into_run(&accent, styles)?;

    let width = elem.size.resolve(styles);
    let mut iter = accent.iter_mut();
    if let Some(item) = iter.next()
        && iter.next().is_none()
        && let MathItem::Glyph(glyph) = item
    {
        glyph.props.set_class(MathClass::Diacritic);
        glyph.stretch = Some((width, Some(Axis::X)));
    }

    // let base_text_like = base.is_text_like();
    // let base_class = base.class();

    ctx.push(AccentItem::new(base, accent, !top_accent, styles));

    Ok(())
}

fn resolve_attach<'a, 'v, 'e>(
    elem: &'a Packed<AttachElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let merged = elem.merge_base();
    let elem = ctx.arenas.bump.alloc(merged.unwrap_or(elem.clone()));
    // let stretch = elem.stretch_size(styles);

    let bumped_styles = ctx.arenas.bump.alloc(styles);

    let mut base = ctx.resolve_into_run(&elem.base, styles)?;
    let sup_style = ctx.store_styles(style_for_superscript(styles));
    let sup_style_chain = bumped_styles.chain(sup_style);
    let tl = elem.tl.get_cloned(sup_style_chain);
    let tr = elem.tr.get_cloned(sup_style_chain);
    let primed = tr.as_ref().is_some_and(|content| content.is::<PrimesElem>());
    let t = elem.t.get_cloned(sup_style_chain);

    let sub_style = ctx.store_styles(style_for_subscript(styles));
    let sub_style_chain = bumped_styles.chain(sub_style);
    let bl = elem.bl.get_cloned(sub_style_chain);
    let br = elem.br.get_cloned(sub_style_chain);
    let b = elem.b.get_cloned(sub_style_chain);

    let limits = base.into_item(styles).limits().active(styles);
    let (t, tr) = match (t, tr) {
        (Some(t), Some(tr)) if primed && !limits => (None, Some(tr + t)),
        (Some(t), None) if !limits => (None, Some(t)),
        (t, tr) => (t, tr),
    };
    let (b, br) = if limits || br.is_some() { (b, br) } else { (None, b) };

    macro_rules! layout {
        ($content:ident, $style_chain:ident) => {
            $content
                .map(|elem| ctx.resolve_into_run(ctx.store(elem), $style_chain))
                .transpose()
        };
    }

    if base.0.len() == 1
        && let MathItem::Glyph(glyph) = base.0.last_mut().unwrap()
    {
        if let Some((stretch, None)) = glyph.stretch {
            glyph.stretch = Some((stretch, Some(Axis::X)));
        }
    }

    let top = layout!(t, sup_style_chain)?;
    let bottom = layout!(b, sub_style_chain)?;
    let top_left = layout!(tl, sup_style_chain)?;
    let bottom_left = layout!(bl, sub_style_chain)?;
    let top_right = layout!(tr, sup_style_chain)?;
    let bottom_right = layout!(br, sub_style_chain)?;

    ctx.push(ScriptsItem::new(
        base,
        top,
        bottom,
        top_left,
        bottom_left,
        top_right,
        bottom_right,
        styles,
    ));

    Ok(())
}

fn resolve_primes<'a, 'v, 'e>(
    elem: &'a Packed<PrimesElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    match elem.count {
        count @ 1..=4 => {
            let c = match count {
                1 => '′',
                2 => '″',
                3 => '‴',
                4 => '⁗',
                _ => unreachable!(),
            };
            let f = ctx.resolve_into_item(
                ctx.store(SymbolElem::packed(c).spanned(elem.span())),
                styles,
            )?;
            ctx.push(f);
        }
        count => {
            // Custom amount of primes
            let prime = ctx.resolve_into_run(
                ctx.store(SymbolElem::packed('′').spanned(elem.span())),
                styles,
            )?;
            ctx.push(PrimesItem::new(prime, count, styles));
        }
    }
    Ok(())
}

fn resolve_scripts<'a, 'v, 'e>(
    elem: &'a Packed<ScriptsElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let mut item = ctx.resolve_into_item(&elem.body, styles)?;
    item.set_limits(Limits::Never);
    ctx.push(item);
    Ok(())
}

fn resolve_limits<'a, 'v, 'e>(
    elem: &'a Packed<LimitsElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let mut item = ctx.resolve_into_item(&elem.body, styles)?;
    let limits = if elem.inline.get(styles) { Limits::Always } else { Limits::Display };
    item.set_limits(limits);
    ctx.push(item);
    Ok(())
}

fn resolve_stretch<'a, 'v, 'e>(
    elem: &'a Packed<StretchElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let mut item = ctx.resolve_into_item(&elem.body, styles)?;
    let size = elem.size.resolve(styles);
    if let MathItem::Glyph(ref mut glyph) = item {
        let size = if let Some((stretch, _)) = glyph.stretch {
            Rel::new(stretch.rel * size.rel, size.rel.of(stretch.abs) + size.abs)
        } else {
            size
        };
        glyph.stretch = Some((size, None));
    }
    ctx.push(item);
    Ok(())
}

fn resolve_cancel<'a, 'v, 'e>(
    elem: &'a Packed<CancelElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let body = ctx.resolve_into_run(&elem.body, styles)?;
    // Preserve properties of body.
    // let body_class = body.class();
    // let body_text_like = body.is_text_like();

    let length = elem.length.resolve(styles);

    let stroke = elem.stroke.resolve(styles).unwrap_or(FixedStroke {
        paint: styles.get_ref(TextElem::fill).as_decoration(),
        ..Default::default()
    });

    let invert = elem.inverted.get(styles);
    let cross = elem.cross.get(styles);
    let angle = elem.angle.get_ref(styles);

    let invert_first_line = !cross && invert;

    ctx.push(CancelItem::new(
        body,
        elem.span(),
        length,
        stroke,
        invert_first_line,
        cross,
        angle.clone(),
        styles,
    ));

    Ok(())
}

fn resolve_frac<'a, 'v, 'e>(
    elem: &'a Packed<FracElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_binom<'a, 'v, 'e>(
    elem: &'a Packed<BinomElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    resolve_vertical_frac_like(ctx, styles, &elem.upper, &elem.lower, true, elem.span())
}

/// Resolve a vertical fraction or binomial.
fn resolve_vertical_frac_like<'a, 'v, 'e>(
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
    num: &'a Content,
    denom: &[Content],
    binom: bool,
    span: Span,
) -> SourceResult<()> {
    let num_style = ctx.store_styles(style_for_numerator(styles));
    let denom_style = ctx.store_styles(style_for_denominator(styles));
    let bumped_styles = ctx.arenas.bump.alloc(styles);

    let numerator = ctx.resolve_into_run(num, bumped_styles.chain(num_style))?;

    let denominator = ctx.resolve_into_run(
        ctx.store(Content::sequence(
            // Add a comma between each element.
            denom
                .iter()
                .flat_map(|a| [SymbolElem::packed(',').spanned(span), a.clone()])
                .skip(1),
        )),
        bumped_styles.chain(denom_style),
    )?;

    let frac = FractionItem::new(numerator, denominator, !binom, span, styles);

    if binom {
        let mut left = ctx.resolve_into_item(
            ctx.store(SymbolElem::packed('(').spanned(span)),
            styles,
        )?;
        if let MathItem::Glyph(ref mut glyph) = left {
            glyph.stretch = Some((Rel::one(), Some(Axis::Y)));
        }

        let mut right = ctx.resolve_into_item(
            ctx.store(SymbolElem::packed(')').spanned(span)),
            styles,
        )?;
        if let MathItem::Glyph(ref mut glyph) = right {
            glyph.stretch = Some((Rel::one(), Some(Axis::Y)));
        }

        ctx.push(GroupItem::new(vec![left, frac.into(), right], styles));
    } else {
        ctx.push(frac);
    }

    Ok(())
}

// Resolve a horizontal fraction
fn resolve_horizontal_frac<'a, 'v, 'e>(
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
    num: &'a Content,
    denom: &'a Content,
    span: Span,
    num_deparen: bool,
    denom_deparen: bool,
) -> SourceResult<()> {
    let num = if num_deparen {
        ctx.store(
            LrElem::new(Content::sequence(vec![
                SymbolElem::packed('('),
                num.clone(),
                SymbolElem::packed(')'),
            ]))
            .pack(),
        )
    } else {
        num
    };
    let num = ctx.resolve_into_item(num, styles)?;
    ctx.push(num);

    let slash =
        ctx.resolve_into_item(ctx.store(SymbolElem::packed('/').spanned(span)), styles)?;
    ctx.push(slash);

    let denom = if denom_deparen {
        ctx.store(
            LrElem::new(Content::sequence(vec![
                SymbolElem::packed('('),
                denom.clone(),
                SymbolElem::packed(')'),
            ]))
            .pack(),
        )
    } else {
        denom
    };
    let denom = ctx.resolve_into_item(denom, styles)?;
    ctx.push(denom);

    Ok(())
}

/// Resolve a skewed fraction.
fn resolve_skewed_frac<'a, 'v, 'e>(
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
    num: &'a Content,
    denom: &'a Content,
) -> SourceResult<()> {
    let num_style = ctx.store_styles(style_for_numerator(styles));
    let denom_style = ctx.store_styles(style_for_denominator(styles));
    let bumped_styles = ctx.arenas.bump.alloc(styles);

    let numerator = ctx.resolve_into_run(num, bumped_styles.chain(num_style))?;
    let denominator = ctx.resolve_into_run(denom, bumped_styles.chain(denom_style))?;

    ctx.push(SkewedFractionItem::new(numerator, denominator, styles));

    Ok(())
}

fn resolve_lr<'a, 'v, 'e>(
    elem: &'a Packed<LrElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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
                glyph.stretch = Some((height, Some(Axis::Y)));
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
            glyph.stretch = Some((height, Some(Axis::Y)));
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

fn resolve_mid<'a, 'v, 'e>(
    elem: &'a Packed<MidElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_vec<'a, 'v, 'e>(
    elem: &'a Packed<VecElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let span = elem.span();

    let rows: Vec<Vec<&Content>> =
        elem.children.iter().map(|child| vec![child]).collect();
    let cells = resolve_cells(
        ctx,
        styles,
        rows,
        span,
        elem.align.resolve(styles),
        LeftRightAlternator::Right,
        None,
        Axes::with_y(elem.gap.resolve(styles)),
        "elements",
    )?;

    let delim = elem.delim.get(styles);
    resolve_delimiters(ctx, styles, cells, delim.open(), delim.close(), span)
}

fn resolve_mat<'a, 'v, 'e>(
    elem: &'a Packed<MatElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let span = elem.span();

    let rows: Vec<Vec<&Content>> =
        elem.rows.iter().map(|row| row.iter().collect()).collect();
    let nrows = rows.len();
    let ncols = rows.first().map_or(0, |row| row.len());

    let augment = elem.augment.resolve(styles);
    if let Some(aug) = &augment {
        for &offset in &aug.hline.0 {
            if offset > nrows as isize || offset.unsigned_abs() > nrows {
                bail!(
                    span,
                    "cannot draw a horizontal line at offset {offset} \
                     in a matrix with {nrows} rows",
                );
            }
        }

        for &offset in &aug.vline.0 {
            if offset > ncols as isize || offset.unsigned_abs() > ncols {
                bail!(
                    span,
                    "cannot draw a vertical line at offset {offset} \
                     in a matrix with {ncols} columns",
                );
            }
        }
    }

    let cells = resolve_cells(
        ctx,
        styles,
        rows,
        span,
        elem.align.resolve(styles),
        LeftRightAlternator::Right,
        augment,
        Axes::new(elem.column_gap.resolve(styles), elem.row_gap.resolve(styles)),
        "cells",
    )?;

    let delim = elem.delim.get(styles);
    resolve_delimiters(ctx, styles, cells, delim.open(), delim.close(), span)
}

fn resolve_cases<'a, 'v, 'e>(
    elem: &'a Packed<CasesElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let span = elem.span();

    let rows: Vec<Vec<&Content>> =
        elem.children.iter().map(|child| vec![child]).collect();
    let cells = resolve_cells(
        ctx,
        styles,
        rows,
        span,
        FixedAlignment::Start,
        LeftRightAlternator::None,
        None,
        Axes::with_y(elem.gap.resolve(styles)),
        "branches",
    )?;

    let delim = elem.delim.get(styles);
    let (open, close) = if elem.reverse.get(styles) {
        (None, delim.close())
    } else {
        (delim.open(), None)
    };
    resolve_delimiters(ctx, styles, cells, open, close, span)
}

/// Layout the inner contents of a matrix, vector, or cases.
fn resolve_cells<'a, 'v, 'e>(
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
    rows: Vec<Vec<&'a Content>>,
    span: Span,
    align: FixedAlignment,
    alternator: LeftRightAlternator,
    augment: Option<Augment<Abs>>,
    gap: Axes<Rel<Abs>>,
    children: &str,
) -> SourceResult<MathItem<'a>> {
    let denom_style = ctx.store_styles(style_for_denominator(styles));
    let bumped_styles = ctx.arenas.bump.alloc(styles);
    let cells = rows
        .iter()
        .map(|row| {
            row.iter()
                .map(|cell| {
                    let cell_span = cell.span();
                    let cell =
                        ctx.resolve_into_run(cell, bumped_styles.chain(denom_style))?;

                    // We ignore linebreaks in the cells as we can't differentiate
                    // alignment points for the whole body from ones for a specific
                    // cell, and multiline cells don't quite make sense at the moment.
                    if cell.is_multiline() {
                        ctx.engine.sink.warn(warning!(
                           cell_span,
                           "linebreaks are ignored in {}", children;
                           hint: "use commas instead to separate each line"
                        ));
                    }

                    Ok(cell)
                })
                .collect::<SourceResult<_>>()
        })
        .collect::<SourceResult<_>>();

    Ok(TableItem::new(cells?, styles, gap, align, alternator, augment, span).into())
}

/// Resolve the outer wrapper around the body of a vector or matrix.
fn resolve_delimiters<'a, 'v, 'e>(
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
    cells: MathItem<'a>,
    left: Option<char>,
    right: Option<char>,
    span: Span,
) -> SourceResult<()> {
    // let mut items = vec![];

    let target = Rel::new(Ratio::new(1.1), Abs::zero());

    let open = left
        .map(|c| {
            ctx.resolve_into_run(ctx.store(SymbolElem::packed(c).spanned(span)), styles)
        })
        .transpose()?;
    let close = right
        .map(|c| {
            ctx.resolve_into_run(ctx.store(SymbolElem::packed(c).spanned(span)), styles)
        })
        .transpose()?;

    ctx.push(FencedItem::new(
        open,
        close,
        span,
        MathRun(vec![cells]),
        styles,
        false,
        target,
    ));

    // if let Some(left_c) = left {
    //     let mut left = ctx.resolve_into_item(
    //         ctx.store(SymbolElem::packed(left_c).spanned(span)),
    //         styles,
    //     )?;
    //     if let MathItem::Glyph(ref mut glyph) = left {
    //         glyph.stretch = Some((target, Some(Axis::Y)));
    //     }
    //     items.push(left);
    // }

    // items.push(cells);

    // if let Some(right_c) = right {
    //     let mut right = ctx.resolve_into_item(
    //         ctx.store(SymbolElem::packed(right_c).spanned(span)),
    //         styles,
    //     )?;
    //     if let MathItem::Glyph(ref mut glyph) = right {
    //         glyph.stretch = Some((target, Some(Axis::Y)));
    //     }
    //     items.push(right);
    // }

    // ctx.push(GroupItem::new(items, styles));

    Ok(())
}

fn resolve_class<'a, 'v, 'e>(
    elem: &'a Packed<ClassElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let style = ctx.store_styles(EquationElem::class.set(Some(elem.class)).wrap());
    let bumped_styles = ctx.arenas.bump.alloc(styles);
    let mut item = ctx.resolve_into_item(&elem.body, bumped_styles.chain(style))?;
    item.set_class(elem.class);
    item.set_limits(Limits::for_class(elem.class));
    ctx.push(item);
    Ok(())
}

fn resolve_op<'a, 'v, 'e>(
    elem: &'a Packed<OpElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    // TODO: should be wrapped to match typst-layout
    let mut item = ctx.resolve_into_item(&elem.text, styles)?;
    item.set_class(MathClass::Large);
    item.set_limits(if elem.limits.get(styles) {
        Limits::Display
    } else {
        Limits::Never
    });
    ctx.push(item);
    Ok(())
}

fn resolve_root<'a, 'v, 'e>(
    elem: &'a Packed<RootElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let bumped_styles = ctx.arenas.bump.alloc(styles);
    let radicand = {
        let cramped = ctx.store_styles(style_cramped());
        ctx.resolve_into_run(&elem.radicand, bumped_styles.chain(cramped))?
    };
    let index = {
        let sscript =
            ctx.store_styles(EquationElem::size.set(MathSize::ScriptScript).wrap());
        elem.index
            .get_ref(styles)
            .as_ref()
            .map(|elem| ctx.resolve_into_run(elem, bumped_styles.chain(sscript)))
            .transpose()?
    };
    let sqrt = ctx.resolve_into_run(
        ctx.store(SymbolElem::packed('√').spanned(elem.span())),
        styles,
    )?;
    ctx.push(RadicalItem::new(radicand, index, styles, sqrt, elem.span()));
    Ok(())
}

fn resolve_underline<'a, 'v, 'e>(
    elem: &'a Packed<UnderlineElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let base = ctx.resolve_into_run(&elem.body, styles)?;
    ctx.push(LineItem::new(base, true, styles, elem.span()));
    Ok(())
}

fn resolve_overline<'a, 'v, 'e>(
    elem: &'a Packed<OverlineElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
) -> SourceResult<()> {
    let cramped = ctx.store_styles(style_cramped());
    let bumped_styles = ctx.arenas.bump.alloc(styles);
    let base = ctx.resolve_into_run(&elem.body, bumped_styles.chain(cramped))?;
    ctx.push(LineItem::new(base, false, styles, elem.span()));
    Ok(())
}

fn resolve_underbrace<'a, 'v, 'e>(
    elem: &Packed<UnderbraceElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_overbrace<'a, 'v, 'e>(
    elem: &Packed<OverbraceElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_underbracket<'a, 'v, 'e>(
    elem: &Packed<UnderbracketElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_overbracket<'a, 'v, 'e>(
    elem: &Packed<OverbracketElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_underparen<'a, 'v, 'e>(
    elem: &Packed<UnderparenElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_overparen<'a, 'v, 'e>(
    elem: &Packed<OverparenElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_undershell<'a, 'v, 'e>(
    elem: &Packed<UndershellElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

fn resolve_overshell<'a, 'v, 'e>(
    elem: &Packed<OvershellElem>,
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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
fn resolve_underoverspreader<'a, 'v, 'e>(
    ctx: &mut MathContext<'a, 'v, 'e>,
    styles: StyleChain<'a>,
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

    let item = ctx.resolve_into_item(ctx.store(base), styles)?;
    ctx.push(item);
    Ok(())
}
