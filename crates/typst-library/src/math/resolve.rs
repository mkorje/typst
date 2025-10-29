use std::ops::{Deref, DerefMut};

use codex::styling::{MathStyle, to_style};
use ecow::EcoString;
use typst_syntax::Span;
use typst_utils::default_math_class;
use unicode_segmentation::UnicodeSegmentation;

use crate::diag::SourceResult;
use crate::engine::Engine;
use crate::foundations::{Content, Packed, Resolve, StyleChain, SymbolElem};
use crate::introspection::{SplitLocator, Tag, TagElem};
use crate::layout::{Abs, BoxElem, HElem, Rel, Spacing};
use crate::routines::{Arenas, RealizationKind};
use crate::text::{LinebreakElem, SpaceElem, TextElem};

use crate::math::*;

/// The top-level item in a math sequence.
#[derive(Debug, Clone)]
pub enum MathItem {
    Group(MathComponent<GroupItem>),
    Radical(MathComponent<RadicalItem>),
    Fraction(MathComponent<FractionItem>),
    SkewedFraction(MathComponent<SkewedFractionItem>),
    Table(MathComponent<TableItem>),
    Scripts(MathComponent<ScriptsItem>),
    Accent(MathComponent<AccentItem>),
    // Cancel(MathComponent<CancelItem>),
    // Line(MathComponent<LineItem>),
    Glyph(MathComponent<GlyphItem>),
    Text(MathComponent<TextItem>),

    Spacing(Abs, bool),
    Space,
    Linebreak,
    Align,
    Tag(Tag),
}

macro_rules! match_set_component_props {
    ($item:expr, $prop:ident, $value:ident) => {
        match $item {
            MathItem::Group(comp) => comp.props.$prop = $value,
            MathItem::Radical(comp) => comp.props.$prop = $value,
            MathItem::Fraction(comp) => comp.props.$prop = $value,
            MathItem::SkewedFraction(comp) => comp.props.$prop = $value,
            MathItem::Table(comp) => comp.props.$prop = $value,
            MathItem::Scripts(comp) => comp.props.$prop = $value,
            MathItem::Accent(comp) => comp.props.$prop = $value,
            // MathItem::Cancel(comp) => comp.props.$prop = $value,
            // MathItem::Line(comp) => comp.props.$prop = $value,
            MathItem::Glyph(comp) => comp.props.$prop = $value,
            MathItem::Text(comp) => comp.props.$prop = $value,
            _ => {}
        }
    };
}

macro_rules! match_component_props {
    ($item:expr, $prop:ident, $( $other_arms:tt )*) => {
        match $item {
            MathItem::Group(comp) => comp.props.$prop,
            MathItem::Radical(comp) => comp.props.$prop,
            MathItem::Fraction(comp) => comp.props.$prop,
            MathItem::SkewedFraction(comp) => comp.props.$prop,
            MathItem::Table(comp) => comp.props.$prop,
            MathItem::Scripts(comp) => comp.props.$prop,
            MathItem::Accent(comp) => comp.props.$prop,
            // MathItem::Cancel(comp) => comp.props.$prop,
            // MathItem::Line(comp) => comp.props.$prop,
            MathItem::Glyph(comp) => comp.props.$prop,
            MathItem::Text(comp) => comp.props.$prop,

            $( $other_arms )*
        }
    };
}

impl MathItem {
    pub fn set_limits(&mut self, limits: Limits) {
        match_set_component_props!(self, limits, limits)
    }

    pub fn limits(&self) -> Limits {
        match_component_props!(self, limits,
            _ => Limits::Never,
        )
    }

    pub fn set_class(&mut self, class: MathClass) {
        match_set_component_props!(self, class, class)
    }

    pub fn class(&self) -> MathClass {
        match_component_props!(self, class,
            Self::Spacing(_, _) | Self::Space | Self::Linebreak => MathClass::Space,
            Self::Align | Self::Tag(_) => MathClass::Special,
        )
    }

    pub fn is_ignorant(&self) -> bool {
        match_component_props!(self, ignorant,
            Self::Tag(_) => true,
            _ => false,
        )
    }
}

/// A generic component that bundles a specific item with common properties.
#[derive(Debug, Clone)]
pub struct MathComponent<T> {
    /// The specific item.
    pub kind: T,
    /// The properties attached to this component.
    pub props: MathProperties,
}

impl<T> Deref for MathComponent<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T> DerefMut for MathComponent<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

macro_rules! impl_from_component {
    ($item:ty => $variant:ident) => {
        impl From<MathComponent<$item>> for MathItem {
            fn from(comp: MathComponent<$item>) -> Self {
                MathItem::$variant(comp)
            }
        }
    };
}

impl_from_component!(GroupItem => Group);
impl_from_component!(RadicalItem => Radical);
impl_from_component!(FractionItem => Fraction);
impl_from_component!(SkewedFractionItem => SkewedFraction);
impl_from_component!(TableItem => Table);
impl_from_component!(ScriptsItem => Scripts);
impl_from_component!(AccentItem => Accent);
// impl_from_component!(CancelItem => Cancel);
// impl_from_component!(LineItem => Line);
impl_from_component!(GlyphItem => Glyph);
impl_from_component!(TextItem => Text);

/// Shared properties for layoutable components.
#[derive(Debug, Clone)]
pub struct MathProperties {
    limits: Limits,
    class: MathClass,
    size: MathSize,
    ignorant: bool,
    text_like: bool,
    spaced: bool,
}

impl MathProperties {
    pub fn default(styles: StyleChain) -> Self {
        Self {
            limits: Limits::Never,
            class: styles.get(EquationElem::class).unwrap_or(MathClass::Normal),
            size: styles.get(EquationElem::size),
            ignorant: false,
            text_like: false,
            spaced: false,
        }
    }

    pub fn set_class(&mut self, class: MathClass) {
        self.class = class;
    }

    pub fn class(&self) -> MathClass {
        self.class
    }

    pub fn set_limits(&mut self, limits: Limits) {
        self.limits = limits;
    }
}

#[derive(Debug, Clone)]
pub struct GroupItem {
    pub items: MathRun,
}

impl GroupItem {
    pub fn new(items: Vec<MathItem>, styles: StyleChain) -> MathComponent<Self> {
        let kind = Self { items: MathRun::new(items) };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct RadicalItem {
    pub radicand: MathRun,
    pub index: Option<MathRun>,
}

impl RadicalItem {
    pub fn new(
        radicand: MathRun,
        index: Option<MathRun>,
        styles: StyleChain,
    ) -> MathComponent<Self> {
        let kind = Self { radicand, index };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct FractionItem {
    pub numerator: MathRun,
    pub denominator: MathRun,
    pub line: bool,
}

impl FractionItem {
    pub fn new(
        numerator: MathRun,
        denominator: MathRun,
        line: bool,
        styles: StyleChain,
    ) -> MathComponent<Self> {
        let kind = Self { numerator, denominator, line };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct SkewedFractionItem {
    pub numerator: MathRun,
    pub denominator: MathRun,
}

impl SkewedFractionItem {
    pub fn new(
        numerator: MathRun,
        denominator: MathRun,
        styles: StyleChain,
    ) -> MathComponent<Self> {
        let kind = Self { numerator, denominator };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct TableItem {
    /// By row.
    pub cells: Vec<Vec<MathRun>>,
}

impl TableItem {
    pub fn new(cells: Vec<Vec<MathRun>>, styles: StyleChain) -> MathComponent<Self> {
        let kind = Self { cells };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct ScriptsItem {
    pub base: MathRun,
    pub top: Option<MathRun>,
    pub bottom: Option<MathRun>,
    pub top_left: Option<MathRun>,
    pub bottom_left: Option<MathRun>,
    pub top_right: Option<MathRun>,
    pub bottom_right: Option<MathRun>,
}

impl ScriptsItem {
    pub fn new(
        base: MathRun,
        top: Option<MathRun>,
        bottom: Option<MathRun>,
        top_left: Option<MathRun>,
        bottom_left: Option<MathRun>,
        top_right: Option<MathRun>,
        bottom_right: Option<MathRun>,
        styles: StyleChain,
    ) -> MathComponent<Self> {
        let kind = Self {
            base,
            top,
            bottom,
            top_left,
            bottom_left,
            top_right,
            bottom_right,
        };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct AccentItem {
    pub base: MathRun,
    pub accent: MathRun,
    pub is_bottom: bool,
}

impl AccentItem {
    pub fn new(
        base: MathRun,
        accent: MathRun,
        is_bottom: bool,
        styles: StyleChain,
    ) -> MathComponent<Self> {
        let kind = Self { base, accent, is_bottom };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct TextItem {
    pub text: EcoString,
    pub span: Span,
}

impl TextItem {
    pub fn new(text: EcoString, styles: StyleChain, span: Span) -> MathComponent<Self> {
        let kind = Self { text, span };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct GlyphItem {
    pub text: EcoString,
    pub span: Span,
    pub stretch: Option<(Rel<Abs>, bool)>,
    pub mid_stretched: Option<bool>,
}

impl GlyphItem {
    pub fn new(text: EcoString, styles: StyleChain, span: Span) -> MathComponent<Self> {
        assert!(text.graphemes(true).count() == 1);

        let c = text.chars().next().unwrap();

        let limits = Limits::for_char(c);
        let class = styles
            .get(EquationElem::class)
            .or_else(|| default_math_class(c))
            .unwrap_or(MathClass::Normal);

        let kind = Self { text, span, stretch: None, mid_stretched: None };
        let mut props = MathProperties::default(styles);
        props.set_class(class);
        props.set_limits(limits);
        MathComponent { kind, props }
    }
}

/// A linear collection of [`MathItem`]s.
#[derive(Debug, Default, Clone)]
pub struct MathRun(Vec<MathItem>);

impl MathRun {
    /// Takes the given [`MathItem`]s and do some basic processing.
    pub fn new(items: Vec<MathItem>) -> Self {
        let iter = items.into_iter().peekable();
        let mut last: Option<usize> = None;
        let mut space: Option<MathItem> = None;
        let mut resolved: Vec<MathItem> = vec![];

        for mut item in iter {
            match item {
                // Keep space only if supported by spaced items.
                MathItem::Space => {
                    if last.is_some() {
                        space = Some(item);
                    }
                    continue;
                }

                // Explicit spacing disables automatic spacing.
                MathItem::Spacing(width, weak) => {
                    last = None;
                    space = None;

                    if weak {
                        match resolved.last_mut() {
                            None => continue,
                            Some(MathItem::Spacing(prev, true)) => {
                                *prev = (*prev).max(width);
                                continue;
                            }
                            Some(_) => {}
                        }
                    }

                    resolved.push(item);
                    continue;
                }

                // Alignment points are resolved later.
                MathItem::Align => {
                    resolved.push(item);
                    continue;
                }

                // New line, new things.
                MathItem::Linebreak => {
                    resolved.push(item);
                    space = None;
                    last = None;
                    continue;
                }

                _ => {}
            }

            // Convert variable operators into binary operators if something
            // precedes them and they are not preceded by a operator or comparator.
            if item.class() == MathClass::Vary
                && matches!(
                    last.map(|i| resolved[i].class()),
                    Some(
                        MathClass::Normal
                            | MathClass::Alphabetic
                            | MathClass::Closing
                            | MathClass::Fence
                    )
                )
            {
                item.set_class(MathClass::Binary);
            }

            // Insert spacing between the last and this non-ignorant item.
            if !item.is_ignorant() {
                // TODO
                // if let Some(i) = last
                //     && let Some(s) = spacing(&resolved[i], space.take(), &item)
                // {
                //     resolved.insert(i + 1, s);
                // }

                last = Some(resolved.len());
            }

            resolved.push(item);
        }

        if let Some(MathItem::Spacing(_, true)) = resolved.last() {
            resolved.pop();
        }

        Self(resolved)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, MathItem> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, MathItem> {
        self.0.iter_mut()
    }

    pub fn is_multiline(&self) -> bool {
        self.iter().any(|item| matches!(item, MathItem::Linebreak))
    }

    /// Split by linebreaks, and copy [`MathItem`]s into rows.
    pub fn rows(&self) -> Vec<Self> {
        let mut rows: Vec<_> = self
            .0
            .split(|item| matches!(item, MathItem::Linebreak))
            .map(|slice| Self(slice.to_vec()))
            .collect();
        rows.pop_if(|row| row.0.is_empty());
        rows
    }

    pub fn alignments(&self) -> (Vec<Vec<Self>>, usize) {
        let mut max_len = 0;
        let matrix = self
            .rows()
            .into_iter()
            .map(|row| {
                let cells: Vec<_> = row
                    .0
                    .split(|item| matches!(item, MathItem::Align))
                    .map(|slice| Self(slice.to_vec()))
                    .collect();
                max_len = max_len.max(cells.len());
                cells
            })
            .collect();
        (matrix, max_len)
    }

    pub fn row_count(&self) -> usize {
        let mut count =
            1 + self.0.iter().filter(|f| matches!(f, MathItem::Linebreak)).count();

        // A linebreak at the very end does not introduce an extra row.
        if let Some(f) = self.0.last()
            && matches!(f, MathItem::Linebreak)
        {
            count -= 1
        }
        count
    }

    pub fn into_item(&self, styles: StyleChain) -> MathItem {
        let mut items = self.0.clone();
        if items.len() == 1 {
            items.pop().unwrap()
        } else {
            GroupItem::new(items, styles).into()
        }
    }
}

/// The context for math layout.
pub struct MathContext<'a, 'v, 'e> {
    // External.
    engine: &'v mut Engine<'e>,
    locator: &'v mut SplitLocator<'a>,
    // Mutable.
    items: Vec<MathItem>,
}

impl<'a, 'v, 'e> MathContext<'a, 'v, 'e> {
    /// Create a new math context.
    pub fn new(engine: &'v mut Engine<'e>, locator: &'v mut SplitLocator<'a>) -> Self {
        Self { engine, locator, items: vec![] }
    }

    /// Push a item.
    pub fn push(&mut self, item: impl Into<MathItem>) {
        self.items.push(item.into());
    }

    /// Push multiple items.
    pub fn extend(&mut self, items: impl IntoIterator<Item = MathItem>) {
        self.items.extend(items);
    }

    /// Resolve the given element and return the resulting [`MathItem`]s.
    pub fn resolve_into_items(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Vec<MathItem>> {
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
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathRun> {
        Ok(MathRun::new(self.resolve_into_items(elem, styles)?))
    }

    pub fn resolve_into_item(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathItem> {
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
        content: &Content,
        styles: StyleChain,
    ) -> SourceResult<()> {
        let arenas = Arenas::default();
        let pairs = (self.engine.routines.realize)(
            RealizationKind::Math,
            self.engine,
            self.locator,
            &arenas,
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
fn resolve_realized(
    elem: &Content,
    ctx: &mut MathContext,
    styles: StyleChain,
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

    // accent.rs
    } else if let Some(elem) = elem.to_packed::<AccentElem>() {
        resolve_accent(elem, ctx, styles)?;

    // attach.rs
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

    // cancel.rs
    } else if let Some(elem) = elem.to_packed::<CancelElem>() {
        resolve_cancel(elem, ctx, styles)?;

    // frac.rs
    } else if let Some(elem) = elem.to_packed::<FracElem>() {
        resolve_frac(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BinomElem>() {
        resolve_binom(elem, ctx, styles)?;

    // lr.rs
    } else if let Some(elem) = elem.to_packed::<LrElem>() {
        resolve_lr(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<MidElem>() {
        resolve_mid(elem, ctx, styles)?;

    // matrix.rs
    } else if let Some(elem) = elem.to_packed::<VecElem>() {
        resolve_vec(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<MatElem>() {
        resolve_mat(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<CasesElem>() {
        resolve_cases(elem, ctx, styles)?;

    // mod.rs
    } else if elem.is::<AlignPointElem>() {
        ctx.push(MathItem::Align);
    } else if let Some(elem) = elem.to_packed::<ClassElem>() {
        resolve_class(elem, ctx, styles)?;

    // op.rs
    } else if let Some(elem) = elem.to_packed::<OpElem>() {
        resolve_op(elem, ctx, styles)?;

    // root.rs
    } else if let Some(elem) = elem.to_packed::<RootElem>() {
        resolve_root(elem, ctx, styles)?;

    // underover.rs
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

    // Everything else
    } else {
        // TODO
    }

    Ok(())
}

pub fn resolve_h(
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

pub fn resolve_text(
    elem: &Packed<TextElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    // TODO

    let variant = styles.get(EquationElem::variant);
    let bold = styles.get(EquationElem::bold);
    // Disable auto-italic.
    let italic = styles.get(EquationElem::italic).or(Some(false));

    let styled_text: EcoString = elem
        .text
        .chars()
        .flat_map(|c| to_style(c, MathStyle::select(c, variant, bold, italic)))
        .collect();

    ctx.push(TextItem::new(styled_text, styles, elem.span()));
    Ok(())
}

pub fn resolve_symbol(
    elem: &Packed<SymbolElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
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

pub fn resolve_box(
    _elem: &Packed<BoxElem>,
    _ctx: &mut MathContext,
    _styles: StyleChain,
) -> SourceResult<()> {
    Ok(())
}
