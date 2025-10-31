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

use crate::math::*;
use crate::visualize::FixedStroke;

/// The top-level item in a math sequence.
#[derive(Debug, Clone)]
pub enum MathItem<'a> {
    Group(MathComponent<'a, GroupItem<'a>>),
    Radical(MathComponent<'a, RadicalItem<'a>>),
    Fenced(MathComponent<'a, FencedItem<'a>>),
    Fraction(MathComponent<'a, FractionItem<'a>>),
    SkewedFraction(MathComponent<'a, SkewedFractionItem<'a>>),
    Table(MathComponent<'a, TableItem<'a>>),
    Scripts(MathComponent<'a, ScriptsItem<'a>>),
    Accent(MathComponent<'a, AccentItem<'a>>),
    Cancel(MathComponent<'a, CancelItem<'a>>),
    Line(MathComponent<'a, LineItem<'a>>),
    Primes(MathComponent<'a, PrimesItem<'a>>),
    Glyph(MathComponent<'a, GlyphItem>),
    Text(MathComponent<'a, TextItem>),
    External(MathComponent<'a, ExternalItem<'a>>),
    Box(MathComponent<'a, BoxItem<'a>>),

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
            MathItem::Fenced(comp) => comp.props.$prop = $value,
            MathItem::Fraction(comp) => comp.props.$prop = $value,
            MathItem::SkewedFraction(comp) => comp.props.$prop = $value,
            MathItem::Table(comp) => comp.props.$prop = $value,
            MathItem::Scripts(comp) => comp.props.$prop = $value,
            MathItem::Accent(comp) => comp.props.$prop = $value,
            MathItem::Cancel(comp) => comp.props.$prop = $value,
            MathItem::Line(comp) => comp.props.$prop = $value,
            MathItem::Primes(comp) => comp.props.$prop = $value,
            MathItem::Glyph(comp) => comp.props.$prop = $value,
            MathItem::Text(comp) => comp.props.$prop = $value,
            MathItem::External(comp) => comp.props.$prop = $value,
            MathItem::Box(comp) => comp.props.$prop = $value,
            _ => {}
        }
    };
}

macro_rules! match_component_props {
    ($item:expr, $prop:ident, $( $other_arms:tt )*) => {
        match $item {
            MathItem::Group(comp) => comp.props.$prop,
            MathItem::Radical(comp) => comp.props.$prop,
            MathItem::Fenced(comp) => comp.props.$prop,
            MathItem::Fraction(comp) => comp.props.$prop,
            MathItem::SkewedFraction(comp) => comp.props.$prop,
            MathItem::Table(comp) => comp.props.$prop,
            MathItem::Scripts(comp) => comp.props.$prop,
            MathItem::Accent(comp) => comp.props.$prop,
            MathItem::Cancel(comp) => comp.props.$prop,
            MathItem::Line(comp) => comp.props.$prop,
            MathItem::Primes(comp) => comp.props.$prop,
            MathItem::Glyph(comp) => comp.props.$prop,
            MathItem::Text(comp) => comp.props.$prop,
            MathItem::External(comp) => comp.props.$prop,
            MathItem::Box(comp) => comp.props.$prop,

            $( $other_arms )*
        }
    };
}

macro_rules! match_component_props_option {
    ($item:expr, $prop:ident, $( $other_arms:tt )*) => {
        match $item {
            MathItem::Group(comp) => Some(comp.props.$prop),
            MathItem::Radical(comp) => Some(comp.props.$prop),
            MathItem::Fenced(comp) => Some(comp.props.$prop),
            MathItem::Fraction(comp) => Some(comp.props.$prop),
            MathItem::SkewedFraction(comp) => Some(comp.props.$prop),
            MathItem::Table(comp) => Some(comp.props.$prop),
            MathItem::Scripts(comp) => Some(comp.props.$prop),
            MathItem::Accent(comp) => Some(comp.props.$prop),
            MathItem::Cancel(comp) => Some(comp.props.$prop),
            MathItem::Line(comp) => Some(comp.props.$prop),
            MathItem::Primes(comp) => Some(comp.props.$prop),
            MathItem::Glyph(comp) => Some(comp.props.$prop),
            MathItem::Text(comp) => Some(comp.props.$prop),
            MathItem::External(comp) => Some(comp.props.$prop),
            MathItem::Box(comp) => Some(comp.props.$prop),

            $( $other_arms )*
        }
    };
}

impl<'a> MathItem<'a> {
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

    pub fn set_size(&mut self, size: MathSize) {
        match_set_component_props!(self, size, size)
    }

    pub fn size(&self) -> Option<MathSize> {
        match_component_props_option!(self, size,
            _ => None,
        )
    }

    pub fn set_lspace(&mut self, lspace: Option<Em>) {
        match_set_component_props!(self, lspace, lspace)
    }

    pub fn lspace(&self) -> Option<Em> {
        match_component_props!(self, lspace,
            _ => None,
        )
    }

    pub fn set_rspace(&mut self, rspace: Option<Em>) {
        match_set_component_props!(self, rspace, rspace)
    }

    pub fn rspace(&self) -> Option<Em> {
        match_component_props!(self, rspace,
            _ => None,
        )
    }

    pub fn styles(&self) -> Option<StyleChain<'a>> {
        match_component_props_option!(self, styles,
            _ => None,
        )
    }

    pub fn set_spaced(&mut self, spaced: bool) {
        match_set_component_props!(self, spaced, spaced)
    }

    pub fn set_text_like(&mut self, text_like: bool) {
        match_set_component_props!(self, text_like, text_like)
    }

    pub fn is_spaced(&self) -> bool {
        if self.class() == MathClass::Fence {
            return true;
        }

        match_component_props_option!(self, spaced, _ => None)
            .and_then(|x| {
                (x && matches!(self.class(), MathClass::Normal | MathClass::Alphabetic))
                    .then_some(true)
            })
            .unwrap_or_default()
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
pub struct MathComponent<'a, T> {
    /// The specific item.
    pub kind: T,
    /// The properties attached to this component.
    pub props: MathProperties<'a>,
}

impl<'a, T> Deref for MathComponent<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<'a, T> DerefMut for MathComponent<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}

macro_rules! impl_from_component {
    ($item:ty => $variant:ident) => {
        impl<'a> From<MathComponent<'a, $item>> for MathItem<'a> {
            fn from(comp: MathComponent<'a, $item>) -> MathItem<'a> {
                MathItem::$variant(comp)
            }
        }
    };
}

impl_from_component!(GroupItem<'a> => Group);
impl_from_component!(RadicalItem<'a> => Radical);
impl_from_component!(FencedItem<'a> => Fenced);
impl_from_component!(FractionItem<'a> => Fraction);
impl_from_component!(SkewedFractionItem<'a> => SkewedFraction);
impl_from_component!(TableItem<'a> => Table);
impl_from_component!(ScriptsItem<'a> => Scripts);
impl_from_component!(AccentItem<'a> => Accent);
impl_from_component!(CancelItem<'a> => Cancel);
impl_from_component!(LineItem<'a> => Line);
impl_from_component!(PrimesItem<'a> => Primes);
impl_from_component!(GlyphItem => Glyph);
impl_from_component!(TextItem => Text);
impl_from_component!(ExternalItem<'a> => External);
impl_from_component!(BoxItem<'a> => Box);

/// Shared properties for layoutable components.
#[derive(Debug, Clone)]
pub struct MathProperties<'a> {
    limits: Limits,
    class: MathClass,
    size: MathSize,
    ignorant: bool,
    text_like: bool,
    spaced: bool,
    lspace: Option<Em>,
    rspace: Option<Em>,
    styles: StyleChain<'a>,
}

impl<'a> MathProperties<'a> {
    pub fn default(styles: StyleChain<'a>) -> MathProperties<'a> {
        Self {
            limits: Limits::Never,
            class: styles.get(EquationElem::class).unwrap_or(MathClass::Normal),
            size: styles.get(EquationElem::size),
            ignorant: false,
            text_like: false,
            spaced: false,
            lspace: None,
            rspace: None,
            styles,
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

    pub fn limits(&self) -> Limits {
        self.limits
    }

    pub fn set_spaced(&mut self, spaced: bool) {
        self.spaced = spaced;
    }

    pub fn set_ignorant(&mut self, ignorant: bool) {
        self.ignorant = ignorant;
    }

    pub fn set_text_like(&mut self, text_like: bool) {
        self.text_like = text_like;
    }
}

#[derive(Debug, Clone)]
pub struct GroupItem<'a> {
    pub items: MathRun<'a>,
}

impl<'a> GroupItem<'a> {
    pub fn new(
        items: Vec<MathItem<'a>>,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { items: MathRun::new(items) };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct RadicalItem<'a> {
    pub radicand: MathRun<'a>,
    pub index: Option<MathRun<'a>>,
    pub span: Span,
    pub sqrt: MathRun<'a>,
}

impl<'a> RadicalItem<'a> {
    pub fn new(
        radicand: MathRun<'a>,
        index: Option<MathRun<'a>>,
        styles: StyleChain<'a>,
        sqrt: MathRun<'a>,
        span: Span,
    ) -> MathComponent<'a, Self> {
        let kind = Self { radicand, index, span, sqrt };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct FencedItem<'a> {
    pub open: Option<MathRun<'a>>,
    pub close: Option<MathRun<'a>>,
    pub span: Span,
    pub body: MathRun<'a>,
    pub balanced: bool,
    pub target: Rel<Abs>,
}

impl<'a> FencedItem<'a> {
    pub fn new(
        open: Option<MathRun<'a>>,
        close: Option<MathRun<'a>>,
        span: Span,
        body: MathRun<'a>,
        styles: StyleChain<'a>,
        balanced: bool,
        target: Rel<Abs>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { open, close, span, body, balanced, target };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct FractionItem<'a> {
    pub numerator: MathRun<'a>,
    pub denominator: MathRun<'a>,
    pub line: bool,
    pub span: Span,
}

impl<'a> FractionItem<'a> {
    pub fn new(
        numerator: MathRun<'a>,
        denominator: MathRun<'a>,
        line: bool,
        span: Span,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { numerator, denominator, line, span };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct SkewedFractionItem<'a> {
    pub numerator: MathRun<'a>,
    pub denominator: MathRun<'a>,
}

impl<'a> SkewedFractionItem<'a> {
    pub fn new(
        numerator: MathRun<'a>,
        denominator: MathRun<'a>,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { numerator, denominator };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct TableItem<'a> {
    /// By row.
    pub cells: Vec<Vec<MathRun<'a>>>,
    pub gap: Axes<Rel<Abs>>,
    pub span: Span,
    pub augment: Option<Augment<Abs>>,
    pub align: FixedAlignment,
    pub alternator: LeftRightAlternator,
}

impl<'a> TableItem<'a> {
    pub fn new(
        cells: Vec<Vec<MathRun<'a>>>,
        styles: StyleChain<'a>,
        gap: Axes<Rel<Abs>>,
        align: FixedAlignment,
        alternator: LeftRightAlternator,
        augment: Option<Augment<Abs>>,
        span: Span,
    ) -> MathComponent<'a, Self> {
        let kind = Self { cells, gap, span, align, alternator, augment };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct ScriptsItem<'a> {
    pub base: MathRun<'a>,
    pub top: Option<MathRun<'a>>,
    pub bottom: Option<MathRun<'a>>,
    pub top_left: Option<MathRun<'a>>,
    pub bottom_left: Option<MathRun<'a>>,
    pub top_right: Option<MathRun<'a>>,
    pub bottom_right: Option<MathRun<'a>>,
}

impl<'a> ScriptsItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        top: Option<MathRun<'a>>,
        bottom: Option<MathRun<'a>>,
        top_left: Option<MathRun<'a>>,
        bottom_left: Option<MathRun<'a>>,
        top_right: Option<MathRun<'a>>,
        bottom_right: Option<MathRun<'a>>,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let mut props = MathProperties::default(styles);
        props.set_class(base.class());
        let kind = Self {
            base,
            top,
            bottom,
            top_left,
            bottom_left,
            top_right,
            bottom_right,
        };
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct AccentItem<'a> {
    pub base: MathRun<'a>,
    pub accent: MathRun<'a>,
    pub is_bottom: bool,
}

impl<'a> AccentItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        accent: MathRun<'a>,
        is_bottom: bool,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { base, accent, is_bottom };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct CancelItem<'a> {
    pub base: MathRun<'a>,
    pub span: Span,
    pub length: Rel<Abs>,
    pub stroke: FixedStroke,
    pub cross: bool,
    pub invert_first_line: bool,
    pub angle: Smart<CancelAngle>,
}

impl<'a> CancelItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        span: Span,
        length: Rel<Abs>,
        stroke: FixedStroke,
        invert_first_line: bool,
        cross: bool,
        angle: Smart<CancelAngle>,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self {
            base,
            span,
            length,
            stroke,
            invert_first_line,
            angle,
            cross,
        };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct LineItem<'a> {
    pub base: MathRun<'a>,
    pub under: bool,
    pub span: Span,
}

impl<'a> LineItem<'a> {
    pub fn new(
        base: MathRun<'a>,
        under: bool,
        styles: StyleChain<'a>,
        span: Span,
    ) -> MathComponent<'a, Self> {
        let kind = Self { base, under, span };
        let props = MathProperties::default(styles);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct PrimesItem<'a> {
    pub prime: MathRun<'a>,
    pub count: usize,
}

impl<'a> PrimesItem<'a> {
    pub fn new(
        prime: MathRun<'a>,
        count: usize,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { prime, count };
        let mut props = MathProperties::default(styles);
        props.set_text_like(true);
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
    pub stretch: Option<(Rel<Abs>, Option<Axis>)>,
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

#[derive(Debug, Clone)]
pub struct BoxItem<'a> {
    pub elem: &'a Packed<BoxElem>,
}

impl<'a> BoxItem<'a> {
    pub fn new(
        elem: &'a Packed<BoxElem>,
        styles: StyleChain<'a>,
    ) -> MathComponent<'a, Self> {
        let kind = Self { elem };
        let mut props = MathProperties::default(styles);
        props.set_spaced(true);
        MathComponent { kind, props }
    }
}

#[derive(Debug, Clone)]
pub struct ExternalItem<'a> {
    pub content: &'a Content,
}

impl<'a> ExternalItem<'a> {
    pub fn new(content: &'a Content, styles: StyleChain<'a>) -> MathComponent<'a, Self> {
        let kind = Self { content };
        let mut props = MathProperties::default(styles);
        props.set_spaced(true);
        props.set_ignorant(content.is::<PlaceElem>());
        MathComponent { kind, props }
    }
}

/// A linear collection of [`MathItem`]s.
#[derive(Debug, Default, Clone)]
pub struct MathRun<'a>(Vec<MathItem<'a>>);

impl<'a> MathRun<'a> {
    /// Takes the given [`MathItem`]s and do some basic processing.
    pub fn new(items: Vec<MathItem<'a>>) -> MathRun<'a> {
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
                if let Some(i) = last
                    && let Some(s) = spacing(&mut resolved[i], space.take(), &mut item)
                {
                    resolved.insert(i + 1, s);
                }

                last = Some(resolved.len());
            }

            resolved.push(item);
        }

        if let Some(MathItem::Spacing(_, true)) = resolved.last() {
            resolved.pop();
        }

        Self(resolved)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, MathItem<'a>> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, MathItem<'a>> {
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

    pub fn into_item(&self, styles: StyleChain<'a>) -> MathItem<'a> {
        let mut items = self.0.clone();
        if items.len() == 1 {
            items.pop().unwrap()
        } else {
            GroupItem::new(items, styles).into()
        }
    }

    pub fn class(&self) -> MathClass {
        if self.0.len() == 1 {
            self.0.first().map(|item| item.class()).unwrap_or(MathClass::Normal)
        } else {
            MathClass::Normal
        }
    }
}

/// Create the spacing between two items in a given style.
fn spacing<'a>(
    l: &mut MathItem,
    space: Option<MathItem<'a>>,
    r: &mut MathItem,
) -> Option<MathItem<'a>> {
    use MathClass::*;

    let script = |f: &MathItem| f.size().is_some_and(|s| s <= MathSize::Script);

    let mut l_class = l.class();
    let mut r_class = r.class();
    if let MathItem::Fenced(fenced) = l
        && fenced.close.is_some()
    {
        l_class = Closing;
    } else if let MathItem::Group(group) = l
        && let Some(last) = group.items.0.last()
    {
        l_class = last.class();
    }
    if let MathItem::Fenced(fenced) = r
        && fenced.open.is_some()
    {
        r_class = Opening;
    } else if let MathItem::Group(group) = r
        && let Some(first) = group.items.0.first()
    {
        r_class = first.class();
    }

    match (l_class, r_class) {
        // No spacing before punctuation; thin spacing after punctuation, unless
        // in script size.
        (_, Punctuation) => {}
        (Punctuation, _) if !script(l) => l.set_rspace(Some(THIN)),

        // No spacing after opening delimiters and before closing delimiters.
        (Opening, _) | (_, Closing) => {}

        // Thick spacing around relations, unless followed by a another relation
        // or in script size.
        (Relation, Relation) => {}
        (Relation, _) if !script(l) => l.set_rspace(Some(THICK)),
        (_, Relation) if !script(r) => r.set_lspace(Some(THICK)),

        // Medium spacing around binary operators, unless in script size.
        (Binary, _) if !script(l) => l.set_rspace(Some(MEDIUM)),
        (_, Binary) if !script(r) => r.set_lspace(Some(MEDIUM)),

        // Thin spacing around large operators, unless to the left of
        // an opening delimiter. TeXBook, p170
        (Large, Opening | Fence) => {}
        (Large, _) => l.set_rspace(Some(THIN)),

        (_, Large) => r.set_lspace(Some(THIN)),

        // Spacing around spaced frames.
        _ if (l.is_spaced() || r.is_spaced()) => return space.clone(),

        _ => {}
    };

    return None;
}

/// An iterator that alternates between the `Left` and `Right` values, if the
/// initial value is not `None`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LeftRightAlternator {
    None,
    Left,
    Right,
}

impl Iterator for LeftRightAlternator {
    type Item = LeftRightAlternator;

    fn next(&mut self) -> Option<Self::Item> {
        let r = Some(*self);
        match self {
            Self::None => {}
            Self::Left => *self = Self::Right,
            Self::Right => *self = Self::Left,
        }
        r
    }
}

/// The context for math layout.
pub struct MathContext<'a, 'v, 'e> {
    // External.
    engine: &'v mut Engine<'e>,
    locator: &'v mut SplitLocator<'a>,
    pub arenas: &'a Arenas,
    // Mutable.
    items: Vec<MathItem<'a>>,
}

impl<'a, 'v, 'e> MathContext<'a, 'v, 'e> {
    /// Create a new math context.
    pub fn new(
        engine: &'v mut Engine<'e>,
        locator: &'v mut SplitLocator<'a>,
        arenas: &'a Arenas,
    ) -> Self {
        Self { engine, locator, items: vec![], arenas }
    }

    /// Lifetime-extends some styles.
    pub fn store_styles(&self, styles: impl Into<Styles>) -> &'a Styles {
        self.arenas.styles.alloc(styles.into())
    }

    /// Lifetime-extends some content.
    pub fn store(&self, content: Content) -> &'a Content {
        self.arenas.content.alloc(content)
    }

    /// Push a item.
    pub fn push(&mut self, item: impl Into<MathItem<'a>>) {
        self.items.push(item.into());
    }

    /// Push multiple items.
    pub fn extend(&mut self, items: impl IntoIterator<Item = MathItem<'a>>) {
        self.items.extend(items);
    }

    /// Resolve the given element and return the resulting [`MathItem`]s.
    pub fn resolve_into_items(
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

    pub fn resolve_into_item(
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

/// Styles something as cramped.
fn style_cramped() -> Styles {
    EquationElem::cramped.set(true).wrap().into()
}

/// Sets flac OpenType feature.
fn style_flac() -> Styles {
    TextElem::features
        .set(FontFeatures(vec![(ttf_parser::Tag::from_bytes(b"flac"), 1)]))
        .wrap()
        .into()
}

/// Sets dtls OpenType feature.
fn style_dtls() -> Styles {
    TextElem::features
        .set(FontFeatures(vec![(ttf_parser::Tag::from_bytes(b"dtls"), 1)]))
        .wrap()
        .into()
}

/// The style for subscripts in the current style.
fn style_for_subscript(styles: StyleChain) -> Styles {
    let mut new = Styles::new();
    new.apply(style_for_superscript(styles));
    new.apply(EquationElem::cramped.set(true).wrap().into());
    new
}

/// The style for superscripts in the current style.
fn style_for_superscript(styles: StyleChain) -> Styles {
    EquationElem::size
        .set(match styles.get(EquationElem::size) {
            MathSize::Display | MathSize::Text => MathSize::Script,
            MathSize::Script | MathSize::ScriptScript => MathSize::ScriptScript,
        })
        .wrap()
        .into()
}

/// The style for numerators in the current style.
fn style_for_numerator(styles: StyleChain) -> Styles {
    EquationElem::size
        .set(match styles.get(EquationElem::size) {
            MathSize::Display => MathSize::Text,
            MathSize::Text => MathSize::Script,
            MathSize::Script | MathSize::ScriptScript => MathSize::ScriptScript,
        })
        .wrap()
        .into()
}

/// The style for denominators in the current style.
fn style_for_denominator(styles: StyleChain) -> Styles {
    let mut new = Styles::new();
    new.apply(style_for_numerator(styles));
    new.apply(EquationElem::cramped.set(true).wrap().into());
    new
}
