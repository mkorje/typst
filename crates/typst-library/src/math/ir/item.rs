#![allow(clippy::too_many_arguments)]
use std::cell::Cell;
use std::ops::{Deref, DerefMut, MulAssign};

use bumpalo::Bump;
use ecow::EcoString;
use smallvec::SmallVec;
use typst_syntax::Span;
use typst_utils::{Get, default_math_class};
use unicode_math_class::MathClass;
use unicode_segmentation::UnicodeSegmentation;

use crate::foundations::{Content, Packed, Smart, StyleChain};
use crate::introspection::Tag;
use crate::layout::{Abs, Axes, Axis, BoxElem, Em, FixedAlignment, PlaceElem, Rel};
use crate::math::{
    Augment, CancelAngle, EquationElem, LeftRightAlternator, Limits, MEDIUM, MathSize,
    THICK, THIN,
};
use crate::routines::Arenas;
use crate::visualize::FixedStroke;

/// The top-level item in the math intermediate representation.
///
/// This enum represents all possible items that can appear in a resolved math
/// expression. The resolution phase converts the content tree into a flat
/// sequence of these items, which are then processed by the layout phase.
#[derive(Debug, Clone)]
pub enum MathItem<'a> {
    /// A layoutable component with associated properties and styles.
    Component(MathComponent<'a>),
    /// Explicit spacing. The boolean indicates whether the spacing is "weak"
    /// (can be merged with adjacent weak spacing, taking the maximum).
    Spacing(Abs, bool),
    /// A regular space (e.g., from whitespace in the source).
    Space,
    /// A line break within the equation.
    Linebreak,
    /// An alignment point for multi-line equations.
    Align,
    /// An introspection tag (doesn't affect layout).
    Tag(Tag),
}

impl<'a> From<MathComponent<'a>> for MathItem<'a> {
    fn from(comp: MathComponent<'a>) -> MathItem<'a> {
        MathItem::Component(comp)
    }
}

impl<'a> MathItem<'a> {
    /// Returns the limit placement configuration for this item.
    pub fn limits(&self) -> Limits {
        match self {
            Self::Component(comp) => comp.props.limits,
            _ => Limits::Never,
        }
    }

    /// Returns the math class of this item.
    pub fn class(&self) -> MathClass {
        match self {
            Self::Component(comp) => comp.props.class,
            Self::Spacing(_, _) | Self::Space | Self::Linebreak => MathClass::Space,
            Self::Align | Self::Tag(_) => MathClass::Special,
        }
    }

    /// Returns the effective math class on the right side of this item.
    ///
    /// For fenced items with a closing delimiter, this returns `Closing`
    /// instead of the item's overall class.
    pub fn rclass(&self) -> MathClass {
        match self {
            Self::Component(MathComponent { kind: MathKind::Fenced(fence), .. })
                if fence.close.is_some() =>
            {
                MathClass::Closing
            }
            _ => self.class(),
        }
    }

    /// Returns the effective math class on the left side of this item.
    ///
    /// For fenced items with an opening delimiter, this returns `Opening`
    /// instead of the item's overall class.
    pub fn lclass(&self) -> MathClass {
        match self {
            Self::Component(MathComponent { kind: MathKind::Fenced(fence), .. })
                if fence.open.is_some() =>
            {
                MathClass::Opening
            }
            _ => self.class(),
        }
    }

    /// Returns the math size of this item, if it is a component.
    pub fn size(&self) -> Option<MathSize> {
        match self {
            Self::Component(comp) => Some(comp.props.size),
            _ => None,
        }
    }

    /// Whether this item should have space around it when adjacent to text.
    pub fn is_spaced(&self) -> bool {
        if self.class() == MathClass::Fence {
            return true;
        }

        if let Self::Component(comp) = self
            && comp.props.spaced
            && matches!(comp.props.class, MathClass::Normal | MathClass::Alphabetic)
        {
            true
        } else {
            false
        }
    }

    /// Whether this item should be ignored for spacing calculations.
    ///
    /// Ignorant items (like tags and placed content) don't affect the spacing
    /// between their neighbors.
    pub fn is_ignorant(&self) -> bool {
        match self {
            Self::Component(comp) => comp.props.ignorant,
            Self::Tag(_) => true,
            _ => false,
        }
    }

    /// Returns the source span of this item.
    pub fn span(&self) -> Span {
        match self {
            Self::Component(comp) => comp.props.span,
            _ => Span::detached(),
        }
    }

    /// Returns the style chain of this item, if it is a component.
    pub fn styles(&self) -> Option<StyleChain<'a>> {
        match self {
            Self::Component(comp) => Some(comp.styles),
            _ => None,
        }
    }

    /// Returns whether this glyph has been stretched as a middle delimiter.
    ///
    /// Returns `None` for non-glyph items or glyphs that haven't been processed
    /// for middle stretching yet.
    pub fn mid_stretched(&self) -> Option<bool> {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.mid_stretched.get()
        } else {
            None
        }
    }

    /// Whether this item contains multiple lines.
    pub fn is_multiline(&self) -> bool {
        let items = self.as_slice();
        let len = items.len();
        for (i, item) in items.iter().enumerate() {
            let is_last = i == len - 1;

            match item {
                // If it's a linebreak and not the last item, it counts.
                MathItem::Linebreak if !is_last => return true,
                MathItem::Component(MathComponent {
                    kind: MathKind::Fenced(fence),
                    ..
                }) => {
                    // Check for linebreak in the middle of the body, e.g.
                    // `(a \ b)`.
                    if fence.body.is_multiline() {
                        return true;
                    }

                    // The above check leaves out `(a \ )` and `(a \`, in the
                    // former case it should always count, but in the latter
                    // case it should only count if this isn't the last item.
                    if fence.body.ends_with_linebreak()
                        && (fence.close.is_some() || !is_last)
                    {
                        return true;
                    }
                }
                _ => {}
            }
        }

        false
    }

    /// Whether this item ends with a line break.
    fn ends_with_linebreak(&self) -> bool {
        match self.as_slice().last() {
            Some(MathItem::Linebreak) => true,
            Some(MathItem::Component(MathComponent {
                kind: MathKind::Fenced(fence),
                ..
            })) if fence.close.is_none() => fence.body.ends_with_linebreak(),
            _ => false,
        }
    }

    /// Returns the inner items if this is a group, or a slice containing
    /// just this item otherwise.
    pub fn as_slice(&self) -> &[MathItem<'a>] {
        if let MathItem::Component(comp) = self
            && let MathKind::Group(group) = &comp.kind
        {
            group.items
        } else {
            core::slice::from_ref(self)
        }
    }

    /// Sets the limit placement configuration for this item.
    pub(crate) fn set_limits(&mut self, limits: Limits) {
        if let Self::Component(comp) = self {
            comp.props.limits = limits;
        }
    }

    /// Sets the math class of this item.
    pub(crate) fn set_class(&mut self, class: MathClass) {
        if let Self::Component(comp) = self {
            comp.props.class = class;
        }
    }

    /// Sets the left spacing for this item.
    pub(crate) fn set_lspace(&mut self, lspace: Option<Em>) {
        if let Self::Component(comp) = self {
            comp.props.lspace = lspace;
        }
    }

    /// Sets the right spacing for this item.
    pub(crate) fn set_rspace(&mut self, rspace: Option<Em>) {
        if let Self::Component(comp) = self {
            comp.props.rspace = rspace;
        }
    }

    /// Sets whether this glyph has been stretched as a middle delimiter.
    pub(crate) fn set_mid_stretched(&self, mid_stretched: Option<bool>) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.mid_stretched.set(mid_stretched);
        }
    }

    /// Sets the stretch configuration for this glyph.
    pub(crate) fn set_stretch(&self, stretch: Stretch) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.stretch.replace(stretch);
        }
    }

    /// Sets the vertical stretch info for this glyph.
    pub(crate) fn set_y_stretch(&self, info: StretchInfo) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.stretch.update(|stretch| stretch.with_y(info));
        }
    }

    /// Updates the stretch info for both axes of this glyph.
    pub(crate) fn update_stretch(&self, info: StretchInfo) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.stretch.update(|stretch| stretch.update(info));
        }
    }

    /// Sets the reference size for relative stretching on the given axis.
    pub fn set_stretch_relative_to(&self, relative_to: Abs, axis: Axis) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.stretch.update(|stretch| stretch.relative_to(relative_to, axis));
        }
    }

    /// Sets the font size to use for short-fall calculations on the given axis.
    pub fn set_stretch_font_size(&self, font_size: Abs, axis: Axis) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.stretch.update(|stretch| stretch.font_size(font_size, axis));
        }
    }

    /// Enables the `flac` (flattened accents) OpenType feature for this glyph.
    pub fn set_flac(&self) {
        if let Self::Component(comp) = self
            && let MathKind::Glyph(glyph) = &comp.kind
        {
            glyph.flac.set(true);
        }
    }
}

/// A generic component that bundles a specific math item kind with common
/// properties and styles.
///
/// This is the main building block of the math IR, combining the specific
/// item type ([`MathKind`]) with shared metadata ([`MathProperties`]) and
/// the style chain at the point of resolution.
#[derive(Debug, Clone)]
pub struct MathComponent<'a> {
    /// The specific kind of math item.
    pub kind: MathKind<'a>,
    /// The properties attached to this component (class, spacing, etc.).
    pub props: MathProperties,
    /// The style chain at the point where this item was resolved.
    pub styles: StyleChain<'a>,
}

/// The specific kind of a layoutable math item.
///
/// Recursive or large variants are allocated in a bump arena and stored
/// as references to avoid excessive stack usage.
#[derive(Debug, Clone)]
pub enum MathKind<'a> {
    /// A group of items laid out horizontally.
    Group(GroupItem<'a>),
    /// Text content (e.g., from `text()` or multi-character identifiers).
    Text(TextItem<'a>),
    /// External content that needs to be laid out separately.
    External(ExternalItem<'a>),
    /// A box element.
    Box(BoxItem<'a>),
    /// A single glyph (grapheme cluster) in the math font.
    Glyph(&'a GlyphItem),
    /// An over/underline (`overline`, `underline`).
    Line(&'a LineItem<'a>),
    /// Grouped prime symbols.
    Primes(&'a PrimesItem<'a>),
    /// A radical (square root or nth root).
    Radical(&'a RadicalItem<'a>),
    /// Fenced content with optional delimiters (`lr`).
    Fenced(&'a FencedItem<'a>),
    /// A vertical fraction.
    Fraction(&'a FractionItem<'a>),
    /// An inline skewed fraction.
    SkewedFraction(&'a SkewedFractionItem<'a>),
    /// A table/matrix.
    Table(&'a TableItem<'a>),
    /// A base with scripts/limits attached.
    Scripts(&'a ScriptsItem<'a>),
    /// A base with an accent above or below.
    Accent(&'a AccentItem<'a>),
    /// A base with a cancel line through it.
    Cancel(&'a CancelItem<'a>),
}

/// Shared properties for all layoutable math components.
///
/// These properties are computed during the resolution phase and used
/// during layout to determine spacing, positioning, and other behavior.
#[derive(Debug, Clone)]
pub struct MathProperties {
    /// How attachments (scripts/limits) should be positioned.
    pub limits: Limits,
    /// The math class, which determines spacing behavior.
    pub class: MathClass,
    /// The current math size (display, text, script, scriptscript).
    pub size: MathSize,
    /// Whether this item should be ignored for spacing calculations.
    pub ignorant: bool,
    /// Whether this item should have space around it when adjacent to text.
    pub spaced: bool,
    /// Explicit left spacing override.
    pub lspace: Option<Em>,
    /// Explicit right spacing override.
    pub rspace: Option<Em>,
    /// The source span for error reporting.
    pub span: Span,
}

impl MathProperties {
    /// Creates default properties from the given style chain.
    ///
    /// This looks up the math class and size from the styles.
    pub fn default(styles: StyleChain) -> MathProperties {
        Self {
            limits: Limits::Never,
            class: styles.get(EquationElem::class).unwrap_or(MathClass::Normal),
            size: styles.get(EquationElem::size),
            ignorant: false,
            spaced: false,
            lspace: None,
            rspace: None,
            span: Span::detached(),
        }
    }

    /// Creates properties with an explicit class, avoiding the style lookup.
    ///
    /// Use this when you already know the class to avoid redundant style lookups.
    fn with_explicit_class(styles: StyleChain, class: MathClass) -> MathProperties {
        Self {
            limits: Limits::Never,
            class,
            size: styles.get(EquationElem::size),
            ignorant: false,
            spaced: false,
            lspace: None,
            rspace: None,
            span: Span::detached(),
        }
    }

    /// Creates properties with explicit limits and class, avoiding style lookups.
    ///
    /// Use this when you already know both values to avoid redundant style lookups.
    fn with_explicit_limits_and_class(
        styles: StyleChain,
        limits: Limits,
        class: MathClass,
    ) -> MathProperties {
        Self {
            limits,
            class,
            size: styles.get(EquationElem::size),
            ignorant: false,
            spaced: false,
            lspace: None,
            rspace: None,
            span: Span::detached(),
        }
    }

    /// Sets whether this item should be ignored for spacing calculations.
    fn with_ignorant(mut self, ignorant: bool) -> Self {
        self.ignorant = ignorant;
        self
    }

    /// Sets whether this item should have space around it when adjacent to text.
    fn with_spaced(mut self, spaced: bool) -> Self {
        self.spaced = spaced;
        self
    }

    /// Sets the source span for this item.
    fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

/// A group of math items laid out horizontally.
///
/// Groups are created during resolution to combine multiple items that
/// should be treated as a unit for layout purposes.
#[derive(Debug, Clone)]
pub struct GroupItem<'a> {
    /// The items in this group, after preprocessing (spacing computed).
    pub items: &'a [MathItem<'a>],
}

impl<'a> GroupItem<'a> {
    /// Creates a new group from the given items.
    ///
    /// The items are preprocessed to compute spacing between them.
    /// The `closing_exists` parameter indicates whether this group ends with
    /// closing punctuation, which affects trailing space behavior.
    pub(crate) fn create<I>(
        items: I,
        closing_exists: bool,
        styles: StyleChain<'a>,
        arenas: &'a Arenas,
    ) -> MathItem<'a>
    where
        I: IntoIterator<Item = MathItem<'a>>,
        I::IntoIter: ExactSizeIterator,
    {
        let props = MathProperties::default(styles);
        let kind =
            MathKind::Group(Self { items: preprocess(items, arenas, closing_exists) });
        MathComponent { kind, props, styles }.into()
    }
}

/// A radical (square root or nth root).
///
/// The radical consists of the radicand (the expression under the radical),
/// an optional index for nth roots, and the radical symbol itself.
#[derive(Debug, Clone)]
pub struct RadicalItem<'a> {
    /// The expression under the radical sign.
    pub radicand: MathItem<'a>,
    /// The index for nth roots (e.g., 3 for cube root). `None` for square roots.
    pub index: Option<MathItem<'a>>,
    /// The radical symbol (√), which may be stretched to fit the radicand.
    pub sqrt: MathItem<'a>,
}

impl<'a> RadicalItem<'a> {
    /// Creates a new radical item.
    pub(crate) fn create(
        radicand: MathItem<'a>,
        index: Option<MathItem<'a>>,
        sqrt: MathItem<'a>,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let kind = MathKind::Radical(bump.alloc(Self { radicand, index, sqrt }));
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props, styles }.into()
    }
}

/// Fenced content with optional opening and closing delimiters.
///
/// This represents `lr()` constructs in Typst, which allow pairing of
/// delimiters that stretch to match their content height.
#[derive(Debug, Clone)]
pub struct FencedItem<'a> {
    /// The opening delimiter (e.g., `(`, `[`, `{`). `None` if omitted.
    pub open: Option<MathItem<'a>>,
    /// The closing delimiter (e.g., `)`, `]`, `}`). `None` if omitted.
    pub close: Option<MathItem<'a>>,
    /// The content between the delimiters.
    pub body: MathItem<'a>,
    /// Whether the delimiters should be sized to match each other.
    pub balanced: bool,
}

impl<'a> FencedItem<'a> {
    /// Creates a new fenced item.
    pub(crate) fn create(
        open: Option<MathItem<'a>>,
        close: Option<MathItem<'a>>,
        body: MathItem<'a>,
        balanced: bool,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let kind = MathKind::Fenced(bump.alloc(Self { open, close, body, balanced }));
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props, styles }.into()
    }
}

/// A vertical fraction with numerator stacked above denominator.
#[derive(Debug, Clone)]
pub struct FractionItem<'a> {
    /// The numerator (top part of the fraction).
    pub numerator: MathItem<'a>,
    /// The denominator (bottom part of the fraction).
    pub denominator: MathItem<'a>,
    /// Whether to draw a fraction line between numerator and denominator.
    /// False for binomial coefficients and similar constructs.
    pub line: bool,
    /// Spacing to add around the numerator and denominator.
    pub around: Em,
}

impl<'a> FractionItem<'a> {
    /// Creates a new fraction item.
    pub(crate) fn create(
        numerator: MathItem<'a>,
        denominator: MathItem<'a>,
        line: bool,
        around: Em,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let kind =
            MathKind::Fraction(bump.alloc(Self { numerator, denominator, line, around }));
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props, styles }.into()
    }
}

/// An inline skewed fraction (e.g., `1/2` displayed as `¹⁄₂`).
///
/// Unlike vertical fractions, skewed fractions are laid out horizontally
/// with a slanted fraction bar.
#[derive(Debug, Clone)]
pub struct SkewedFractionItem<'a> {
    /// The numerator (left/top part of the fraction).
    pub numerator: MathItem<'a>,
    /// The denominator (right/bottom part of the fraction).
    pub denominator: MathItem<'a>,
    /// The fraction slash character.
    pub slash: MathItem<'a>,
}

impl<'a> SkewedFractionItem<'a> {
    /// Creates a new skewed fraction item.
    pub(crate) fn create(
        numerator: MathItem<'a>,
        denominator: MathItem<'a>,
        slash: MathItem<'a>,
        styles: StyleChain<'a>,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let kind =
            MathKind::SkewedFraction(bump.alloc(Self { numerator, denominator, slash }));
        let props = MathProperties::default(styles);
        MathComponent { kind, props, styles }.into()
    }
}

/// A table or matrix of math items.
///
/// This is used for `mat()`, `vec()`, `cases()`, and similar constructs
/// that arrange content in a grid.
#[derive(Debug, Clone)]
pub struct TableItem<'a> {
    /// The cells of the table, organized by row.
    pub cells: &'a [&'a [MathItem<'a>]],
    /// The gap between rows (y) and columns (x).
    pub gap: Axes<Rel<Abs>>,
    /// Augmentation lines to draw in the matrix.
    pub augment: Option<Augment<Abs>>,
    /// The alignment for cells.
    pub align: FixedAlignment,
    /// Controls left/right alternation for alignment in `cases()`.
    pub alternator: LeftRightAlternator,
}

impl<'a> TableItem<'a> {
    /// Creates a new table item.
    pub(crate) fn create(
        cells: Vec<Vec<MathItem<'a>>>,
        gap: Axes<Rel<Abs>>,
        augment: Option<Augment<Abs>>,
        align: FixedAlignment,
        alternator: LeftRightAlternator,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let cells = bump.alloc_slice_fill_iter(cells.into_iter().map(|row| {
            let row_slice = bump.alloc_slice_fill_iter(row);
            row_slice as &[MathItem<'a>]
        }));
        let kind =
            MathKind::Table(bump.alloc(Self { cells, gap, augment, align, alternator }));
        let props = MathProperties::default(styles).with_span(span);
        MathComponent { kind, props, styles }.into()
    }
}

/// A base with scripts (subscripts/superscripts) or limits attached.
///
/// Scripts can be placed in six positions around the base: top, bottom,
/// top-left, bottom-left, top-right, and bottom-right. The `top` and `bottom`
/// positions may be rendered as limits (above/below) or scripts (superscript/
/// subscript) depending on the base and context.
#[derive(Debug, Clone)]
pub struct ScriptsItem<'a> {
    /// The base expression.
    pub base: MathItem<'a>,
    /// The top attachment (superscript or limit above).
    pub top: Option<MathItem<'a>>,
    /// The bottom attachment (subscript or limit below).
    pub bottom: Option<MathItem<'a>>,
    /// The top-left attachment (pre-superscript).
    pub top_left: Option<MathItem<'a>>,
    /// The bottom-left attachment (pre-subscript).
    pub bottom_left: Option<MathItem<'a>>,
    /// The top-right attachment (post-superscript).
    pub top_right: Option<MathItem<'a>>,
    /// The bottom-right attachment (post-subscript).
    pub bottom_right: Option<MathItem<'a>>,
}

impl<'a> ScriptsItem<'a> {
    /// Creates a new scripts item.
    ///
    /// The item inherits its math class from the base.
    pub(crate) fn create(
        base: MathItem<'a>,
        top: Option<MathItem<'a>>,
        bottom: Option<MathItem<'a>>,
        top_left: Option<MathItem<'a>>,
        bottom_left: Option<MathItem<'a>>,
        top_right: Option<MathItem<'a>>,
        bottom_right: Option<MathItem<'a>>,
        styles: StyleChain<'a>,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let props = MathProperties::with_explicit_class(styles, base.class());
        let kind = MathKind::Scripts(bump.alloc(Self {
            base,
            top,
            bottom,
            top_left,
            bottom_left,
            top_right,
            bottom_right,
        }));
        MathComponent { kind, props, styles }.into()
    }
}

/// A base with an accent mark above or below.
///
/// This handles diacritical marks like hats (^), tildes (~), bars, etc.
/// that are placed above or below the base expression.
#[derive(Debug, Clone)]
pub struct AccentItem<'a> {
    /// The base expression that receives the accent.
    pub base: MathItem<'a>,
    /// The accent character, which may be stretched to match the base width.
    pub accent: MathItem<'a>,
    /// Whether this is a bottom accent (underaccent) rather than a top accent.
    pub is_bottom: bool,
    /// Whether the frame width should match exactly (for wide accents like `wide`).
    pub exact_frame_width: bool,
}

impl<'a> AccentItem<'a> {
    /// Creates a new accent item.
    ///
    /// The item inherits its math class from the base.
    pub(crate) fn create(
        base: MathItem<'a>,
        accent: MathItem<'a>,
        is_bottom: bool,
        exact_frame_width: bool,
        styles: StyleChain<'a>,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let props = MathProperties::with_explicit_class(styles, base.class());
        let kind = MathKind::Accent(bump.alloc(Self {
            base,
            accent,
            is_bottom,
            exact_frame_width,
        }));
        MathComponent { kind, props, styles }.into()
    }
}

/// A base with a cancel line drawn through it.
///
/// This is used for the `cancel()` function to strike through expressions.
#[derive(Debug, Clone)]
pub struct CancelItem<'a> {
    /// The base expression to cancel.
    pub base: MathItem<'a>,
    /// The length of the cancel line relative to the base size.
    pub length: Rel<Abs>,
    /// The stroke style for the cancel line.
    pub stroke: FixedStroke,
    /// Whether to draw a cross (two lines) instead of a single line.
    pub cross: bool,
    /// Whether to invert the angle of the first line.
    pub invert_first_line: bool,
    /// The angle of the cancel line.
    pub angle: Smart<CancelAngle>,
}

impl<'a> CancelItem<'a> {
    /// Creates a new cancel item.
    ///
    /// The item inherits its math class from the base.
    pub(crate) fn create(
        base: MathItem<'a>,
        length: Rel<Abs>,
        stroke: FixedStroke,
        cross: bool,
        invert_first_line: bool,
        angle: Smart<CancelAngle>,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let props =
            MathProperties::with_explicit_class(styles, base.class()).with_span(span);
        let kind = MathKind::Cancel(bump.alloc(Self {
            base,
            length,
            stroke,
            cross,
            invert_first_line,
            angle,
        }));
        MathComponent { kind, props, styles }.into()
    }
}

/// A base with a line drawn above or below.
///
/// This is used for `overline()` and `underline()` functions.
#[derive(Debug, Clone)]
pub struct LineItem<'a> {
    /// The base expression.
    pub base: MathItem<'a>,
    /// Whether the line is drawn below (`underline`) rather than above (`overline`).
    pub under: bool,
}

impl<'a> LineItem<'a> {
    /// Creates a new line item.
    ///
    /// The item inherits its math class from the base.
    pub(crate) fn create(
        base: MathItem<'a>,
        under: bool,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let props =
            MathProperties::with_explicit_class(styles, base.class()).with_span(span);
        let kind = MathKind::Line(bump.alloc(Self { base, under }));
        MathComponent { kind, props, styles }.into()
    }
}

/// Grouped prime symbols (e.g., `f'''` for triple prime).
///
/// Multiple apostrophes in math mode are grouped into a single primes item
/// for proper typesetting.
#[derive(Debug, Clone)]
pub struct PrimesItem<'a> {
    /// The prime character to repeat.
    pub prime: MathItem<'a>,
    /// The number of primes to display.
    pub count: usize,
}

impl<'a> PrimesItem<'a> {
    /// Creates a new primes item.
    pub(crate) fn create(
        prime: MathItem<'a>,
        count: usize,
        styles: StyleChain<'a>,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let kind = MathKind::Primes(bump.alloc(Self { prime, count }));
        let props = MathProperties::default(styles);
        MathComponent { kind, props, styles }.into()
    }
}

/// Text content in a math expression.
///
/// This is used for multi-character identifiers and text produced by
/// the `text()` function within math mode.
#[derive(Debug, Clone)]
pub struct TextItem<'a> {
    /// The text content.
    pub text: &'a str,
}

impl<'a> TextItem<'a> {
    /// Creates a new text item.
    ///
    /// If `line` is true, the text is treated as a line of text (like from
    /// the `text()` function) and gets `Alphabetic` class with spacing.
    pub(crate) fn create(
        text: EcoString,
        line: bool,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        let text = bump.alloc_str(&text);
        let kind = MathKind::Text(Self { text });
        let props = if line {
            // Avoid class lookup when we're going to override it anyway.
            MathProperties::with_explicit_class(styles, MathClass::Alphabetic)
                .with_spaced(true)
        } else {
            MathProperties::default(styles)
        }
        .with_span(span);
        MathComponent { kind, props, styles }.into()
    }
}

/// Stretch configuration for a glyph on both axes.
///
/// This stores optional stretch information for the horizontal and vertical
/// axes, which is used to stretch delimiters and large operators.
#[derive(Debug, Clone, Copy)]
pub struct Stretch(Axes<Option<StretchInfo>>);

impl Stretch {
    /// Creates a new stretch with no stretching on either axis.
    pub(crate) fn new() -> Self {
        Self(Axes::splat(None))
    }

    /// Adds horizontal stretch information.
    pub(crate) fn with_x(mut self, info: StretchInfo) -> Self {
        self.0.x = Some(info);
        self
    }

    /// Adds vertical stretch information.
    pub(crate) fn with_y(mut self, info: StretchInfo) -> Self {
        self.0.y = Some(info);
        self
    }

    /// Updates stretch info for both axes, combining with existing info.
    pub(crate) fn update(mut self, info: StretchInfo) -> Self {
        match &mut self.0.x {
            Some(val) => *val *= info,
            None => self.0.x = Some(info),
        }
        match &mut self.0.y {
            Some(val) => *val *= info,
            None => self.0.y = Some(info),
        }
        self
    }

    /// Sets the reference size for relative stretching on the given axis.
    ///
    /// Only sets the value if not already set.
    pub fn relative_to(mut self, relative_to: Abs, axis: Axis) -> Self {
        if let Some(info) = self.0.get_mut(axis)
            && info.relative_to.is_none()
        {
            info.relative_to = Some(relative_to);
        }
        self
    }

    /// Sets the font size for short-fall calculations on the given axis.
    ///
    /// Only sets the value if not already set.
    pub fn font_size(mut self, font_size: Abs, axis: Axis) -> Self {
        if let Some(info) = self.0.get_mut(axis)
            && info.font_size.is_none()
        {
            info.font_size = Some(font_size);
        }
        self
    }

    /// Returns the stretch info for the given axis, if any.
    pub fn resolve(self, axis: Axis) -> Option<StretchInfo> {
        self.0.get(axis)
    }
}

/// Information about how to stretch a glyph on one axis.
#[derive(Debug, Clone, Copy)]
pub struct StretchInfo {
    /// The target size to stretch to (may be relative).
    pub target: Rel<Abs>,
    /// The short-fall amount for glyph assembly.
    pub short_fall: Em,
    /// The reference size for relative targets. Resolved during layout.
    pub relative_to: Option<Abs>,
    /// The font size to use for short-fall. Resolved during layout.
    pub font_size: Option<Abs>,
}

impl StretchInfo {
    /// Creates new stretch info with the given target and short-fall.
    pub(crate) fn new(target: Rel<Abs>, short_fall: Em) -> Self {
        Self {
            target,
            short_fall,
            relative_to: None,
            font_size: None,
        }
    }
}

impl MulAssign for StretchInfo {
    /// Combines two stretch infos by multiplying their targets.
    fn mul_assign(&mut self, rhs: Self) {
        self.target = Rel::new(
            self.target.rel * rhs.target.rel,
            rhs.target.rel.of(self.target.abs) + rhs.target.abs,
        );
        // TODO: how should short_fall work here???
        self.short_fall = rhs.short_fall;
    }
}

/// A single glyph (grapheme cluster) in a math expression.
#[derive(Debug, Clone)]
pub struct GlyphItem {
    /// The text content (a single grapheme cluster).
    pub text: EcoString,
    /// How the glyph should be stretched (for delimiters and large operators).
    pub stretch: Cell<Stretch>,
    /// Whether this glyph has been stretched as a middle delimiter.
    pub mid_stretched: Cell<Option<bool>>,
    /// Whether to apply the `flac` (flattened accents) OpenType feature.
    pub flac: Cell<bool>,
    /// Whether to use dotless forms (for characters under accents).
    pub dtls: bool,
}

impl GlyphItem {
    /// Creates a [`MathItem`] for a single glyph (grapheme cluster).
    ///
    /// The `dtls` parameter indicates whether dotless forms should be used
    /// (for characters under accents like i and j).
    pub(crate) fn create<'a>(
        text: EcoString,
        dtls: bool,
        styles: StyleChain<'a>,
        span: Span,
        bump: &'a Bump,
    ) -> MathItem<'a> {
        assert!(text.graphemes(true).count() == 1);

        let c = text.chars().next().unwrap();

        // Compute the default class once and reuse it for both limits and
        // class determination. This avoids calling `default_math_class` twice,
        // which is a measurable optimization for math-heavy documents.
        let default_class = default_math_class(c);
        let limits = Limits::for_char_with_class(c, default_class);
        let class = styles
            .get(EquationElem::class)
            .or(default_class)
            .unwrap_or(MathClass::Normal);

        let kind = MathKind::Glyph(bump.alloc(Self {
            text,
            stretch: Cell::new(Stretch::new()),
            mid_stretched: Cell::new(None),
            flac: Cell::new(false),
            dtls,
        }));
        let props = MathProperties::with_explicit_limits_and_class(styles, limits, class)
            .with_span(span);
        MathComponent { kind, props, styles }.into()
    }
}

/// A box element in a math expression.
///
/// This wraps a Typst `box` element that appears within math content.
#[derive(Debug, Clone)]
pub struct BoxItem<'a> {
    /// The box element to layout.
    pub elem: &'a Packed<BoxElem>,
}

impl<'a> BoxItem<'a> {
    /// Creates a new box item.
    pub(crate) fn create(
        elem: &'a Packed<BoxElem>,
        styles: StyleChain<'a>,
    ) -> MathItem<'a> {
        let kind = MathKind::Box(Self { elem });
        let props = MathProperties::default(styles).with_spaced(true);
        MathComponent { kind, props, styles }.into()
    }
}

/// External content that needs separate layout.
///
/// This represents non-math content that appears within a math expression,
/// such as images or other block-level elements.
#[derive(Debug, Clone)]
pub struct ExternalItem<'a> {
    /// The content to layout externally.
    pub content: &'a Content,
}

impl<'a> ExternalItem<'a> {
    /// Creates a new external item.
    ///
    /// If the content is a `PlaceElem`, it will be marked as ignorant
    /// (doesn't affect spacing between neighbors).
    pub(crate) fn create(content: &'a Content, styles: StyleChain<'a>) -> MathItem<'a> {
        let kind = MathKind::External(Self { content });
        let props = MathProperties::default(styles)
            .with_spaced(true)
            .with_ignorant(content.is::<PlaceElem>());
        MathComponent { kind, props, styles }.into()
    }
}

/// Takes the given [`MathItem`]s and do some basic processing.
///
/// The behavior of spacing around alignment points is subtle and differs from
/// the `align` environment in amsmath. The current policy is:
/// > always put the correct spacing between fragments separated by an
/// > alignment point, and always put the space on the left of the alignment
/// > point
pub(crate) fn preprocess<'a, I>(
    items: I,
    arenas: &'a Arenas,
    closing: bool,
) -> &'a [MathItem<'a>]
where
    I: IntoIterator<Item = MathItem<'a>>,
    I::IntoIter: ExactSizeIterator,
{
    let iter = items.into_iter();
    let mut resolved = MathBuffer::with_capacity(iter.len());
    let iter = iter.peekable();

    let mut last: Option<usize> = None;
    let mut space: Option<MathItem> = None;

    for mut item in iter {
        match item {
            // Tags don't affect layout.
            MathItem::Tag(_) => {
                resolved.push(item);
                continue;
            }

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
                    let Some(resolved_last) = resolved.last_mut() else { continue };
                    if let MathItem::Spacing(prev, true) = resolved_last {
                        *prev = (*prev).max(width);
                        continue;
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

    // Apply closing punctuation spacing if applicable.
    if closing
        && let Some(item) = resolved.last_mut()
        && item.rclass() == MathClass::Punctuation
        && item.size().is_none_or(|s| s > MathSize::Script)
    {
        item.set_rspace(Some(THIN))
    } else if let Some(idx) = resolved.last_index()
        && let MathItem::Spacing(_, true) = resolved.0[idx]
    {
        resolved.0.remove(idx);
    }

    arenas.bump.alloc_slice_fill_iter(resolved.0)
}

/// Computes the spacing between two adjacent math items.
///
/// This implements the spacing rules from the TeXBook, setting appropriate
/// left and right spacing on the items based on their math classes. Returns
/// a space item if spacing should be inserted for spaced frames.
fn spacing<'a>(
    l: &mut MathItem,
    space: Option<MathItem<'a>>,
    r: &mut MathItem,
) -> Option<MathItem<'a>> {
    use MathClass::*;

    let script = |f: &MathItem| f.size().is_some_and(|s| s <= MathSize::Script);

    match (l.rclass(), r.lclass()) {
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
        _ if (l.is_spaced() || r.is_spaced()) => return space,

        _ => {}
    };

    None
}

/// A wrapper around `SmallVec<[MathItem; 8]>` that ignores [`MathItem::Tag`]s
/// in some access methods.
struct MathBuffer<'a>(SmallVec<[MathItem<'a>; 8]>);

impl<'a> MathBuffer<'a> {
    /// Creates a new buffer with the given capacity.
    fn with_capacity(size: usize) -> Self {
        Self(SmallVec::with_capacity(size))
    }

    /// Returns a mutable reference to the last non-Tag fragment.
    fn last_mut(&mut self) -> Option<&mut MathItem<'a>> {
        self.0.iter_mut().rev().find(|f| !matches!(f, MathItem::Tag(_)))
    }

    /// Returns the physical index of the last non-Tag fragment.
    fn last_index(&self) -> Option<usize> {
        self.0.iter().rposition(|f| !matches!(f, MathItem::Tag(_)))
    }
}

impl<'a> Deref for MathBuffer<'a> {
    type Target = SmallVec<[MathItem<'a>; 8]>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for MathBuffer<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
