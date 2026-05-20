//! Shallow math IR types.
//!
//! Unlike the previous recursive IR, this is a flat sequence of `MathChild`
//! values. Structural children (fractions, attachments, fences, ...) hold their
//! bodies as unresolved `Content` + a `StyleChain`; recursion into those bodies
//! happens at layout time when the math layouter encounters such a child.
//!
//! Markers that previously lived in a shadow `RawMathItem` enum (`Linebreak`,
//! `Align`, `Mid`) are now first-class child variants. The containers that
//! care about them (the top-level equation layout, fenced layout) scan the
//! flat sequence at layout time and split on them.
//!
//! "Resolve-then-tweak" elements (`class`, `op`, `limits`, `scripts`,
//! `stretch`) collapse into a single `OverrideChild`, which carries the
//! overrides without forcing the body to be resolved.
#![allow(clippy::too_many_arguments)]

use std::cell::Cell;

use ecow::EcoString;
use typst_syntax::Span;
use typst_utils::{Get as _, default_math_class};
use unicode_math_class::MathClass;

use crate::foundations::{Content, Packed, Smart, StyleChain};
use crate::introspection::{Locator, Tag};
use crate::layout::{
    Abs, Axes, Axis, BoxElem, Em, FixedAlignment, Length, Rel,
};
use crate::math::{
    Augment, CancelAngle, EquationElem, LeftRightAlternator, Limits, MathSize,
};
use crate::visualize::FixedStroke;

// ---------------------------------------------------------------------------
// Core enum
// ---------------------------------------------------------------------------

/// A single element in the flat math IR sequence.
///
/// Each `MathChild` is either:
/// - a leaf (text, glyph, spacing, tag, ...),
/// - a flat-sequence marker (`Linebreak`, `Align`, `Mid`), consumed by the
///   enclosing layout container, or
/// - a structural wrapper that carries an unresolved `Content` body which
///   gets collected and laid out recursively at layout time.
#[derive(Debug)]
pub enum MathChild<'a> {
    // ---- Leaves -----------------------------------------------------------
    /// A regular space character.
    Space,
    /// Explicit absolute spacing. The second field is the font size at the
    /// point of creation; the third indicates weak spacing.
    Spacing(Length, Abs, bool),
    /// An introspection tag.
    Tag(Tag),
    /// Five or more grouped primes (1–4 are emitted as `Glyph` directly).
    Primes(PrimesData),
    /// A single grapheme cluster.
    Glyph(GlyphData),
    /// A text run (e.g. a multi-letter identifier).
    Text(TextData<'a>),
    /// A numeric literal.
    Number(NumberData),
    /// Inline content (a `BoxElem`) embedded in math.
    Box(BoxData<'a>),
    /// A MathML HTML element with an optional body.
    Mathml(MathmlData<'a>),
    /// External content that needs to be laid out separately.
    External(ExternalData<'a>),

    // ---- Markers ----------------------------------------------------------
    /// A hard line break, splitting the surrounding container into rows.
    Linebreak,
    /// An alignment point, splitting the surrounding row into columns.
    Align,
    /// A middle-stretched delimiter inside a `\left ... \right` group.
    ///
    /// Produced from `MidElem`. The body is rendered with mid-stretch behavior
    /// at layout time, in coordination with the enclosing `Lr`.
    Mid(MidData<'a>),

    // ---- Structural wrappers ----------------------------------------------
    /// An anonymous group with an unresolved body. Used for sequences that
    /// must be treated as a single element by their parent (e.g. an attach
    /// base that comes out as multiple children).
    Group(BodyChild<'a>),
    /// A scripted base (`AttachElem`). The synthesize pass on `AttachElem`
    /// has already merged any nested attachments, so this child holds the
    /// fully merged 6 positions.
    Attach(Box<AttachChild<'a>>),
    /// A vertical or horizontal fraction.
    Frac(Box<FracChild<'a>>),
    /// A binomial coefficient. Layout-time wraps it in parentheses.
    Binom(Box<BinomChild<'a>>),
    /// An inline skewed fraction.
    SkewedFrac(Box<SkewedFracChild<'a>>),
    /// An accent above or below a base.
    Accent(Box<AccentChild<'a>>),
    // NOTE: `op()`, `underbrace`, `overbrace`, `underbracket`, `overbracket`,
    // `underparen`, `overparen`, `undershell`, `overshell` do *not* appear
    // here — they desugar via show rules into combinations of `AttachChild`,
    // `AccentChild`, and `Override` before reaching the collector.
    /// An overline or underline.
    Line(Box<LineChild<'a>>),
    /// A `cancel` element.
    Cancel(Box<CancelChild<'a>>),
    /// A radical (square root or nth root).
    Root(Box<RootChild<'a>>),
    /// A `left ... right` delimited group. The body is an *unresolved*
    /// `Content` so that its inner flat sequence (with `Mid`/`Align`/
    /// `Linebreak` markers) is collected lazily during layout.
    Lr(Box<LrChild<'a>>),
    /// A matrix, vector, or cases element.
    Table(Box<TableChild<'a>>),
    /// Wraps a body, overriding one or more properties of the resolved
    /// result. Subsumes the old `class`/`op`/`limits`/`scripts`/`stretch`
    /// resolvers from bucket 2.
    Override(Box<OverrideChild<'a>>),
}

// ---------------------------------------------------------------------------
// Cheap accessors
// ---------------------------------------------------------------------------

impl<'a> MathChild<'a> {
    /// The math class this child reports without forcing recursion into its
    /// body.
    ///
    /// For leaves this is exact. For structural wrappers the class is derived
    /// from intrinsic properties (e.g. fractions are `Inner`, fences are
    /// `Fence`/`Opening`/`Closing`). For `Override` it's the override value
    /// if set, else delegated to the wrapped body's *element kind*. For
    /// children whose class genuinely depends on a deferred body, callers
    /// must resolve the body first.
    pub fn class(&self) -> MathClass {
        match self {
            Self::Space | Self::Spacing(..) => MathClass::Space,
            Self::Tag(_) => MathClass::Special,
            Self::Linebreak | Self::Align | Self::Mid(_) => MathClass::Special,
            Self::Primes(_) => MathClass::Normal,
            Self::Glyph(g) => g.class,
            Self::Text(_) => MathClass::Alphabetic,
            Self::Number(_) => MathClass::Normal,
            Self::Box(_) | Self::External(_) => MathClass::Normal,
            Self::Mathml(_) => MathClass::Normal,
            Self::Group(_) => MathClass::Normal,
            // Structural class is intrinsic to the variant.
            Self::Attach(_) | Self::Accent(_) | Self::Cancel(_) | Self::Line(_) => {
                // Inherits from base; resolved at layout time.
                MathClass::Normal
            }
            Self::Frac(_) | Self::Binom(_) | Self::SkewedFrac(_) => MathClass::Normal,
            Self::Root(_) => MathClass::Normal,
            Self::Lr(_) => MathClass::Normal,
            Self::Table(_) => MathClass::Normal,
            Self::Override(o) => o.class.unwrap_or(MathClass::Normal),
        }
    }

    /// Whether this child should be ignored for spacing calculations.
    pub fn is_ignorant(&self) -> bool {
        match self {
            Self::Tag(_) => true,
            Self::External(e) => e.ignorant,
            _ => false,
        }
    }

    /// Is this a marker that the enclosing container needs to split on?
    pub fn is_marker(&self) -> bool {
        matches!(self, Self::Linebreak | Self::Align)
    }

    /// If this child is (or contains) a single glyph eligible to receive
    /// stretch updates, returns it.
    ///
    /// Used by layout helpers that push stretch metadata into the body's
    /// underlying glyph before it's laid out — matching the old IR's
    /// behavior where `MathItem::set_stretch` propagated to the resolved
    /// glyph item.
    pub fn stretch_target(&self) -> Option<&GlyphData> {
        match self {
            Self::Glyph(g) => Some(g),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Glyph(g) => g.span,
            Self::Text(t) => t.span,
            Self::Number(n) => n.span,
            Self::Box(b) => b.elem.span(),
            Self::External(e) => e.content.span(),
            Self::Mathml(m) => m.elem.span(),
            Self::Mid(m) => m.span,
            Self::Group(g) => g.span,
            Self::Attach(c) => c.span,
            Self::Frac(c) => c.span,
            Self::Binom(c) => c.span,
            Self::SkewedFrac(c) => c.span,
            Self::Accent(c) => c.span,
            Self::Line(c) => c.span,
            Self::Cancel(c) => c.span,
            Self::Root(c) => c.span,
            Self::Lr(c) => c.span,
            Self::Table(c) => c.span,
            Self::Override(c) => c.span,
            _ => Span::detached(),
        }
    }
}

// ---------------------------------------------------------------------------
// Leaf data
// ---------------------------------------------------------------------------

/// A single styled grapheme cluster.
#[derive(Debug, Clone)]
pub struct GlyphData {
    pub text: EcoString,
    pub class: MathClass,
    pub limits: Limits,
    pub size: MathSize,
    pub span: Span,
    /// Mutable stretch state, updated by the enclosing `Lr` layout when this
    /// glyph acts as a delimiter.
    pub stretch: Cell<Stretch>,
    pub mid_stretched: Cell<Option<bool>>,
    pub flac: Cell<bool>,
}

impl GlyphData {
    pub fn new(text: EcoString, styles: StyleChain<'_>, span: Span) -> Self {
        let c = text.chars().next().unwrap();
        let default_class = default_math_class(c);
        let limits = Limits::for_char_with_class(c, default_class);
        let class = styles
            .get(EquationElem::class)
            .or(default_class)
            .unwrap_or(MathClass::Normal);
        Self {
            text,
            class,
            limits,
            size: styles.get(EquationElem::size),
            span,
            stretch: Cell::new(Stretch::new()),
            mid_stretched: Cell::new(None),
            flac: Cell::new(false),
        }
    }

    /// Sets the stretch configuration, marking it as explicit.
    pub fn set_stretch(&self, mut stretch: Stretch) {
        if let Some(info) = &mut stretch.0.x {
            info.explicit = true;
        }
        if let Some(info) = &mut stretch.0.y {
            info.explicit = true;
        }
        self.replace_stretch(stretch);
    }

    /// Replaces the stretch configuration.
    pub fn replace_stretch(&self, stretch: Stretch) {
        self.stretch.set(stretch);
    }

    /// Updates the vertical stretch info, combining with existing info.
    pub fn set_y_stretch(&self, mut info: StretchInfo) {
        info.explicit = true;
        let stretch = self.stretch.get();
        self.stretch.set(stretch.with_y(info));
    }

    /// Updates stretch info on both axes, combining with existing info.
    pub fn update_stretch(&self, info: StretchInfo) {
        let stretch = self.stretch.get();
        self.stretch.set(stretch.update(info));
    }

    /// Sets the reference size for relative stretching on the given axis,
    /// if not already set.
    pub fn set_stretch_relative_to(&self, relative_to: Abs, axis: Axis) {
        let stretch = self.stretch.get();
        self.stretch.set(stretch.relative_to(relative_to, axis));
    }

    /// Sets the font size for short-fall on the given axis, if not
    /// already set.
    pub fn set_stretch_font_size(&self, font_size: Abs, axis: Axis) {
        let stretch = self.stretch.get();
        self.stretch.set(stretch.font_size(font_size, axis));
    }

    /// Enables the flac OpenType feature.
    pub fn set_flac(&self) {
        self.flac.set(true);
    }
}

#[derive(Debug)]
pub struct TextData<'a> {
    pub text: EcoString,
    pub styles: StyleChain<'a>,
    pub locator: Locator<'a>,
    pub span: Span,
}

#[derive(Debug)]
pub struct NumberData {
    pub text: EcoString,
    pub span: Span,
}

#[derive(Debug)]
pub struct PrimesData {
    /// Always >= 5 (1–4 primes are emitted as a single `Glyph`).
    pub count: usize,
    pub span: Span,
}

#[derive(Debug)]
pub struct BoxData<'a> {
    pub elem: &'a Packed<BoxElem>,
    pub styles: StyleChain<'a>,
    pub locator: Locator<'a>,
}

#[derive(Debug)]
pub struct MathmlData<'a> {
    pub elem: &'a Content,
    /// Resolved body for HTML target. `None` otherwise.
    pub body: Option<Vec<MathChild<'a>>>,
    pub styles: StyleChain<'a>,
}

#[derive(Debug)]
pub struct ExternalData<'a> {
    pub content: &'a Content,
    pub styles: StyleChain<'a>,
    pub locator: Locator<'a>,
    pub ignorant: bool,
}

// ---------------------------------------------------------------------------
// Bodies
// ---------------------------------------------------------------------------

/// A body to be collected and laid out lazily.
///
/// This is the workhorse "unresolved subtree" handle. It carries the
/// `Content` to recurse into plus the style chain that should be in effect.
/// `Copy` so callers can freely pass it by value to layout helpers without
/// thinking about borrows.
#[derive(Debug, Clone, Copy)]
pub struct Body<'a> {
    pub content: &'a Content,
    pub styles: StyleChain<'a>,
}

impl<'a> Body<'a> {
    pub fn new(content: &'a Content, styles: StyleChain<'a>) -> Self {
        Self { content, styles }
    }
}

/// A structural child that wraps a single body with its own span/styles.
///
/// Used for `Group` (anonymous wrappers) and as a building block.
#[derive(Debug)]
pub struct BodyChild<'a> {
    pub body: Body<'a>,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Marker payload
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct MidData<'a> {
    pub body: Body<'a>,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Structural wrappers
// ---------------------------------------------------------------------------

/// A scripted base with attachments.
///
/// `base` is *eagerly collected* into a flat `Vec<MathChild>` at IR-build
/// time. This is the one place where we sacrifice laziness, because the
/// cross-level attachment merge (see `merge_nested_attachments` in
/// `collect`) needs to detect nested `AttachChild`s — and "is the base
/// itself an attach?" is a question best asked on already-collected IR
/// rather than by peeking at `Content`.
///
/// Other slots (`t`, `b`, `tl`, `tr`, `bl`, `br`) remain deferred `Body`
/// handles — they're collected during layout under script/limit styles.
/// The `tr` slot is *opportunistically* collected only when the primed-tr
/// special case might apply (see the merge).
#[derive(Debug)]
pub struct AttachChild<'a> {
    /// The (already-collected) base sequence.
    pub base: Vec<MathChild<'a>>,
    pub t: Option<Body<'a>>,
    pub b: Option<Body<'a>>,
    pub tl: Option<Body<'a>>,
    pub tr: Option<Body<'a>>,
    pub bl: Option<Body<'a>>,
    pub br: Option<Body<'a>>,
    pub span: Span,
}

impl<'a> AttachChild<'a> {
    /// Whether all six attachment slots are empty.
    pub fn is_empty(&self) -> bool {
        self.t.is_none()
            && self.b.is_none()
            && self.tl.is_none()
            && self.tr.is_none()
            && self.bl.is_none()
            && self.br.is_none()
    }
}

#[derive(Debug)]
pub struct FracChild<'a> {
    pub num: Body<'a>,
    pub denom: Body<'a>,
    /// Vertical-with-rule, vertical-no-rule (binom), or another style.
    pub style: FracStyleKind,
    pub padding: Em,
    pub span: Span,
}

/// Indicates which fraction style the user requested.
///
/// The `Binom` case is folded into a fenced wrapper at layout time.
#[derive(Debug, Clone, Copy)]
pub enum FracStyleKind {
    /// Standard vertical fraction with a rule.
    Vertical,
    /// Inline display using a `/`.
    Horizontal { num_deparen: bool, denom_deparen: bool },
}

#[derive(Debug)]
pub struct BinomChild<'a> {
    pub upper: Body<'a>,
    /// The lower row contains the comma-separated children. Each is its own
    /// body to avoid having to manufacture a `Content::sequence` at collect
    /// time — the layouter joins them with commas.
    pub lower: Vec<Body<'a>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct SkewedFracChild<'a> {
    pub num: Body<'a>,
    pub denom: Body<'a>,
    /// The fraction slash glyph, pre-built by the IR with its stretch
    /// metadata so layout can drive vertical stretching just like the
    /// old `resolve_skewed_frac` did with a resolved `MathItem`.
    pub slash: GlyphData,
    pub span: Span,
}

#[derive(Debug)]
pub struct AccentChild<'a> {
    pub base: Body<'a>,
    /// The accent glyph, pre-built by the IR with `MathClass::Diacritic`
    /// — same shape as the old `AccentItem.accent: MathItem`.
    pub accent: GlyphData,
    pub position: Position,
    pub dotless: bool,
    pub size: Rel<Length>,
    /// Whether the resulting frame should be sized to the accent's
    /// width (true for spreader characters such as `⏟`, `⏞`, etc.).
    pub exact_frame_width: bool,
    pub span: Span,
}

#[derive(Debug)]
pub struct LineChild<'a> {
    pub base: Body<'a>,
    pub position: Position,
    pub span: Span,
}

#[derive(Debug)]
pub struct CancelChild<'a> {
    pub base: Body<'a>,
    pub length: Rel<Abs>,
    pub stroke: FixedStroke,
    pub cross: bool,
    pub invert_first_line: bool,
    pub angle: Smart<CancelAngle>,
    pub span: Span,
}

#[derive(Debug)]
pub struct RootChild<'a> {
    pub radicand: Body<'a>,
    pub index: Option<Body<'a>>,
    /// The sqrt glyph, pre-built by the IR — same shape as the old
    /// `RadicalItem.sqrt: MathItem`.
    pub sqrt: GlyphData,
    pub span: Span,
}

#[derive(Debug)]
pub struct LrChild<'a> {
    /// The full LR body, including any delimiters at its ends and any
    /// `MidElem`/`AlignPointElem`/`LinebreakElem` markers inside. Collected
    /// lazily during layout — this is what enables the marker-aware
    /// processing (delimiter detection, mid stretching, multiline fence
    /// expansion) to live in layout-land instead of resolver-land.
    pub body: Body<'a>,
    pub size: Rel<Length>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TableChild<'a> {
    /// Cells, organized by row then column. Each cell is its own deferred
    /// body.
    pub cells: Vec<Vec<Body<'a>>>,
    pub gap: Axes<Rel<Abs>>,
    pub augment: Option<Augment<Abs>>,
    pub align: FixedAlignment,
    pub alternator: LeftRightAlternator,
    /// The optional opening / closing delimiter glyphs (matrix, vec,
    /// cases each have their own default pair), pre-built by the IR.
    pub open: Option<GlyphData>,
    pub close: Option<GlyphData>,
    pub span: Span,
}

/// Wraps a body and overrides one or more synthesized properties of its
/// resolved form.
///
/// Subsumes the previous `class`/`op`/`limits`/`scripts`/`stretch` resolvers:
/// each set the corresponding field. At layout time the body is resolved
/// normally, then the override is applied to the resulting fragment.
#[derive(Debug)]
pub struct OverrideChild<'a> {
    pub body: Body<'a>,
    /// If set, force the resolved item's math class to this value.
    pub class: Option<MathClass>,
    /// If set, force the resolved item's limit placement.
    pub limits: Option<Limits>,
    /// If set, apply this stretch on the resolved item.
    pub stretch: Option<StretchInfo>,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Stretch (preserved from the old IR)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Default)]
pub struct Stretch(Axes<Option<StretchInfo>>);

impl Stretch {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_x(mut self, info: StretchInfo) -> Self {
        self.0.x = Some(info);
        self
    }

    pub fn with_y(mut self, info: StretchInfo) -> Self {
        self.0.y = Some(info);
        self
    }

    /// Updates stretch info for both axes, combining with existing info.
    /// Marks them as explicit.
    pub fn update(mut self, mut info: StretchInfo) -> Self {
        info.explicit = true;
        match &mut self.0.x {
            Some(_) => self.0.x = Some(info),
            None => self.0.x = Some(info),
        }
        match &mut self.0.y {
            Some(_) => self.0.y = Some(info),
            None => self.0.y = Some(info),
        }
        self
    }

    /// Sets the reference size for relative stretching on the given axis.
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
    /// Only sets the value if not already set.
    pub fn font_size(mut self, font_size: Abs, axis: Axis) -> Self {
        if let Some(info) = self.0.get_mut(axis)
            && info.font_size.is_none()
        {
            info.font_size = Some(font_size);
        }
        self
    }

    pub fn resolve(self, axis: Axis) -> Option<StretchInfo> {
        self.0.get(axis)
    }

    pub fn is_explicit(self, axis: Axis) -> bool {
        self.0.get(axis).is_some_and(|info| info.explicit)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StretchInfo {
    /// The target size to stretch to.
    pub target: Rel<Abs>,
    /// Whether this stretch is explicit (vs. an auto-stretch e.g. from a
    /// large operator in display style).
    pub explicit: bool,
    /// The user-requested stretch target, if any.
    pub requested_target: Option<Rel<Length>>,
    /// The short-fall amount for glyph assembly.
    pub short_fall: Em,
    /// The reference size for relative targets.
    pub relative_to: Option<Abs>,
    /// The font size to use for short-fall.
    pub font_size: Option<Abs>,
}

impl StretchInfo {
    pub fn new(target: Rel<Abs>, short_fall: Em) -> Self {
        Self {
            target,
            explicit: false,
            requested_target: None,
            short_fall,
            relative_to: None,
            font_size: None,
        }
    }

    pub fn from_size(size: Rel<Length>, short_fall: Em, font_size: Abs) -> Self {
        Self {
            target: size.map(|l| l.at(font_size)),
            explicit: false,
            requested_target: (!size.is_one()).then_some(size),
            short_fall,
            relative_to: None,
            font_size: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Position {
    Above,
    Below,
}
