//! Math IR collector.
//!
//! Walks realized content and produces a flat `Vec<MathChild>`. Bodies on
//! structural children stay as `Body<'a>` handles for the layout pass to
//! resolve lazily; the one exception is `AttachChild::base`, which is
//! eagerly collected so the nested-attachment merge can run on IR values
//! rather than on `Content`.
//!
//! ## Show-rule cooperation
//!
//! Several elements that used to have IR variants are now desugared via
//! show rules before they reach the collector:
//!
//! - `op()` -> `class(Large, limits(body) | scripts(body))`
//! - `underbrace`/`overbrace`/`underbracket`/`overbracket`/`underparen`/
//!   `overparen`/`undershell`/`overshell` -> `accent(body, char)` optionally
//!   wrapped in `attach(_, b: annotation)` / `attach(_, t: annotation)`
//!
//! As a result this collector has no per-element handler for any of those
//! variants — they're impossible to encounter here.

use std::sync::LazyLock;

use codex::styling::{MathStyle, to_style};
use ecow::EcoString;
use typst_syntax::split_newlines;
use typst_utils::LazyHash;
use unicode_segmentation::UnicodeSegmentation;

use super::item::*;
use crate::diag::SourceResult;
use crate::engine::Engine;
use crate::foundations::{
    Content, Packed, Style, StyleChain, Styles, SymbolElem, TargetElem,
};
use crate::introspection::{Locator, SplitLocator, TagElem};
use crate::layout::{
    Axes, BoxElem, Em, FixedAlignment, HElem, PlaceElem, Spacing,
};
use crate::math::*;
use crate::routines::{Arenas, RealizationKind};
use crate::text::{
    BottomEdge, BottomEdgeMetric, LinebreakElem, SpaceElem, TextElem, TopEdge,
    TopEdgeMetric, is_default_ignorable,
};

/// Base styles applied to text children.
static TEXT_BASE_LOCAL_STYLES: LazyLock<[LazyHash<Style>; 3]> = LazyLock::new(|| {
    [
        TextElem::top_edge.set(TopEdge::Metric(TopEdgeMetric::Bounds)),
        TextElem::bottom_edge.set(BottomEdge::Metric(BottomEdgeMetric::Bounds)),
        TextElem::overhang.set(false),
    ]
    .map(|p| p.wrap())
});

/// Collects a piece of math content into a flat sequence of `MathChild`s.
///
/// This is what layout-time recursion calls when it needs to lay out a body:
/// `let children = collect(body.content, body.styles, ...)?`.
pub fn collect<'a>(
    content: &'a Content,
    styles: StyleChain<'a>,
    engine: &mut Engine,
    locator: Locator<'a>,
    arenas: &'a Arenas,
) -> SourceResult<Vec<MathChild<'a>>> {
    let mut collector = Collector::new(engine, locator, arenas);
    collector.collect_into_self(content, styles)?;
    Ok(collector.children)
}

/// The collector state.
pub(crate) struct Collector<'a, 'v, 'e> {
    engine: &'v mut Engine<'e>,
    locator: SplitLocator<'a>,
    arenas: &'a Arenas,
    children: Vec<MathChild<'a>>,
}

impl<'a, 'v, 'e> Collector<'a, 'v, 'e> {
    pub(crate) fn new(
        engine: &'v mut Engine<'e>,
        locator: Locator<'a>,
        arenas: &'a Arenas,
    ) -> Self {
        Self {
            engine,
            locator: locator.split(),
            arenas,
            children: Vec::new(),
        }
    }

    fn push(&mut self, child: MathChild<'a>) {
        self.children.push(child);
    }

    fn store_chain<'c>(&self, styles: StyleChain<'c>) -> &'a StyleChain<'c> {
        self.arenas.bump.alloc(styles)
    }

    fn store_styles(&self, styles: impl Into<Styles>) -> &'a Styles {
        self.arenas.styles.alloc(styles.into())
    }

    fn chain_styles(
        &self,
        base: StyleChain<'a>,
        new: impl Into<Styles>,
    ) -> StyleChain<'a> {
        self.store_chain(base).chain(self.store_styles(new))
    }

    /// Sub-collection: produce a flat `Vec<MathChild>` for some content
    /// without affecting our own running children buffer.
    fn collect_sub(
        &mut self,
        content: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<Vec<MathChild<'a>>> {
        // Drain a fresh segment of the children buffer, mirroring how
        // `collect_into_self` works but isolated.
        let start = self.children.len();
        self.collect_into_self(content, styles)?;
        Ok(self.children.drain(start..).collect())
    }

    /// Realize a piece of content and dispatch each realized element.
    fn collect_into_self(
        &mut self,
        content: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let pairs = (self.engine.library.routines.realize)(
            RealizationKind::Math,
            self.engine,
            &mut self.locator,
            self.arenas,
            content,
            styles,
        )?;
        for (elem, styles) in pairs {
            self.dispatch(elem, styles)?;
        }
        Ok(())
    }

    /// Dispatch a single realized element to its handler.
    fn dispatch(
        &mut self,
        elem: &'a Content,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        if elem.is::<SpaceElem>() {
            self.push(MathChild::Space);
        } else if elem.is::<LinebreakElem>() {
            self.push(MathChild::Linebreak);
        } else if elem.is::<AlignPointElem>() {
            self.push(MathChild::Align);
        } else if let Some(elem) = elem.to_packed::<SymbolElem>() {
            self.collect_symbol(elem, styles);
        } else if let Some(elem) = elem.to_packed::<TextElem>() {
            self.collect_text(elem, styles);
        } else if let Some(elem) = elem.to_packed::<HElem>() {
            self.collect_h(elem, styles);
        } else if let Some(elem) = elem.to_packed::<TagElem>() {
            self.push(MathChild::Tag(elem.tag.clone()));
        } else if let Some(elem) = elem.to_packed::<PrimesElem>() {
            self.collect_primes(elem, styles);
        } else if let Some(elem) = elem.to_packed::<AttachElem>() {
            self.collect_attach(elem, styles)?;
        } else if let Some(elem) = elem.to_packed::<FracElem>() {
            self.collect_frac(elem, styles);
        } else if let Some(elem) = elem.to_packed::<BinomElem>() {
            self.collect_binom(elem, styles);
        } else if let Some(elem) = elem.to_packed::<AccentElem>() {
            self.collect_accent(elem, styles);
        } else if let Some(elem) = elem.to_packed::<LrElem>() {
            self.collect_lr(elem, styles);
        } else if let Some(elem) = elem.to_packed::<MidElem>() {
            self.collect_mid(elem, styles);
        } else if let Some(elem) = elem.to_packed::<RootElem>() {
            self.collect_root(elem, styles);
        } else if let Some(elem) = elem.to_packed::<OverlineElem>() {
            self.collect_line(&elem.body, Position::Above, styles);
        } else if let Some(elem) = elem.to_packed::<UnderlineElem>() {
            self.collect_line(&elem.body, Position::Below, styles);
        } else if let Some(elem) = elem.to_packed::<CancelElem>() {
            self.collect_cancel(elem, styles);
        } else if let Some(elem) = elem.to_packed::<VecElem>() {
            self.collect_vec(elem, styles);
        } else if let Some(elem) = elem.to_packed::<MatElem>() {
            self.collect_mat(elem, styles);
        } else if let Some(elem) = elem.to_packed::<CasesElem>() {
            self.collect_cases(elem, styles);
        } else if let Some(elem) = elem.to_packed::<ClassElem>() {
            self.collect_override(&elem.body, styles, |o| {
                o.class = Some(elem.class);
                o.limits = Some(Limits::for_class(elem.class));
            });
        } else if let Some(elem) = elem.to_packed::<LimitsElem>() {
            let limits = if elem.inline.get(styles) {
                Limits::Always
            } else {
                Limits::Display
            };
            self.collect_override(&elem.body, styles, |o| o.limits = Some(limits));
        } else if let Some(elem) = elem.to_packed::<ScriptsElem>() {
            self.collect_override(&elem.body, styles, |o| {
                o.limits = Some(Limits::Never);
            });
        } else if let Some(elem) = elem.to_packed::<StretchElem>() {
            let size = elem.size.get(styles);
            let font_size = styles.resolve(TextElem::size);
            let info = StretchInfo::from_size(size, Em::zero(), font_size);
            self.collect_override(&elem.body, styles, |o| o.stretch = Some(info));
        } else if let Some(elem) = elem.to_packed::<BoxElem>() {
            let locator = self.locator.next(&elem.span());
            self.push(MathChild::Box(BoxData { elem, styles, locator }));
        } else if let Some(body) =
            (self.engine.library.routines.html_mathml_body)(elem, styles)
        {
            self.collect_mathml(elem, body, styles)?;
        } else {
            let locator = self.locator.next(&elem.span());
            let ignorant = elem.is::<PlaceElem>();
            self.push(MathChild::External(ExternalData {
                content: elem,
                styles,
                locator,
                ignorant,
            }));
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Per-element handlers
    // -----------------------------------------------------------------------

    fn collect_symbol(
        &mut self,
        elem: &'a Packed<SymbolElem>,
        styles: StyleChain<'a>,
    ) {
        let variant = styles.get(EquationElem::variant);
        let bold = styles.get(EquationElem::bold);
        let italic = styles.get(EquationElem::italic);
        for cluster in elem.text.graphemes(true) {
            if cluster.chars().all(is_default_ignorable) {
                continue;
            }
            let text: EcoString = cluster
                .chars()
                .flat_map(|c| to_style(c, MathStyle::select(c, variant, bold, italic)))
                .collect();
            let data = GlyphData::new(text, styles, elem.span());

            // Display-size large operators auto-stretch vertically.
            if data.class == unicode_math_class::MathClass::Large
                && data.size == MathSize::Display
            {
                let target = crate::layout::Rel::new(
                    crate::layout::Ratio::one(),
                    crate::layout::Abs::zero(),
                );
                let stretch = Stretch::new()
                    .with_y(StretchInfo::new(target, crate::layout::Em::zero()));
                data.stretch.set(stretch);
            }
            self.push(MathChild::Glyph(data));
        }
    }

    fn collect_text(
        &mut self,
        elem: &'a Packed<TextElem>,
        styles: StyleChain<'a>,
    ) {
        let variant = styles.get(EquationElem::variant);
        let bold = styles.get(EquationElem::bold);
        let italic = styles.get(EquationElem::italic).or(Some(false));
        let local_styles = self.store_chain(styles).chain(&*TEXT_BASE_LOCAL_STYLES);

        let make = |text: &str, locator: Locator<'a>| -> MathChild<'a> {
            let num = text.chars().all(|c| c.is_ascii_digit() || c == '.');
            let styled: EcoString = text
                .chars()
                .flat_map(|c| to_style(c, MathStyle::select(c, variant, bold, italic)))
                .collect();
            if num {
                MathChild::Number(NumberData { text: styled, span: elem.span() })
            } else {
                MathChild::Text(TextData {
                    text: styled,
                    styles: local_styles,
                    locator,
                    span: elem.span(),
                })
            }
        };

        let mut lines = split_newlines(&elem.text);
        lines.pop_if(|x| x.is_empty());
        let mut first = true;
        for line in lines {
            if !first {
                self.push(MathChild::Linebreak);
            }
            first = false;
            let loc = self.locator.next(&elem.span());
            self.push(make(line, loc));
        }
    }

    fn collect_h(&mut self, elem: &Packed<HElem>, styles: StyleChain<'a>) {
        if let Spacing::Rel(rel) = elem.amount
            && rel.rel.is_zero()
        {
            self.push(MathChild::Spacing(
                rel.abs,
                styles.resolve(TextElem::size),
                elem.weak.get(styles),
            ));
        }
    }

    fn collect_primes(
        &mut self,
        elem: &'a Packed<PrimesElem>,
        styles: StyleChain<'a>,
    ) {
        match elem.count {
            count @ 1..=4 => {
                let c = match count {
                    1 => '′',
                    2 => '″',
                    3 => '‴',
                    4 => '⁗',
                    _ => unreachable!(),
                };
                let text: EcoString = std::iter::once(c).collect();
                self.push(MathChild::Glyph(GlyphData::new(text, styles, elem.span())));
            }
            count => {
                self.push(MathChild::Primes(PrimesData { count, span: elem.span() }));
            }
        }
    }

    fn collect_attach(
        &mut self,
        elem: &'a Packed<AttachElem>,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let sup_chain = self.chain_styles(styles, style_for_superscript(styles));
        let sub_chain = self.chain_styles(styles, style_for_subscript(styles));

        // Eagerly collect the base — required by the merge below.
        let base = self.collect_sub(&elem.base, styles)?;

        let attach = AttachChild {
            base,
            t: elem.t.get_ref(styles).as_ref().map(|c| Body::new(c, sup_chain)),
            b: elem.b.get_ref(styles).as_ref().map(|c| Body::new(c, sub_chain)),
            tl: elem.tl.get_ref(styles).as_ref().map(|c| Body::new(c, sup_chain)),
            tr: elem.tr.get_ref(styles).as_ref().map(|c| Body::new(c, sup_chain)),
            bl: elem.bl.get_ref(styles).as_ref().map(|c| Body::new(c, sub_chain)),
            br: elem.br.get_ref(styles).as_ref().map(|c| Body::new(c, sub_chain)),
            span: elem.span(),
        };
        let merged = merge_nested_attachments(Box::new(attach), self)?;
        self.push(MathChild::Attach(merged));
        Ok(())
    }

    fn collect_frac(&mut self, elem: &'a Packed<FracElem>, styles: StyleChain<'a>) {
        let num_chain = self.chain_styles(styles, style_for_numerator(styles));
        let denom_chain = self.chain_styles(styles, style_for_denominator(styles));
        let style = match elem.style.get(styles) {
            FracStyle::Vertical => FracStyleKind::Vertical,
            FracStyle::Horizontal => FracStyleKind::Horizontal {
                num_deparen: elem.num_deparenthesized.get(styles),
                denom_deparen: elem.denom_deparenthesized.get(styles),
            },
            FracStyle::Skewed => {
                let mut slash = GlyphData::new("\u{2044}".into(), styles, elem.span());
                slash.stretch.set(
                    Stretch::new()
                        .with_y(StretchInfo::new(
                            crate::layout::Rel::new(
                                crate::layout::Ratio::one(),
                                crate::layout::Abs::zero(),
                            ),
                            crate::math::DELIM_SHORT_FALL,
                        )),
                );
                self.push(MathChild::SkewedFrac(Box::new(SkewedFracChild {
                    num: Body::new(&elem.num, num_chain),
                    denom: Body::new(&elem.denom, denom_chain),
                    slash,
                    span: elem.span(),
                })));
                return;
            }
        };
        self.push(MathChild::Frac(Box::new(FracChild {
            num: Body::new(&elem.num, num_chain),
            denom: Body::new(&elem.denom, denom_chain),
            style,
            padding: FRAC_PADDING,
            span: elem.span(),
        })));
    }

    fn collect_binom(&mut self, elem: &'a Packed<BinomElem>, styles: StyleChain<'a>) {
        let num_chain = self.chain_styles(styles, style_for_numerator(styles));
        let denom_chain = self.chain_styles(styles, style_for_denominator(styles));
        self.push(MathChild::Binom(Box::new(BinomChild {
            upper: Body::new(&elem.upper, num_chain),
            lower: elem
                .lower
                .iter()
                .map(|c| Body::new(c, denom_chain))
                .collect(),
            span: elem.span(),
        })));
    }

    fn collect_accent(
        &mut self,
        elem: &'a Packed<AccentElem>,
        styles: StyleChain<'a>,
    ) {
        let above = !elem.accent.is_bottom();
        let position = if above { Position::Above } else { Position::Below };

        let base_chain = if above {
            let mut new = Styles::new();
            new.apply(style_cramped().into());
            if elem.dotless.get(styles) {
                new.apply(style_dtls().into());
            }
            self.chain_styles(styles, new)
        } else {
            styles
        };

        // Pre-build the accent glyph as a `GlyphData` with the diacritic
        // class. Same shape as the old `AccentItem.accent: MathItem`.
        let c = elem.accent.0;
        let mut accent = GlyphData::new(c.to_string().into(), styles, elem.span());
        accent.class = unicode_math_class::MathClass::Diacritic;

        let exact_frame_width =
            matches!(c, '⏟' | '⏞' | '⎵' | '⎴' | '⏝' | '⏜' | '⏡' | '⏠');

        self.push(MathChild::Accent(Box::new(AccentChild {
            base: Body::new(&elem.base, base_chain),
            accent,
            position,
            dotless: elem.dotless.get(styles),
            size: elem.size.get(styles),
            exact_frame_width,
            span: elem.span(),
        })));
    }

    fn collect_lr(&mut self, elem: &'a Packed<LrElem>, styles: StyleChain<'a>) {
        self.push(MathChild::Lr(Box::new(LrChild {
            body: Body::new(&elem.body, styles),
            size: elem.size.get(styles),
            span: elem.span(),
        })));
    }

    fn collect_mid(&mut self, elem: &'a Packed<MidElem>, styles: StyleChain<'a>) {
        self.push(MathChild::Mid(MidData {
            body: Body::new(&elem.body, styles),
            span: elem.span(),
        }));
    }

    fn collect_root(&mut self, elem: &'a Packed<RootElem>, styles: StyleChain<'a>) {
        let cramped = self.chain_styles(styles, style_cramped());
        let index_chain = if elem.index.get_ref(styles).is_some() {
            let sscript = self
                .store_styles(EquationElem::size.set(MathSize::ScriptScript).wrap());
            self.store_chain(cramped).chain(sscript)
        } else {
            styles
        };

        // Pre-build the sqrt glyph with a vertical-stretch hint.
        let mut sqrt = GlyphData::new("\u{221A}".into(), styles, elem.span());
        sqrt.stretch.set(Stretch::new().with_y(StretchInfo::new(
            crate::layout::Rel::new(
                crate::layout::Ratio::one(),
                crate::layout::Abs::zero(),
            ),
            crate::layout::Em::zero(),
        )));

        self.push(MathChild::Root(Box::new(RootChild {
            radicand: Body::new(&elem.radicand, cramped),
            index: elem
                .index
                .get_ref(styles)
                .as_ref()
                .map(|c| Body::new(c, index_chain)),
            sqrt,
            span: elem.span(),
        })));
    }

    fn collect_line(
        &mut self,
        body: &'a Content,
        position: Position,
        styles: StyleChain<'a>,
    ) {
        let chain = match position {
            Position::Above => self.chain_styles(styles, style_cramped()),
            Position::Below => styles,
        };
        self.push(MathChild::Line(Box::new(LineChild {
            base: Body::new(body, chain),
            position,
            span: body.span(),
        })));
    }

    fn collect_cancel(
        &mut self,
        elem: &'a Packed<CancelElem>,
        styles: StyleChain<'a>,
    ) {
        let length = elem.length.resolve(styles);
        let default_stroke = crate::visualize::FixedStroke {
            paint: styles.get_ref(TextElem::fill).as_decoration(),
            ..Default::default()
        };
        let stroke = elem.stroke.resolve(styles).unwrap_or(default_stroke);
        let invert = elem.inverted.get(styles);
        let cross = elem.cross.get(styles);
        self.push(MathChild::Cancel(Box::new(CancelChild {
            base: Body::new(&elem.body, styles),
            length,
            stroke,
            cross,
            invert_first_line: !cross && invert,
            angle: elem.angle.get_ref(styles).clone(),
            span: elem.span(),
        })));
    }

    fn collect_vec(&mut self, elem: &'a Packed<VecElem>, styles: StyleChain<'a>) {
        let cell_chain = self.chain_styles(styles, style_for_denominator(styles));
        let cells = elem
            .children
            .iter()
            .map(|c| vec![Body::new(c, cell_chain)])
            .collect();
        let delim = elem.delim.get(styles);
        let gap = Axes::with_y(elem.gap.resolve(styles));
        let (open, close) = build_delim_glyphs(delim.open(), delim.close(), styles, elem.span());
        self.push(MathChild::Table(Box::new(TableChild {
            cells,
            gap,
            augment: None,
            align: elem.align.resolve(styles),
            alternator: LeftRightAlternator::Right,
            open,
            close,
            span: elem.span(),
        })));
    }

    fn collect_mat(&mut self, elem: &'a Packed<MatElem>, styles: StyleChain<'a>) {
        let cell_chain = self.chain_styles(styles, style_for_denominator(styles));
        let cells = elem
            .rows
            .iter()
            .map(|row| row.iter().map(|c| Body::new(c, cell_chain)).collect())
            .collect();
        let delim = elem.delim.get(styles);
        let gap = Axes::new(
            elem.column_gap.resolve(styles),
            elem.row_gap.resolve(styles),
        );
        let (open, close) = build_delim_glyphs(delim.open(), delim.close(), styles, elem.span());
        self.push(MathChild::Table(Box::new(TableChild {
            cells,
            gap,
            augment: elem.augment.resolve(styles),
            align: elem.align.resolve(styles),
            alternator: LeftRightAlternator::Right,
            open,
            close,
            span: elem.span(),
        })));
    }

    fn collect_cases(&mut self, elem: &'a Packed<CasesElem>, styles: StyleChain<'a>) {
        let cell_chain = self.chain_styles(styles, style_for_denominator(styles));
        let cells = elem
            .children
            .iter()
            .map(|c| vec![Body::new(c, cell_chain)])
            .collect();
        let delim = elem.delim.get(styles);
        let (open_c, close_c) = if elem.reverse.get(styles) {
            (None, delim.close())
        } else {
            (delim.open(), None)
        };
        let gap = Axes::with_y(elem.gap.resolve(styles));
        let (open, close) = build_delim_glyphs(open_c, close_c, styles, elem.span());
        self.push(MathChild::Table(Box::new(TableChild {
            cells,
            gap,
            augment: None,
            align: FixedAlignment::Start,
            alternator: LeftRightAlternator::None,
            open,
            close,
            span: elem.span(),
        })));
    }

    fn collect_override(
        &mut self,
        body: &'a Content,
        styles: StyleChain<'a>,
        configure: impl FnOnce(&mut OverrideChild<'a>),
    ) {
        let mut child = OverrideChild {
            body: Body::new(body, styles),
            class: None,
            limits: None,
            stretch: None,
            span: body.span(),
        };
        configure(&mut child);
        self.push(MathChild::Override(Box::new(child)));
    }

    fn collect_mathml(
        &mut self,
        elem: &'a Content,
        body: Option<&'a Content>,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let body = if styles.get(TargetElem::target).is_html()
            && let Some(body) = body
        {
            Some(self.collect_sub(body, styles)?)
        } else {
            None
        };
        self.push(MathChild::Mathml(MathmlData { elem, body, styles }));
        Ok(())
    }
}

// ===========================================================================
// Nested-attachment merge
// ===========================================================================

/// Recursively merge a chain of nested `AttachChild`s into a normalized
/// structure.
///
/// Equivalent to the old `resolve_inner_attach` (and its `AttachmentList`
/// linked list), but operating on `MathChild` values rather than `Content`.
/// The outer's eagerly-collected `base` may contain a single
/// `MathChild::Attach`; we cascade outer slots inward where the inner is
/// missing them, then either:
///
/// - return the merged inner if outer ends up with no slots of its own
///   (outer is a no-op wrapper),
/// - or rebuild outer with `base = [Attach(merged_inner)]` and return it.
///
/// Visits in post-order so the innermost attach has its slots populated
/// before its enclosing attach decides what to keep.
pub(crate) fn merge_nested_attachments<'a, 'v, 'e>(
    mut outer: Box<AttachChild<'a>>,
    collector: &mut Collector<'a, 'v, 'e>,
) -> SourceResult<Box<AttachChild<'a>>> {
    // Check whether the outer's base is a single nested `AttachChild`.
    let nested = matches!(outer.base.as_slice(), [MathChild::Attach(_)]);
    if !nested {
        return Ok(outer);
    }

    let mut inner = match outer.base.pop().unwrap() {
        MathChild::Attach(inner) => inner,
        _ => unreachable!(),
    };

    // Post-order: merge the inner *first*.
    inner = merge_nested_attachments(inner, collector)?;

    // Cascade non-tr slots: if the inner is missing it and the outer has
    // it, move outer's value into inner.
    macro_rules! cascade {
        ($field:ident) => {
            if inner.$field.is_none() && outer.$field.is_some() {
                inner.$field = outer.$field.take();
            }
        };
    }
    cascade!(t);
    cascade!(b);
    cascade!(tl);
    cascade!(bl);
    cascade!(br);

    // tr has a special interaction: if we'd bring a primed `tr` inward
    // *and* the outer also has `t`, leaving `tr` outside avoids inverting
    // the visual order (`tr + t` looks bad). We check primality on
    // collected IR, not on `Content`.
    if inner.tr.is_none() && outer.tr.is_some() {
        let suppress = if outer.t.is_some() {
            let tr_body = outer.tr.as_ref().unwrap();
            tr_resolves_to_primes(tr_body, collector)?
        } else {
            false
        };
        if !suppress {
            inner.tr = outer.tr.take();
        }
    }

    // If outer ended up with no slots, outer is a no-op wrapper: return
    // the merged inner directly.
    if outer.is_empty() {
        return Ok(inner);
    }

    // Otherwise rebuild outer.base from the merged inner.
    outer.base = vec![MathChild::Attach(inner)];
    Ok(outer)
}

/// Does the given body, when collected, look like a sequence of primes?
///
/// Used only by the primed-tr special case in `merge_nested_attachments`.
/// We do this on collected IR rather than checking `Content` for a
/// `PrimesElem` — that would be exactly the kind of Content peeking we're
/// trying to avoid.
fn tr_resolves_to_primes<'a, 'v, 'e>(
    body: &Body<'a>,
    collector: &mut Collector<'a, 'v, 'e>,
) -> SourceResult<bool> {
    let collected = collector.collect_sub(body.content, body.styles)?;
    Ok(match collected.as_slice() {
        [MathChild::Primes(_)] => true,
        [MathChild::Glyph(g)] => g.text.chars().all(is_prime_char),
        _ => false,
    })
}

fn is_prime_char(c: char) -> bool {
    matches!(c, '′' | '″' | '‴' | '⁗')
}

/// Build the optional opening / closing delimiter glyphs for a
/// matrix/vector/cases body. Matches the old `resolve_delimiters` —
/// stretches with the standard delimiter short-fall and target of 1.1
/// relative to the body height (filled in at layout time).
fn build_delim_glyphs(
    open: Option<char>,
    close: Option<char>,
    styles: StyleChain<'_>,
    span: typst_syntax::Span,
) -> (Option<GlyphData>, Option<GlyphData>) {
    let make = |c: char| {
        let mut g = GlyphData::new(c.to_string().into(), styles, span);
        let target = crate::layout::Rel::new(
            crate::layout::Ratio::new(1.1),
            crate::layout::Abs::zero(),
        );
        g.stretch
            .set(Stretch::new().with_y(StretchInfo::new(target, crate::math::DELIM_SHORT_FALL)));
        g
    };
    (open.map(make), close.map(make))
}
