mod accent;
mod cancel;
mod fenced;
mod fraction;
mod fragment;
mod line;
mod radical;
mod run;
mod scripts;
mod shaping;
mod table;
mod text;

use comemo::Tracked;
use typst_library::World;
use typst_library::diag::{At, SourceResult, warning};
use typst_library::engine::Engine;
use typst_library::foundations::{NativeElement, Packed, Resolve, Style, StyleChain};
use typst_library::introspection::{Counter, Locator, SplitLocator};
use typst_library::layout::{
    Abs, AlignElem, Axes, BlockElem, Em, FixedAlignment, Fragment, Frame, InlineItem,
    OuterHAlignment, Point, Region, Regions, Size, SpecificAlignment, VAlignment,
};
use typst_library::math::ir::{
    AttachChild, Body, BoxData, ExternalData, FracChild, GlyphData, LrChild, MathChild,
    MathmlData, MultilineView, NumberData, OverrideChild, TextData, collect,
    collect_equation,
};
use typst_library::math::{EquationElem, Limits, MathSize, families};
use typst_library::model::ParElem;
use typst_library::routines::Arenas;
use typst_library::text::{Font, FontFlags, TextEdgeBounds, TextElem, variant};
use typst_syntax::Span;
use typst_utils::{LazyHash, Numeric};
use unicode_math_class::MathClass;

use self::accent::layout_accent;
use self::cancel::layout_cancel;
use self::fenced::layout_fenced;
use self::fraction::{layout_fraction, layout_skewed_fraction};
use self::fragment::{FrameFragment, MathFragment};
use self::line::layout_line;
use self::radical::layout_radical;
use self::run::{MathFragmentsExt, MathRun, MathRunFrameBuilder, layout_multiline};
use self::scripts::{layout_primes, layout_scripts};
use self::table::layout_table;
use self::text::{layout_glyph, layout_number, layout_text};

// ===========================================================================
// MathProperties: layout-internal
// ===========================================================================

/// Properties attached to a math child at layout time.
///
/// Previously this was part of the IR, but it's really a layout-time
/// concept: it bundles inherited style information (class, size, cramped,
/// span) with adjustable spacing/limits state for use by the per-variant
/// layout helpers. Build via [`MathProperties::for_child`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct MathProperties {
    pub limits: Limits,
    pub class: MathClass,
    pub size: MathSize,
    pub cramped: bool,
    pub ignorant: bool,
    pub spaced: bool,
    pub lspace: Option<Em>,
    pub rspace: Option<Em>,
    pub align_form_infix: bool,
    pub span: Span,
}

impl MathProperties {
    /// Derive default properties from a child + the active style chain.
    pub fn for_child(child: &MathChild, styles: StyleChain) -> Self {
        Self {
            limits: child_default_limits(child),
            class: child.class(),
            size: styles.get(EquationElem::size),
            cramped: styles.get(EquationElem::cramped),
            ignorant: child.is_ignorant(),
            spaced: matches!(
                child,
                MathChild::Text(_) | MathChild::Box(_) | MathChild::External(_)
            ),
            lspace: None,
            rspace: None,
            align_form_infix: false,
            span: child.span(),
        }
    }
}

fn child_default_limits(child: &MathChild) -> Limits {
    match child {
        MathChild::Glyph(g) => g.limits,
        _ => Limits::Never,
    }
}

// ===========================================================================
// Entry points
// ===========================================================================

/// Layout an inline equation (in a paragraph).
#[typst_macros::time(span = elem.span())]
pub fn layout_equation_inline(
    elem: &Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    region: Size,
) -> SourceResult<Vec<InlineItem>> {
    assert!(!elem.block.get(styles));

    let span = elem.span();
    let font = get_font(engine.world, styles, span)?;
    warn_non_math_font(&font, engine, span);

    let scale_style = style_for_script_scale(&font);
    let styles = styles.chain(&scale_style);

    let arenas = Arenas::default();
    let children = collect_equation(elem, engine, locator, &arenas, styles)?;

    let mut ctx = MathContext::new(engine, &arenas, region, font.clone());
    let view = MultilineView::new(&children);

    let mut items = if view.is_flat() {
        ctx.layout_children(&children, styles)?.into_par_items()
    } else {
        let builder = layout_multiline(&view, &mut ctx, styles)?;
        vec![InlineItem::Frame(builder.build_unaligned())]
    };

    if items.is_empty() {
        items.push(InlineItem::Frame(Frame::soft(Size::zero())));
    }

    for item in &mut items {
        let InlineItem::Frame(frame) = item else { continue };

        let slack = styles.resolve(ParElem::leading) * 0.7;

        let (t, b) = font.edges(
            styles.get(TextElem::top_edge),
            styles.get(TextElem::bottom_edge),
            styles.resolve(TextElem::size),
            TextEdgeBounds::Frame(frame),
        );

        let ascent = t.max(frame.ascent() - slack);
        let descent = b.max(frame.descent() - slack);
        frame.translate(Point::with_y(ascent - frame.baseline()));
        frame.size_mut().y = ascent + descent;
    }

    Ok(items)
}

/// Layout a block-level equation (in a flow).
#[typst_macros::time(span = elem.span())]
pub fn layout_equation_block(
    elem: &Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment> {
    assert!(elem.block.get(styles));

    let span = elem.span();
    let font = get_font(engine.world, styles, span)?;
    warn_non_math_font(&font, engine, span);

    let scale_style = style_for_script_scale(&font);
    let styles = styles.chain(&scale_style);

    let arenas = Arenas::default();
    let children = collect_equation(elem, engine, locator.relayout(), &arenas, styles)?;

    let mut ctx = MathContext::new(engine, &arenas, regions.base(), font.clone());
    let view = MultilineView::new(&children);

    let full_equation_builder = if !view.is_flat() {
        layout_multiline(&view, &mut ctx, styles)?
    } else {
        ctx.layout_children(&children, styles)?.into_frame().into()
    };
    let width = full_equation_builder.size.x;

    let equation_builders = if styles.get(BlockElem::breakable) {
        let mut rows = full_equation_builder.frames.into_iter().peekable();
        let mut equation_builders = vec![];
        let mut last_first_pos = Point::zero();
        let mut regions = regions;

        while let Some(&(_, first_pos)) = rows.peek() {
            last_first_pos = first_pos;
            let mut frames = vec![];
            let mut height = Abs::zero();
            while let Some((sub, pos)) = rows.peek() {
                let mut pos = *pos;
                pos.y -= first_pos.y;
                if !regions.size.y.fits(sub.height() + pos.y)
                    && (regions.may_progress()
                        || (regions.may_break() && !frames.is_empty()))
                {
                    break;
                }
                let (sub, _) = rows.next().unwrap();
                height = height.max(pos.y + sub.height());
                frames.push((sub, pos));
            }
            equation_builders
                .push(MathRunFrameBuilder { frames, size: Size::new(width, height) });
            regions.next();
        }

        if let Some(equation_builder) = equation_builders.last_mut() {
            equation_builder.frames.extend(rows.map(|(frame, mut pos)| {
                pos.y -= last_first_pos.y;
                (frame, pos)
            }));
            let height = equation_builder
                .frames
                .iter()
                .map(|(frame, pos)| frame.height() + pos.y)
                .max()
                .unwrap_or(equation_builder.size.y);
            equation_builder.size.y = height;
        }

        if equation_builders.is_empty() {
            equation_builders
                .push(MathRunFrameBuilder { frames: vec![], size: Size::zero() });
        }

        equation_builders
    } else {
        vec![full_equation_builder]
    };

    let Some(numbering) = elem.numbering.get_ref(styles) else {
        let frames = equation_builders
            .into_iter()
            .map(MathRunFrameBuilder::build_aligned)
            .collect();
        return Ok(Fragment::frames(frames));
    };

    let pod = Region::new(regions.base(), Axes::splat(false));
    let counter = Counter::of(EquationElem::ELEM)
        .display_at(engine, elem.location().unwrap(), styles, numbering, span)?
        .spanned(span);
    let mut locator = locator.split();
    let number = crate::layout_frame(engine, &counter, locator.next(&()), styles, pod)?;

    static NUMBER_GUTTER: Em = Em::new(0.5);
    let full_number_width = number.width() + NUMBER_GUTTER.resolve(styles);

    let number_align = match elem.number_align.get(styles) {
        SpecificAlignment::H(h) => SpecificAlignment::Both(h, VAlignment::Horizon),
        SpecificAlignment::V(v) => SpecificAlignment::Both(OuterHAlignment::End, v),
        SpecificAlignment::Both(h, v) => SpecificAlignment::Both(h, v),
    };

    let region_count = equation_builders.len();
    let frames = equation_builders
        .into_iter()
        .map(|builder| {
            if builder.frames.is_empty() && region_count > 1 {
                return builder.build_aligned();
            }
            add_equation_number(
                builder,
                number.clone(),
                number_align.resolve(styles),
                styles.get(AlignElem::alignment).resolve(styles).x,
                regions.size.x,
                full_number_width,
            )
        })
        .collect();

    Ok(Fragment::frames(frames))
}

fn add_equation_number(
    equation_builder: MathRunFrameBuilder,
    number: Frame,
    number_align: Axes<FixedAlignment>,
    equation_align: FixedAlignment,
    region_size_x: Abs,
    full_number_width: Abs,
) -> Frame {
    let first = equation_builder.frames.first().map_or(
        (equation_builder.size, Point::zero(), Abs::zero()),
        |(frame, pos)| (frame.size(), *pos, frame.baseline()),
    );
    let last = equation_builder.frames.last().map_or(
        (equation_builder.size, Point::zero(), Abs::zero()),
        |(frame, pos)| (frame.size(), *pos, frame.baseline()),
    );
    let line_count = equation_builder.frames.len();
    let mut equation = equation_builder.build_aligned();

    let width = if region_size_x.is_finite() {
        region_size_x
    } else {
        equation.width() + 2.0 * full_number_width
    };

    let is_multiline = line_count >= 2;
    let resizing_offset = resize_equation(
        &mut equation,
        &number,
        number_align,
        equation_align,
        width,
        is_multiline,
        [first, last],
    );
    equation.translate(Point::with_x(match (equation_align, number_align.x) {
        (FixedAlignment::Start, FixedAlignment::Start) => full_number_width,
        (FixedAlignment::End, FixedAlignment::End) => -full_number_width,
        _ => Abs::zero(),
    }));

    let x = match number_align.x {
        FixedAlignment::Start => Abs::zero(),
        FixedAlignment::End => equation.width() - number.width(),
        _ => unreachable!(),
    };
    let y = {
        let align_baselines = |(_, pos, baseline): (_, Point, Abs), number: &Frame| {
            resizing_offset.y + pos.y + baseline - number.baseline()
        };
        match number_align.y {
            FixedAlignment::Start => align_baselines(first, &number),
            FixedAlignment::Center if !is_multiline => align_baselines(first, &number),
            FixedAlignment::Center => (equation.height() - number.height()) / 2.0,
            FixedAlignment::End => align_baselines(last, &number),
        }
    };

    equation.push_frame(Point::new(x, y), number);
    equation
}

fn resize_equation(
    equation: &mut Frame,
    number: &Frame,
    number_align: Axes<FixedAlignment>,
    equation_align: FixedAlignment,
    width: Abs,
    is_multiline: bool,
    [first, last]: [(Axes<Abs>, Point, Abs); 2],
) -> Point {
    if matches!(number_align.y, FixedAlignment::Center if is_multiline) {
        return equation.resize(
            Size::new(width, equation.height().max(number.height())),
            Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Center),
        );
    }

    let excess_above = Abs::zero().max({
        if !is_multiline || matches!(number_align.y, FixedAlignment::Start) {
            let (.., baseline) = first;
            number.baseline() - baseline
        } else {
            Abs::zero()
        }
    });
    let excess_below = Abs::zero().max({
        if !is_multiline || matches!(number_align.y, FixedAlignment::End) {
            let (size, .., baseline) = last;
            (number.height() - number.baseline()) - (size.y - baseline)
        } else {
            Abs::zero()
        }
    });

    let resizing_offset = equation.resize(
        Size::new(width, equation.height() + excess_above + excess_below),
        Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Start),
    );
    equation.translate(Point::with_y(excess_above));
    resizing_offset + Point::with_y(excess_above)
}

// ===========================================================================
// MathContext
// ===========================================================================

pub(crate) struct MathContext<'a, 'v, 'e> {
    pub engine: &'v mut Engine<'e>,
    pub arenas: &'a Arenas,
    pub region: Region,
    pub locator: SplitLocator<'a>,
    fonts_stack: Vec<Font>,
    fragments: MathRun,
}

impl<'a, 'v, 'e> MathContext<'a, 'v, 'e> {
    pub fn new(
        engine: &'v mut Engine<'e>,
        arenas: &'a Arenas,
        base: Size,
        font: Font,
    ) -> Self {
        Self {
            engine,
            arenas,
            region: Region::new(base, Axes::splat(false)),
            locator: Locator::root().split(),
            fonts_stack: vec![font],
            fragments: vec![],
        }
    }

    #[inline]
    pub fn font(&self) -> &Font {
        self.fonts_stack.last().unwrap()
    }

    pub fn push(&mut self, fragment: impl Into<MathFragment>) {
        self.fragments.push(fragment.into());
    }

    pub fn extend(&mut self, fragments: impl IntoIterator<Item = MathFragment>) {
        self.fragments.extend(fragments);
    }

    /// Lay out a flat sequence of children, returning the resulting fragments.
    pub fn layout_children(
        &mut self,
        children: &[MathChild<'a>],
        styles: StyleChain<'a>,
    ) -> SourceResult<MathRun> {
        let start = self.fragments.len();
        for child in children {
            self.layout_child(child, styles)?;
        }
        Ok(self.fragments.drain(start..).collect())
    }

    /// Lay out a deferred body. Compatibility shim with the same name as
    /// the old method (which took a `MathItem`); ignores the extra
    /// `styles` argument since `Body` carries its own style chain.
    pub fn layout_into_fragments(
        &mut self,
        body: Body<'a>,
        _styles: StyleChain<'a>,
    ) -> SourceResult<MathRun> {
        self.layout_body(body)
    }

    /// Lay out a deferred body and wrap it into a single [`MathFragment`].
    /// Compatibility shim — see [`Self::layout_into_fragments`].
    pub fn layout_into_fragment(
        &mut self,
        body: Body<'a>,
        _styles: StyleChain<'a>,
    ) -> SourceResult<MathFragment> {
        self.layout_body_as_fragment(body)
    }

    /// Lay out a deferred body: collect it, then lay out the resulting
    /// children. The body's own style chain is used.
    pub fn layout_body(&mut self, body: Body<'a>) -> SourceResult<MathRun> {
        let span = body.content.span();
        let loc = self.locator.next(&span);
        let children =
            collect(body.content, body.styles, self.engine, loc, self.arenas)?;
        self.layout_children(&children, body.styles)
    }

    /// Like `layout_body`, but wrap into a single [`MathFragment`].
    pub fn layout_body_as_fragment(
        &mut self,
        body: Body<'a>,
    ) -> SourceResult<MathFragment> {
        let fragments = self.layout_body(body)?;
        self.combine_into_fragment(fragments, body.styles)
    }

    /// Lay out a sequence of children (already-collected) and combine into
    /// a single fragment.
    pub fn layout_children_as_fragment(
        &mut self,
        children: &[MathChild<'a>],
        styles: StyleChain<'a>,
    ) -> SourceResult<MathFragment> {
        let fragments = self.layout_children(children, styles)?;
        self.combine_into_fragment(fragments, styles)
    }

    fn combine_into_fragment(
        &self,
        fragments: MathRun,
        styles: StyleChain<'a>,
    ) -> SourceResult<MathFragment> {
        if fragments.len() == 1 {
            return Ok(fragments.into_iter().next().unwrap());
        }

        let text_like = fragments
            .iter()
            .filter(|e| e.math_size().is_some())
            .all(|e| e.is_text_like());

        let class = styles
            .get(EquationElem::class)
            .unwrap_or(MathClass::Normal);
        let size = styles.get(EquationElem::size);
        let cramped = styles.get(EquationElem::cramped);
        let _ = cramped;
        let props = MathProperties {
            limits: Limits::Never,
            class,
            size,
            cramped: false,
            ignorant: false,
            spaced: false,
            lspace: None,
            rspace: None,
            align_form_infix: false,
            span: Span::detached(),
        };
        let frame = fragments.into_frame();
        Ok(FrameFragment::new(&props, styles, frame).with_text_like(text_like).into())
    }

    /// Dispatch a single child to its layout handler.
    fn layout_child(
        &mut self,
        child: &MathChild<'a>,
        styles: StyleChain<'a>,
    ) -> SourceResult<()> {
        let props = MathProperties::for_child(child, styles);
        layout_child(child, self, styles, props)
    }
}

// ===========================================================================
// Dispatch
// ===========================================================================

/// Layout a single math child.
fn layout_child<'a>(
    child: &MathChild<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    props: MathProperties,
) -> SourceResult<()> {
    // Markers / non-component leaves.
    match child {
        MathChild::Space => {
            ctx.push(MathFragment::Space(ctx.font().math().space_width.resolve(styles)));
            return Ok(());
        }
        MathChild::Spacing(amount, font_size, _) => {
            ctx.push(MathFragment::Space(amount.at(*font_size)));
            return Ok(());
        }
        MathChild::Tag(tag) => {
            ctx.push(MathFragment::Tag(tag.clone()));
            return Ok(());
        }
        MathChild::Linebreak | MathChild::Align | MathChild::Mid(_) => {
            // Markers are consumed by the enclosing container (block
            // equation / Lr layout). In a single-row context, they're
            // ignored — same as the old behavior.
            return Ok(());
        }
        _ => {}
    }

    // Left spacing.
    if let Some(lspace) = props.lspace
        && !props.align_form_infix
        && !lspace.is_zero()
    {
        let width = lspace.at(styles.resolve(TextElem::size));
        ctx.push(MathFragment::Space(width));
    }

    match child {
        MathChild::Glyph(item) => layout_glyph(item, ctx, styles, &props)?,
        MathChild::Text(item) => layout_text(item, ctx, styles, &props)?,
        MathChild::Number(item) => layout_number(item, ctx, styles, &props)?,
        MathChild::Primes(item) => layout_primes(item, ctx, styles, &props)?,
        MathChild::Box(item) => layout_box(item, ctx, styles, &props)?,
        MathChild::External(item) => layout_external(item, ctx, styles, &props)?,
        MathChild::Mathml(item) => layout_mathml(item, ctx, styles, &props)?,
        MathChild::Frac(item) => layout_fraction(item, ctx, styles, &props)?,
        MathChild::SkewedFrac(item) => layout_skewed_fraction(item, ctx, styles, &props)?,
        MathChild::Binom(_) => todo!("layout binomial (frac + fenced)"),
        MathChild::Attach(item) => layout_scripts(item, ctx, styles, &props)?,
        MathChild::Accent(item) => layout_accent(item, ctx, styles, &props)?,
        MathChild::Line(item) => layout_line(item, ctx, styles, &props)?,
        MathChild::Cancel(item) => layout_cancel(item, ctx, styles, &props)?,
        MathChild::Root(item) => layout_radical(item, ctx, styles, &props)?,
        MathChild::Lr(item) => layout_fenced(item, ctx, styles, &props)?,
        MathChild::Table(item) => layout_table(item, ctx, styles, &props)?,
        MathChild::Group(item) => {
            let fragment = ctx.layout_body_as_fragment(item.body)?;
            let italics = fragment.italics_correction();
            let accent_attach = fragment.accent_attach();
            ctx.push(
                FrameFragment::new(&props, styles, fragment.into_frame())
                    .with_italics_correction(italics)
                    .with_accent_attach(accent_attach),
            );
        }
        MathChild::Override(item) => layout_override(item, ctx, styles, &props)?,
        // Already handled above.
        MathChild::Space
        | MathChild::Spacing(..)
        | MathChild::Tag(_)
        | MathChild::Linebreak
        | MathChild::Align
        | MathChild::Mid(_) => unreachable!(),
    }

    // Right spacing.
    if let Some(rspace) = props.rspace
        && !rspace.is_zero()
    {
        let width = rspace.at(styles.resolve(TextElem::size));
        ctx.push(MathFragment::Space(width));
    }

    Ok(())
}

/// Lay out an `OverrideChild`: resolve the body, then apply the overrides.
///
/// `stretch` overrides need to land on the underlying glyph's stretch
/// cell *before* layout (mirroring how the old IR's `resolve_stretch`
/// updated the resolved item's stretch in-place). For that, we eagerly
/// collect the body, push the stretch into its stretchable glyph, then
/// lay out the collected children.
fn layout_override<'a>(
    o: &OverrideChild<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    _props: &MathProperties,
) -> SourceResult<()> {
    if let Some(info) = o.stretch {
        let span = o.body.content.span();
        let loc = ctx.locator.next(&span);
        let children = typst_library::math::ir::collect(
            o.body.content,
            o.body.styles,
            ctx.engine,
            loc,
            ctx.arenas,
        )?;
        for child in &children {
            if let Some(g) = child.stretch_target() {
                g.update_stretch(info);
            }
        }
        let fragments = ctx.layout_children(&children, o.body.styles)?;
        let fragment = ctx.combine_into_fragment(fragments, styles)?;
        ctx.push(fragment);
    } else {
        let fragment = ctx.layout_body_as_fragment(o.body)?;
        ctx.push(fragment);
    }
    // NOTE: `class` and `limits` overrides flow through the style chain
    // (set by `ClassElem` / `LimitsElem` at IR-collect time); the
    // resolved fragment inherits them naturally. Restoring the
    // post-resolution `set_class` / `set_limits` mutators would need
    // fragment-level setters on `FrameFragment`.
    let _ = (o.class, o.limits);
    Ok(())
}

// ===========================================================================
// MathML / Box / External
// ===========================================================================

fn layout_mathml(
    item: &MathmlData,
    ctx: &mut MathContext,
    _styles: StyleChain,
    _props: &MathProperties,
) -> SourceResult<()> {
    ctx.engine.sink.warn(warning!(
        item.elem.span(),
        "MathML element was ignored during paged export",
    ));
    Ok(())
}

fn layout_box<'a>(
    item: &BoxData<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    props: &MathProperties,
) -> SourceResult<()> {
    let frame = crate::inline::layout_box(
        item.elem,
        ctx.engine,
        item.locator.relayout(),
        styles,
        ctx.region.size,
    )?;
    ctx.push(FrameFragment::new(props, styles, frame));
    Ok(())
}

fn layout_external<'a>(
    item: &ExternalData<'a>,
    ctx: &mut MathContext<'a, '_, '_>,
    styles: StyleChain<'a>,
    props: &MathProperties,
) -> SourceResult<()> {
    let mut frame = crate::layout_frame(
        ctx.engine,
        item.content,
        item.locator.relayout(),
        styles,
        ctx.region,
    )?;
    if !frame.has_baseline() {
        let axis = ctx.font().math().axis_height.resolve(styles);
        frame.set_baseline(frame.height() / 2.0 + axis);
    }
    ctx.push(FrameFragment::new(props, styles, frame));
    Ok(())
}

// ===========================================================================
// Misc
// ===========================================================================

fn style_for_script_scale(font: &Font) -> LazyHash<Style> {
    EquationElem::script_scale
        .set((
            font.math().script_percent_scale_down,
            font.math().script_script_percent_scale_down,
        ))
        .wrap()
}

fn get_font(
    world: Tracked<dyn World + '_>,
    styles: StyleChain,
    span: Span,
) -> SourceResult<Font> {
    let variant = variant(styles);
    families(styles)
        .find_map(|family| {
            world
                .book()
                .select(family.as_str(), variant)
                .and_then(|id| world.font(id))
                .filter(|_| family.covers().is_none())
        })
        .ok_or("no font could be found")
        .at(span)
}

fn warn_non_math_font(font: &Font, engine: &mut Engine, span: Span) {
    if !font.info().flags.contains(FontFlags::MATH) {
        engine.sink.warn(warning!(
            span,
            "current font is not designed for math";
            hint: "rendering may be poor";
        ))
    }
}
