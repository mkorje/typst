#[macro_use]
mod shared;
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
use typst_library::math::{self, *};
use typst_library::model::ParElem;
use typst_library::routines::Arenas;
use typst_library::text::{Font, FontFlags, TextEdgeBounds, TextElem, variant};
use typst_syntax::Span;
use typst_utils::{LazyHash, Numeric};

use crate::math::accent::layout_accent;
use crate::math::cancel::layout_cancel;
use crate::math::fenced::layout_fenced;
use crate::math::fraction::{layout_fraction, layout_skewed_fraction};
use crate::math::line::layout_line;
use crate::math::radical::layout_radical;
use crate::math::run::{MathFragmentsExt, MathRunFrameBuilder};
use crate::math::scripts::{layout_primes, layout_scripts};
use crate::math::table::layout_table;
use crate::math::text::{layout_glyph, layout_text};

use self::fragment::{FrameFragment, GlyphFragment, MathFragment};
use self::shared::*;

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

    let mut locator = locator.split();

    let arenas = Arenas::default();
    let run = resolve_equation(elem, engine, &mut locator, &arenas, styles)?;

    // Convert to grid to handle linebreaks and split fences.
    let grid = run.into_grid(&arenas.bump);

    let mut ctx = MathContext::new(engine, &mut locator, region, font.clone());
    let mut items = if !grid.is_multiline() {
        // Single row: can break across lines in paragraph.
        let fragments = ctx.layout_grid_row_into_fragments(&grid.rows[0], styles)?;
        fragments.into_par_items()
    } else {
        // Multiple rows: layout as a single frame.
        let rows = ctx.layout_grid_into_fragment_rows(&grid, styles)?;
        let frame = grid_rows_into_frame(rows, styles);
        vec![InlineItem::Frame(frame)]
    };

    // An empty equation should have a height, so we still create a frame
    // (which is then resized in the loop).
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

    let mut locator = locator.split();

    let arenas = Arenas::default();
    let run = resolve_equation(elem, engine, &mut locator, &arenas, styles)?;

    // Convert to grid to handle linebreaks and split fences.
    let grid = run.into_grid(&arenas.bump);

    let mut ctx = MathContext::new(engine, &mut locator, regions.base(), font.clone());
    let rows = ctx.layout_grid_into_fragment_rows(&grid, styles)?;
    let full_equation_builder = grid_rows_into_builder(rows, styles);
    let width = full_equation_builder.size.x;

    let equation_builders = if styles.get(BlockElem::breakable) {
        let mut rows = full_equation_builder.frames.into_iter().peekable();
        let mut equation_builders = vec![];
        let mut last_first_pos = Point::zero();
        let mut regions = regions;

        loop {
            // Keep track of the position of the first row in this region,
            // so that the offset can be reverted later.
            let Some(&(_, first_pos)) = rows.peek() else { break };
            last_first_pos = first_pos;

            let mut frames = vec![];
            let mut height = Abs::zero();
            while let Some((sub, pos)) = rows.peek() {
                let mut pos = *pos;
                pos.y -= first_pos.y;

                // Finish this region if the line doesn't fit. Only do it if
                // we placed at least one line _or_ we still have non-last
                // regions. Crucially, we don't want to infinitely create
                // new regions which are too small.
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

        // Append remaining rows to the equation builder of the last region.
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

        // Ensure that there is at least one frame, even for empty equations.
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
            .map(MathRunFrameBuilder::build)
            .collect();
        return Ok(Fragment::frames(frames));
    };

    let pod = Region::new(regions.base(), Axes::splat(false));
    let counter = Counter::of(EquationElem::ELEM)
        .display_at(engine, elem.location().unwrap(), styles, numbering, span)?
        .spanned(span);
    let number = crate::layout_frame(engine, &counter, locator.next(&()), styles, pod)?;

    static NUMBER_GUTTER: Em = Em::new(0.5);
    let full_number_width = number.width() + NUMBER_GUTTER.resolve(styles);

    let number_align = match elem.number_align.get(styles) {
        SpecificAlignment::H(h) => SpecificAlignment::Both(h, VAlignment::Horizon),
        SpecificAlignment::V(v) => SpecificAlignment::Both(OuterHAlignment::End, v),
        SpecificAlignment::Both(h, v) => SpecificAlignment::Both(h, v),
    };

    // Add equation numbers to each equation region.
    let region_count = equation_builders.len();
    let frames = equation_builders
        .into_iter()
        .map(|builder| {
            if builder.frames.is_empty() && region_count > 1 {
                // Don't number empty regions, but do number empty equations.
                return builder.build();
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
    let first =
        equation_builder.frames.first().map_or(
            (equation_builder.size, Point::zero(), Abs::zero()),
            |(frame, pos)| (frame.size(), *pos, frame.baseline()),
        );
    let last =
        equation_builder.frames.last().map_or(
            (equation_builder.size, Point::zero(), Abs::zero()),
            |(frame, pos)| (frame.size(), *pos, frame.baseline()),
        );
    let line_count = equation_builder.frames.len();
    let mut equation = equation_builder.build();

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
            // In this case, the center lines (not baselines) of the number frame
            // and the equation frame shall be aligned.
            FixedAlignment::Center => (equation.height() - number.height()) / 2.0,
            FixedAlignment::End => align_baselines(last, &number),
        }
    };

    equation.push_frame(Point::new(x, y), number);
    equation
}

/// Resize the equation's frame accordingly so that it encompasses the number.
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
        // In this case, the center lines (not baselines) of the number frame
        // and the equation frame shall be aligned.
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

    // The vertical expansion is asymmetric on the top and bottom edges, so we
    // first align at the top then translate the content downward later.
    let resizing_offset = equation.resize(
        Size::new(width, equation.height() + excess_above + excess_below),
        Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Start),
    );
    equation.translate(Point::with_y(excess_above));
    resizing_offset + Point::with_y(excess_above)
}

/// Build a MathRunFrameBuilder from pre-split rows of fragments.
/// Uses the same alignment logic as multiline_frame_builder.
fn grid_rows_into_builder(
    rows: Vec<Vec<MathFragment>>,
    styles: StyleChain,
) -> MathRunFrameBuilder {
    use crate::math::run::MathFragmentsExt;

    let row_count = rows.len();
    let alignments = alignments(&rows);

    let leading = if styles.get(EquationElem::size) >= MathSize::Text {
        styles.resolve(ParElem::leading)
    } else {
        Em::new(0.25).resolve(styles)
    };

    let align = styles.resolve(AlignElem::alignment).x;
    let mut frames: Vec<(Frame, Point)> = vec![];
    let mut size = Size::zero();

    for (i, row) in rows.into_iter().enumerate() {
        if i == row_count - 1 && row.is_empty() {
            continue;
        }

        let sub = row.into_line_frame(&alignments.points, LeftRightAlternator::Right);
        if i > 0 {
            size.y += leading;
        }

        let mut pos = Point::with_y(size.y);
        if alignments.points.is_empty() {
            pos.x = align.position(alignments.width - sub.width());
        }
        size.x.set_max(sub.width());
        size.y += sub.height();
        frames.push((sub, pos));
    }

    MathRunFrameBuilder { size, frames }
}

/// Build a frame from pre-split rows of fragments.
fn grid_rows_into_frame(rows: Vec<Vec<MathFragment>>, styles: StyleChain) -> Frame {
    grid_rows_into_builder(rows, styles).build()
}

/// The context for math layout.
struct MathContext<'a, 'v, 'e> {
    // External.
    engine: &'v mut Engine<'e>,
    locator: &'v mut SplitLocator<'a>,
    region: Region,
    // Mutable.
    fonts_stack: Vec<Font>,
    fragments: Vec<MathFragment>,
}

impl<'a, 'v, 'e> MathContext<'a, 'v, 'e> {
    /// Create a new math context.
    fn new(
        engine: &'v mut Engine<'e>,
        locator: &'v mut SplitLocator<'a>,
        base: Size,
        font: Font,
    ) -> Self {
        Self {
            engine,
            locator,
            region: Region::new(base, Axes::splat(false)),
            fonts_stack: vec![font],
            fragments: vec![],
        }
    }

    /// Get the current base font.
    #[inline]
    fn font(&self) -> &Font {
        // Will always be at least one font in the stack.
        self.fonts_stack.last().unwrap()
    }

    /// Push a fragment.
    fn push(&mut self, fragment: impl Into<MathFragment>) {
        self.fragments.push(fragment.into());
    }

    /// Push multiple fragments.
    fn extend(&mut self, fragments: impl IntoIterator<Item = MathFragment>) {
        self.fragments.extend(fragments);
    }

    /// Layout the given element and return the resulting [`MathFragment`]s.
    fn layout_into_fragments(
        &mut self,
        run: &MathItem,
        styles: StyleChain,
    ) -> SourceResult<Vec<MathFragment>> {
        let start = self.fragments.len();
        self.layout_into_self(run, styles)?;
        Ok(self.fragments.drain(start..).collect())
    }

    /// Layout a slice of items and return the resulting [`MathFragment`]s.
    fn layout_items_into_fragments(
        &mut self,
        items: &[MathItem],
        styles: StyleChain,
    ) -> SourceResult<Vec<MathFragment>> {
        let start = self.fragments.len();
        self.layout_items_into_self(items, styles)?;
        Ok(self.fragments.drain(start..).collect())
    }

    /// Layout the given element and return the resulting [`MathFragment`]s.
    fn layout_into_fragment(
        &mut self,
        run: &MathItem,
        styles: StyleChain,
    ) -> SourceResult<MathFragment> {
        let fragments = self.layout_into_fragments(run, styles)?;
        if fragments.len() == 1 {
            return Ok(fragments.into_iter().next().unwrap());
        }

        // Fragments without a math_size are ignored: the notion of size do not
        // apply to them, so their text-likeness is meaningless.
        let text_like = fragments
            .iter()
            .filter(|e| e.math_size().is_some())
            .all(|e| e.is_text_like());

        let styles = run.styles().unwrap_or(styles);
        let props = MathProperties::default(styles);
        let frame = fragments.into_frame(styles);
        Ok(FrameFragment::new(&props, styles, frame)
            .with_text_like(text_like)
            .into())
    }

    fn layout_into_self(
        &mut self,
        run: &MathItem,
        styles: StyleChain,
    ) -> SourceResult<()> {
        let outer_styles = run.styles().unwrap_or(styles);
        self.layout_items_into_self(run.as_slice(), outer_styles)
    }

    /// Layout a slice of items into self.
    fn layout_items_into_self(
        &mut self,
        items: &[MathItem],
        styles: StyleChain,
    ) -> SourceResult<()> {
        let outer_font = styles.get_ref(TextElem::font);

        for item in items {
            let item_styles = item.styles().unwrap_or(styles);

            // Whilst this check isn't exact, it more or less suffices as a
            // change in font variant probably won't have an effect on metrics.
            if item_styles != styles && item_styles.get_ref(TextElem::font) != outer_font
            {
                self.fonts_stack.push(get_font(
                    self.engine.world,
                    item_styles,
                    item.span(),
                )?);
                let scale_style = style_for_script_scale(self.font());
                layout_realized(item, self, item_styles.chain(&scale_style))?;
                self.fonts_stack.pop();
            } else {
                layout_realized(item, self, item_styles)?;
            }
        }

        Ok(())
    }

    /// Layout a grid row (multiple columns) into fragments.
    /// Inserts Align markers between columns.
    fn layout_grid_row_into_fragments(
        &mut self,
        row: &math::MathRow,
        styles: StyleChain,
    ) -> SourceResult<Vec<MathFragment>> {
        let start = self.fragments.len();

        for (i, column) in row.columns.iter().enumerate() {
            if i > 0 {
                // Insert alignment marker between columns
                self.push(MathFragment::Align);
            }
            self.layout_items_into_self(column, styles)?;
        }

        Ok(self.fragments.drain(start..).collect())
    }

    /// Layout a full MathGrid into a list of fragment rows.
    /// Each row is a Vec<MathFragment> with Align markers between columns.
    fn layout_grid_into_fragment_rows(
        &mut self,
        grid: &math::MathGrid,
        styles: StyleChain,
    ) -> SourceResult<Vec<Vec<MathFragment>>> {
        let mut rows = Vec::with_capacity(grid.rows.len());
        for row in &grid.rows {
            rows.push(self.layout_grid_row_into_fragments(row, styles)?);
        }
        Ok(rows)
    }
}

/// Lays out a leaf element resulting from realization.
fn layout_realized(
    item: &MathItem,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let MathItem::Component(comp) = item else {
        match item {
            MathItem::Spacing(amount, _) => ctx.push(MathFragment::Space(*amount)),
            MathItem::Space => ctx
                .push(MathFragment::Space(ctx.font().math().space_width.resolve(styles))),
            MathItem::Linebreak => ctx.push(MathFragment::Linebreak),
            MathItem::Align => ctx.push(MathFragment::Align),
            MathItem::Tag(tag) => ctx.push(MathFragment::Tag(tag.clone())),
            _ => unreachable!(),
        }
        return Ok(());
    };

    let props = &comp.props;

    if let Some(lspace) = props.lspace {
        let width = lspace.at(styles.resolve(TextElem::size));
        let frag = MathFragment::Space(width);
        // TODO: not ignoring tags
        if ctx.fragments.last().is_some_and(|x| matches!(x, MathFragment::Align)) {
            ctx.fragments.insert(ctx.fragments.len() - 1, frag);
        } else {
            ctx.push(frag);
        }
    }

    match &comp.kind {
        MathKind::Box(item) => layout_box(item, ctx, styles, props)?,
        MathKind::External(item) => layout_external(item, ctx, styles, props)?,
        MathKind::Glyph(item) => layout_glyph(item, ctx, styles, props)?,
        MathKind::Cancel(item) => layout_cancel(item, ctx, styles, props)?,
        MathKind::Radical(item) => layout_radical(item, ctx, styles, props)?,
        MathKind::Line(item) => layout_line(item, ctx, styles, props)?,
        MathKind::Accent(item) => layout_accent(item, ctx, styles, props)?,
        MathKind::Scripts(item) => layout_scripts(item, ctx, styles, props)?,
        MathKind::Primes(item) => layout_primes(item, ctx, styles, props)?,
        MathKind::Table(item) => layout_table(item, ctx, styles, props)?,
        MathKind::Fraction(item) => layout_fraction(item, ctx, styles, props)?,
        MathKind::SkewedFraction(item) => {
            layout_skewed_fraction(item, ctx, styles, props)?
        }
        MathKind::Text(item) => layout_text(item, ctx, styles, props)?,
        MathKind::Fenced(item) => layout_fenced(item, ctx, styles, props)?,
        MathKind::Group(_) => {
            let fragment = ctx.layout_into_fragment(item, styles)?;
            let italics = fragment.italics_correction();
            let accent_attach = fragment.accent_attach();
            ctx.push(
                FrameFragment::new(props, styles, fragment.into_frame())
                    .with_italics_correction(italics)
                    .with_accent_attach(accent_attach),
            );
        }
    }

    if let Some(rspace) = props.rspace {
        let width = rspace.at(styles.resolve(TextElem::size));
        ctx.push(MathFragment::Space(width));
    }

    Ok(())
}

/// Lays out an [`BoxItem`].
fn layout_box(
    item: &BoxItem,
    ctx: &mut MathContext,
    styles: StyleChain,
    props: &MathProperties,
) -> SourceResult<()> {
    let frame = crate::inline::layout_box(
        item.elem,
        ctx.engine,
        ctx.locator.next(&item.elem.span()),
        styles,
        ctx.region.size,
    )?;
    ctx.push(FrameFragment::new(props, styles, frame));
    Ok(())
}

/// Layout into a frame with normal layout.
fn layout_external(
    item: &ExternalItem,
    ctx: &mut MathContext,
    styles: StyleChain,
    props: &MathProperties,
) -> SourceResult<()> {
    let mut frame = crate::layout_frame(
        ctx.engine,
        item.content,
        ctx.locator.next(&item.content.span()),
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

/// Styles to add font constants to the style chain.
fn style_for_script_scale(font: &Font) -> LazyHash<Style> {
    EquationElem::script_scale
        .set((
            font.math().script_percent_scale_down,
            font.math().script_script_percent_scale_down,
        ))
        .wrap()
}

/// Get the current base font.
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

/// Check if the top-level base font has a MATH table.
fn warn_non_math_font(font: &Font, engine: &mut Engine, span: Span) {
    if !font.info().flags.contains(FontFlags::MATH) {
        engine.sink.warn(warning!(
            span,
            "current font is not designed for math";
            hint: "rendering may be poor";
        ))
    }
}
