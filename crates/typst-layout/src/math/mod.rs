#[macro_use]
mod shared;
mod accent;
mod attach;
mod cancel;
mod frac;
mod fragment;
mod lr;
mod mat;
mod root;
mod run;
mod stretch;
mod text;
mod underover;

use comemo::Tracked;
use typst_library::World;
use typst_library::diag::{At, SourceResult, warning};
use typst_library::engine::Engine;
use typst_library::foundations::{
    Content, NativeElement, Packed, Resolve, Style, StyleChain, Styles, SymbolElem,
};
use typst_library::introspection::{Counter, Locator, SplitLocator, TagElem};
use typst_library::layout::{
    Abs, AlignElem, Axes, Axis, BlockElem, BoxElem, Em, FixedAlignment, Fragment, Frame,
    FrameItem, HElem, InlineItem, OuterHAlignment, PlaceElem, Point, Region, Regions,
    Rel, Size, Spacing, SpecificAlignment, VAlignment,
};
use typst_library::math::{MathContext as MathCtx, MathRun as MathRn, *};
use typst_library::model::ParElem;
use typst_library::routines::{Arenas, RealizationKind};
use typst_library::text::{
    BottomEdge, BottomEdgeMetric, Font, FontFlags, LinebreakElem, SpaceElem,
    TextEdgeBounds, TextElem, TopEdge, TopEdgeMetric, variant,
};
use typst_library::visualize::{FixedStroke, Geometry, LineCap};
use typst_syntax::{Span, is_newline};
use typst_utils::{Get, LazyHash, Numeric};

use unicode_math_class::MathClass;

use crate::math::attach::layout_attachments;
use crate::math::cancel::draw_cancel_line;
use crate::math::mat::line_item;

use self::fragment::{
    FrameFragment, GlyphFragment, MathFragment, has_dtls_feat, stretch_axes,
};
use self::run::{MathRun, MathRunFrameBuilder};
use self::shared::*;
use self::stretch::stretch_fragment;

macro_rules! measure {
    ($e: ident, $attr: ident) => {
        $e.as_ref().map(|e| e.$attr()).unwrap_or_default()
    };
}

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

    let mut locator = locator.split();
    let arenas = Arenas::default();
    let scale_style = style_for_script_scale(&font);
    let styles = styles.chain(&scale_style);
    let mut ctx = MathCtx::new(engine, &mut locator, &arenas);

    let run = ctx.resolve_into_run(&elem.body, styles)?;

    let mut items = {
        let region = Region::new(region, Axes::splat(false));
        let mut fonts_stack = vec![font.clone()];
        let fragments = convert_to_fragments(
            engine,
            &mut locator,
            region,
            &mut fonts_stack,
            &run,
            styles,
        )?;
        if run.row_count() == 1 {
            fragments.into_par_items()
        } else {
            vec![InlineItem::Frame(fragments.into_fragment(styles).into_frame())]
        }
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

fn convert_to_fragments(
    engine: &mut Engine,
    locator: &mut SplitLocator,
    region: Region,
    fonts_stack: &mut Vec<Font>,
    run: &MathRn,
    outer_styles: StyleChain,
) -> SourceResult<MathRun> {
    let mut fragments = vec![];
    let outer_font = outer_styles.get_ref(TextElem::font);

    let mut items = run.iter().enumerate().peekable();
    while let Some((i, item)) = items.next() {
        let styles = item.styles().unwrap_or(outer_styles);
        let mut new_styles = Styles::new();

        // Whilst this check isn't exact, it more or less suffices as a
        // change in font variant probably won't have an effect on metrics.
        let new_font =
            styles != outer_styles && styles.get_ref(TextElem::font) != outer_font;
        if new_font {
            // TODO: use proper span.
            fonts_stack.push(get_font(engine.world, styles, Span::detached())?);
            new_styles.apply(style_for_script_scale(fonts_stack.last().unwrap()).into());
        }

        let styles = styles.chain(&new_styles);

        let mut push_space = |item: &MathItem, size, fragment: MathFragment| {
            if let Some(lspace) = item.lspace() {
                let width = lspace.at(size);
                let frag = MathFragment::Spacing(width, false);
                if fragments.last().is_some_and(|x| matches!(x, MathFragment::Align)) {
                    fragments.insert(fragments.len() - 1, frag);
                } else {
                    fragments.push(frag);
                }
            }
            fragments.push(fragment);
            if let Some(rspace) = item.rspace() {
                let width = rspace.at(size);
                fragments.push(MathFragment::Spacing(width, false));
            }
        };

        match item {
            MathItem::Tag(tag) => fragments.push(MathFragment::Tag(tag.clone())),
            MathItem::Space => {
                fragments.push(MathFragment::Space(
                    fonts_stack.last().unwrap().math().space_width.resolve(styles),
                ));
            }
            MathItem::Linebreak => fragments.push(MathFragment::Linebreak),
            MathItem::Glyph(glyph_item) => {
                if let Some(mut glyph) = GlyphFragment::new(
                    engine.world,
                    styles,
                    &glyph_item.text,
                    glyph_item.span,
                )? {
                    if glyph.class == MathClass::Large {
                        if styles.get(EquationElem::size) == MathSize::Display {
                            let height = glyph
                                .item
                                .font
                                .math()
                                .display_operator_min_height
                                .at(glyph.item.size);
                            glyph.stretch_vertical(engine, height, Abs::zero());
                        };
                        // TeXbook p 155. Large operators are always vertically centered on
                        // the axis.
                        glyph.center_on_axis();
                    }
                    let axes = stretch_axes(&glyph.item.font, glyph.base_glyph.id);
                    let size = glyph.item.size;
                    let mut glyph = glyph.into();
                    if let Some((stretch, axis)) = glyph_item.stretch {
                        let relative = match axis {
                            Some(axis) => axes.get(axis),
                            None => false,
                        };

                        if !relative {
                            stretch_fragment(
                                engine,
                                &mut glyph,
                                None,
                                None,
                                stretch,
                                Abs::zero(),
                            );
                        } else {
                            glyph.set_stretch(Some(stretch));
                        }
                    }
                    push_space(item, size, glyph);
                }
            }

            MathItem::Primes(primes) => {
                let prime = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &primes.prime,
                    styles,
                )?
                .into_fragment(styles)
                .into_frame();
                let width = prime.width() * (primes.count + 1) as f64 / 2.0;
                let mut frame = Frame::soft(Size::new(width, prime.height()));
                frame.set_baseline(prime.ascent());

                for i in 0..primes.count {
                    frame.push_frame(
                        Point::new(prime.width() * (i as f64 / 2.0), Abs::zero()),
                        prime.clone(),
                    )
                }
                fragments.push(FrameFragment::new(styles, frame).into());
            }

            MathItem::Fenced(fenced) => {
                let body = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &fenced.body,
                    styles,
                )?
                .0;
                let relative_to =
                    body.iter().map(|f| f.height()).max().unwrap_or_default();
                if let Some(mut open) = fenced
                    .open
                    .as_ref()
                    .map(|open| {
                        convert_to_fragments(
                            engine,
                            locator,
                            region,
                            fonts_stack,
                            &open,
                            styles,
                        )
                    })
                    .transpose()?
                    .map(|open| open.into_fragment(styles))
                {
                    let short_fall =
                        DELIM_SHORT_FALL.at(open.font_size().unwrap_or_default());
                    stretch_fragment(
                        engine,
                        &mut open,
                        Some(Axis::Y),
                        Some(relative_to),
                        fenced.target,
                        short_fall,
                    );
                    fragments.push(open);
                }

                fragments.extend(body);
                if let Some(mut close) = fenced
                    .close
                    .as_ref()
                    .map(|close| {
                        convert_to_fragments(
                            engine,
                            locator,
                            region,
                            fonts_stack,
                            &close,
                            styles,
                        )
                    })
                    .transpose()?
                    .map(|close| close.into_fragment(styles))
                {
                    let short_fall =
                        DELIM_SHORT_FALL.at(close.font_size().unwrap_or_default());
                    stretch_fragment(
                        engine,
                        &mut close,
                        Some(Axis::Y),
                        Some(relative_to),
                        fenced.target,
                        short_fall,
                    );
                    fragments.push(close);
                }
            }

            MathItem::Group(group) => {
                let mut frags = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &group.items,
                    styles,
                )?
                .0;

                let mut max_extent = Abs::zero();
                for fragment in frags.iter() {
                    let (font, size) = fragment.font2(fonts_stack, styles);
                    let axis = font.math().axis_height.at(size);
                    let extent =
                        (fragment.ascent() - axis).max(fragment.descent() + axis);
                    max_extent = max_extent.max(extent);
                }

                let relative_to = 2.0 * max_extent;
                // let (ascent, descent) = (
                //     frags.iter().map(|frag| frag.ascent()).max().unwrap_or_default(),
                //     frags.iter().map(|frag| frag.descent()).max().unwrap_or_default(),
                // );
                // let height = ascent + descent;
                for frag in frags.iter_mut() {
                    if let Some(size) = frag.stretch() {
                        let short_fall =
                            DELIM_SHORT_FALL.at(frag.font_size().unwrap_or_default());
                        stretch_fragment(
                            engine,
                            frag,
                            Some(Axis::Y),
                            Some(relative_to),
                            size,
                            short_fall,
                        );
                        frag.set_stretch(None);
                    }
                }

                let size = styles.resolve(TextElem::size);
                if let Some(lspace) = item.lspace() {
                    let width = lspace.at(size);
                    fragments.push(MathFragment::Spacing(width, false));
                }
                fragments.extend(frags);
                if let Some(rspace) = item.rspace() {
                    let width = rspace.at(size);
                    fragments.push(MathFragment::Spacing(width, false));
                }
            }

            MathItem::Radical(radical) => {
                let span = radical.span;

                // Layout radicand.
                let radicand = {
                    let run = convert_to_fragments(
                        engine,
                        locator,
                        region,
                        fonts_stack,
                        &radical.radicand,
                        styles,
                    )?;
                    let multiline = run.is_multiline();
                    let radicand = run.into_fragment(styles);
                    if multiline {
                        // Align the frame center line with the math axis.
                        let (font, size) = radicand.font2(&fonts_stack, styles);
                        let axis = font.math().axis_height.at(size);
                        let mut radicand = radicand.into_frame();
                        radicand.set_baseline(radicand.height() / 2.0 + axis);
                        radicand
                    } else {
                        radicand.into_frame()
                    }
                };

                // Layout root symbol.
                let mut sqrt = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &radical.sqrt,
                    styles,
                )?
                .into_fragment(styles);

                let (font, size) = sqrt.font2(&fonts_stack, styles);
                let thickness = font.math().radical_rule_thickness.at(size);
                let extra_ascender = font.math().radical_extra_ascender.at(size);
                let kern_before = font.math().radical_kern_before_degree.at(size);
                let kern_after = font.math().radical_kern_after_degree.at(size);
                let raise_factor = font.math().radical_degree_bottom_raise_percent;
                let gap = match styles.get(EquationElem::size) {
                    MathSize::Display => font.math().radical_display_style_vertical_gap,
                    _ => font.math().radical_vertical_gap,
                }
                .at(size);

                let line = FrameItem::Shape(
                    Geometry::Line(Point::with_x(radicand.width())).stroked(
                        FixedStroke::from_pair(
                            sqrt.fill().unwrap_or_else(|| {
                                styles.get_ref(TextElem::fill).as_decoration()
                            }),
                            thickness,
                        ),
                    ),
                    span,
                );

                let target = radicand.height() + thickness + gap;
                sqrt.stretch_vertical(engine, target, Abs::zero());
                let sqrt = sqrt.into_frame();

                // Layout the index.
                let index = radical
                    .index
                    .as_ref()
                    .map(|elem| {
                        convert_to_fragments(
                            engine,
                            locator,
                            region,
                            fonts_stack,
                            &elem,
                            styles,
                        )
                    })
                    .transpose()?
                    .map(|idx| idx.into_fragment(styles).into_frame());

                // TeXbook, page 443, item 11
                // Keep original gap, and then distribute any remaining free space
                // equally above and below.
                let gap =
                    gap.max((sqrt.height() - thickness - radicand.height() + gap) / 2.0);

                let sqrt_ascent = radicand.ascent() + gap + thickness;
                let descent = sqrt.height() - sqrt_ascent;
                let inner_ascent = sqrt_ascent + extra_ascender;

                let mut sqrt_offset = Abs::zero();
                let mut shift_up = Abs::zero();
                let mut ascent = inner_ascent;

                if let Some(index) = &index {
                    sqrt_offset = kern_before + index.width() + kern_after;
                    // The formula below for how much raise the index by comes from
                    // the TeXbook, page 360, in the definition of `\root`.
                    // However, the `+ index.descent()` part is different from TeX.
                    // Without it, descenders can collide with the surd, a rarity
                    // in practice, but possible.  MS Word also adjusts index positions
                    // for descenders.
                    shift_up = raise_factor * (inner_ascent - descent) + index.descent();
                    ascent.set_max(shift_up + index.ascent());
                }

                let sqrt_x = sqrt_offset.max(Abs::zero());
                let radicand_x = sqrt_x + sqrt.width();
                let radicand_y = ascent - radicand.ascent();
                let width = radicand_x + radicand.width();
                let size = Size::new(width, ascent + descent);

                // The extra "- thickness" comes from the fact that the sqrt is placed
                // in `push_frame` with respect to its top, not its baseline.
                let sqrt_pos = Point::new(sqrt_x, radicand_y - gap - thickness);
                let line_pos =
                    Point::new(radicand_x, radicand_y - gap - (thickness / 2.0));
                let radicand_pos = Point::new(radicand_x, radicand_y);

                let mut frame = Frame::soft(size);
                frame.set_baseline(ascent);

                if let Some(index) = index {
                    let index_x = -sqrt_offset.min(Abs::zero()) + kern_before;
                    let index_pos =
                        Point::new(index_x, ascent - index.ascent() - shift_up);
                    frame.push_frame(index_pos, index);
                }

                frame.push_frame(sqrt_pos, sqrt);
                frame.push(line_pos, line);
                frame.push_frame(radicand_pos, radicand);
                fragments.push(FrameFragment::new(styles, frame).into());
            }

            MathItem::Fraction(fraction) => {
                let num = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &fraction.numerator,
                    styles,
                )?
                .into_fragment(styles)
                .into_frame();
                let denom = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &fraction.denominator,
                    styles,
                )?
                .into_fragment(styles)
                .into_frame();

                let constants = fonts_stack.last().unwrap().math();
                let axis = constants.axis_height.resolve(styles);
                let thickness = constants.fraction_rule_thickness.resolve(styles);
                let size = styles.get(EquationElem::size);
                let shift_up = match size {
                    MathSize::Display => {
                        constants.fraction_numerator_display_style_shift_up
                    }
                    _ => constants.fraction_numerator_shift_up,
                }
                .resolve(styles);
                let shift_down = match size {
                    MathSize::Display => {
                        constants.fraction_denominator_display_style_shift_down
                    }
                    _ => constants.fraction_denominator_shift_down,
                }
                .resolve(styles);
                let num_min = match size {
                    MathSize::Display => constants.fraction_num_display_style_gap_min,
                    _ => constants.fraction_numerator_gap_min,
                }
                .resolve(styles);
                let denom_min = match size {
                    MathSize::Display => constants.fraction_denom_display_style_gap_min,
                    _ => constants.fraction_denominator_gap_min,
                }
                .resolve(styles);

                const FRAC_AROUND: Em = Em::new(0.1);
                let around = FRAC_AROUND.resolve(styles);
                let num_gap =
                    (shift_up - (axis + thickness / 2.0) - num.descent()).max(num_min);
                let denom_gap = (shift_down + (axis - thickness / 2.0) - denom.ascent())
                    .max(denom_min);

                let line_width = num.width().max(denom.width());
                let width = line_width + 2.0 * around;
                let height =
                    num.height() + num_gap + thickness + denom_gap + denom.height();
                let size = Size::new(width, height);
                let num_pos = Point::with_x((width - num.width()) / 2.0);
                let line_pos = Point::new(
                    (width - line_width) / 2.0,
                    num.height() + num_gap + thickness / 2.0,
                );
                let denom_pos =
                    Point::new((width - denom.width()) / 2.0, height - denom.height());
                let baseline = line_pos.y + axis;

                let mut frame = Frame::soft(size);
                frame.set_baseline(baseline);
                frame.push_frame(num_pos, num);
                frame.push_frame(denom_pos, denom);

                if fraction.line {
                    frame.push(
                        line_pos,
                        FrameItem::Shape(
                            Geometry::Line(Point::with_x(line_width)).stroked(
                                FixedStroke::from_pair(
                                    styles.get_ref(TextElem::fill).as_decoration(),
                                    thickness,
                                ),
                            ),
                            fraction.span,
                        ),
                    );
                }
                fragments.push(FrameFragment::new(styles, frame).into());
            }

            MathItem::Text(text) => {
                let span = text.span;
                let text = &text.text;

                let mut layout_inline_text = |line: &str| -> SourceResult<FrameFragment> {
                    if line.chars().all(|c| c.is_ascii_digit() || c == '.') {
                        let mut frags = vec![];
                        for c in line.chars() {
                            // This won't panic as ASCII digits and '.' will never end up as
                            // nothing after shaping.
                            let glyph = GlyphFragment::new(
                                engine.world,
                                styles,
                                c.encode_utf8(&mut [0; 4]),
                                span,
                            )?
                            .unwrap();
                            frags.push(glyph.into());
                        }
                        let frame = MathRun::new(frags).into_frame(styles);
                        Ok(FrameFragment::new(styles, frame).with_text_like(true))
                    } else {
                        let local = [
                            TextElem::top_edge
                                .set(TopEdge::Metric(TopEdgeMetric::Bounds)),
                            TextElem::bottom_edge
                                .set(BottomEdge::Metric(BottomEdgeMetric::Bounds)),
                        ]
                        .map(|p| p.wrap());

                        let styles = styles.chain(&local);
                        let elem = TextElem::packed(text).spanned(span);

                        // There isn't a natural width for a paragraph in a math environment;
                        // because it will be placed somewhere probably not at the left margin
                        // it will overflow. So emulate an `hbox` instead and allow the
                        // paragraph to extend as far as needed.
                        let frame = crate::inline::layout_inline(
                            engine,
                            &[(&elem, styles)],
                            &mut locator.next(&span).split(),
                            styles,
                            Size::splat(Abs::inf()),
                            false,
                        )?
                        .into_frame();

                        Ok(FrameFragment::new(styles, frame)
                            .with_class(MathClass::Alphabetic)
                            .with_text_like(true)
                            .with_spaced(true))
                    }
                };

                let fragment = if text.contains(is_newline) {
                    let mut frags = vec![];
                    for (i, line) in text.split(is_newline).enumerate() {
                        if i != 0 {
                            frags.push(MathFragment::Linebreak);
                        }
                        if !line.is_empty() {
                            frags.push(layout_inline_text(line)?.into());
                        }
                    }
                    let mut frame = MathRun::new(frags).into_frame(styles);
                    let axis =
                        fonts_stack.last().unwrap().math().axis_height.resolve(styles);
                    frame.set_baseline(frame.height() / 2.0 + axis);
                    FrameFragment::new(styles, frame)
                } else {
                    layout_inline_text(text)?
                };

                push_space(item, styles.resolve(TextElem::size), fragment.into());
            }

            MathItem::Accent(accent) => {
                let top_accent = !accent.is_bottom;
                let base = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &accent.base,
                    styles,
                )?
                .into_fragment(styles);
                let (font, size) = base.font2(&fonts_stack, styles);

                let base_class = base.class();

                let base_attach = base.accent_attach();

                let flattened_base_height =
                    font.math().flattened_accent_base_height.at(size);
                let flac = style_flac();
                let accent_styles = if top_accent && base.ascent() > flattened_base_height
                {
                    styles.chain(&flac)
                } else {
                    styles
                };

                let accent = &accent.accent;
                let mut iter = accent.iter();
                let width = if let Some(item) = iter.next()
                    && iter.next().is_none()
                    && let MathItem::Glyph(glyph) = item
                {
                    glyph.stretch.map(|(w, b)| w)
                } else {
                    None
                };

                let mut accent = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &accent,
                    accent_styles,
                )?
                .into_fragment(accent_styles);

                // Forcing the accent to be at least as large as the base makes it too wide
                // in many cases.
                let width = width.unwrap_or(Rel::one()).relative_to(base.width());
                const ACCENT_SHORT_FALL: Em = Em::new(0.5);
                let short_fall = ACCENT_SHORT_FALL.at(size);
                accent.stretch_horizontal(engine, width, short_fall);
                let accent_attach = accent.accent_attach().0;
                let accent = accent.into_frame();

                let (gap, accent_pos, base_pos) = if top_accent {
                    // Descent is negative because the accent's ink bottom is above the
                    // baseline. Therefore, the default gap is the accent's negated descent
                    // minus the accent base height. Only if the base is very small, we
                    // need a larger gap so that the accent doesn't move too low.
                    let accent_base_height = font.math().accent_base_height.at(size);
                    let gap = -accent.descent() - base.ascent().min(accent_base_height);
                    let accent_pos = Point::with_x(base_attach.0 - accent_attach);
                    let base_pos = Point::with_y(accent.height() + gap);
                    (gap, accent_pos, base_pos)
                } else {
                    let gap = -accent.ascent();
                    let accent_pos =
                        Point::new(base_attach.1 - accent_attach, base.height() + gap);
                    let base_pos = Point::zero();
                    (gap, accent_pos, base_pos)
                };

                let size = Size::new(base.width(), accent.height() + gap + base.height());
                let baseline = base_pos.y + base.ascent();

                let base_italics_correction = base.italics_correction();
                let base_text_like = base.is_text_like();
                let base_ascent = match &base {
                    MathFragment::Frame(frame) => frame.base_ascent,
                    _ => base.ascent(),
                };
                let base_descent = match &base {
                    MathFragment::Frame(frame) => frame.base_descent,
                    _ => base.descent(),
                };

                let mut frame = Frame::soft(size);
                frame.set_baseline(baseline);
                frame.push_frame(accent_pos, accent);
                frame.push_frame(base_pos, base.into_frame());
                fragments.push(
                    FrameFragment::new(styles, frame)
                        .with_class(base_class)
                        .with_base_ascent(base_ascent)
                        .with_base_descent(base_descent)
                        .with_italics_correction(base_italics_correction)
                        .with_accent_attach(base_attach)
                        .with_text_like(base_text_like)
                        .into(),
                );
            }

            MathItem::Table(table) => {
                let span = table.span;
                let rows = &table.cells;
                let nrows = rows.len();
                let ncols = rows.first().map_or(0, |row| row.len());
                // Transpose rows of the matrix into columns.
                let mut row_iters: Vec<_> = rows.iter().map(|i| i.iter()).collect();
                let columns: Vec<Vec<_>> = (0..ncols)
                    .map(|_| row_iters.iter_mut().map(|i| i.next().unwrap()).collect())
                    .collect();

                let nrows = columns.first().map_or(0, |col| col.len());
                let ncols = columns.len();
                if ncols == 0 || nrows == 0 {
                    fragments.push(
                        FrameFragment::new(styles, Frame::soft(Size::zero())).into(),
                    );
                    continue;
                }

                let gap = table.gap.zip_map(region.size, Rel::relative_to);
                let half_gap = gap * 0.5;

                // We provide a default stroke thickness that scales
                // with font size to ensure that augmentation lines
                // look correct by default at all matrix sizes.
                // The line cap is also set to square because it looks more "correct".
                const DEFAULT_STROKE_THICKNESS: Em = Em::new(0.05);
                let default_stroke_thickness = DEFAULT_STROKE_THICKNESS.resolve(styles);
                let default_stroke = FixedStroke {
                    thickness: default_stroke_thickness,
                    paint: styles.get_ref(TextElem::fill).as_decoration(),
                    cap: LineCap::Square,
                    ..Default::default()
                };

                let (mut hline, mut vline, stroke) = match &table.augment {
                    Some(augment) => {
                        // We need to get stroke here for ownership.
                        let stroke = augment
                            .stroke
                            .clone()
                            .unwrap_or_default()
                            .unwrap_or(default_stroke);
                        (augment.hline.clone(), augment.vline.clone(), stroke)
                    }
                    _ => (
                        AugmentOffsets::default(),
                        AugmentOffsets::default(),
                        default_stroke,
                    ),
                };

                // Before the full matrix body can be laid out, the
                // individual cells must first be independently laid out
                // so we can ensure alignment across rows and columns.
                let mut cols = vec![vec![]; ncols];

                // This variable stores the maximum ascent and descent for each row.
                let mut heights = vec![(Abs::zero(), Abs::zero()); nrows];

                // We pad ascent and descent with the ascent and descent of the paren
                // to ensure that normal matrices are aligned with others unless they are
                // way too big.
                // This will never panic as a paren will never shape into nothing.
                let denom_style = style_for_denominator(styles);
                let paren = GlyphFragment::new(
                    engine.world,
                    styles.chain(&denom_style),
                    '('.encode_utf8(&mut [0; 4]),
                    Span::detached(),
                )?
                .unwrap();

                for (column, col) in columns.iter().zip(&mut cols) {
                    for (cell, (ascent, descent)) in column.iter().zip(&mut heights) {
                        let cell = convert_to_fragments(
                            engine,
                            locator,
                            region,
                            fonts_stack,
                            cell,
                            styles,
                        )?;

                        ascent.set_max(cell.ascent().max(paren.ascent()));
                        descent.set_max(cell.descent().max(paren.descent()));

                        col.push(cell);
                    }
                }

                for line in hline.0.iter_mut() {
                    if *line < 0 {
                        *line += nrows as isize;
                    }
                }

                for line in vline.0.iter_mut() {
                    if *line < 0 {
                        *line += ncols as isize;
                    }
                }

                // For each row, combine maximum ascent and descent into a row height.
                // Sum the row heights, then add the total height of the gaps between rows.
                let mut total_height = heights.iter().map(|&(a, b)| a + b).sum::<Abs>()
                    + gap.y * (nrows - 1) as f64;

                if hline.0.contains(&0) {
                    total_height += gap.y;
                }

                if hline.0.contains(&(nrows as isize)) {
                    total_height += gap.y;
                }

                // Width starts at zero because it can't be calculated until later
                let mut frame = Frame::soft(Size::new(Abs::zero(), total_height));

                let mut x = Abs::zero();

                if vline.0.contains(&0) {
                    frame.push(
                        Point::with_x(x + half_gap.x),
                        line_item(total_height, true, stroke.clone(), span),
                    );
                    x += gap.x;
                }

                for (index, col) in cols.into_iter().enumerate() {
                    let AlignmentResult { points, width: rcol } = alignments(&col);

                    let mut y = if hline.0.contains(&0) { gap.y } else { Abs::zero() };

                    for (cell, &(ascent, descent)) in col.into_iter().zip(&heights) {
                        let cell = cell.into_line_frame(&points, table.alternator);
                        let pos = Point::new(
                            if points.is_empty() {
                                x + table.align.position(rcol - cell.width())
                            } else {
                                x
                            },
                            y + ascent - cell.ascent(),
                        );

                        frame.push_frame(pos, cell);

                        y += ascent + descent + gap.y;
                    }

                    // Advance to the end of the column
                    x += rcol;

                    // If a vertical line should be inserted after this column
                    if vline.0.contains(&(index as isize + 1)) {
                        frame.push(
                            Point::with_x(x + half_gap.x),
                            line_item(total_height, true, stroke.clone(), span),
                        );
                    }

                    // Advance to the start of the next column
                    x += gap.x;
                }

                let total_width =
                    if !(vline.0.contains(&(ncols as isize))) { x - gap.x } else { x };

                // This allows the horizontal lines to be laid out
                for line in hline.0 {
                    let offset = if line == 0 {
                        gap.y
                    } else {
                        (heights[0..line as usize]
                            .iter()
                            .map(|&(a, b)| a + b)
                            .sum::<Abs>()
                            + gap.y * (line - 1) as f64)
                            + half_gap.y
                    };

                    frame.push(
                        Point::with_y(offset),
                        line_item(total_width, false, stroke.clone(), span),
                    );
                }

                frame.size_mut().x = total_width;

                let axis = fonts_stack.last().unwrap().math().axis_height.resolve(styles);
                let height = frame.height();
                frame.set_baseline(height / 2.0 + axis);

                fragments.push(FrameFragment::new(styles, frame).into());
            }

            MathItem::Scripts(scripts) => {
                let mut base = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &scripts.base,
                    styles,
                )?
                .into_fragment(styles);

                macro_rules! layout {
                    ($content:ident) => {
                        scripts
                            .$content
                            .as_ref()
                            .map(|elem| {
                                convert_to_fragments(
                                    engine,
                                    locator,
                                    region,
                                    fonts_stack,
                                    elem,
                                    styles,
                                )
                            })
                            .transpose()?
                            .map(|elem| elem.into_fragment(styles))
                    };
                }

                // Layout the top and bottom attachments early so we can measure their
                // widths, in order to calculate what the stretch size is relative to.
                let top = layout!(top);
                let bottom = layout!(bottom);
                if let Some(size) = base.stretch() {
                    let relative_to_width =
                        measure!(top, width).max(measure!(bottom, width));
                    stretch_fragment(
                        engine,
                        &mut base,
                        Some(Axis::X),
                        Some(relative_to_width),
                        size,
                        Abs::zero(),
                    );
                    base.set_stretch(None);
                }

                let frags = [
                    layout!(top_left),
                    top,
                    layout!(top_right),
                    layout!(bottom_left),
                    bottom,
                    layout!(bottom_right),
                ];
                push_space(
                    item,
                    styles.resolve(TextElem::size),
                    layout_attachments(styles, fonts_stack, base, frags)?,
                );
            }

            MathItem::Spacing(amount, weak) => {
                fragments.push(MathFragment::Spacing(*amount, *weak));
            }

            MathItem::Line(line) => {
                let (
                    extra_height,
                    content,
                    line_pos,
                    content_pos,
                    baseline,
                    bar_height,
                    line_adjust,
                );
                if line.under {
                    content = convert_to_fragments(
                        engine,
                        locator,
                        region,
                        fonts_stack,
                        &line.base,
                        styles,
                    )?
                    .into_fragment(styles);

                    let (font, size) = content.font2(fonts_stack, styles);
                    let sep = font.math().underbar_extra_descender.at(size);
                    bar_height = font.math().underbar_rule_thickness.at(size);
                    let gap = font.math().underbar_vertical_gap.at(size);
                    extra_height = sep + bar_height + gap;

                    line_pos = Point::with_y(content.height() + gap + bar_height / 2.0);
                    content_pos = Point::zero();
                    baseline = content.ascent();
                    line_adjust = -content.italics_correction();
                } else {
                    content = convert_to_fragments(
                        engine,
                        locator,
                        region,
                        fonts_stack,
                        &line.base,
                        styles,
                    )?
                    .into_fragment(styles);

                    let (font, size) = content.font2(fonts_stack, styles);
                    let sep = font.math().overbar_extra_ascender.at(size);
                    bar_height = font.math().overbar_rule_thickness.at(size);
                    let gap = font.math().overbar_vertical_gap.at(size);
                    extra_height = sep + bar_height + gap;

                    line_pos = Point::with_y(sep + bar_height / 2.0);
                    content_pos = Point::with_y(extra_height);
                    baseline = content.ascent() + extra_height;
                    line_adjust = Abs::zero();
                }

                let width = content.width();
                let height = content.height() + extra_height;
                let size = Size::new(width, height);
                let line_width = width + line_adjust;

                let content_class = content.class();
                let content_is_text_like = content.is_text_like();
                let content_italics_correction = content.italics_correction();
                let mut frame = Frame::soft(size);
                frame.set_baseline(baseline);
                frame.push_frame(content_pos, content.into_frame());
                frame.push(
                    line_pos,
                    FrameItem::Shape(
                        Geometry::Line(Point::with_x(line_width)).stroked(FixedStroke {
                            paint: styles.get_ref(TextElem::fill).as_decoration(),
                            thickness: bar_height,
                            ..FixedStroke::default()
                        }),
                        line.span,
                    ),
                );

                fragments.push(
                    FrameFragment::new(styles, frame)
                        .with_class(content_class)
                        .with_text_like(content_is_text_like)
                        .with_italics_correction(content_italics_correction)
                        .into(),
                );
            }
            MathItem::Cancel(cancel) => {
                let body = convert_to_fragments(
                    engine,
                    locator,
                    region,
                    fonts_stack,
                    &cancel.base,
                    styles,
                )?
                .into_fragment(styles);

                // Preserve properties of body.
                let body_class = body.class();
                let body_italics = body.italics_correction();
                let body_attach = body.accent_attach();
                let body_text_like = body.is_text_like();

                let mut body = body.into_frame();
                let body_size = body.size();

                let first_line = draw_cancel_line(
                    engine,
                    cancel.length,
                    cancel.stroke.clone(),
                    cancel.invert_first_line,
                    &cancel.angle,
                    body_size,
                    styles,
                    cancel.span,
                )?;

                // The origin of our line is the very middle of the element.
                let center = body_size.to_point() / 2.0;
                body.push_frame(center, first_line);

                if cancel.cross {
                    // Draw the second line.
                    let second_line = draw_cancel_line(
                        engine,
                        cancel.length,
                        cancel.stroke.clone(),
                        true,
                        &cancel.angle,
                        body_size,
                        styles,
                        cancel.span,
                    )?;

                    body.push_frame(center, second_line);
                }

                fragments.push(
                    FrameFragment::new(styles, body)
                        .with_class(body_class)
                        .with_italics_correction(body_italics)
                        .with_accent_attach(body_attach)
                        .with_text_like(body_text_like)
                        .into(),
                );
            }

            MathItem::SkewedFraction(skewed_fraction) => {}

            MathItem::Align => fragments.push(MathFragment::Align),

            MathItem::Box(boxed) => {
                let frame = crate::inline::layout_box(
                    boxed.elem,
                    engine,
                    locator.next(&boxed.elem.span()),
                    styles,
                    region.size,
                )?;
                fragments.push(FrameFragment::new(styles, frame).into());
            }

            MathItem::External(external) => {
                let mut frame = crate::layout_frame(
                    engine,
                    external.content,
                    locator.next(&external.content.span()),
                    styles,
                    region,
                )?;
                if !frame.has_baseline() {
                    let axis =
                        fonts_stack.last().unwrap().math().axis_height.resolve(styles);
                    frame.set_baseline(frame.height() / 2.0 + axis);
                }
                fragments.push(FrameFragment::new(styles, frame).into());
            }
        }
        if new_font {
            fonts_stack.pop();
        }
    }

    Ok(MathRun(fragments))
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

    let mut locator = locator.split();
    let arenas = Arenas::default();
    let scale_style = style_for_script_scale(&font);
    let styles = styles.chain(&scale_style);
    let mut ctx = MathCtx::new(engine, &mut locator, &arenas);

    let run = ctx.resolve_into_run(&elem.body, styles)?;

    let mut fonts_stack = vec![font.clone()];
    let region = Region::new(regions.base(), Axes::splat(false));
    let full_equation_builder = convert_to_fragments(
        engine,
        &mut locator,
        region,
        &mut fonts_stack,
        &run,
        styles,
    )?
    .multiline_frame_builder(styles);
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
        .display_at_loc(engine, elem.location().unwrap(), styles, numbering)?
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

    /// Layout the given element and return the result as a [`MathRun`].
    fn layout_into_run(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathRun> {
        Ok(MathRun::new(self.layout_into_fragments(elem, styles)?))
    }

    /// Layout the given element and return the resulting [`MathFragment`]s.
    fn layout_into_fragments(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Vec<MathFragment>> {
        // The element's layout_math() changes the fragments held in this
        // MathContext object, but for convenience this function shouldn't change
        // them, so we restore the MathContext's fragments after obtaining the
        // layout result.
        let prev = std::mem::take(&mut self.fragments);
        self.layout_into_self(elem, styles)?;
        Ok(std::mem::replace(&mut self.fragments, prev))
    }

    /// Layout the given element and return the result as a
    /// unified [`MathFragment`].
    fn layout_into_fragment(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<MathFragment> {
        Ok(self.layout_into_run(elem, styles)?.into_fragment(styles))
    }

    /// Layout the given element and return the result as a [`Frame`].
    fn layout_into_frame(
        &mut self,
        elem: &Content,
        styles: StyleChain,
    ) -> SourceResult<Frame> {
        Ok(self.layout_into_fragment(elem, styles)?.into_frame())
    }

    /// Layout arbitrary content.
    fn layout_into_self(
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

        let outer_styles = styles;
        let outer_font = styles.get_ref(TextElem::font);
        for (elem, styles) in pairs {
            // Whilst this check isn't exact, it more or less suffices as a
            // change in font variant probably won't have an effect on metrics.
            if styles != outer_styles && styles.get_ref(TextElem::font) != outer_font {
                self.fonts_stack
                    .push(get_font(self.engine.world, styles, elem.span())?);
                let scale_style = style_for_script_scale(self.font());
                layout_realized(elem, self, styles.chain(&scale_style))?;
                self.fonts_stack.pop();
            } else {
                layout_realized(elem, self, styles)?;
            }
        }

        Ok(())
    }
}

/// Lays out a leaf element resulting from realization.
fn layout_realized(
    elem: &Content,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    if let Some(elem) = elem.to_packed::<TagElem>() {
        ctx.push(MathFragment::Tag(elem.tag.clone()));
    } else if elem.is::<SpaceElem>() {
        ctx.push(MathFragment::Space(ctx.font().math().space_width.resolve(styles)));
    } else if elem.is::<LinebreakElem>() {
        ctx.push(MathFragment::Linebreak);
    } else if let Some(elem) = elem.to_packed::<HElem>() {
        layout_h(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<TextElem>() {
        self::text::layout_text(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<SymbolElem>() {
        self::text::layout_symbol(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BoxElem>() {
        layout_box(elem, ctx, styles)?;
    } else if elem.is::<AlignPointElem>() {
        ctx.push(MathFragment::Align);
    } else if let Some(elem) = elem.to_packed::<ClassElem>() {
        layout_class(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AccentElem>() {
        self::accent::layout_accent(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<AttachElem>() {
        self::attach::layout_attach(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<PrimesElem>() {
        self::attach::layout_primes(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<ScriptsElem>() {
        self::attach::layout_scripts(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LimitsElem>() {
        self::attach::layout_limits(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<CancelElem>() {
        self::cancel::layout_cancel(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<FracElem>() {
        self::frac::layout_frac(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<BinomElem>() {
        self::frac::layout_binom(elem, ctx, styles)?;
    } else if let Some(elem) = elem.to_packed::<LrElem>() {
        self::lr::layout_lr(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<MidElem>() {
        self::lr::layout_mid(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<VecElem>() {
        self::mat::layout_vec(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<MatElem>() {
        self::mat::layout_mat(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<CasesElem>() {
        self::mat::layout_cases(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OpElem>() {
        layout_op(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<RootElem>() {
        self::root::layout_root(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<StretchElem>() {
        self::stretch::layout_stretch(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderlineElem>() {
        self::underover::layout_underline(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverlineElem>() {
        self::underover::layout_overline(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderbraceElem>() {
        self::underover::layout_underbrace(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverbraceElem>() {
        self::underover::layout_overbrace(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderbracketElem>() {
        self::underover::layout_underbracket(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverbracketElem>() {
        self::underover::layout_overbracket(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UnderparenElem>() {
        self::underover::layout_underparen(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OverparenElem>() {
        self::underover::layout_overparen(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<UndershellElem>() {
        self::underover::layout_undershell(elem, ctx, styles)?
    } else if let Some(elem) = elem.to_packed::<OvershellElem>() {
        self::underover::layout_overshell(elem, ctx, styles)?
    } else {
        let mut frame = layout_external(elem, ctx, styles)?;
        if !frame.has_baseline() {
            let axis = ctx.font().math().axis_height.resolve(styles);
            frame.set_baseline(frame.height() / 2.0 + axis);
        }
        ctx.push(
            FrameFragment::new(styles, frame)
                .with_spaced(true)
                .with_ignorant(elem.is::<PlaceElem>()),
        );
    }

    Ok(())
}

/// Lays out an [`BoxElem`].
fn layout_box(
    elem: &Packed<BoxElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let frame = crate::inline::layout_box(
        elem,
        ctx.engine,
        ctx.locator.next(&elem.span()),
        styles,
        ctx.region.size,
    )?;
    ctx.push(FrameFragment::new(styles, frame).with_spaced(true));
    Ok(())
}

/// Lays out an [`HElem`].
fn layout_h(
    elem: &Packed<HElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    if let Spacing::Rel(rel) = elem.amount
        && rel.rel.is_zero()
    {
        ctx.push(MathFragment::Spacing(rel.abs.resolve(styles), elem.weak.get(styles)));
    }
    Ok(())
}

/// Lays out a [`ClassElem`].
#[typst_macros::time(name = "math.class", span = elem.span())]
fn layout_class(
    elem: &Packed<ClassElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let style = EquationElem::class.set(Some(elem.class)).wrap();
    let mut fragment = ctx.layout_into_fragment(&elem.body, styles.chain(&style))?;
    fragment.set_class(elem.class);
    fragment.set_limits(Limits::for_class(elem.class));
    ctx.push(fragment);
    Ok(())
}

/// Lays out an [`OpElem`].
#[typst_macros::time(name = "math.op", span = elem.span())]
fn layout_op(
    elem: &Packed<OpElem>,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<()> {
    let fragment = ctx.layout_into_fragment(&elem.text, styles)?;
    let italics = fragment.italics_correction();
    let accent_attach = fragment.accent_attach();
    let text_like = fragment.is_text_like();

    ctx.push(
        FrameFragment::new(styles, fragment.into_frame())
            .with_class(MathClass::Large)
            .with_italics_correction(italics)
            .with_accent_attach(accent_attach)
            .with_text_like(text_like)
            .with_limits(if elem.limits.get(styles) {
                Limits::Display
            } else {
                Limits::Never
            }),
    );
    Ok(())
}

/// Layout into a frame with normal layout.
fn layout_external(
    content: &Content,
    ctx: &mut MathContext,
    styles: StyleChain,
) -> SourceResult<Frame> {
    crate::layout_frame(
        ctx.engine,
        content,
        ctx.locator.next(&content.span()),
        styles,
        ctx.region,
    )
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
            hint: "rendering may be poor"
        ))
    }
}
