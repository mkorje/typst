#[macro_use]
mod shared;
mod accent;
mod attach;
mod cancel;
mod ctx;
mod frac;
mod fragment;
mod lr;
mod mat;
mod root;
mod run;
mod stretch;
mod text;
mod underover;

use typst_library::diag::{bail, SourceResult};
use typst_library::engine::Engine;
use typst_library::foundations::{NativeElement, Packed, Resolve, StyleChain};
use typst_library::introspection::{Counter, Location, Locator, Tag};
use typst_library::layout::{
    Abs, AlignElem, Axes, BlockElem, Em, FixedAlignment, Fragment, Frame, FrameItem,
    InlineItem, OuterHAlignment, Point, Region, Regions, Size, SpecificAlignment,
    VAlignment,
};
use typst_library::math::*;
use typst_library::model::{Numbering, ParElem};
use typst_library::text::{families, variant, Font, TextEdgeBounds, TextElem};
use typst_library::World;
use typst_syntax::Span;
use typst_utils::Numeric;

use self::ctx::MathContext;
use self::fragment::{
    FrameFragment, GlyphFragment, GlyphwiseSubsts, Limits, MathFragment, VariantFragment,
};
use self::run::{LeftRightAlternator, MathRun, MathRunEquation, MathRunFrameBuilder};
use self::shared::*;
use self::stretch::{stretch_fragment, stretch_glyph};

/// Minimum gap between the equation number and the content of the equation.
const NUMBER_GUTTER: Em = Em::new(0.5);

/// Layout an inline equation (in a paragraph).
#[typst_macros::time(span = elem.span())]
pub fn layout_equation_inline(
    elem: &Packed<EquationElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    region: Size,
) -> SourceResult<Vec<InlineItem>> {
    assert!(!elem.block(styles));

    let font = find_math_font(engine, styles, elem.span())?;

    let mut locator = locator.split();
    let mut ctx = MathContext::new(engine, &mut locator, styles, region, &font);
    let run = ctx.layout_into_run(&elem.body, styles)?;

    let mut items = if run.row_count() == 1 {
        run.into_par_items()
    } else {
        vec![InlineItem::Frame(run.into_fragment(&ctx, styles).into_frame())]
    };

    // An empty equation should have a height, so we still create a frame
    // (which is then resized in the loop).
    if items.is_empty() {
        items.push(InlineItem::Frame(Frame::soft(Size::zero())));
    }

    for item in &mut items {
        let InlineItem::Frame(frame) = item else { continue };

        let font_size = scaled_font_size(&ctx, styles);
        let slack = ParElem::leading_in(styles) * 0.7;

        let (t, b) = font.edges(
            TextElem::top_edge_in(styles),
            TextElem::bottom_edge_in(styles),
            font_size,
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
    assert!(elem.block(styles));

    let span = elem.span();
    let font = find_math_font(engine, styles, span)?;

    let mut locator = locator.split();
    let mut ctx = MathContext::new(engine, &mut locator, styles, regions.base(), &font);

    // Early exit if we are not numbering equations.
    let Some(numbering) = elem.numbering(styles) else {
        let full_equation_builder =
            ctx.layout_into_multiline_frame_builder(&elem.body, styles)?;

        let breakable = BlockElem::breakable_in(styles);
        let equation_builders = full_equation_builder.region_split(breakable, regions);

        let frames = equation_builders
            .into_iter()
            .map(MathRunFrameBuilder::build)
            .collect();
        return Ok(Fragment::frames(frames));
    };

    // Number equations.
    match (**elem).numbering_mode(styles) {
        NumberingMode::Equation => {
            number_by_equation(&mut ctx, styles, regions, elem, numbering, span)
        }
        NumberingMode::Line => {
            number_by_line(&mut ctx, styles, regions, elem, numbering, span)
        }
        NumberingMode::Label => todo!(),
        NumberingMode::Reference => todo!(),
    }
}

fn find_math_font(
    engine: &mut Engine<'_>,
    styles: StyleChain,
    span: Span,
) -> SourceResult<Font> {
    let variant = variant(styles);
    let world = engine.world;
    let Some(font) = families(styles).find_map(|family| {
        let id = world.book().select(family, variant)?;
        let font = world.font(id)?;
        let _ = font.ttf().tables().math?.constants?;
        Some(font)
    }) else {
        bail!(span, "current font does not support math");
    };
    Ok(font)
}

fn number_by_line(
    ctx: &mut MathContext,
    styles: StyleChain,
    regions: Regions,
    elem: &Packed<EquationElem>,
    numbering: &Numbering,
    span: Span,
) -> SourceResult<Fragment> {
    let full_equation_builder =
        ctx.layout_into_multiline_frame_builder(&elem.body, styles)?;

    if full_equation_builder.frames.len() == 1 {
        return number_by_equation(ctx, styles, regions, elem, numbering, span);
    }

    let breakable = BlockElem::breakable_in(styles);
    let equation_builders = full_equation_builder.region_split(breakable, regions);

    let number_align = match elem.number_align(styles) {
        SpecificAlignment::H(h) => SpecificAlignment::Both(h, VAlignment::Horizon),
        SpecificAlignment::V(v) => SpecificAlignment::Both(OuterHAlignment::End, v),
        SpecificAlignment::Both(h, v) => SpecificAlignment::Both(h, v),
    }
    .resolve(styles)
    .x;

    let equation_align = AlignElem::alignment_in(styles).resolve(styles).x;

    // Add equation numbers to each equation region.
    let mut frames = vec![];
    let region_count = equation_builders.len();
    for builder in equation_builders {
        if builder.frames.is_empty() && region_count > 1 {
            // Don't number empty regions, but do number empty equations.
            frames.push(builder.build());
            continue;
        }

        let frame = add_line_equation_numbers(
            ctx,
            styles,
            regions,
            builder,
            numbering,
            number_align,
            equation_align,
        )?;
        frames.push(frame.mark_box());
    }

    Ok(Fragment::frames(frames))
}

fn get_equation_number(
    ctx: &mut MathContext,
    styles: StyleChain,
    regions: Regions,
    numbering: &Numbering,
    location: Location,
    span: Span,
) -> SourceResult<(Frame, Abs)> {
    let pod = Region::new(regions.base(), Axes::splat(false));
    let counter = Counter::of(EquationElem::elem())
        .display_at_loc(ctx.engine, location, styles, numbering)?
        .spanned(span);
    let number = (ctx.engine.routines.layout_frame)(
        ctx.engine,
        &counter,
        ctx.locator.next(&()),
        styles,
        pod,
    )?;

    let full_number_width = number.width() + NUMBER_GUTTER.resolve(styles);

    Ok((number, full_number_width))
}

fn add_line_equation_numbers(
    ctx: &mut MathContext,
    styles: StyleChain,
    regions: Regions,
    builder: MathRunFrameBuilder,
    numbering: &Numbering,
    number_align: FixedAlignment,
    equation_align: FixedAlignment,
) -> SourceResult<Frame> {
    // Need to make sure following assumptions are fine:
    // - number of frames is not zero
    // - each frame is a line equation, and so has tag at the start and end.

    // Get layouted number frame for each line.
    // We also calculate the maximum width, and excess height above and below.
    let mut max_width = Abs::zero();
    let mut max_full_number_width = Abs::zero();
    let mut excess_above = Abs::zero();
    let mut excess_below = Abs::zero();
    let mut numbers: Vec<Option<(Frame, Abs)>> = Vec::new();
    let sub_count = builder.frames.len();
    for (i, (sub, pos)) in builder.frames.iter().enumerate() {
        let (_, FrameItem::Tag(Tag::Start(content))) = sub.items().next().unwrap() else {
            unreachable!()
        };

        if let Some(label) = content.label() {
            // Skip line if it has a do not label marker `<*>` attached to it.
            if label.as_str() == "*" {
                numbers.push(None);
                max_width = max_width.max(sub.width());
                continue;
            }
        }

        let (number, full_number_width) = get_equation_number(
            ctx,
            styles,
            regions,
            numbering,
            content.location().unwrap(),
            content.span(),
        )?;
        let y = pos.y + sub.baseline() - number.baseline();

        if i == 0 {
            excess_above = excess_above.max(number.baseline() - sub.baseline());
        } else if i == sub_count {
            excess_below = excess_below.max(
                (number.height() - number.baseline()) - (sub.height() - sub.baseline()),
            );
        }

        numbers.push(Some((number, y)));

        max_width = max_width.max(sub.width() + 2.0 * full_number_width);
        max_full_number_width = max_full_number_width.max(full_number_width);
    }

    // Calculate new width of the builder.
    let region_size_x = regions.size.x;
    let width = if region_size_x.is_finite() { region_size_x } else { max_width };

    // Build the final frame.
    let mut final_frame = builder.build();

    // Resize it.
    // The vertical expansion is asymmetric on the top and bottom edges, so we
    // first align at the top then translate the content downward later.
    final_frame.resize(
        Size::new(width, final_frame.height() + excess_above + excess_below),
        Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Start),
    );
    final_frame.translate(Point::with_y(excess_above));

    // Add room for numbers if both it and the equation are aligned to the start or end.
    final_frame.translate(Point::with_x(match (equation_align, number_align) {
        (FixedAlignment::Start, FixedAlignment::Start) => max_full_number_width,
        (FixedAlignment::End, FixedAlignment::End) => -max_full_number_width,
        _ => Abs::zero(),
    }));

    // Add the numbers to each line.
    for number in numbers {
        match number {
            Some((frame, y)) => {
                let x = match number_align {
                    FixedAlignment::Start => Abs::zero(),
                    FixedAlignment::End => width - frame.width(),
                    _ => unreachable!(),
                };
                final_frame.push_frame(Point::new(x, y + excess_above), frame.mark_box());
            }
            None => continue,
        }
    }

    Ok(final_frame)
}

/// Resize an equation line's frame accordingly so that it encompasses the number.
fn resize_line(
    line: &mut Frame,
    line_size: Axes<Abs>,
    line_baseline: Abs,
    number: &Frame,
    equation_align: FixedAlignment,
    width: Abs,
) -> Point {
    let excess_above = Abs::zero().max(number.baseline() - line_baseline);
    let excess_below = Abs::zero()
        .max((number.height() - number.baseline()) - (line_size.y - line_baseline));

    // The vertical expansion is asymmetric on the top and bottom edges, so we
    // first align at the top then translate the content downward later.
    let resizing_offset = line.resize(
        Size::new(width, line.height() + excess_above + excess_below),
        Axes::<FixedAlignment>::new(equation_align, FixedAlignment::Start),
    );
    line.translate(Point::with_y(excess_above));
    resizing_offset + Point::with_y(excess_above)
}

fn number_by_equation(
    ctx: &mut MathContext,
    styles: StyleChain,
    regions: Regions,
    elem: &Packed<EquationElem>,
    numbering: &Numbering,
    span: Span,
) -> SourceResult<Fragment> {
    let full_equation_builder =
        ctx.layout_into_multiline_frame_builder(&elem.body, styles)?;

    let breakable = BlockElem::breakable_in(styles);
    let equation_builders = full_equation_builder.region_split(breakable, regions);

    let location = if let Some(label) = elem.label() {
        // Early exit if this element has a do not label marker `<*>` attached
        // to it.
        if label.as_str() == "*" {
            let frames = equation_builders
                .into_iter()
                .map(MathRunFrameBuilder::build)
                .collect();
            return Ok(Fragment::frames(frames));
        }

        // Use the number at the location of the first EquationElem with the
        // label. This also skips stepping the EquationElem counter.
        get_equations(ctx.engine, label)
            .first()
            .map(|x| x.location().unwrap())
            .unwrap_or(elem.location().unwrap())
    } else {
        elem.location().unwrap()
    };

    let pod = Region::new(regions.base(), Axes::splat(false));
    let counter = Counter::of(EquationElem::elem())
        .display_at_loc(ctx.engine, location, styles, numbering)?
        .spanned(span);
    let number = (ctx.engine.routines.layout_frame)(
        ctx.engine,
        &counter,
        ctx.locator.next(&()),
        styles,
        pod,
    )?;

    let full_number_width = number.width() + NUMBER_GUTTER.resolve(styles);

    let number_align = match elem.number_align(styles) {
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
                AlignElem::alignment_in(styles).resolve(styles).x,
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
