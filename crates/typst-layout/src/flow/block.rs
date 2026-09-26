use std::cell::LazyCell;

use smallvec::SmallVec;
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Packed, Resolve, StyleChain};
use typst_library::introspection::Locator;
use typst_library::layout::{
    Abs, Axes, BlockBody, BlockElem, Followup, Frame, FrameKind, MultiState, MultiStep,
    Region, Regions, Rel, Sides, Size, Sizing,
};
use typst_library::visualize::Stroke;
use typst_utils::Numeric;

use super::RegionHistory;
use crate::modifiers::{FrameModifiers, FrameModify};
use crate::shapes::{clip_rect, fill_and_stroke};

/// Lay this out as an unbreakable block.
#[typst_macros::time(name = "block", span = elem.span())]
pub fn layout_single_block(
    elem: &Packed<BlockElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    region: Region,
) -> SourceResult<Frame> {
    // Fetch sizing properties.
    let width = elem.width.get(styles);
    let height = elem.height.get(styles);
    let inset = elem.inset.resolve(styles).unwrap_or_default();

    // Build the pod regions.
    let pod = unbreakable_pod(&width.into(), &height, &inset, styles, region.size);

    // Layout the body.
    let body = elem.body.get_ref(styles);
    let mut frame = match body {
        // If we have no body, just create one frame. Its size will be
        // adjusted below.
        None => Frame::hard(Size::zero()),

        // If we have content as our body, just layout it.
        Some(BlockBody::Content(body)) => {
            crate::layout_frame(engine, body, locator.relayout(), styles, pod)?
        }

        // If we have a child that wants to layout with just access to the
        // base region, give it that.
        Some(BlockBody::SingleLayouter(callback)) => {
            callback.call(engine, locator, styles, pod)?
        }

        // If we have a child that wants to layout with full region access,
        // we layout it into the single region.
        //
        // For auto-sized multi-layouters, we propagate the outer expansion
        // so that they can decide for themselves. We also ensure again to
        // only expand if the size is finite.
        Some(BlockBody::MultiLayouter(callback)) => {
            let expand = (pod.expand | region.expand) & pod.size.map(Abs::is_finite);
            let pod = Region { expand, ..pod };
            crate::flow::layout_steps(pod.into(), |regions, state| {
                callback.call(engine, locator.relayout(), styles, regions, state)
            })?
            .into_frame()
        }
    };

    // Explicit blocks are boundaries for gradient relativeness.
    if matches!(body, None | Some(BlockBody::Content(_))) {
        frame.set_kind(FrameKind::Hard);
    }

    // Enforce a correct frame size on the expanded axes. Do this before
    // applying the inset, since the pod shrunk.
    frame.set_size(pod.expand.select(pod.size, frame.size()));

    // Apply the inset.
    if !inset.is_zero() {
        crate::pad::grow(&mut frame, &inset);
    }

    // Prepare fill and stroke.
    let fill = elem.fill.get_cloned(styles);
    let stroke = elem
        .stroke
        .resolve(styles)
        .unwrap_or_default()
        .map(|s| s.map(Stroke::unwrap_or_default));

    // Only fetch these if necessary (for clipping or filling/stroking).
    let outset = LazyCell::new(|| elem.outset.resolve(styles).unwrap_or_default());
    let radius = LazyCell::new(|| elem.radius.resolve(styles).unwrap_or_default());

    // Clip the contents, if requested.
    if elem.clip.get(styles) {
        frame.clip(clip_rect(frame.size(), &radius, &stroke, &outset));
    }

    // Add fill and/or stroke.
    if fill.is_some() || stroke.iter().any(Option::is_some) {
        fill_and_stroke(&mut frame, fill, &stroke, &outset, &radius, elem.span());
    }

    // Assign label to each frame in the fragment.
    if let Some(label) = elem.label() {
        frame.label(label);
    }

    Ok(frame)
}

/// Builds the pod region for an unbreakable sized container.
pub(crate) fn unbreakable_pod(
    width: &Sizing,
    height: &Sizing,
    inset: &Sides<Rel<Abs>>,
    styles: StyleChain,
    base: Size,
) -> Region {
    // Resolve the size.
    let mut size = Size::new(
        match width {
            // - For auto, the whole region is available.
            // - Fr is handled outside and already factored into the `region`,
            //   so we can treat it equivalently to 100%.
            Sizing::Auto | Sizing::Fr(_) => base.x,
            // Resolve the relative sizing.
            Sizing::Rel(rel) => rel.resolve(styles).relative_to(base.x),
        },
        match height {
            Sizing::Auto | Sizing::Fr(_) => base.y,
            Sizing::Rel(rel) => rel.resolve(styles).relative_to(base.y),
        },
    );

    // Take the inset, if any, into account.
    if !inset.is_zero() {
        size = crate::pad::shrink(size, inset);
    }

    // If the child is manually, the size is forced and we should enable
    // expansion.
    let expand = Axes::new(
        *width != Sizing::Auto && size.x.is_finite(),
        *height != Sizing::Auto && size.y.is_finite(),
    );

    Region::new(size, expand)
}

/// Builds the pod regions for a breakable sized container.
fn breakable_pod<'a>(
    width: &Sizing,
    height: &Sizing,
    inset: &Sides<Rel<Abs>>,
    styles: StyleChain,
    regions: Regions<'a>,
    buf: &'a mut SmallVec<[Abs; 2]>,
    shrunk: &'a mut Followup,
) -> Regions<'a> {
    let base = regions.base();

    // Resolve the horizontal sizing to a concrete width.
    let width_abs = match width {
        Sizing::Auto | Sizing::Fr(_) => regions.width(),
        Sizing::Rel(rel) => rel.resolve(styles).relative_to(base.x),
    };

    // If the block has a fixed height, things are very different, so we
    // handle that case completely separately.
    let pod = match height {
        // If the block is automatically sized, we can just inherit the
        // regions.
        Sizing::Auto | Sizing::Fr(_) => regions.with_width(width_abs),

        Sizing::Rel(rel) => {
            // Resolve the sizing to a concrete size.
            let resolved = rel.resolve(styles).relative_to(base.y);

            // Distribute the fixed height across a start region and a backlog.
            let (first, backlog) = distribute(resolved, regions, buf);

            // Since we're manually sized, the resolved size is the base height.
            // We also don't want a final repeatable region.
            let size = Size::new(width_abs, first);
            Regions::new(size, resolved, backlog, None, regions.expand)
        }
    };

    // Take the inset, if any, into account.
    let pod = if inset.is_zero() { pod } else { pod.shrink(inset, shrunk) };

    // If the child is manually, the size is forced and we should enable
    // expansion.
    let expand = Axes::new(
        *width != Sizing::Auto && pod.width().is_finite(),
        *height != Sizing::Auto && pod.is_finite(),
    );

    pod.with_expand(expand)
}

/// Distribute a fixed height spread over existing regions into a new first
/// height and a new backlog.
///
/// Note that, if the given height fits within the first region, no backlog is
/// generated and the first region's height shrinks to fit exactly the given
/// height. In particular, negative and zero heights always fit in any region,
/// so such heights are always directly returned as the new first region
/// height.
fn distribute<'a>(
    height: Abs,
    mut regions: Regions,
    buf: &'a mut SmallVec<[Abs; 2]>,
) -> (Abs, &'a mut [Abs]) {
    // Build new region heights from old regions.
    let mut remaining = height;

    // Negative and zero heights always fit, so just keep them.
    // No backlog is generated.
    if remaining <= Abs::zero() {
        buf.push(remaining);
        return (buf[0], &mut buf[1..]);
    }

    loop {
        // This clamp is safe (min <= max), as 'remaining' won't be negative
        // due to the initial check above (on the first iteration) and due to
        // stopping on 'remaining.approx_empty()' below (for the second
        // iteration onwards).
        let limited = regions.height().clamp(Abs::zero(), remaining);
        buf.push(limited);
        remaining -= limited;
        if remaining.approx_empty()
            || !regions.may_break()
            || (!regions.may_progress() && limited.approx_empty())
        {
            break;
        }
        regions.next();
    }

    // If there is still something remaining, apply it to the
    // last region (it will overflow, but there's nothing else
    // we can do).
    if !remaining.approx_empty()
        && let Some(last) = buf.last_mut()
    {
        *last += remaining;
    }

    // Distribute the heights to the first region and the
    // backlog. There is no last region, since the height is
    // fixed.
    (buf[0], &mut buf[1..])
}

/// Where a breakable block continues.
#[derive(Clone, Hash)]
pub(super) struct BlockState {
    /// Where the body continues.
    body: MultiState,
    /// The regions the block was already laid out in, if it has a fixed
    /// height, which is distributed over all of its regions.
    history: Option<RegionHistory>,
    /// The width the body is laid out with.
    width: BodyWidth,
}

/// The width the body of a breakable block is laid out with.
#[derive(Copy, Clone, Hash)]
enum BodyWidth {
    /// The body's natural width.
    Natural,
    /// The body's natural width, which the frames so far had consistently.
    /// The following frames should have this width, too.
    Consistent(Abs),
    /// A fixed width, since the body was laid out again in the first region
    /// because of inconsistent frame widths.
    Forced(Abs),
}

impl BodyWidth {
    /// The fixed width, if any.
    fn forced(self) -> Option<Abs> {
        match self {
            Self::Forced(width) => Some(width),
            _ => None,
        }
    }
}

/// The result of laying out one region of a breakable block.
#[derive(Clone)]
pub(super) struct BlockStep {
    /// The frame for the region.
    pub frame: Frame,
    /// Where the block continues, if it does.
    pub next: Option<BlockState>,
    /// Whether any frame of the block is non-empty. Only computed for the
    /// first region.
    pub exist_non_empty_frame: bool,
    /// Whether nothing of the block's body was placed into the frame, so that
    /// it holds at most the block's own decoration, like fill and stroke.
    pub decoration_only: bool,
    /// For each region after this one, how much height the state has already
    /// laid out into it, including the block's insets (see
    /// [`MultiStep::ahead`]).
    pub ahead: Vec<Abs>,
}

/// Lay this out as a breakable block, one region at a time.
///
/// The `regions` contain the current region followed by predictions of the
/// upcoming ones. Earlier regions are taken from the `state`, which is `None`
/// for the first region. Also applies the frame `modifiers`.
#[typst_macros::time(name = "block", span = elem.span())]
pub(super) fn layout_multi_block(
    elem: &Packed<BlockElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    modifiers: &FrameModifiers,
    regions: Regions,
    state: Option<&BlockState>,
) -> SourceResult<BlockStep> {
    let body = elem.body.get_ref(styles).as_ref();

    // Fetch sizing properties.
    let width = elem.width.get(styles);
    let height = elem.height.get(styles);
    let inset = elem.inset.resolve(styles).unwrap_or_default();

    // Determine the regions the block is laid out into. A block with a fixed
    // height distributes it over all of its regions, so they are
    // reconstructed from the regions it was already laid out in, followed by
    // the current one and the predictions. Otherwise, the current regions
    // suffice. As for all regions after the first one, the full height of
    // the current region is then its remaining height.
    let mut followup = Followup::default();
    let (outer, skip) = match state {
        None => (regions, 0),
        Some(BlockState { history: Some(history), .. }) => {
            (history.regions(regions, &mut followup), history.len())
        }
        Some(_) => (regions.with_full(regions.height()), 0),
    };

    // Build the pod regions for the whole block.
    let mut buf = SmallVec::<[Abs; 2]>::new();
    let mut shrunk = Followup::default();
    let pod = breakable_pod(
        &width.into(),
        &height,
        &inset,
        styles,
        outer,
        &mut buf,
        &mut shrunk,
    );

    let Some(state) = state else {
        let fixed = matches!(height, Sizing::Rel(_));
        return layout_multi_block_first(
            elem, body, engine, locator, styles, modifiers, pod, inset, regions, fixed,
        );
    };

    // Advance to the current region and lay out the body into it.
    let mut region = body_pod(body, pod, outer.expand);
    for _ in 0..skip {
        region.next();
    }
    let lay_out = |engine: &mut Engine, width| {
        step_body(
            body,
            engine,
            &locator,
            styles,
            body_regions(region, width),
            Some(&state.body),
        )
    };

    // The first region checks whether the widths of the body's frames are
    // consistent, but only knows predictions of the upcoming regions. If they
    // were wrong, this frame's width may differ. If it is narrower, it is
    // laid out again with the width of the earlier frames, and the side
    // effects of the first layout don't count. If it is wider, it keeps its
    // width, since its content wouldn't fit otherwise, and the later frames
    // match it instead.
    let mut width = state.width;
    let step = match width {
        BodyWidth::Consistent(target) => {
            let (step, sink) = engine.isolate(|engine| lay_out(engine, None));
            let step = step?;
            let natural = step.frame.width();
            if natural.approx_eq(target) || natural > target {
                width = BodyWidth::Consistent(natural);
                engine.commit(sink);
                step
            } else {
                lay_out(engine, Some(target))?
            }
        }
        _ => lay_out(engine, width.forced())?,
    };
    let MultiStep { mut frame, next, ahead } = step;
    let decoration_only = frame.is_empty();

    // Frames beyond the pod regions are not post-processed.
    if pod.has_region(skip) {
        finish_multi_frame(
            elem,
            styles,
            &mut frame,
            &region,
            pod.expand,
            &inset,
            is_explicit(body),
            true,
        );
    }
    label_multi_frame(elem, &mut frame, true);
    frame.modify(modifiers);

    let next = next.map(|body| BlockState {
        body,
        history: state.history.as_ref().map(|history| history.then(regions)),
        width,
    });

    Ok(BlockStep {
        frame,
        next,
        exist_non_empty_frame: true,
        decoration_only,
        ahead: crate::pad::grow_ahead(ahead, &inset, regions),
    })
}

/// Lays out the first region of a breakable block.
#[expect(clippy::too_many_arguments)]
fn layout_multi_block_first(
    elem: &Packed<BlockElem>,
    body: Option<&BlockBody>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    modifiers: &FrameModifiers,
    pod: Regions,
    inset: Sides<Rel<Abs>>,
    regions: Regions,
    fixed: bool,
) -> SourceResult<BlockStep> {
    let first = body_pod(body, pod, regions.expand);

    // If the body is automatically sized and produces more than one frame,
    // ensure that the width is consistent across all regions. If it isn't, we
    // need to relayout with expansion. This requires knowing all frames, so we
    // lay the body out into the predicted regions.
    let check = matches!(body, Some(BlockBody::Content(_))) && !pod.expand.x;

    // If the first frame may be laid out again, the side effects of its first
    // layout only count if it is kept.
    let (step, mut sink) = if check {
        let (step, sink) = engine
            .isolate(|engine| step_body(body, engine, &locator, styles, first, None));
        (step, Some(sink))
    } else {
        (step_body(body, engine, &locator, styles, first, None), None)
    };
    let MultiStep { mut frame, mut next, mut ahead } = step?;

    // The raw frames of the body after the first one, if they are already
    // known.
    let mut rest: Option<Vec<Frame>> = None;

    let mut width = BodyWidth::Natural;
    if check {
        let frames =
            layout_rest(body, engine, &locator, styles, next.clone(), first, true)?;
        let widths: Vec<Abs> = std::iter::once(frame.width())
            .chain(frames.iter().map(Frame::width))
            .collect();
        if widths.windows(2).any(|w| !w[0].approx_eq(w[1])) {
            let max_width = widths.iter().copied().max().unwrap_or_default();
            width = BodyWidth::Forced(max_width);
            sink = None;
            MultiStep { frame, next, ahead } = step_body(
                body,
                engine,
                &locator,
                styles,
                body_regions(first, Some(max_width)),
                None,
            )?;
        } else {
            width = BodyWidth::Consistent(frame.width());
            rest = Some(frames);
        }
    }
    if let Some(sink) = sink {
        engine.commit(sink);
    }

    // Determine the remaining raw frames if needed below.
    let needs_rest = frame.is_empty();
    if needs_rest && rest.is_none() {
        rest = Some(layout_rest(
            body,
            engine,
            &locator,
            styles,
            next.clone(),
            body_regions(first, width.forced()),
            false,
        )?);
    }

    // Skip filling, stroking and labeling the first frame if it is empty and
    // a non-empty one follows.
    let skip_first =
        needs_rest && rest.as_ref().is_some_and(|r| r.iter().any(|f| !f.is_empty()));

    let explicit = is_explicit(body);
    finish_multi_frame(
        elem,
        styles,
        &mut frame,
        &pod,
        pod.expand,
        &inset,
        explicit,
        !skip_first,
    );
    label_multi_frame(elem, &mut frame, !skip_first);
    frame.modify(modifiers);

    // Whether any of the finished frames is non-empty.
    let mut exist_non_empty_frame = !frame.is_empty();
    if !exist_non_empty_frame && let Some(rest) = rest {
        let mut region = pod;
        for (i, mut raw) in rest.into_iter().enumerate() {
            let i = i + 1;
            region.next();
            if pod.has_region(i) {
                finish_multi_frame(
                    elem, styles, &mut raw, &region, pod.expand, &inset, explicit, true,
                );
            }
            label_multi_frame(elem, &mut raw, true);
            raw.modify(modifiers);
            if !raw.is_empty() {
                exist_non_empty_frame = true;
                break;
            }
        }
    }

    let next = next.map(|body| BlockState {
        body,
        history: fixed.then(|| RegionHistory::default().then(regions)),
        width,
    });

    Ok(BlockStep {
        frame,
        next,
        exist_non_empty_frame,
        decoration_only: needs_rest,
        ahead: crate::pad::grow_ahead(ahead, &inset, regions),
    })
}

/// Lays out one region of a block's body.
fn step_body(
    body: Option<&BlockBody>,
    engine: &mut Engine,
    locator: &Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    match body {
        // If we have no body, just create one frame per region the block
        // expands into. We create them zero-sized; if necessary, their size
        // will be adjusted during post-processing.
        None => {
            let next =
                (regions.expand.y && regions.has_backlog()).then(|| MultiState::new(()));
            Ok(MultiStep::new(Frame::hard(Size::zero()), next))
        }

        // If we have content as our body, just layout it.
        Some(BlockBody::Content(body)) => crate::flow::layout_fragment_step(
            engine,
            body,
            locator.relayout(),
            styles,
            regions,
            state,
        ),

        // If we have a child that wants to layout with just access to the
        // base region, give it that.
        Some(BlockBody::SingleLayouter(callback)) => {
            let pod = Region::new(regions.base(), regions.expand);
            let frame = callback.call(engine, locator.relayout(), styles, pod)?;
            Ok(MultiStep::new(frame, None))
        }

        // If we have a child that wants to layout with full region access,
        // we layout it.
        Some(BlockBody::MultiLayouter(callback)) => {
            callback.call(engine, locator.relayout(), styles, regions, state)
        }
    }
}

/// Lays out the rest of a body into the regions following the first one, only
/// to inspect the frames. Unless `all` is set, stops after the first non-empty
/// frame.
fn layout_rest(
    body: Option<&BlockBody>,
    engine: &mut Engine,
    locator: &Locator,
    styles: StyleChain,
    state: Option<MultiState>,
    regions: Regions,
    all: bool,
) -> SourceResult<Vec<Frame>> {
    crate::flow::peek_remaining(
        engine,
        regions,
        state,
        |engine, regions, state| {
            step_body(body, engine, locator, styles, regions, Some(state))
        },
        |frame| !all && !frame.is_empty(),
    )
}

/// Whether the block's frames are boundaries for gradient relativeness, which
/// is the case for explicit blocks.
fn is_explicit(body: Option<&BlockBody>) -> bool {
    matches!(body, None | Some(BlockBody::Content(_)))
}

/// The regions a block's body is laid out into, given the block's pod regions
/// and the regions the block itself is laid out into.
///
/// For auto-sized multi-region layouters, we propagate the outer expansion so
/// that they can decide for themselves. We also ensure again to only expand
/// if the size is finite.
fn body_pod<'a>(
    body: Option<&BlockBody>,
    pod: Regions<'a>,
    outer: Axes<bool>,
) -> Regions<'a> {
    match body {
        Some(BlockBody::MultiLayouter(_)) => {
            let expand = Axes::new(
                (pod.expand.x || outer.x) && pod.width().is_finite(),
                (pod.expand.y || outer.y) && pod.is_finite(),
            );
            pod.with_expand(expand)
        }
        _ => pod,
    }
}

/// The regions a body is laid out into, given the block's pod regions and the
/// width it is laid out with to be consistent with its other frames, if any.
///
/// A body that is prepared without such a width may continue in the resulting
/// regions (see [`layout_flow_step`](super::layout_flow_step)).
fn body_regions(pod: Regions, relayout: Option<Abs>) -> Regions {
    match relayout {
        Some(width) => pod.with_width(width).with_expand(Axes::new(true, pod.expand.y)),
        None => pod,
    }
}

/// Post-processes a frame of a breakable block to apply insets, clipping, and,
/// if it is `decorated`, fills and strokes.
#[expect(clippy::too_many_arguments)]
fn finish_multi_frame(
    elem: &Packed<BlockElem>,
    styles: StyleChain,
    frame: &mut Frame,
    region: &Regions,
    expand: Axes<bool>,
    inset: &Sides<Rel<Abs>>,
    explicit: bool,
    decorated: bool,
) {
    let fill = elem.fill.get_ref(styles);
    let stroke = elem
        .stroke
        .resolve(styles)
        .unwrap_or_default()
        .map(|s| s.map(Stroke::unwrap_or_default));
    let outset = LazyCell::new(|| elem.outset.resolve(styles).unwrap_or_default());
    let radius = LazyCell::new(|| elem.radius.resolve(styles).unwrap_or_default());

    // Explicit blocks are boundaries for gradient relativeness.
    if explicit {
        frame.set_kind(FrameKind::Hard);
    }

    // Enforce a correct frame size on the expanded axes. Do this before
    // applying the inset, since the pod shrunk.
    let mut size = frame.size();
    if expand.x {
        size.x = region.width();
    }
    if expand.y {
        size.y = region.height();
    }
    frame.set_size(size);

    // Apply the inset.
    if !inset.is_zero() {
        crate::pad::grow(frame, inset);
    }

    // Clip the contents, if requested.
    if elem.clip.get(styles) {
        frame.clip(clip_rect(frame.size(), &radius, &stroke, &outset));
    }

    // Add fill and/or stroke.
    if decorated && (fill.is_some() || stroke.iter().any(Option::is_some)) {
        fill_and_stroke(frame, fill.clone(), &stroke, &outset, &radius, elem.span());
    }
}

/// Labels a frame of a breakable block if it is `decorated`.
fn label_multi_frame(elem: &Packed<BlockElem>, frame: &mut Frame, decorated: bool) {
    // Skip empty orphan frames, as a label would make them non-empty.
    if let Some(label) = elem.label()
        && decorated
    {
        frame.label(label);
    }
}
