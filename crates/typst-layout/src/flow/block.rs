use std::cell::LazyCell;

use smallvec::SmallVec;
use typst_library::diag::SourceResult;
use typst_library::engine::Engine;
use typst_library::foundations::{Packed, Resolve, StyleChain};
use typst_library::introspection::Locator;
use typst_library::layout::{
    Abs, Axes, BlockBody, BlockElem, Frame, FrameKind, MultiState, MultiStep, Region,
    Regions, Rel, Sides, Size, Sizing,
};
use typst_library::visualize::Stroke;
use typst_utils::Numeric;

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
            callback
                .call(engine, locator.relayout(), styles, pod.into(), None)?
                .frame
        }
    };

    finish_frame(elem, styles, &mut frame, pod.size, pod.expand, &inset, true);
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
    regions: Regions,
    buf: &'a mut SmallVec<[Abs; 2]>,
) -> Regions<'a> {
    let base = regions.base();

    // The vertical region sizes we're about to build.
    let first;
    let full;
    let backlog: &mut [Abs];
    let last;
    let predicted;

    // If the block has a fixed height, things are very different, so we
    // handle that case completely separately.
    match height {
        Sizing::Auto | Sizing::Fr(_) => {
            // If the block is automatically sized, we can just inherit the
            // regions.
            first = regions.size.y;
            full = regions.full;
            buf.extend_from_slice(regions.backlog);
            backlog = buf;
            last = regions.last;
            predicted = regions.predicted;
        }

        Sizing::Rel(rel) => {
            // Resolve the sizing to a concrete size.
            let resolved = rel.resolve(styles).relative_to(base.y);

            // Since we're manually sized, the resolved size is the base height.
            full = resolved;

            // Distribute the fixed height across a start region and a backlog.
            (first, backlog) = distribute(resolved, regions, buf);

            // If the height is manually sized, we don't want a final repeatable
            // region.
            last = None;
            predicted = 0;
        }
    }

    // Resolve the horizontal sizing to a concrete width and combine
    // `width` and `first` into `size`.
    let mut size = Size::new(
        match width {
            Sizing::Auto | Sizing::Fr(_) => regions.size.x,
            Sizing::Rel(rel) => rel.resolve(styles).relative_to(base.x),
        },
        first,
    );

    // Take the inset, if any, into account, applying it to the
    // individual region components.
    let (mut full, mut last) = (full, last);
    if !inset.is_zero() {
        crate::pad::shrink_multiple(&mut size, &mut full, backlog, &mut last, inset);
    }

    // If the child is manually, the size is forced and we should enable
    // expansion.
    let expand = Axes::new(
        *width != Sizing::Auto && size.x.is_finite(),
        *height != Sizing::Auto && size.y.is_finite(),
    );

    Regions { size, full, backlog, last, predicted, expand }
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
        let limited = regions.size.y.clamp(Abs::zero(), remaining);
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

/// The regions that a multi-region layout already produced frames for.
///
/// Allows reconstructing the regions the whole layout would have been laid out
/// into, for layouts that can't proceed region by region.
#[derive(Debug, Clone, Default, Hash)]
struct RegionHistory {
    /// The full height of the first region.
    full: Abs,
    /// The heights of the regions, in order.
    heights: Vec<Abs>,
}

impl RegionHistory {
    /// The number of regions.
    fn len(&self) -> usize {
        self.heights.len()
    }

    /// The history with the first of the `regions` added to it.
    fn then(&self, regions: Regions) -> Self {
        let mut heights = self.heights.clone();
        heights.push(regions.size.y);
        let full = if self.heights.is_empty() { regions.full } else { self.full };
        Self { full, heights }
    }

    /// The regions starting with the first region in the history, followed by
    /// the given `regions`.
    fn regions<'a>(&self, regions: Regions<'a>, buf: &'a mut Vec<Abs>) -> Regions<'a> {
        let Some((&first, rest)) = self.heights.split_first() else {
            return regions;
        };
        buf.extend(rest.iter().chain([&regions.size.y]).chain(regions.backlog));
        Regions {
            size: Size::new(regions.size.x, first),
            full: self.full,
            backlog: buf,
            ..regions
        }
    }
}

/// Where a breakable block continues.
#[derive(Clone, Hash)]
pub(super) struct BlockState {
    /// Where the body continues.
    body: MultiState,
    /// The regions the block was already laid out in, if it has a fixed
    /// height, which is distributed over all of its regions.
    history: Option<RegionHistory>,
    /// The width the body's frames are laid out with, if they must have a
    /// consistent width.
    width: Option<Abs>,
}

/// The result of laying out one region of a breakable block.
#[derive(Clone)]
pub(super) struct BlockStep {
    /// The frame for the region.
    pub frame: Frame,
    /// Where the block continues, if it does.
    pub next: Option<BlockState>,
    /// Whether the frame is an orphan: the first frame, holding none of the
    /// body, which continues with content in a later region. It is left
    /// undecorated, and the flow may move the whole block to the next region
    /// instead.
    pub orphan: bool,
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
    // Fetch sizing properties.
    let width = elem.width.get(styles);
    let height = elem.height.get(styles);
    let inset = elem.inset.resolve(styles).unwrap_or_default();
    let body = elem.body.get_ref(styles).as_ref();

    // Determine the regions the block is laid out into. A block with a fixed
    // height distributes it over all of its regions, so they are
    // reconstructed from the regions it was already laid out in, followed by
    // the current one and the predictions. Otherwise, the current regions
    // suffice. As for all regions after the first one, the full height of
    // the current region is then its remaining height.
    let mut backlog = vec![];
    let (outer, index) = match state {
        None => (regions, 0),
        Some(BlockState { history: Some(history), .. }) => {
            (history.regions(regions, &mut backlog), history.len())
        }
        Some(_) => (Regions { full: regions.size.y, ..regions }, 0),
    };

    // Build the pod regions for the whole block.
    let mut buf = SmallVec::<[Abs; 2]>::new();
    let pod = breakable_pod(&width.into(), &height, &inset, styles, outer, &mut buf);

    // Advance to the current region. For auto-sized multi-region layouters,
    // we propagate the outer expansion so that they can decide for
    // themselves. We also ensure again to only expand if the size is finite.
    let mut region = match body {
        Some(BlockBody::MultiLayouter(_)) => {
            let expand = (pod.expand | outer.expand) & pod.size.map(Abs::is_finite);
            Regions { expand, ..pod }
        }
        _ => pod,
    };
    for _ in 0..index {
        region.next();
    }

    let step = |engine: &mut Engine, regions: Regions, state: Option<&MultiState>| {
        step_body(body, engine, &locator, styles, regions, state)
    };
    let peek = |engine: &mut Engine, regions, state, all: bool| {
        crate::flow::peek_remaining(
            engine,
            regions,
            state,
            |engine, regions, state| step(engine, regions, Some(state)),
            |frame| !all && !frame.is_empty(),
        )
    };

    // Lay out the body into the current region. If it is automatically sized,
    // its frames must have a consistent width. So its first region also lays
    // it out into the predicted regions, and if the widths differ, lays it out
    // again with the widest one. The following regions are laid out with the
    // width of the first one. The side effects of a replaced layout don't
    // count.
    let (MultiStep { mut frame, next, ahead }, width) = match state {
        Some(state) => {
            let region = with_width(region, state.width);
            (step(engine, region, Some(&state.body))?, state.width)
        }
        None if matches!(body, Some(BlockBody::Content(_))) && !region.expand.x => {
            let (first, sink) = engine.isolate(|engine| step(engine, region, None));
            let first = first?;
            let widths: Vec<Abs> = std::iter::once(first.frame.width())
                .chain(
                    peek(engine, region, first.next.clone(), true)?
                        .iter()
                        .map(Frame::width),
                )
                .collect();
            if widths.windows(2).all(|w| w[0].approx_eq(w[1])) {
                engine.commit(sink);
                (first, Some(widths[0]))
            } else {
                let max = widths.iter().copied().max().unwrap_or_default();
                (step(engine, with_width(region, Some(max)), None)?, Some(max))
            }
        }
        None => (step(engine, region, None)?, None),
    };
    let decoration_only = frame.is_empty();

    // An empty first frame is an orphan if a non-empty one follows.
    let orphan = state.is_none()
        && decoration_only
        && peek(engine, with_width(region, width), next.clone(), false)?
            .iter()
            .any(|frame| !frame.is_empty());

    // Post-process the frame, unless it is beyond the pod regions. Skip
    // decorating and labeling orphans, as a label would make them non-empty.
    if pod.iter().nth(index).is_some() {
        finish_frame(elem, styles, &mut frame, region.size, pod.expand, &inset, !orphan);
    }
    if let Some(label) = elem.label()
        && !orphan
    {
        frame.label(label);
    }
    frame.modify(modifiers);

    // A block with a fixed height records the regions it was laid out in.
    let history = match state {
        None => matches!(height, Sizing::Rel(_)).then(RegionHistory::default),
        Some(state) => state.history.clone(),
    };
    let next = next.map(|body| BlockState {
        body,
        history: history.map(|history| history.then(regions)),
        width,
    });

    Ok(BlockStep {
        frame,
        next,
        orphan,
        decoration_only,
        ahead: crate::pad::grow_ahead(ahead, &inset),
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

/// The regions with the given width, if any, to which they expand.
fn with_width(regions: Regions, width: Option<Abs>) -> Regions {
    match width {
        Some(width) => Regions {
            size: Size::new(width, regions.size.y),
            expand: Axes::new(true, regions.expand.y),
            ..regions
        },
        None => regions,
    }
}

/// Post-processes a frame of a block to apply its size on the axes that
/// `expand`, insets, clipping, and, if it is `decorated`, fills and strokes.
fn finish_frame(
    elem: &Packed<BlockElem>,
    styles: StyleChain,
    frame: &mut Frame,
    size: Size,
    expand: Axes<bool>,
    inset: &Sides<Rel<Abs>>,
    decorated: bool,
) {
    // Explicit blocks are boundaries for gradient relativeness.
    if matches!(elem.body.get_ref(styles), None | Some(BlockBody::Content(_))) {
        frame.set_kind(FrameKind::Hard);
    }

    // Enforce a correct frame size on the expanded axes. Do this before
    // applying the inset, since the pod shrunk.
    frame.set_size(expand.select(size, frame.size()));

    // Apply the inset.
    if !inset.is_zero() {
        crate::pad::grow(frame, inset);
    }

    // Only fetch these if necessary (for clipping or filling/stroking).
    let fill = elem.fill.get_ref(styles);
    let stroke = elem
        .stroke
        .resolve(styles)
        .unwrap_or_default()
        .map(|s| s.map(Stroke::unwrap_or_default));
    let outset = LazyCell::new(|| elem.outset.resolve(styles).unwrap_or_default());
    let radius = LazyCell::new(|| elem.radius.resolve(styles).unwrap_or_default());

    // Clip the contents, if requested.
    if elem.clip.get(styles) {
        frame.clip(clip_rect(frame.size(), &radius, &stroke, &outset));
    }

    // Add fill and/or stroke.
    if decorated && (fill.is_some() || stroke.iter().any(Option::is_some)) {
        fill_and_stroke(frame, fill.clone(), &stroke, &outset, &radius, elem.span());
    }
}
