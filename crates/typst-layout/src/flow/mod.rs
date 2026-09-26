//! Layout of content into a [`Frame`] or [`Fragment`].

mod block;
mod collect;
mod compose;
mod distribute;

pub(crate) use self::block::unbreakable_pod;

use std::collections::BTreeMap;
use std::num::NonZeroUsize;
use std::sync::Arc;

use comemo::{Track, Tracked, TrackedMut};
use ecow::EcoVec;
use rustc_hash::{FxHashMap, FxHashSet};
use typst_library::diag::{At, SourceResult, bail};
use typst_library::engine::{Engine, Route, Sink, Traced};
use typst_library::foundations::{Content, Packed, Resolve, StyleChain};
use typst_library::introspection::{
    Introspector, Location, Locator, LocatorLink, SplitLocator,
};
use typst_library::layout::{
    Abs, Angle, ColumnsElem, Dir, Em, Followup, Fragment, Frame, HAlignment, MultiState,
    MultiStep, PageElem, Region, Regions, RegionsLink, Rel, Size, VAlignment,
};
use typst_library::model::{
    ArtifactKind, FootnoteElem, FootnoteEntry, LineNumberingScope, ParLine,
};
use typst_library::routines::{Arenas, FragmentKind, Pair, RealizationKind};
use typst_library::text::TextElem;
use typst_library::visualize::LineElem;
use typst_library::{Library, World};
use typst_syntax::Span;
use typst_utils::{LazyHash, NonZeroExt, Numeric, Protected};

use self::block::{BlockState, BlockStep, layout_multi_block, layout_single_block};
use self::collect::{
    Child, LineChild, MultiChild, MultiSpill, PlacedChild, SingleChild, base_styles,
    collect,
};
use self::compose::{RelayoutStop, compose};

/// Lays out content into a single region, producing a single frame.
pub fn layout_frame(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
    region: Region,
) -> SourceResult<Frame> {
    layout_fragment(engine, content, locator, styles, region.into())
        .map(Fragment::into_frame)
}

/// Lays out content into multiple regions.
///
/// When laying out into just one region, prefer [`layout_frame`].
pub fn layout_fragment(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment> {
    // Eager layout is keyed by the exact regions, which its callers typically
    // compute precisely anyway (like the heights of grid rows). Tracking the
    // questions asked about them would just add overhead.
    let mut buf = Followup::default();
    layout_fragment_impl(
        engine.world,
        engine.library,
        engine.introspector.into_raw(),
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        content,
        locator.track(),
        styles,
        regions.materialize(&mut buf),
    )
}

/// Lays out content into multiple regions, like [`layout_fragment`], but
/// only depending on the answers to the questions layout asks about the
/// regions instead of on the exact regions.
///
/// This is useful when the regions often vary in ways that don't matter to the
/// content, like when they depend on the content's position. Otherwise,
/// prefer [`layout_fragment`], as tracking has some overhead.
pub fn layout_fragment_tracked(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment> {
    layout_fragment_tracked_impl(
        engine.world,
        engine.library,
        engine.introspector.into_raw(),
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        content,
        locator.track(),
        styles,
        regions.track(),
    )
}

/// Lays out content one region at a time.
///
/// When called with `state` set to `None` for the first region and to the
/// returned state for each following region, this produces the same frames as
/// [`layout_fragment`] would produce for these regions in one go. Frames only
/// depend on the upcoming regions insofar as the content's layout does.
///
/// The step never declares content laid out ahead ([`MultiStep::ahead`]): The
/// flow checks what its children laid out ahead itself.
pub fn layout_fragment_step(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    layout_content_step(
        engine,
        content,
        locator,
        styles,
        regions,
        state,
        ColumnOptions::single(),
    )
}

/// Layout the columns, one region at a time.
///
/// This is different from just laying out into column-sized regions as the
/// columns can interact due to parent-scoped placed elements.
#[typst_macros::time(span = elem.span())]
pub fn layout_columns(
    elem: &Packed<ColumnsElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
) -> SourceResult<MultiStep> {
    layout_content_step(
        engine,
        &elem.body,
        locator,
        styles,
        regions,
        state,
        ColumnOptions {
            count: elem.count.get(styles),
            balanced: elem.balanced.get(styles),
            gutter: elem.gutter.resolve(styles),
            separator: elem.separator.get_cloned(styles),
        },
    )
}

/// Lays out a multi-region layouter one region at a time until it is done,
/// producing the frames it would produce for the given regions.
pub fn layout_steps(
    regions: Regions,
    mut step: impl FnMut(Regions, Option<&MultiState>) -> SourceResult<MultiStep>,
) -> SourceResult<Fragment> {
    let MultiStep { frame, next, .. } = step(regions, None)?;
    let rest =
        layout_remaining(regions, next, |regions, state| step(regions, Some(state)));
    std::iter::once(Ok(frame))
        .chain(rest)
        .collect::<SourceResult<_>>()
        .map(Fragment::frames)
}

/// Lays out the remaining regions of a multi-region layouter one region at a
/// time, continuing from the `state` it produced for the first of the
/// `regions`. Yields the frames for the regions after the first one.
pub fn layout_remaining<'r>(
    mut regions: Regions<'r>,
    mut state: Option<MultiState>,
    mut step: impl FnMut(Regions<'r>, &MultiState) -> SourceResult<MultiStep>,
) -> impl Iterator<Item = SourceResult<Frame>> {
    std::iter::from_fn(move || {
        let current = state.take()?;
        regions.next();
        Some(step(regions, &current).map(|MultiStep { frame, next, .. }| {
            state = next;
            frame
        }))
    })
}

/// Lays out the remaining regions like [`layout_remaining`], but only to
/// inspect the frames, up to and including the first one for which `stop`
/// returns `true`.
///
/// The side effects of laying them out are dropped, since the actual layout
/// records them.
pub fn peek_remaining(
    engine: &mut Engine,
    regions: Regions,
    state: Option<MultiState>,
    mut step: impl FnMut(&mut Engine, Regions, &MultiState) -> SourceResult<MultiStep>,
    mut stop: impl FnMut(&Frame) -> bool,
) -> SourceResult<Vec<Frame>> {
    engine
        .isolate(|engine| {
            let mut frames = vec![];
            for frame in layout_remaining(regions, state, |regions, state| {
                step(engine, regions, state)
            }) {
                let frame = frame?;
                let done = stop(&frame);
                frames.push(frame);
                if done {
                    break;
                }
            }
            Ok(frames)
        })
        .0
}

/// Where content laid out with [`layout_content_step`] continues: The prepared
/// flow of the content and where it continues.
struct ContentState {
    flow: Arc<PreparedFlow>,
    state: FlowState,
}

/// The shared implementation of [`layout_fragment_step`] and
/// [`layout_columns`].
fn layout_content_step(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    state: Option<&MultiState>,
    column: ColumnOptions,
) -> SourceResult<MultiStep> {
    // Like `layout_fragment_impl`, each step is a layout route segment of its
    // own, so that nested layouts hit the layout depth limit in time.
    let mut engine = Engine {
        library: engine.library,
        world: engine.world,
        introspector: engine.introspector,
        traced: engine.traced,
        sink: TrackedMut::reborrow_mut(&mut engine.sink),
        route: Route::extend(engine.route.track()),
    };
    engine.route.check_layout_depth().at(content.span())?;

    let locator = locator.track();
    let (flow, state) = match state.map(MultiState::get::<ContentState>) {
        None => {
            check_expansion(regions, content.span())?;

            let flow = prepare_flow(
                &mut engine,
                content,
                locator,
                styles,
                column.width(regions),
                regions.full(),
                regions.expand.x,
            )?;
            (flow, None)
        }
        Some(ContentState { flow, state }) => (flow.clone(), Some(state)),
    };

    let (frame, next) =
        layout_flow_step(&mut engine, &flow, state, locator, styles, regions, column)?;
    let next = next.map(|state| MultiState::new(ContentState { flow, state }));
    Ok(MultiStep::new(frame, next))
}

/// The regions that a multi-region layout already produced frames for.
///
/// Allows reconstructing the regions the whole layout would have been laid out
/// into, for layouts that can't proceed region by region.
#[derive(Debug, Clone, Default, Hash)]
pub(crate) struct RegionHistory {
    /// The full height of the first region.
    full: Abs,
    /// The heights of the regions, in order.
    heights: Vec<Abs>,
}

impl RegionHistory {
    /// The number of regions.
    pub fn len(&self) -> usize {
        self.heights.len()
    }

    /// The history with the first of the `regions` added to it.
    pub fn then(&self, regions: Regions) -> Self {
        let mut heights = self.heights.clone();
        heights.push(regions.height());
        let full = if self.heights.is_empty() { regions.full() } else { self.full };
        Self { full, heights }
    }

    /// The regions starting with the first region in the history, followed by
    /// the given `regions`.
    pub fn regions<'a>(
        &self,
        regions: Regions<'a>,
        buf: &'a mut Followup,
    ) -> Regions<'a> {
        let Some((&first, rest)) = self.heights.split_first() else {
            return regions;
        };
        let current = std::iter::once(regions.height());
        *buf = regions.followup().prepend(rest.iter().copied().chain(current));
        Regions::new(
            Size::new(regions.width(), first),
            self.full,
            &[],
            None,
            regions.expand,
        )
        .with_followup(buf)
    }
}

/// The cached, internal implementation of [`layout_fragment`].
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn layout_fragment_impl(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment> {
    layout_fragment_inner(
        world,
        library,
        introspector,
        traced,
        sink,
        route,
        content,
        locator,
        styles,
        regions,
    )
}

/// The cached, internal implementation of [`layout_fragment_tracked`].
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn layout_fragment_tracked_impl(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Tracked<Regions>,
) -> SourceResult<Fragment> {
    let link = RegionsLink::new(regions);
    layout_fragment_inner(
        world,
        library,
        introspector,
        traced,
        sink,
        route,
        content,
        locator,
        styles,
        Regions::link(&link),
    )
}

/// The shared implementation of [`layout_fragment_impl`] and
/// [`layout_fragment_tracked_impl`].
#[expect(clippy::too_many_arguments)]
fn layout_fragment_inner(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment> {
    check_expansion(regions, content.span())?;

    let introspector = Protected::from_raw(introspector);
    let link = LocatorLink::new(locator);
    let mut locator = Locator::link(&link).split();
    let mut engine = Engine {
        library,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    engine.route.check_layout_depth().at(content.span())?;

    let mut kind = FragmentKind::Block;
    let arenas = Arenas::default();
    let children = (engine.library.routines.realize)(
        RealizationKind::Fragment { kind: &mut kind },
        &mut engine,
        &mut locator,
        &arenas,
        content,
        styles,
    )?;

    layout_flow(
        &mut engine,
        &children,
        &mut locator,
        styles,
        regions,
        ColumnOptions::single(),
        kind.into(),
    )
}

/// Ensures that the regions are finite along the axes content is expanded to.
fn check_expansion(regions: Regions, span: Span) -> SourceResult<()> {
    if regions.expand.x && !regions.width().is_finite() {
        bail!(span, "cannot expand into infinite width");
    }
    if regions.expand.y && !regions.is_finite() {
        bail!(span, "cannot expand into infinite height");
    }
    Ok(())
}

/// The mode a flow can be laid out in.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FlowMode {
    /// A root flow with block-level elements. Like `FlowMode::Block`, but can
    /// additionally host footnotes and line numbers.
    Root,
    /// A flow whose children are block-level elements.
    Block,
    /// A flow whose children are inline-level elements.
    Inline,
}

impl From<FragmentKind> for FlowMode {
    fn from(value: FragmentKind) -> Self {
        match value {
            FragmentKind::Inline => Self::Inline,
            FragmentKind::Block => Self::Block,
        }
    }
}

/// A flow whose content was realized and collected, so that it can be laid
/// out one region at a time with [`layout_flow_step`], possibly across
/// multiple calls.
pub(super) struct PreparedFlow {
    /// The prepared children.
    children: Vec<Child>,
    /// The mode the flow is laid out in.
    mode: FlowMode,
    /// The number of sublocators the flow's split locator produced for `()`
    /// before the per-region locators.
    region_locators: usize,
}

/// Where a [`PreparedFlow`] continues.
#[derive(Clone)]
pub(super) struct FlowState {
    /// The work that is left to do.
    work: Work,
    /// The index of the region to lay out next.
    region: usize,
}

/// Realizes and collects content for layout with [`layout_flow_step`].
///
/// This performs the same preparation as [`layout_fragment_impl`] for a flow
/// into regions with the given column width, full height, and horizontal
/// expansion.
fn prepare_flow(
    engine: &mut Engine,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
    width: Abs,
    full: Abs,
    expand: bool,
) -> SourceResult<Arc<PreparedFlow>> {
    prepare_flow_impl(
        engine.world,
        engine.library,
        engine.introspector.into_raw(),
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        content,
        locator,
        styles,
        width,
        full,
        expand,
    )
}

/// The cached, internal implementation of [`prepare_flow`].
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn prepare_flow_impl(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
    width: Abs,
    full: Abs,
    expand: bool,
) -> SourceResult<Arc<PreparedFlow>> {
    let introspector = Protected::from_raw(introspector);
    let link = LocatorLink::new(locator);
    let mut locator = Locator::link(&link).split();
    let mut engine = Engine {
        library,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    engine.route.check_layout_depth().at(content.span())?;

    let mut kind = FragmentKind::Block;
    let arenas = Arenas::default();
    let children = (engine.library.routines.realize)(
        RealizationKind::Fragment { kind: &mut kind },
        &mut engine,
        &mut locator,
        &arenas,
        content,
        styles,
    )?;

    let mode = FlowMode::from(kind);
    // The children's styles are stored relative to the styles, since the flow
    // is laid out with them in later calls, after the realized content is gone.
    let children = collect(
        &mut engine,
        &children,
        locator.next(&()),
        styles,
        Size::new(width, full),
        expand,
        mode,
    )?;

    Ok(Arc::new(PreparedFlow {
        children,
        mode,
        region_locators: locator.count(&()),
    }))
}

/// Lays out the next region of a prepared flow.
///
/// Must be called with the same locator, styles, and column options the flow
/// was prepared with. The regions should have the same width, too: The lines
/// of the flow's paragraphs were already laid out with it. In regions with a
/// different width, they keep their width and are only aligned, while
/// everything else is laid out with the regions' width. (Breakable blocks use
/// this to give a frame the same width as earlier ones.) Like
/// [`layout_fragment`], the caller must ensure that the first regions are
/// finite along expanded axes. Returns the frame for the first of the
/// `regions` and, if the flow isn't done, where it continues. Unlike
/// [`layout_flow`], this cannot restart earlier regions, since they were
/// already handed out. Breakable children whose last frame turns out to be
/// inconsistent with the actual regions continue from the state that frame was
/// laid out with instead.
fn layout_flow_step(
    engine: &mut Engine,
    prepared: &PreparedFlow,
    state: Option<&FlowState>,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Regions,
    column: ColumnOptions,
) -> SourceResult<(Frame, Option<FlowState>)> {
    let config = configuration(styles, regions, column, prepared.mode);

    let link = LocatorLink::new(locator);
    let base = Locator::link(&link);
    let region_locator = base
        .relayout()
        .split()
        .nth(&(), prepared.region_locators + state.map_or(0, |s| s.region));

    let cx = FlowCx {
        children: &prepared.children,
        styles,
        locator: base,
    };

    let region = state.map_or(0, |s| s.region);
    let mut work =
        state.map_or_else(|| Work::new(prepared.children.len()), |s| s.work.clone());
    let predictions = Predictions::disabled();
    let frame = match compose(
        engine,
        &mut work,
        &cx,
        &config,
        region_locator,
        regions,
        region,
        &predictions,
    ) {
        Ok(frame) => frame,
        Err(RelayoutStop::Error(error)) => return Err(error),
        Err(RelayoutStop::Restart(_)) => unreachable!("restarts are disabled"),
        Err(RelayoutStop::Relayout(never)) => match never {},
    };

    let done = work.done() && (!regions.expand.y || !regions.has_backlog());
    let next = (!done).then(|| FlowState { work, region: region + 1 });
    Ok((frame, next))
}

/// Lays out realized content into regions, potentially with columns.
pub fn layout_flow<'a>(
    engine: &mut Engine,
    children: &[Pair<'a>],
    locator: &mut SplitLocator<'a>,
    shared: StyleChain<'a>,
    mut regions: Regions,
    column: ColumnOptions,
    mode: FlowMode,
) -> SourceResult<Fragment> {
    // Prepare configuration that is shared across the whole flow.
    let config = configuration(shared, regions, column, mode);

    // Collect the elements into pre-processed children. These are much easier
    // to handle than the raw elements.
    let collect_locator = locator.next(&());
    let base_locator = collect_locator.relayout();
    let base_styles = base_styles(children, shared);
    let children = collect(
        engine,
        children,
        collect_locator,
        base_styles,
        Size::new(config.columns.width, regions.full()),
        regions.expand.x,
        mode,
    )?;

    let cx = FlowCx {
        children: &children,
        styles: base_styles,
        locator: base_locator,
    };

    let mut work = Work::new(children.len());
    let mut finished = vec![];

    // State for restarting at an earlier region: What was learned about the
    // space in upcoming subregions and, per region, the locator as well as the
    // work and regions at its start. The side effects of each region's layout
    // are only recorded once no restart can discard it anymore.
    let mut predictions = Predictions::default();
    let mut locators = vec![];
    let mut checkpoints = vec![];
    let mut sinks = vec![];

    // This loop runs once per region produced by the flow layout.
    loop {
        let index = finished.len();
        if index == locators.len() {
            locators.push(locator.next(&()));
        }
        checkpoints.truncate(index);
        checkpoints.push((work.clone(), regions));

        let locator = locators[index].relayout();
        let (result, sink) = engine.isolate(|engine| {
            compose(
                engine,
                &mut work,
                &cx,
                &config,
                locator,
                regions,
                index,
                &predictions,
            )
        });
        match result {
            Ok(frame) => {
                finished.push(frame);
                sinks.push(sink);
            }
            Err(RelayoutStop::Restart(restart)) => {
                // Restart at the region containing the inconsistent frame,
                // now with better knowledge of the mispredicted subregion.
                predictions.learn(&restart);
                let target = restart.from / config.columns.count;
                (work, regions) = checkpoints[target].clone();
                finished.truncate(target);
                sinks.truncate(target);
                continue;
            }
            Err(RelayoutStop::Error(error)) => return Err(error),
            Err(RelayoutStop::Relayout(never)) => match never {},
        }

        // Terminate the loop when everything is processed, though draining the
        // backlog if necessary.
        if work.done() && (!regions.expand.y || !regions.has_backlog()) {
            break;
        }

        regions.next();
    }

    for sink in sinks {
        engine.commit(sink);
    }

    Ok(Fragment::frames(finished))
}

/// Determine the flow's configuration.
fn configuration<'x>(
    shared: StyleChain<'x>,
    regions: Regions,
    column: ColumnOptions,
    mode: FlowMode,
) -> Config<'x> {
    Config {
        mode,
        shared,
        columns: {
            let (count, gutter, width) = column.resolve(regions);
            let dir = shared.resolve(TextElem::dir);
            ColumnConfig {
                count,
                width,
                gutter,
                dir,
                balanced: column.balanced,
                separator: column.separator.map(|separator| {
                    separator
                        .set(LineElem::length, Rel::one())
                        .set(LineElem::angle, Angle::deg(90.0))
                        .aligned(HAlignment::Center + VAlignment::Horizon)
                }),
            }
        },
        footnote: FootnoteConfig {
            separator: shared
                .get_cloned(FootnoteEntry::separator)
                .artifact(ArtifactKind::Other),
            clearance: shared.resolve(FootnoteEntry::clearance),
            gap: shared.resolve(FootnoteEntry::gap),
            expand: regions.expand.x,
        },
        line_numbers: (mode == FlowMode::Root).then(|| LineNumberConfig {
            scope: shared.get(ParLine::numbering_scope),
            default_clearance: {
                let width = if shared.get(PageElem::flipped) {
                    shared.resolve(PageElem::height)
                } else {
                    shared.resolve(PageElem::width)
                };

                // Clamp below is safe (min <= max): if the font size is
                // negative, we set min = max = 0; otherwise,
                // `0.75 * size <= 2.5 * size` for zero and positive sizes.
                (0.026 * width.unwrap_or_default()).clamp(
                    Em::new(0.75).resolve(shared).max(Abs::zero()),
                    Em::new(2.5).resolve(shared).max(Abs::zero()),
                )
            },
        }),
    }
}

/// Context for laying out the children of a flow.
struct FlowCx<'a, 'b> {
    /// The prepared children.
    children: &'b [Child],
    /// The styles the flow's content was realized with. The styles of the
    /// children are stored relative to them.
    styles: StyleChain<'a>,
    /// A locator with the same link as the locators of the children, which
    /// are stored by their local hash.
    locator: Locator<'a>,
}

/// The work that is left to do by flow layout.
///
/// Refers to the children of the flow by their index, so that it doesn't
/// borrow from them.
#[derive(Clone)]
struct Work {
    /// The index of the first child that we haven't processed yet.
    cursor: usize,
    /// The number of children.
    len: usize,
    /// Leftovers from a breakable block.
    spill: Option<MultiSpill>,
    /// Queued floats that didn't fit in previous regions, by child index.
    floats: EcoVec<usize>,
    /// Queued footnotes that didn't fit in previous regions.
    footnotes: EcoVec<Packed<FootnoteElem>>,
    /// Spilled frames of a footnote that didn't fully fit. Similar to `spill`.
    footnote_spill: Option<std::vec::IntoIter<Frame>>,
    /// Queued tags that will be attached to the next frame, by child index.
    tags: EcoVec<usize>,
    /// Identifies floats and footnotes that can be skipped if visited because
    /// they were already handled and incorporated as column or page level
    /// insertions.
    skips: Arc<FxHashSet<Location>>,
}

impl Work {
    /// Create the initial work state for the given number of children.
    fn new(len: usize) -> Self {
        Self {
            cursor: 0,
            len,
            spill: None,
            floats: EcoVec::new(),
            footnotes: EcoVec::new(),
            footnote_spill: None,
            tags: EcoVec::new(),
            skips: Arc::new(FxHashSet::default()),
        }
    }

    /// Get the index of the first unprocessed child, if any.
    fn head(&self) -> Option<usize> {
        (self.cursor < self.len).then_some(self.cursor)
    }

    /// Mark the `head()` child as processed.
    fn advance(&mut self) {
        self.cursor += 1;
    }

    /// Whether all work is done. This means we can terminate flow layout.
    fn done(&self) -> bool {
        self.cursor >= self.len
            && self.spill.is_none()
            && self.floats.is_empty()
            && self.footnote_spill.is_none()
            && self.footnotes.is_empty()
    }

    /// Add skipped floats and footnotes from the insertion areas to the skip
    /// set.
    fn extend_skips(&mut self, skips: &[Location]) {
        if !skips.is_empty() {
            Arc::make_mut(&mut self.skips).extend(skips.iter().copied());
        }
    }
}

/// A request to restart flow layout at an earlier subregion.
///
/// This is issued when frames that were already emitted by a breakable child
/// turn out to be inconsistent with the actual space in a later subregion,
/// which differs from what the child was laid out with. The earlier
/// subregions are then composed again with the learned height.
#[derive(Debug, Clone)]
struct Restart {
    /// The subregion containing the first inconsistent frame.
    from: usize,
    /// The subregion whose space was mispredicted.
    at: usize,
    /// The actual height available in that subregion.
    height: Abs,
}

/// Learned predictions of the space that is actually available at the start
/// of upcoming subregions.
///
/// Each subregion can only trigger a bounded number of restarts, which ensures
/// that restarts terminate. See [`MultiSpill::layout`] for when a prediction
/// may be raised.
#[derive(Default)]
struct Predictions {
    /// The learned heights, keyed by subregion. Ordered, so that the
    /// predictions from a subregion onwards can be found without looking at
    /// all of them.
    heights: BTreeMap<usize, Abs>,
    /// How often a restart was triggered by each subregion.
    restarts: FxHashMap<usize, usize>,
    /// Whether restarts are impossible, because earlier regions were already
    /// handed out (when a flow is laid out one region at a time).
    disabled: bool,
}

impl Predictions {
    /// The maximum number of restarts a single subregion may trigger.
    const MAX_RESTARTS: usize = 3;

    /// Learns from a restart. The latest height replaces earlier ones, since
    /// the space in a subregion can grow when a restart moves insertions out
    /// of it.
    fn learn(&mut self, restart: &Restart) {
        self.heights.insert(restart.at, restart.height);
        *self.restarts.entry(restart.at).or_default() += 1;
    }

    /// How many more restarts may be requested because the given subregion's
    /// space was mispredicted.
    fn restarts(&self, subregion: usize) -> usize {
        if self.disabled {
            return 0;
        }
        let used = self.restarts.get(&subregion).copied().unwrap_or(0);
        Self::MAX_RESTARTS.saturating_sub(used)
    }

    /// Predictions for a flow that can't restart.
    fn disabled() -> Self {
        Self { disabled: true, ..Self::default() }
    }

    /// Whether any predictions exist for subregions from `first` onwards.
    fn affects(&self, first: usize) -> bool {
        self.heights.range(first..).next().is_some()
    }

    /// Applies the predictions to followup regions whose first one is
    /// subregion `first`: To the heights in the backlog and to the predicted
    /// remaining heights of the repetitions of the final region after it,
    /// which are extended if necessary. The repetitions are not moved into
    /// the backlog, since that would make moving on to them count as progress
    /// (see [`Regions::with_predicted`]).
    fn apply(&self, followup: &mut Followup, first: usize) {
        let Some((&max, _)) = self.heights.range(first..).next_back() else {
            return;
        };
        let Followup { backlog, predicted, last } = followup;

        let start = first + backlog.len();
        if let Some(last) = *last
            && max >= start
            && predicted.len() <= max - start
        {
            predicted.resize(max - start + 1, last);
        }

        for (&subregion, &learned) in self.heights.range(first..) {
            let i = subregion - first;
            let height = match i.checked_sub(backlog.len()) {
                None => &mut backlog[i],
                Some(j) => match predicted.get_mut(j) {
                    Some(height) => height,
                    None => break,
                },
            };
            height.set_min(learned);
        }
    }
}

/// Options defining the column layout.
pub struct ColumnOptions {
    /// The number of columns.
    pub count: NonZeroUsize,
    /// Whether column heights are to be equalized.
    pub balanced: bool,
    /// The spacing between columns.
    pub gutter: Rel<Abs>,
    /// The separator between columns.
    pub separator: Option<Content>,
}

impl ColumnOptions {
    /// Options for a single column.
    pub fn single() -> Self {
        Self {
            count: NonZeroUsize::ONE,
            balanced: false,
            gutter: Rel::zero(),
            separator: None,
        }
    }

    /// The width of each column in the given regions.
    fn width(&self, regions: Regions) -> Abs {
        self.resolve(regions).2
    }

    /// The number of columns, the gutter, and the width of each column in the
    /// given regions.
    fn resolve(&self, regions: Regions) -> (usize, Abs, Abs) {
        let mut count = self.count.get();
        if !regions.width().is_finite() {
            count = 1;
        }
        let gutter = self.gutter.relative_to(regions.base().x);
        let width = (regions.width() - gutter * (count - 1) as f64) / count as f64;
        (count, gutter, width)
    }
}

/// Shared configuration for the whole flow.
struct Config<'x> {
    /// Whether this is the root flow, which can host footnotes and line
    /// numbers.
    mode: FlowMode,
    /// The styles shared by the whole flow. This is used for footnotes and line
    /// numbers.
    shared: StyleChain<'x>,
    /// Settings for columns.
    columns: ColumnConfig,
    /// Settings for footnotes.
    footnote: FootnoteConfig,
    /// Settings for line numbers.
    line_numbers: Option<LineNumberConfig>,
}

/// Configuration of footnotes.
struct FootnoteConfig {
    /// The separator between flow content and footnotes. Typically a line.
    separator: Content,
    /// The amount of space left above the separator.
    clearance: Abs,
    /// The gap between footnote entries.
    gap: Abs,
    /// Whether horizontal expansion is enabled for footnotes.
    expand: bool,
}

/// Configuration of columns.
struct ColumnConfig {
    /// The number of columns.
    count: usize,
    /// The width of each column.
    width: Abs,
    /// The amount of space between columns.
    gutter: Abs,
    /// The horizontal direction in which columns progress. Defined by
    /// `text.dir`.
    dir: Dir,
    /// Whether to equalize the height of columns by breaking columns early.
    balanced: bool,
    /// The separator between columns.
    separator: Option<Content>,
}

/// Configuration of line numbers.
struct LineNumberConfig {
    /// Where line numbers are reset.
    scope: LineNumberingScope,
    /// The default clearance for `auto`.
    ///
    /// This value should be relative to the page's width, such that the
    /// clearance between line numbers and text is small when the page is,
    /// itself, small. However, that could cause the clearance to be too small
    /// or too large when considering the current text size; in particular, a
    /// larger text size would require more clearance to be able to tell line
    /// numbers apart from text, whereas a smaller text size requires less
    /// clearance so they aren't way too far apart. Therefore, the default
    /// value is a percentage of the page width clamped between `0.75em` and
    /// `2.5em`.
    default_clearance: Abs,
}
