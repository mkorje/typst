//! Layout of content into a [`Frame`] or [`Fragment`].

mod block;
mod collect;
mod compose;
mod distribute;

pub(crate) use self::block::unbreakable_pod;

use std::collections::BTreeMap;
use std::convert::Infallible;
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
    Abs, Angle, ColumnsElem, Dir, Em, Fragment, Frame, HAlignment, MultiState, MultiStep,
    PageElem, Region, Regions, Rel, Size, VAlignment,
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
        regions,
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

/// Lays out the regions after the first one of a multi-region layouter, one
/// region at a time, continuing from the `state` it produced for the first
/// region. This is only to inspect the frames, up to and including the first
/// one for which `stop` returns `true`.
///
/// The side effects of laying them out are dropped, since the actual layout
/// records them.
pub fn peek_remaining(
    engine: &mut Engine,
    mut regions: Regions,
    mut state: Option<MultiState>,
    mut step: impl FnMut(&mut Engine, Regions, &MultiState) -> SourceResult<MultiStep>,
    mut stop: impl FnMut(&Frame) -> bool,
) -> SourceResult<Vec<Frame>> {
    engine
        .isolate(|engine| {
            let mut frames = vec![];
            while let Some(current) = state.take() {
                regions.next();
                let MultiStep { frame, next, .. } = step(engine, regions, &current)?;
                let done = stop(&frame);
                frames.push(frame);
                if done {
                    break;
                }
                state = next;
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
                engine.world,
                engine.library,
                engine.introspector.into_raw(),
                engine.traced,
                TrackedMut::reborrow_mut(&mut engine.sink),
                engine.route.track(),
                content,
                locator,
                styles,
                Size::new(column.width(regions), regions.full),
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

    let flow = prepare(
        &mut engine,
        content,
        &mut locator,
        styles,
        regions.base(),
        regions.expand.x,
    )?;
    let config = configuration(styles, regions, ColumnOptions::single(), flow.mode);
    layout_prepared_flow(&mut engine, &flow, &locator, styles, &config, regions)
}

/// Ensures that the regions are finite along the axes content is expanded to.
fn check_expansion(regions: Regions, span: Span) -> SourceResult<()> {
    if regions.expand.x && !regions.size.x.is_finite() {
        bail!(span, "cannot expand into infinite width");
    }
    if regions.expand.y && !regions.size.y.is_finite() {
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

impl PreparedFlow {
    /// Collects realized children for a flow into columns with the given
    /// base size (see [`Regions::base`]) and horizontal expansion. The
    /// children's styles are stored relative to the given styles.
    fn new(
        engine: &mut Engine,
        children: &[Pair],
        locator: &mut SplitLocator,
        styles: StyleChain,
        base: Size,
        expand: bool,
        mode: FlowMode,
    ) -> SourceResult<Self> {
        let children =
            collect(engine, children, locator.next(&()), styles, base, expand, mode)?;
        Ok(Self {
            children,
            mode,
            region_locators: locator.count(&()),
        })
    }
}

/// Realizes and collects content for a flow into columns with the given base
/// size and horizontal expansion.
fn prepare(
    engine: &mut Engine,
    content: &Content,
    locator: &mut SplitLocator,
    styles: StyleChain,
    base: Size,
    expand: bool,
) -> SourceResult<PreparedFlow> {
    engine.route.check_layout_depth().at(content.span())?;

    let mut kind = FragmentKind::Block;
    let arenas = Arenas::default();
    let children = (engine.library.routines.realize)(
        RealizationKind::Fragment { kind: &mut kind },
        engine,
        locator,
        &arenas,
        content,
        styles,
    )?;

    PreparedFlow::new(engine, &children, locator, styles, base, expand, kind.into())
}

/// Where a [`PreparedFlow`] continues.
#[derive(Clone)]
pub(super) struct FlowState {
    /// The work that is left to do.
    work: Work,
    /// The index of the region to lay out next.
    region: usize,
}

/// Realizes and collects content for layout with [`layout_flow_step`], like
/// [`layout_fragment`] does.
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn prepare_flow(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    content: &Content,
    locator: Tracked<Locator>,
    styles: StyleChain,
    base: Size,
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
    prepare(&mut engine, content, &mut locator, styles, base, expand).map(Arc::new)
}

/// Lays out the next region of a prepared flow. Returns the frame for the first
/// of the `regions` and, if the flow isn't done, where it continues.
///
/// Must be called with the locator, styles, and column options the flow was
/// prepared with. In regions of another width, the lines of the flow's
/// paragraphs keep the width they were prepared with and are only aligned.
/// Unlike [`layout_prepared_flow`], this can't restart at an earlier region,
/// since it was already handed out.
fn layout_flow_step(
    engine: &mut Engine,
    prepared: &PreparedFlow,
    state: Option<&FlowState>,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Regions,
    column: ColumnOptions,
) -> SourceResult<(Frame, Option<FlowState>)> {
    let link = LocatorLink::new(locator);
    let locator = Locator::link(&link).split();
    let region = state.map_or(0, |s| s.region);
    let mut work =
        state.map_or_else(|| Work::new(prepared.children.len()), |s| s.work.clone());
    let config = configuration(styles, regions, column, prepared.mode);
    let predictions = Predictions::disabled();
    let frame = match compose_region(
        engine,
        prepared,
        &mut work,
        &locator,
        styles,
        &config,
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
    regions: Regions,
    column: ColumnOptions,
    mode: FlowMode,
) -> SourceResult<Fragment> {
    // Prepare configuration that is shared across the whole flow.
    let config = configuration(shared, regions, column, mode);
    let styles = base_styles(children, shared);
    let base = Size::new(config.columns.width, regions.full);
    let flow = PreparedFlow::new(
        engine,
        children,
        locator,
        styles,
        base,
        regions.expand.x,
        mode,
    )?;
    layout_prepared_flow(engine, &flow, locator, styles, &config, regions)
}

/// Lays out a prepared flow into regions, all at once. Unlike
/// [`layout_flow_step`], this can restart at an earlier region.
fn layout_prepared_flow(
    engine: &mut Engine,
    flow: &PreparedFlow,
    locator: &SplitLocator,
    styles: StyleChain,
    config: &Config,
    mut regions: Regions,
) -> SourceResult<Fragment> {
    let mut work = Work::new(flow.children.len());
    let mut finished = vec![];

    // State for restarting at an earlier region: What was learned about the
    // space in upcoming subregions and, per region, the work and regions at
    // its start. The side effects of each region's layout are only recorded
    // once no restart can discard it anymore.
    let mut predictions = Predictions::default();
    let mut checkpoints = vec![];
    let mut sinks = vec![];

    // This loop runs once per region produced by the flow layout.
    loop {
        let index = finished.len();
        checkpoints.truncate(index);
        checkpoints.push((work.clone(), regions));

        let (result, sink) = engine.isolate(|engine| {
            compose_region(
                engine,
                flow,
                &mut work,
                locator,
                styles,
                config,
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

/// Composes region `index` of a prepared flow, continuing with the `work`.
///
/// The `locator` and `styles` must be the ones the flow was prepared with.
#[expect(clippy::too_many_arguments)]
fn compose_region(
    engine: &mut Engine,
    flow: &PreparedFlow,
    work: &mut Work,
    locator: &SplitLocator,
    styles: StyleChain,
    config: &Config,
    regions: Regions,
    index: usize,
    predictions: &Predictions,
) -> Result<Frame, RelayoutStop<Infallible>> {
    // The children's locators only need the link of the flow's locator.
    let cx = FlowCx {
        children: &flow.children,
        styles,
        locator: locator.nth(&(), 0),
    };
    let locator = locator.nth(&(), flow.region_locators + index);
    compose(engine, work, &cx, config, locator, regions, index, predictions)
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

    /// Applies the predictions to the regions after the first of the
    /// `regions`, the first of which is subregion `first`: To the heights in
    /// the backlog and to the predicted remaining heights of the repetitions of
    /// the final region after it, which are added to the backlog as predicted
    /// repetitions if necessary. They are not added as other backlog regions,
    /// since that would make moving on to them count as progress (see
    /// [`Regions::predicted`]).
    fn apply<'a>(
        &self,
        regions: Regions<'a>,
        first: usize,
        buf: &'a mut Vec<Abs>,
    ) -> Regions<'a> {
        let Some((&max, _)) = self.heights.range(first..).next_back() else {
            return regions;
        };

        buf.clear();
        buf.extend_from_slice(regions.backlog);
        let mut predicted = regions.predicted;
        if let Some(last) = regions.last {
            while first + buf.len() <= max {
                buf.push(last);
                predicted += 1;
            }
        }

        for (&subregion, &learned) in self.heights.range(first..) {
            match buf.get_mut(subregion - first) {
                Some(height) => height.set_min(learned),
                None => break,
            }
        }

        let mut regions = Regions { backlog: buf, predicted, ..regions };
        regions.trim_predicted();
        regions
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
        if !regions.size.x.is_finite() {
            count = 1;
        }
        let gutter = self.gutter.relative_to(regions.base().x);
        let width = (regions.size.x - gutter * (count - 1) as f64) / count as f64;
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
