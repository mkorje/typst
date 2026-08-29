//! Layout of content into a [`Frame`] or [`Fragment`].

mod block;
mod collect;
mod compose;
mod distribute;

pub(crate) use self::block::unbreakable_pod;

use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::num::NonZeroUsize;
use std::rc::Rc;
use std::sync::Arc;

use bumpalo::Bump;
use comemo::{Track, Tracked, TrackedMut};
use ecow::EcoVec;
use rustc_hash::{FxHashMap, FxHashSet};
use typst_library::diag::{At, SourceResult, bail};
use typst_library::engine::{EffectTransaction, Engine, Route, Sink, Traced};
use typst_library::foundations::{Content, Packed, Resolve, StyleChain};
use typst_library::introspection::{
    Introspector, Location, Locator, LocatorLink, SplitLocator, Tag,
};
use typst_library::layout::{
    Abs, Axes, ColumnsElem, Dir, Em, Fragment, Frame, PageElem, Region, Regions, Rel,
    Size,
};
use typst_library::model::{FootnoteElem, FootnoteEntry, LineNumberingScope, ParLine};
use typst_library::pdf::ArtifactKind;
use typst_library::routines::{Arenas, FragmentKind, Pair, RealizationKind};
use typst_library::text::TextElem;
use typst_library::{Library, World};
use typst_utils::{LazyHash, NonZeroExt, Numeric, Protected};

use self::block::{layout_multi_block, layout_single_block};
use self::collect::{
    Child, LineChild, MultiChild, MultiLayout, MultiLayoutContext, MultiSpill,
    MultiSpillKey, PlacedChild, SingleChild, collect,
};
use self::compose::{ComposeContext, ComposeStop, compose};
use crate::modifiers::{FrameModifiers, FrameModify};

/// Identifies one transactional replay for local child-layout caches.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(super) struct LayoutAttempt(u64);

/// Stable identity of a collected flow child.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(super) struct ChildId(u32);

impl ChildId {
    fn new(index: usize) -> Self {
        Self(index.try_into().expect("flow has more than u32::MAX children"))
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// Identifies a prepared flow within one optimization session.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub(super) struct FlowPathId(Option<Arc<FlowPathNode>>);

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
struct FlowPathNode {
    parent: Option<Arc<FlowPathNode>>,
    child: ChildId,
}

impl FlowPathId {
    fn root() -> Self {
        Self(None)
    }

    pub(super) fn child(&self, child: ChildId) -> Self {
        Self(Some(Arc::new(FlowPathNode { parent: self.0.clone(), child })))
    }
}

/// The semantic reason an optional region break is available.
#[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
enum BreakClass {
    StickyStart,
    StickyTail,
}

/// Stable identity of one optional breakpoint.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
struct BreakId {
    path: FlowPathId,
    child: ChildId,
    class: BreakClass,
}

/// A request to choose whether to finish the current region before a sticky
/// child.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
struct StickyObservation {
    id: BreakId,
    occurrence: usize,
}

/// One validated decision in a deterministic replay.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub(super) struct BreakChoice {
    observation: StickyObservation,
    migrate: bool,
}

fn insert_choice(choices: &mut Vec<BreakChoice>, choice: BreakChoice) {
    match choices.binary_search_by(|existing| {
        existing.observation.cmp(&choice.observation)
    }) {
        Ok(index) => {
            if choices[index] != choice {
                unreachable!("flow optimizer assigned conflicting breakpoint choices");
            }
        }
        Err(index) => choices.insert(index, choice),
    }
}

#[derive(Debug, Clone, Hash)]
pub(super) struct FlowControl<'a> {
    path: FlowPathId,
    tail_breaks: bool,
    decisions: &'a [BreakChoice],
    limit: usize,
}

/// The part of sticky placement that can affect future regions.
#[derive(Debug, Default, Clone, Eq, PartialEq, Hash)]
struct StickyState {
    pending: Option<PendingKeep>,
    active: Option<ActiveChild>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct PendingKeep {
    child: ChildId,
    tail_here: bool,
    forced: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct ActiveChild {
    child: ChildId,
    sticky: bool,
    saw_non_empty: bool,
    last_non_empty_here: bool,
}

/// Primary sticky quality accumulated by an exact replay.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub(super) struct StickyScore {
    pub(super) detached: u32,
    pub(super) forced_detached: u32,
}

/// Sticky state and score produced by one layout path.
#[derive(Debug, Default, Clone)]
struct StickyTrace {
    state: StickyState,
    score: StickyScore,
}

impl StickyTrace {
    fn checkpoint(&self) -> Self {
        self.clone()
    }

    fn restore(&mut self, checkpoint: Self) {
        *self = checkpoint;
    }

    fn add_score(&mut self, score: StickyScore) {
        self.score.detached = self.score.detached.saturating_add(score.detached);
        self.score.forced_detached =
            self.score.forced_detached.saturating_add(score.forced_detached);
    }

    fn begin(&mut self, child: ChildId, sticky: bool) {
        if self.state.active.as_ref().is_none_or(|active| active.child != child) {
            self.state.active = Some(ActiveChild {
                child,
                sticky,
                saw_non_empty: false,
                last_non_empty_here: false,
            });
        }
    }

    fn place(&mut self, child: ChildId) {
        let Some(active) = self.state.active.as_mut() else { return };
        debug_assert_eq!(active.child, child);

        if !active.saw_non_empty {
            if let Some(pending) = self.state.pending.take()
                && !pending.tail_here
            {
                if pending.forced {
                    self.score.forced_detached =
                        self.score.forced_detached.saturating_add(1);
                } else {
                    self.score.detached = self.score.detached.saturating_add(1);
                }
            }
            active.saw_non_empty = true;
        }

        active.last_non_empty_here = true;
    }

    fn finish(&mut self, child: ChildId) {
        let Some(active) = self.state.active.take() else { return };
        debug_assert_eq!(active.child, child);
        if active.sticky && active.saw_non_empty {
            self.state.pending = Some(PendingKeep {
                child,
                tail_here: active.last_non_empty_here,
                forced: false,
            });
        }
    }

    /// Advance to another physical flow region (a column or outer region).
    fn advance_region(&mut self) {
        if let Some(pending) = self.state.pending.as_mut() {
            pending.tail_here = false;
        }
        if let Some(active) = self.state.active.as_mut() {
            active.last_non_empty_here = false;
        }
    }

    fn force_break(&mut self) {
        if let Some(pending) = self.state.pending.as_mut() {
            pending.forced = true;
        }
    }

    fn finish_flow(&mut self) {
        self.state = StickyState::default();
    }
}

/// Forced sticky choices for one deterministic replay.
struct StickyChoices {
    values: Vec<BreakChoice>,
    lookup: FxHashMap<StickyObservation, usize>,
    cursors: FxHashMap<BreakId, usize>,
    live: Vec<usize>,
    used: Vec<bool>,
    used_count: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct RegionsKey {
    size: Size,
    expand: Axes<bool>,
    full: Abs,
    backlog: Vec<Abs>,
    last: Option<Abs>,
}

impl From<Regions<'_>> for RegionsKey {
    fn from(regions: Regions) -> Self {
        Self {
            size: regions.size,
            expand: regions.expand,
            full: regions.full,
            backlog: regions.backlog.to_vec(),
            last: regions.last,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct FootnoteSpillKey {
    location: Location,
    regions: RegionsKey,
}

#[derive(Clone)]
struct FootnoteSpill {
    key: FootnoteSpillKey,
    frames: Arc<[Frame]>,
    next: usize,
}

impl FootnoteSpill {
    fn new(key: FootnoteSpillKey, frames: Vec<Frame>, next: usize) -> Self {
        Self { key, frames: frames.into(), next }
    }

    fn next(&mut self) -> Option<Frame> {
        let frame = self.frames.get(self.next)?.clone();
        self.next += 1;
        Some(frame)
    }

    fn is_empty(&self) -> bool {
        self.next == self.frames.len()
    }
}

impl StickyChoices {
    fn new(values: &[BreakChoice]) -> Self {
        let mut values = values.to_vec();
        values.sort_unstable();
        let mut lookup = FxHashMap::default();
        for (index, choice) in values.iter().enumerate() {
            if lookup.insert(choice.observation.clone(), index).is_some() {
                unreachable!("flow optimizer replay plan contains a duplicate choice");
            }
        }

        Self {
            used: vec![false; values.len()],
            values,
            lookup,
            cursors: FxHashMap::default(),
            live: vec![],
            used_count: 0,
        }
    }

    fn decide(&mut self, id: BreakId) -> Result<bool, StickyObservation> {
        let occurrence = self.cursors.get(&id).copied().unwrap_or_default();
        let observation = StickyObservation { id, occurrence };
        let Some(&index) = self.lookup.get(&observation) else {
            return Err(observation);
        };
        let migrate = self.values[index].migrate;
        self.consume(index);
        Ok(migrate)
    }

    fn checkpoint(&self) -> usize {
        self.live.len()
    }

    fn restore(&mut self, checkpoint: usize) {
        if checkpoint > self.live.len() {
            unreachable!("flow optimizer restored an invalid choice checkpoint");
        }

        while self.live.len() > checkpoint {
            let index = self.live.pop().unwrap();
            let id = &self.values[index].observation.id;
            let cursor = self.cursors.get_mut(id).unwrap();
            *cursor = cursor.checked_sub(1).unwrap();
        }
    }

    fn values(&self) -> &[BreakChoice] {
        &self.values
    }

    fn used_len(&self) -> usize {
        self.used_count
    }

    fn used_choices(&self) -> Vec<BreakChoice> {
        self.values
            .iter()
            .zip(&self.used)
            .filter(|(_, used)| **used)
            .map(|(choice, _)| choice.clone())
            .collect()
    }

    fn consume_choices(&mut self, choices: &[BreakChoice]) {
        for choice in choices {
            let Some(&index) = self.lookup.get(&choice.observation) else {
                continue;
            };
            if self.values[index].migrate != choice.migrate {
                unreachable!("nested flow replay changed a committed choice");
            }

            let cursor = *self
                .cursors
                .entry(choice.observation.id.clone())
                .or_insert(choice.observation.occurrence);
            match choice.observation.occurrence.cmp(&cursor) {
                Ordering::Less => {}
                Ordering::Equal => self.consume(index),
                Ordering::Greater => {
                    unreachable!("nested flow replay skipped a planned choice");
                }
            }
        }
    }

    fn consume(&mut self, index: usize) {
        let observation = &self.values[index].observation;
        let cursor = self.cursors.entry(observation.id.clone()).or_default();
        if *cursor != observation.occurrence {
            unreachable!("flow optimizer replay consumed a choice out of order");
        }
        *cursor += 1;
        self.live.push(index);
        if !std::mem::replace(&mut self.used[index], true) {
            self.used_count += 1;
        }
    }
}

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
        ColumnOptions {
            count: NonZeroUsize::ONE,
            balanced: false,
            gutter: Rel::zero(),
        },
    )
}

/// Layout the columns.
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
) -> SourceResult<Fragment> {
    layout_fragment_impl(
        engine.world,
        engine.library,
        engine.introspector.into_raw(),
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        &elem.body,
        locator.track(),
        styles,
        regions,
        ColumnOptions {
            count: elem.count.get(styles),
            balanced: elem.balanced.get(styles),
            gutter: elem.gutter.resolve(styles),
        },
    )
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
    column: ColumnOptions,
) -> SourceResult<Fragment> {
    if !regions.size.x.is_finite() && regions.expand.x {
        bail!(content.span(), "cannot expand into infinite width");
    }
    if !regions.size.y.is_finite() && regions.expand.y {
        bail!(content.span(), "cannot expand into infinite height");
    }

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
        column,
        kind.into(),
    )
}

/// A normal-content block layout driven by the enclosing flow's breakpoint
/// choices.
#[derive(Clone)]
pub(super) struct ControlledFragment {
    pub(super) fragment: Fragment,
    pub(super) choices: Vec<BreakChoice>,
    pub(super) score: StickyScore,
    pub(super) complete: bool,
}

impl FrameModify for ControlledFragment {
    fn modify(&mut self, modifiers: &FrameModifiers) {
        FrameModify::modify(&mut self.fragment, modifiers);
    }
}

fn layout_fragment_controlled(
    engine: &mut Engine,
    content: &Content,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
    control: FlowControl,
) -> Result<ControlledFragment, ComposeStop> {
    layout_fragment_controlled_impl(
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
        control,
    )
}

#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn layout_fragment_controlled_impl(
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
    control: FlowControl,
) -> Result<ControlledFragment, ComposeStop> {
    let invalid = (!regions.size.x.is_finite() && regions.expand.x)
        .then_some("cannot expand into infinite width")
        .or_else(|| {
            (!regions.size.y.is_finite() && regions.expand.y)
                .then_some("cannot expand into infinite height")
        });
    if let Some(message) = invalid {
        return Err(ComposeStop::error(content.span(), message));
    }

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

    engine
        .route
        .check_layout_depth()
        .at(content.span())
        .map_err(ComposeStop::Error)?;

    // Only the first reconstruction may emit preparation effects.
    engine.sink.begin_effect_transaction();
    let mut kind = FragmentKind::Block;
    let arenas = Arenas::default();
    let children = match (engine.library.routines.realize)(
        RealizationKind::Fragment { kind: &mut kind },
        &mut engine,
        &mut locator,
        &arenas,
        content,
        styles,
    ) {
        Ok(children) => children,
        Err(error) => {
            finish_controlled_preparation(&mut engine, control.limit);
            return Err(ComposeStop::Error(error));
        }
    };

    layout_flow_controlled(
        &mut engine,
        &children,
        &mut locator,
        styles,
        regions,
        kind.into(),
        control,
    )
}

fn layout_flow_controlled<'a>(
    engine: &mut Engine,
    children: &[Pair<'a>],
    locator: &mut SplitLocator<'a>,
    shared: StyleChain<'a>,
    regions: Regions,
    mode: FlowMode,
    control: FlowControl,
) -> Result<ControlledFragment, ComposeStop> {
    let column = ColumnOptions {
        count: NonZeroUsize::ONE,
        balanced: false,
        gutter: Rel::zero(),
    };
    let mut config = configuration(
        shared,
        regions,
        column,
        mode,
        control.path.clone(),
        control.tail_breaks,
    );
    config.optimize = true;
    let bump = Bump::new();
    let children = match collect(
        engine,
        &bump,
        children,
        locator.next(&()),
        Size::new(config.columns.width, regions.full),
        regions.expand.x,
        mode,
    ) {
        Ok(children) => children,
        Err(error) => {
            finish_controlled_preparation(engine, control.limit);
            return Err(ComposeStop::Error(error));
        }
    };
    finish_controlled_preparation(engine, control.limit);
    let compose_locator = locator.next(&());
    let page_locators = PageLocators::new(&compose_locator);
    render_flow_controlled(
        engine,
        &children,
        &config,
        &page_locators,
        regions,
        control.decisions,
        control.limit,
    )
}

fn finish_controlled_preparation(engine: &mut Engine, limit: usize) {
    if limit == 0 {
        engine.sink.commit_effect_transaction();
    } else {
        engine.sink.rollback_effect_transaction();
    }
}

fn render_flow_controlled(
    engine: &mut Engine,
    children: &[Child<'_>],
    config: &Config,
    locators: &PageLocators,
    mut regions: Regions,
    decisions: &[BreakChoice],
    limit: usize,
) -> Result<ControlledFragment, ComposeStop> {
    let mut work = Work::new(children);
    let mut frames = vec![];
    let mut trace = StickyTrace::default();
    let mut choices = StickyChoices::new(decisions);
    let mut outer = 0;

    loop {
        // Discard visible effects from reconstructed prefix regions.
        let transaction = EffectTransaction::fresh();
        engine.sink.begin_effect_transaction_with(transaction);
        let result = compose(
            engine,
            &mut work,
            config,
            locators.get(outer),
            regions,
            ComposeContext {
                attempt: config.next_attempt(),
                choices: &mut choices,
                trace: &mut trace,
                effects: transaction,
                line_numbers: true,
            },
        );
        if outer < limit {
            engine.sink.rollback_effect_transaction();
        } else {
            engine.sink.commit_effect_transaction();
        }
        let frame = result?;
        frames.push(frame);

        if work.done() {
            trace.finish_flow();
        }

        let complete = work.finished(regions);
        if complete || outer == limit {
            return Ok(ControlledFragment {
                fragment: Fragment::frames(frames),
                choices: choices.used_choices(),
                score: trace.score,
                complete,
            });
        }

        regions.next();
        trace.advance_region();
        outer += 1;
    }
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
    let mut config =
        configuration(shared, regions, column, mode, FlowPathId::root(), false);

    // Collect the elements into pre-processed children. These are much easier
    // to handle than the raw elements.
    let bump = Bump::new();
    let children = collect(
        engine,
        &bump,
        children,
        locator.next(&()),
        Size::new(config.columns.width, regions.full),
        regions.expand.x,
        mode,
    )?;

    // Reserve one locator hierarchy for composition. Search and committed
    // replay always relayout this hierarchy, keeping locations stable.
    let compose_locator = locator.next(&());
    let page_locators = PageLocators::new(&compose_locator);

    // Bypass plan search entirely when this flow has no sticky blocks. Nested
    // flows make the same decision for themselves.
    if !children.iter().any(Child::sticky) {
        return render_flow(engine, &children, &config, &page_locators, regions, None);
    }

    config.optimize = true;

    // Search with an isolated user-visible sink. The tracked World,
    // Introspector, and Traced inputs are intentionally shared so that reads
    // from every candidate which influenced the argmin are part of the outer
    // memoization/convergence constraint.
    let (plan, search_sink) = engine.analyze(|trial| {
        optimize_flow(trial, &children, &config, &page_locators, regions)
    });

    // Candidate warnings, delayed errors, and traced values are discarded.
    // Retain the high-level introspection descriptions so non-convergence
    // diagnostics can explain a dependency used by the optimizer.
    for introspection in search_sink.introspections().iter().cloned() {
        engine.sink.introspection(introspection);
    }

    let plan = match plan {
        Ok(plan) => plan,
        Err(_) => {
            // Reproduce the deterministic failing path against the real sink
            // so its fatal diagnostic and any preceding committed effects come
            // from an actual execution rather than an arbitrary candidate.
            return render_greedy_fallback(
                engine,
                &children,
                &config,
                &page_locators,
                regions,
            );
        }
    };
    render_flow(engine, &children, &config, &page_locators, regions, Some(&plan))
}

fn render_greedy_fallback(
    engine: &mut Engine,
    children: &[Child<'_>],
    config: &Config,
    locators: &PageLocators,
    mut regions: Regions,
) -> SourceResult<Fragment> {
    let mut work = Work::new(children);
    let mut trace = StickyTrace::default();
    let mut finished = vec![];
    let mut outer = 0;

    loop {
        let entry_work = work.clone();
        let entry_trace = trace.clone();
        let mut decisions = vec![];

        let frame = loop {
            work = entry_work.clone();
            trace = entry_trace.clone();
            let mut choices = StickyChoices::new(&decisions);
            let transaction = EffectTransaction::fresh();
            engine.sink.begin_effect_transaction_with(transaction);
            match compose(
                engine,
                &mut work,
                config,
                locators.get(outer),
                regions,
                ComposeContext {
                    attempt: config.next_attempt(),
                    choices: &mut choices,
                    trace: &mut trace,
                    effects: transaction,
                    line_numbers: true,
                },
            ) {
                Ok(frame) => {
                    engine.sink.commit_effect_transaction();
                    break frame;
                }
                Err(ComposeStop::Sticky(observation)) => {
                    engine.sink.rollback_effect_transaction();
                    insert_choice(
                        &mut decisions,
                        BreakChoice { observation, migrate: false },
                    );
                }
                Err(ComposeStop::Error(error)) => {
                    engine.sink.commit_effect_transaction();
                    return Err(error);
                }
            }
        };
        if work.done() {
            trace.finish_flow();
        }
        finished.push(frame);

        if work.finished(regions) {
            return Ok(Fragment::frames(finished));
        }

        regions.next();
        trace.advance_region();
        outer += 1;
    }
}

#[derive(Debug, Clone)]
struct BreakPlan {
    pages: Vec<PagePlan>,
}

#[derive(Debug, Clone)]
struct PagePlan {
    choices: Vec<BreakChoice>,
    expected: OutcomeFingerprint,
}

impl PagePlan {
    fn optional_breaks(&self) -> usize {
        self.choices.iter().filter(|choice| choice.migrate).count()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct OutcomeFingerprint {
    work: Arc<WorkKey>,
    sticky: StickyState,
    score: StickyScore,
}

impl OutcomeFingerprint {
    fn new(work: &Work, trace: &StickyTrace) -> Self {
        Self {
            work: Arc::new(work.key()),
            sticky: trace.state.clone(),
            score: trace.score,
        }
    }
}

/// Render one deterministic plan through the normal composition code. This is
/// the only replay whose warnings, delayed errors, and traced values reach the
/// caller.
fn render_flow(
    engine: &mut Engine,
    children: &[Child<'_>],
    config: &Config,
    locators: &PageLocators,
    mut regions: Regions,
    plan: Option<&BreakPlan>,
) -> SourceResult<Fragment> {
    let mut work = Work::new(children);
    let mut finished = vec![];
    let mut trace = StickyTrace::default();
    let mut page = 0;

    loop {
        let page_plan = plan.and_then(|plan| plan.pages.get(page));
        if plan.is_some() && page_plan.is_none() {
            unreachable!("flow optimizer replay exhausted before layout completed");
        }
        let mut choices =
            StickyChoices::new(page_plan.map_or(&[][..], |page| page.choices.as_slice()));
        let transaction = EffectTransaction::fresh();
        engine.sink.begin_effect_transaction_with(transaction);
        let frame = match compose(
            engine,
            &mut work,
            config,
            locators.get(page),
            regions,
            ComposeContext {
                attempt: config.next_attempt(),
                choices: &mut choices,
                trace: &mut trace,
                effects: transaction,
                line_numbers: true,
            },
        ) {
            Ok(frame) => frame,
            Err(ComposeStop::Sticky(_)) => {
                engine.sink.rollback_effect_transaction();
                unreachable!("flow optimizer replay reached an unplanned breakpoint");
            }
            Err(ComposeStop::Error(error)) => {
                engine.sink.commit_effect_transaction();
                return Err(error);
            }
        };

        if work.done() {
            trace.finish_flow();
        }

        if let Some(page_plan) = page_plan {
            if choices.used_len() != page_plan.choices.len() {
                engine.sink.rollback_effect_transaction();
                unreachable!("flow optimizer replay left breakpoints unconsumed");
            }
            let actual = OutcomeFingerprint::new(&work, &trace);
            if actual != page_plan.expected {
                engine.sink.rollback_effect_transaction();
                unreachable!("flow optimizer replay diverged from its continuation");
            }
        }

        let complete = work.finished(regions);
        if complete && plan.is_some_and(|plan| page + 1 != plan.pages.len()) {
            engine.sink.rollback_effect_transaction();
            unreachable!("flow optimizer replay completed before consuming its plan");
        }

        engine.sink.commit_effect_transaction();
        finished.push(frame);

        // Terminate the loop when everything is processed, though draining the
        // backlog if necessary.
        if complete {
            return Ok(Fragment::frames(finished));
        }

        regions.next();
        trace.advance_region();
        page += 1;
    }
}

/// Lazily reserves the sequential locator hierarchy for outer regions.
///
/// Search can visit region indices out of order and repeatedly. Retaining the
/// original sequential locators preserves the pre-optimizer location scheme
/// without rebuilding the whole prefix for every transition.
struct PageLocators<'a> {
    state: RefCell<PageLocatorState<'a>>,
}

struct PageLocatorState<'a> {
    split: SplitLocator<'a>,
    values: Vec<Locator<'a>>,
}

impl<'a> PageLocators<'a> {
    fn new(locator: &Locator<'a>) -> Self {
        Self {
            state: RefCell::new(PageLocatorState {
                split: locator.relayout().split(),
                values: vec![],
            }),
        }
    }

    fn get(&self, page: usize) -> Locator<'a> {
        let mut state = self.state.borrow_mut();
        let PageLocatorState { split, values } = &mut *state;
        while values.len() <= page {
            values.push(split.next(&()));
        }
        values[page].relayout()
    }
}

#[derive(Clone)]
struct SearchNode<'a, 'b, 'r> {
    key: Arc<SearchKey>,
    work: Work<'a, 'b>,
    trace: StickyTrace,
    regions: Regions<'r>,
    outer: usize,
    quality: PlanQuality,
    predecessor: Option<(usize, PagePlan)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct SearchKey {
    outer: usize,
    regions: RegionsKey,
    work: Arc<WorkKey>,
    sticky: StickyState,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct ContinuationKey {
    regions: RegionsKey,
    work: Arc<WorkKey>,
    sticky: StickyState,
}

impl SearchKey {
    fn continuation(&self) -> ContinuationKey {
        ContinuationKey {
            regions: self.regions.clone(),
            work: self.work.clone(),
            sticky: self.sticky.clone(),
        }
    }
}

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
struct PlanQuality {
    detached: u32,
    regions: u32,
    optional_breaks: u32,
}

impl Ord for PlanQuality {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.detached, self.regions, self.optional_breaks).cmp(&(
            other.detached,
            other.regions,
            other.optional_breaks,
        ))
    }
}

impl PartialOrd for PlanQuality {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone)]
struct PageOutcome<'a, 'b> {
    work: Work<'a, 'b>,
    trace: StickyTrace,
    plan: PagePlan,
}

enum PageAttempt<'a, 'b> {
    Complete(Box<PageOutcome<'a, 'b>>),
    Choice(StickyObservation),
}

fn optimize_flow(
    engine: &mut Engine,
    children: &[Child<'_>],
    config: &Config,
    locators: &PageLocators,
    regions: Regions<'_>,
) -> SourceResult<BreakPlan> {
    // Establish a deterministic upper bound first. Natural continuation is
    // always enumerated before an optional migration, so exact ties retain
    // the current maximally-packed behavior.
    let greedy = greedy_flow(engine, children, config, locators, regions);
    let (greedy_plan, mut incumbent, mut first_error) = match greedy {
        Ok((plan, quality)) => (Some(plan), quality, None),
        Err(error) => (
            None,
            PlanQuality {
                detached: u32::MAX,
                regions: u32::MAX,
                optional_breaks: u32::MAX,
            },
            Some(error),
        ),
    };

    let work = Work::new(children);
    let trace = StickyTrace::default();
    let initial_key = Arc::new(SearchKey {
        outer: 0,
        regions: RegionsKey::from(regions),
        work: Arc::new(work.key()),
        sticky: trace.state.clone(),
    });
    let initial = SearchNode {
        key: initial_key.clone(),
        work,
        trace,
        regions,
        outer: 0,
        quality: PlanQuality::default(),
        predecessor: None,
    };

    let mut nodes = vec![initial];
    let mut table = FxHashMap::default();
    table.insert(initial_key.clone(), 0_usize);
    // Search omits line-number decoration, so the page locator is not observed
    // during composition. Reaching an otherwise identical continuation later
    // in a repeating region is therefore dominated by the better-quality
    // arrival. This is the active-window bound for long sticky chains.
    let mut continuations = FxHashMap::default();
    continuations.insert(
        initial_key.continuation(),
        (PlanQuality::default(), 0_usize),
    );
    let mut active = VecDeque::from([0_usize]);
    let mut queued = vec![true];
    let mut transitions: FxHashMap<
        (Arc<SearchKey>, StickyScore),
        SourceResult<Vec<PageOutcome<'_, '_>>>,
    > = FxHashMap::default();
    let mut terminal = None;

    while let Some(node_id) = active.pop_front() {
        queued[node_id] = false;
        let node = nodes[node_id].clone();
        if continuations.get(&node.key.continuation())
            != Some(&(node.quality, node_id))
        {
            continue;
        }
        let transition_key = (node.key.clone(), node.trace.score);
        let transition = if let Some(cached) = transitions.get(&transition_key) {
            cached.clone()
        } else {
            let result = enumerate_page(
                engine,
                &node.work,
                &node.trace,
                config,
                locators,
                node.regions,
                node.outer,
            );
            transitions.insert(transition_key, result.clone());
            result
        };
        let outcomes = match transition {
            Ok(outcomes) => outcomes,
            Err(error) => {
                first_error.get_or_insert(error);
                continue;
            }
        };

        for outcome in outcomes {
            let optional = outcome.plan.optional_breaks();
            let quality = PlanQuality {
                detached: outcome.trace.score.detached,
                regions: node
                    .quality
                    .regions
                    .saturating_add(config.columns.count.try_into().unwrap_or(u32::MAX)),
                optional_breaks: node
                    .quality
                    .optional_breaks
                    .saturating_add(optional.try_into().unwrap_or(u32::MAX)),
            };

            let complete = outcome.work.finished(node.regions);

            if !complete && quality >= incumbent {
                continue;
            }

            let mut next_regions = node.regions;
            let mut next_trace = outcome.trace;
            if !complete {
                next_regions.next();
                next_trace.advance_region();
            }

            let key = Arc::new(SearchKey {
                outer: node.outer + 1,
                regions: RegionsKey::from(next_regions),
                work: outcome.plan.expected.work.clone(),
                sticky: next_trace.state.clone(),
            });
            let successor = SearchNode {
                key: key.clone(),
                work: outcome.work,
                trace: next_trace,
                regions: next_regions,
                outer: node.outer + 1,
                quality,
                predecessor: Some((node_id, outcome.plan)),
            };

            let continuation = key.continuation();
            if continuations
                .get(&continuation)
                .is_some_and(|(best, _)| *best <= quality)
            {
                continue;
            }

            let (successor_id, accepted) = if let Some(&existing) = table.get(&key) {
                if quality < nodes[existing].quality {
                    nodes[existing] = successor;
                    (existing, true)
                } else {
                    (existing, false)
                }
            } else {
                let id = nodes.len();
                nodes.push(successor);
                queued.push(false);
                table.insert(key, id);
                (id, true)
            };

            if !accepted {
                continue;
            }

            continuations.insert(continuation, (quality, successor_id));

            if complete && quality < incumbent {
                incumbent = quality;
                terminal = Some(successor_id);
            } else if !complete && !queued[successor_id] {
                queued[successor_id] = true;
                active.push_back(successor_id);
            }
        }
    }

    let Some(mut node) = terminal else {
        // A successful greedy layout remains the winner when no strictly
        // better continuation was discovered. If it failed, alternatives were
        // still explored before we select its deterministic error for replay.
        if let Some(plan) = greedy_plan {
            return Ok(plan);
        }
        let Some(error) = first_error else {
            unreachable!("flow optimizer found neither a terminal layout nor an error");
        };
        return Err(error);
    };

    let mut pages = vec![];
    while let Some((previous, page)) = nodes[node].predecessor.clone() {
        pages.push(page);
        node = previous;
    }
    pages.reverse();
    Ok(BreakPlan { pages })
}

fn greedy_flow(
    engine: &mut Engine,
    children: &[Child<'_>],
    config: &Config,
    locators: &PageLocators,
    mut regions: Regions,
) -> SourceResult<(BreakPlan, PlanQuality)> {
    let mut work = Work::new(children);
    let mut trace = StickyTrace::default();
    let mut pages = vec![];
    let mut outer = 0;

    loop {
        let mut choices = vec![];
        let outcome = loop {
            match replay_page(
                engine,
                &work,
                &trace,
                config,
                locators.get(outer),
                regions,
                &choices,
            )? {
                PageAttempt::Complete(outcome) => break outcome,
                PageAttempt::Choice(observation) => {
                    insert_choice(
                        &mut choices,
                        BreakChoice { observation, migrate: false },
                    );
                }
            }
        };

        work = outcome.work;
        trace = outcome.trace;
        pages.push(outcome.plan);

        if work.finished(regions) {
            return Ok((
                BreakPlan { pages },
                PlanQuality {
                    detached: trace.score.detached,
                    regions: outer
                        .saturating_add(1)
                        .saturating_mul(config.columns.count)
                        .try_into()
                        .unwrap_or(u32::MAX),
                    optional_breaks: 0,
                },
            ));
        }

        regions.next();
        trace.advance_region();
        outer += 1;
    }
}

fn enumerate_page<'a, 'b>(
    engine: &mut Engine,
    work: &Work<'a, 'b>,
    trace: &StickyTrace,
    config: &Config,
    locators: &PageLocators,
    regions: Regions,
    outer: usize,
) -> SourceResult<Vec<PageOutcome<'a, 'b>>> {
    // Page composition is itself a fixed-point transition machine. Explore
    // its optional break events with an explicit worklist so a long active
    // breakpoint window cannot overflow the Rust call stack. Push migration
    // first and natural continuation second so the latter is popped first.
    let mut active = vec![vec![]];
    let mut seen = FxHashSet::default();
    seen.insert(vec![]);
    let mut outcomes = vec![];
    let mut first_error = None;
    while let Some(choices) = active.pop() {
        match replay_page(
            engine,
            work,
            trace,
            config,
            locators.get(outer),
            regions,
            &choices,
        ) {
            Ok(PageAttempt::Complete(outcome)) => outcomes.push(*outcome),
            Ok(PageAttempt::Choice(observation)) => {
                let mut migrate = choices.clone();
                insert_choice(
                    &mut migrate,
                    BreakChoice {
                        observation: observation.clone(),
                        migrate: true,
                    },
                );
                if seen.insert(migrate.clone()) {
                    active.push(migrate);
                }

                let mut natural = choices;
                insert_choice(
                    &mut natural,
                    BreakChoice { observation, migrate: false },
                );
                if seen.insert(natural.clone()) {
                    active.push(natural);
                }
            }
            Err(error) => {
                first_error.get_or_insert(error);
            }
        }
    }

    if outcomes.is_empty() {
        let Some(error) = first_error else {
            unreachable!("flow page worklist produced no stable outcome or error");
        };
        return Err(error);
    }

    // Merge exact-equivalent page continuations before they reach the outer
    // table. This is the page-local analogue of Knuth--Plass predecessor
    // dominance.
    let mut deduplicated: FxHashMap<(Arc<WorkKey>, StickyState), usize> =
        FxHashMap::default();
    let mut merged: Vec<PageOutcome> = vec![];
    for outcome in outcomes {
        let key = (outcome.plan.expected.work.clone(), outcome.trace.state.clone());
        let optional = outcome.plan.optional_breaks();
        if let Some(&index) = deduplicated.get(&key) {
            let current = &merged[index];
            let current_optional = current.plan.optional_breaks();
            if (outcome.trace.score.detached, optional)
                < (current.trace.score.detached, current_optional)
            {
                merged[index] = outcome;
            }
        } else {
            let index = merged.len();
            deduplicated.insert(key, index);
            merged.push(outcome);
        }
    }
    Ok(merged)
}

fn replay_page<'a, 'b>(
    engine: &mut Engine,
    entry_work: &Work<'a, 'b>,
    entry_trace: &StickyTrace,
    config: &Config,
    locator: Locator,
    regions: Regions,
    choices: &[BreakChoice],
) -> SourceResult<PageAttempt<'a, 'b>> {
    let mut work = entry_work.clone();
    let mut trace = entry_trace.clone();
    let mut sticky_choices = StickyChoices::new(choices);
    let transaction = EffectTransaction::fresh();
    engine.sink.begin_effect_transaction_with(transaction);
    let result = compose(
        engine,
        &mut work,
        config,
        locator,
        regions,
        ComposeContext {
            attempt: config.next_attempt(),
            choices: &mut sticky_choices,
            trace: &mut trace,
            effects: transaction,
            line_numbers: false,
        },
    );
    // Search transitions retain tracked dependencies and introspection
    // descriptors, but never their user-visible effects.
    engine.sink.rollback_effect_transaction();
    match result {
        Ok(_) => {
            if work.done() {
                trace.finish_flow();
            }
            let expected = OutcomeFingerprint::new(&work, &trace);
            Ok(PageAttempt::Complete(Box::new(PageOutcome {
                work,
                trace,
                plan: PagePlan {
                    choices: sticky_choices.used_choices(),
                    expected,
                },
            })))
        }
        Err(ComposeStop::Sticky(observation)) => Ok(PageAttempt::Choice(observation)),
        Err(ComposeStop::Error(error)) => Err(error),
    }
}

/// Determine the flow's configuration.
fn configuration<'x>(
    shared: StyleChain<'x>,
    regions: Regions,
    column: ColumnOptions,
    mode: FlowMode,
    path: FlowPathId,
    tail_breaks: bool,
) -> Config<'x> {
    Config {
        mode,
        shared,
        path,
        tail_breaks,
        optimize: false,
        next_attempt: Cell::new(0),
        columns: {
            let mut count = column.count.get();
            if !regions.size.x.is_finite() {
                count = 1;
            }

            let gutter = column.gutter.relative_to(regions.base().x);
            let width = (regions.size.x - gutter * (count - 1) as f64) / count as f64;
            let dir = shared.resolve(TextElem::dir);
            ColumnConfig {
                count,
                width,
                gutter,
                dir,
                balanced: column.balanced,
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

/// The work that is left to do by flow layout.
///
/// The lifetimes 'a and 'b are used across flow layout:
/// - 'a is that of the content coming out of realization
/// - 'b is that of the collected/prepared children
#[derive(Clone)]
struct Work<'a, 'b> {
    /// All collected children. Stable child identities are indices into this
    /// slice.
    children: &'b [Child<'a>],
    /// Index of the first unprocessed child.
    cursor: usize,
    /// Leftovers from a breakable block.
    spill: Option<MultiSpill<'a, 'b>>,
    /// Queued floats that didn't fit in previous regions.
    floats: EcoVec<ChildId>,
    /// Queued footnotes that didn't fit in previous regions.
    footnotes: EcoVec<Packed<FootnoteElem>>,
    /// Spilled frames of a footnote that didn't fully fit. Similar to `spill`.
    footnote_spill: Option<FootnoteSpill>,
    /// Queued tags that will be attached to the next frame.
    tags: EcoVec<ChildId>,
    /// Identifies floats and footnotes that can be skipped if visited because
    /// they were already handled and incorporated as column or page level
    /// insertions.
    skips: Rc<FxHashSet<Location>>,
}

impl<'a, 'b> Work<'a, 'b> {
    /// Create the initial work state from a list of children.
    fn new(children: &'b [Child<'a>]) -> Self {
        Self {
            children,
            cursor: 0,
            spill: None,
            floats: EcoVec::new(),
            footnotes: EcoVec::new(),
            footnote_spill: None,
            tags: EcoVec::new(),
            skips: Rc::new(FxHashSet::default()),
        }
    }

    /// Get the first unprocessed child, from the start of the slice.
    fn head(&self) -> Option<(ChildId, &'b Child<'a>)> {
        self.children
            .get(self.cursor)
            .map(|child| (ChildId::new(self.cursor), child))
    }

    /// Mark the `head()` child as processed, advancing the slice by one.
    fn advance(&mut self) {
        self.cursor += 1;
    }

    /// Whether all work is done. This means we can terminate flow layout.
    fn done(&self) -> bool {
        self.cursor == self.children.len()
            && self.spill.is_none()
            && self.floats.is_empty()
            && self.footnote_spill.is_none()
            && self.footnotes.is_empty()
    }

    /// Whether layout may stop after the current region.
    fn finished(&self, regions: Regions) -> bool {
        self.done() && (!regions.expand.y || regions.backlog.is_empty())
    }

    /// Add skipped floats and footnotes from the insertion areas to the skip
    /// set.
    fn extend_skips(&mut self, skips: &[Location]) {
        if !skips.is_empty() {
            Rc::make_mut(&mut self.skips).extend(skips.iter().copied());
        }
    }

    fn child(&self, id: ChildId) -> &'b Child<'a> {
        &self.children[id.index()]
    }

    fn tag(&self, id: ChildId) -> &'a Tag {
        let Child::Tag(tag) = self.child(id) else {
            unreachable!("pending tag identity does not refer to a tag child")
        };
        tag
    }

    fn placed(&self, id: ChildId) -> &'b PlacedChild<'a> {
        let Child::Placed(placed) = self.child(id) else {
            unreachable!("queued float identity does not refer to a placed child")
        };
        placed
    }

    fn key(&self) -> WorkKey {
        let mut skips: Vec<_> = self.skips.iter().copied().collect();
        skips.sort_by_key(|location| (*location).hash());

        WorkKey {
            cursor: self.cursor,
            spill: self.spill.as_ref().map(MultiSpill::key),
            floats: self.floats.to_vec(),
            footnotes: self
                .footnotes
                .iter()
                .map(|footnote| footnote.location().unwrap())
                .collect(),
            footnote_spill: self
                .footnote_spill
                .as_ref()
                .map(|spill| (spill.key.clone(), spill.next)),
            tags: self.tags.to_vec(),
            skips,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct WorkKey {
    cursor: usize,
    spill: Option<MultiSpillKey>,
    floats: Vec<ChildId>,
    footnotes: Vec<Location>,
    footnote_spill: Option<(FootnoteSpillKey, usize)>,
    tags: Vec<ChildId>,
    skips: Vec<Location>,
}

/// Options defining the column layout.
#[derive(Hash)]
pub struct ColumnOptions {
    /// The number of columns.
    pub count: NonZeroUsize,
    /// Whether column heights are to be equalized.
    pub balanced: bool,
    /// The spacing between columns.
    pub gutter: Rel<Abs>,
}

/// Shared configuration for the whole flow.
struct Config<'x> {
    /// Whether this is the root flow, which can host footnotes and line
    /// numbers.
    mode: FlowMode,
    /// The styles shared by the whole flow. This is used for footnotes and line
    /// numbers.
    shared: StyleChain<'x>,
    /// Stable namespace for optional break identities in this flow.
    path: FlowPathId,
    /// Whether ordinary legal boundaries are exposed to shape the tail of an
    /// enclosing breakable sticky block.
    tail_breaks: bool,
    /// Whether nested normal-content flows share this optimizer.
    optimize: bool,
    /// Supplies a fresh cache domain for each transactional replay.
    next_attempt: Cell<u64>,
    /// Settings for columns.
    columns: ColumnConfig,
    /// Settings for footnotes.
    footnote: FootnoteConfig,
    /// Settings for line numbers.
    line_numbers: Option<LineNumberConfig>,
}

impl Config<'_> {
    /// Allocate a new cache domain for a transactional replay.
    fn next_attempt(&self) -> LayoutAttempt {
        let current = self.next_attempt.get();
        self.next_attempt.set(
            current
                .checked_add(1)
                .expect("exhausted flow layout attempt identifiers"),
        );
        LayoutAttempt(current)
    }
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
