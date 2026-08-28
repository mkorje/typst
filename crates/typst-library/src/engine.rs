//! Definition of the central compilation context.

use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use comemo::internal::{Call as ComemoCall, Sink as ComemoSink, to_parts_ref};
use comemo::{Track, Tracked, TrackedMut};
use ecow::EcoVec;
use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use rustc_hash::{FxHashMap, FxHashSet};
use typst_syntax::{FileId, Span};
use typst_utils::{LazyHash, Protected};

use crate::diag::{HintedStrResult, SourceDiagnostic, SourceResult, StrResult, bail};
use crate::foundations::{NormalBindingGuard, Styles, Value};
use crate::introspection::{Introspect, Introspection, Introspector};
use crate::{Library, World};

/// Holds all data needed during compilation.
pub struct Engine<'a> {
    /// The compilation environment.
    pub world: Tracked<'a, dyn World + 'a>,
    /// Definition of Typst's standard library.
    ///
    /// Can be accessed via `world.library()`, but we fetch it once upfront
    /// because it's accessed so frequently and we want to avoid the overhead of
    /// the tracked call.
    pub library: &'a LazyHash<Library>,
    /// Provides access to information about the document.
    pub introspector: Protected<Tracked<'a, dyn Introspector + 'a>>,
    /// May hold a span that is currently under inspection.
    pub traced: Tracked<'a, Traced>,
    /// A pure sink for warnings, delayed errors, and spans under inspection.
    pub sink: TrackedMut<'a, Sink>,
    /// The route the engine took during compilation. This is used to detect
    /// cyclic imports and excessive nesting.
    pub route: Route<'a>,
}

impl<'a> Engine<'a> {
    /// Run speculative work with an isolated user-visible sink.
    ///
    /// Reads through tracked inputs are replayed into the enclosing constraints.
    /// Warnings, delayed errors, and traced values are returned separately.
    pub fn analyze<T>(&mut self, f: impl FnOnce(&mut Engine) -> T) -> (T, Sink) {
        let world = self.world;
        let introspector = *self
            .introspector
            .access("speculative analysis preserves all tracked reads");
        let traced = self.traced;

        let (world, world_parent) = to_parts_ref(world);
        let (introspector, introspector_parent) = to_parts_ref(introspector);
        let (traced, traced_parent) = to_parts_ref(traced);

        let world_journal = CallJournal::new();
        let introspector_journal = CallJournal::new();
        let traced_journal = CallJournal::new();
        let mut sink = Sink::new();

        let value = {
            let mut trial = Engine {
                world: world.track_with(&world_journal),
                library: self.library,
                introspector: Protected::from_raw(
                    introspector.track_with(&introspector_journal),
                ),
                traced: traced.track_with(&traced_journal),
                sink: sink.track_mut(),
                route: self.route.clone(),
            };
            f(&mut trial)
        };

        world_journal.replay(world_parent);
        introspector_journal.replay(introspector_parent);
        traced_journal.replay(traced_parent);

        debug_assert!(sink.effect_buffers.is_empty());

        (value, sink)
    }

    /// Handles a result without immediately terminating execution. Instead, it
    /// produces a delayed error that is only promoted to a fatal one if it
    /// remains by the end of the introspection loop.
    pub fn delay<T: Default>(&mut self, result: SourceResult<T>) -> T {
        match result {
            Ok(value) => value,
            Err(errors) => {
                self.sink.delayed_errors(errors);
                T::default()
            }
        }
    }

    /// Runs tasks on the engine in parallel.
    pub fn parallelize<P, I, T, U, F>(
        &mut self,
        iter: P,
        f: F,
    ) -> impl Iterator<Item = U> + use<P, I, T, U, F>
    where
        P: IntoIterator<IntoIter = I>,
        I: Iterator<Item = T>,
        T: Send,
        U: Send,
        F: Fn(&mut Engine, T) -> U + Send + Sync,
    {
        let Engine {
            world, introspector, traced, ref route, library, ..
        } = *self;

        // We collect into a vector and then call `into_par_iter` instead of
        // using `par_bridge` because it does not retain the ordering.
        let work: Vec<T> = iter.into_iter().collect();

        // Work in parallel.
        let mut pairs: Vec<(U, Sink)> = Vec::with_capacity(work.len());
        work.into_par_iter()
            .map(|value| {
                let mut sink = Sink::new();
                let mut engine = Engine {
                    world,
                    introspector,
                    traced,
                    sink: sink.track_mut(),
                    route: route.clone(),
                    library,
                };
                (f(&mut engine, value), sink)
            })
            .collect_into_vec(&mut pairs);

        // Apply the subsinks to the outer sink.
        for (_, sink) in &mut pairs {
            let sink = std::mem::take(sink);
            self.sink.extend(
                sink.introspections,
                sink.delayed,
                sink.warnings,
                sink.values,
            );
        }

        pairs.into_iter().map(|(output, _)| output)
    }

    /// Performs an introspection on the introspector and returns its result.
    ///
    /// As a side effect, the introspection is stored in the sink. If the
    /// document does not converge, the recorded introspections are used to
    /// determine the cause of non-convergence.
    pub fn introspect<I>(&mut self, introspection: I) -> I::Output
    where
        I: Introspect,
    {
        let introspector = *self.introspector.access("is okay since we're recording it");
        let output = introspection.introspect(self, introspector);
        self.sink.introspection(Introspection::new(introspection));
        output
    }

    /// Create a struct that implements [`crate::foundations::BindingGuard`].
    pub fn binding_guard(&'_ mut self, span: Span) -> NormalBindingGuard<'_, 'a> {
        NormalBindingGuard { engine: self, span }
    }
}

/// A replayable, exactly deduplicated journal for tracked calls.
///
/// Hashes only select a bucket. Calls in that bucket are compared for exact
/// equality before they are merged, so a hash collision cannot lose a
/// dependency.
struct CallJournal<C> {
    repr: Mutex<CallJournalRepr<C>>,
}

struct CallJournalRepr<C> {
    ordered: Vec<(C, u128)>,
    buckets: FxHashMap<u128, Vec<usize>>,
}

impl<C> CallJournal<C> {
    fn new() -> Self {
        Self {
            repr: Mutex::new(CallJournalRepr {
                ordered: vec![],
                buckets: FxHashMap::default(),
            }),
        }
    }
}

impl<C: ComemoCall> CallJournal<C> {
    fn replay(&self, parent: Option<&dyn ComemoSink<Call = C>>) {
        let Some(parent) = parent else { return };
        let repr = self.repr.lock().expect("tracked-call journal was poisoned");
        for (call, result) in &repr.ordered {
            parent.emit(call.clone(), *result);
        }
    }
}

impl<C: ComemoCall> ComemoSink for CallJournal<C> {
    type Call = C;

    fn emit(&self, call: C, result: u128) -> bool {
        let hash = typst_utils::hash128(&call);
        let mut repr = self.repr.lock().expect("tracked-call journal was poisoned");

        if let Some(indices) = repr.buckets.get(&hash) {
            for &index in indices {
                let (existing, existing_result) = &repr.ordered[index];
                if existing == &call {
                    assert_eq!(
                        *existing_result, result,
                        "tracked call returned different results during speculative analysis",
                    );
                    return false;
                }
            }
        }

        let index = repr.ordered.len();
        repr.ordered.push((call, result));
        repr.buckets.entry(hash).or_default().push(index);
        true
    }
}

static NEXT_EFFECT_TRANSACTION: AtomicUsize = AtomicUsize::new(0);

/// Identifies an active visible-effect transaction.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct EffectTransaction(usize);

impl EffectTransaction {
    /// Allocate a fresh transaction identifier.
    pub fn fresh() -> Self {
        let id = NEXT_EFFECT_TRANSACTION
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |id| id.checked_add(1))
            .expect("exhausted visible-effect transaction identifiers");
        Self(id)
    }
}

/// May hold a span that is currently under inspection.
#[derive(Default)]
pub struct Traced(Option<Span>);

impl Traced {
    /// Wraps a to-be-traced `Span`.
    ///
    /// Call `Traced::default()` to trace nothing.
    pub fn new(traced: Span) -> Self {
        Self(Some(traced))
    }
}

#[comemo::track]
impl Traced {
    /// Returns the traced span _if_ it is part of the given source file or
    /// `None` otherwise.
    ///
    /// We hide the span if it isn't in the given file so that only results for
    /// the file with the traced span are invalidated.
    pub fn get(&self, id: FileId) -> Option<Span> {
        if self.0.and_then(Span::id) == Some(id) { self.0 } else { None }
    }
}

/// A push-only sink for recorded introspections, delayed errors, warnings, and
/// traced values.
///
/// All tracked methods of this type are of the form `(&mut self, ..) -> ()`, so
/// in principle they do not need validation (though that optimization is not
/// yet implemented in comemo).
#[derive(Default, Clone)]
pub struct Sink {
    /// Introspections that were performed during compilation.
    introspections: EcoVec<Introspection>,
    /// Delayed errors: Those are errors that we can ignore until the last
    /// iteration. For instance, show rules may throw during earlier iterations
    /// because the introspector is not yet ready. We first ignore that and
    /// proceed with empty content and only if the error remains by the end
    /// of the last iteration, we promote it.
    delayed: EcoVec<SourceDiagnostic>,
    /// Warnings emitted during iteration.
    warnings: EcoVec<SourceDiagnostic>,
    /// Hashes of all warning's spans and messages for warning deduplication.
    warnings_set: FxHashSet<u128>,
    /// A sequence of traced values for a span.
    values: EcoVec<(Value, Option<Styles>)>,
    /// Nested buffers for speculative visible effects. Introspection descriptors
    /// are not buffered because discarded passes still influence the computation.
    effect_buffers: Vec<EffectBuffer>,
}

#[derive(Default, Clone)]
struct EffectBuffer {
    transaction: Option<EffectTransaction>,
    delayed: EcoVec<SourceDiagnostic>,
    warnings: EcoVec<SourceDiagnostic>,
    warnings_set: FxHashSet<u128>,
    values: EcoVec<(Value, Option<Styles>)>,
}

impl Sink {
    /// The maximum number of traced values.
    pub const MAX_VALUES: usize = 10;

    /// Create a new empty sink.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the introspections.
    pub fn introspections(&self) -> &[Introspection] {
        &self.introspections
    }

    /// Get the stored delayed errors.
    pub fn delayed(&mut self) -> EcoVec<SourceDiagnostic> {
        debug_assert!(self.effect_buffers.is_empty());
        std::mem::take(&mut self.delayed)
    }

    /// Get the stored warnings.
    pub fn warnings(self) -> EcoVec<SourceDiagnostic> {
        debug_assert!(self.effect_buffers.is_empty());
        self.warnings
    }

    /// Get the values for the traced span.
    pub fn values(self) -> EcoVec<(Value, Option<Styles>)> {
        debug_assert!(self.effect_buffers.is_empty());
        self.values
    }

    /// Extend from another sink.
    pub fn extend_from_sink(&mut self, other: Sink) {
        debug_assert!(other.effect_buffers.is_empty());
        self.extend(other.introspections, other.delayed, other.warnings, other.values);
    }

    /// Count values already stored at a merge destination. Each transaction
    /// retains its own quota because enclosing buffers may be rolled back or
    /// bypassed by a promotion. This intentionally permits up to `MAX_VALUES`
    /// transient values per active transaction while preserving the root cap.
    fn value_count_at(&self, target: Option<usize>) -> usize {
        self.values.len()
            + target.map_or(0, |index| self.effect_buffers[index].values.len())
    }

    fn active_value_count(&self) -> usize {
        self.value_count_at(self.effect_buffers.len().checked_sub(1))
    }

    fn active_delayed(&mut self) -> &mut EcoVec<SourceDiagnostic> {
        match self.effect_buffers.last_mut() {
            Some(buffer) => &mut buffer.delayed,
            None => &mut self.delayed,
        }
    }

    fn active_values(&mut self) -> &mut EcoVec<(Value, Option<Styles>)> {
        match self.effect_buffers.last_mut() {
            Some(buffer) => &mut buffer.values,
            None => &mut self.values,
        }
    }

    fn warning_seen(&self, hash: u128) -> bool {
        self.warnings_set.contains(&hash)
            || self
                .effect_buffers
                .last()
                .is_some_and(|buffer| buffer.warnings_set.contains(&hash))
    }

    fn push_warning(&mut self, hash: u128, warning: SourceDiagnostic) {
        match self.effect_buffers.last_mut() {
            Some(buffer) => {
                buffer.warnings_set.insert(hash);
                buffer.warnings.push(warning);
            }
            None => {
                self.warnings_set.insert(hash);
                self.warnings.push(warning);
            }
        }
    }

    fn merge_effect_buffer(&mut self, buffer: EffectBuffer, target: Option<usize>) {
        let EffectBuffer { delayed, warnings, values, .. } = buffer;
        match target {
            Some(index) => {
                let remaining =
                    Self::MAX_VALUES.saturating_sub(self.value_count_at(Some(index)));
                let target = &mut self.effect_buffers[index];
                target.delayed.extend(delayed);
                for warning in warnings {
                    let hash = typst_utils::hash128(&(&warning.span, &warning.message));
                    if !self.warnings_set.contains(&hash)
                        && target.warnings_set.insert(hash)
                    {
                        target.warnings.push(warning);
                    }
                }
                target.values.extend(values.into_iter().take(remaining));
            }
            None => {
                let remaining =
                    Self::MAX_VALUES.saturating_sub(self.value_count_at(None));
                self.delayed.extend(delayed);
                for warning in warnings {
                    let hash = typst_utils::hash128(&(&warning.span, &warning.message));
                    if self.warnings_set.insert(hash) {
                        self.warnings.push(warning);
                    }
                }
                self.values.extend(values.into_iter().take(remaining));
            }
        }
    }
}

#[comemo::track]
impl Sink {
    /// Start a nested visible-effect transaction.
    pub fn begin_effect_transaction(&mut self) {
        self.effect_buffers.push(EffectBuffer::default());
    }

    /// Start a transaction that can receive effects promoted from nested work.
    pub fn begin_effect_transaction_with(&mut self, transaction: EffectTransaction) {
        assert!(
            self.effect_buffers
                .iter()
                .all(|buffer| buffer.transaction != Some(transaction)),
            "started a duplicate visible-effect transaction",
        );
        self.effect_buffers.push(EffectBuffer {
            transaction: Some(transaction),
            ..EffectBuffer::default()
        });
    }

    /// Commit the innermost visible-effect transaction.
    pub fn commit_effect_transaction(&mut self) {
        let buffer = self
            .effect_buffers
            .pop()
            .expect("committed a visible-effect transaction that was not started");
        let target = self.effect_buffers.len().checked_sub(1);
        self.merge_effect_buffer(buffer, target);
    }

    /// Commit the innermost effect buffer directly to `target`.
    pub fn promote_effect_transaction(&mut self, target: EffectTransaction) {
        let buffer = self
            .effect_buffers
            .pop()
            .expect("promoted a visible-effect transaction that was not started");
        let target = self
            .effect_buffers
            .iter()
            .rposition(|buffer| buffer.transaction == Some(target))
            .expect("promoted visible effects to an inactive transaction");
        self.merge_effect_buffer(buffer, Some(target));
    }

    /// Roll back warnings, delayed errors, and traced values from the
    /// innermost transaction while retaining its introspection descriptors.
    pub fn rollback_effect_transaction(&mut self) {
        self.effect_buffers
            .pop()
            .expect("rolled back a visible-effect transaction that was not started");
    }

    /// Trace an introspection.
    pub fn introspection(&mut self, introspection: Introspection) {
        self.introspections.push(introspection);
    }

    /// Add a delayed error.
    pub fn delayed_error(&mut self, error: SourceDiagnostic) {
        self.active_delayed().push(error);
    }

    /// Add multiple delayed errors.
    pub fn delayed_errors(&mut self, errors: EcoVec<SourceDiagnostic>) {
        self.active_delayed().extend(errors);
    }

    /// Add a warning.
    pub fn warn(&mut self, warning: SourceDiagnostic) {
        // Check if warning is a duplicate.
        let hash = typst_utils::hash128(&(&warning.span, &warning.message));
        if !self.warning_seen(hash) {
            self.push_warning(hash, warning);
        }
    }

    /// Trace a value and optionally styles for the traced span.
    pub fn value(&mut self, value: Value, styles: Option<Styles>) {
        if self.active_value_count() < Self::MAX_VALUES {
            self.active_values().push((value, styles));
        }
    }

    /// Extend from parts of another sink.
    fn extend(
        &mut self,
        introspections: EcoVec<Introspection>,
        delayed: EcoVec<SourceDiagnostic>,
        warnings: EcoVec<SourceDiagnostic>,
        values: EcoVec<(Value, Option<Styles>)>,
    ) {
        self.introspections.extend(introspections);
        self.active_delayed().extend(delayed);
        for warning in warnings {
            self.warn(warning);
        }
        if let Some(remaining) = Self::MAX_VALUES.checked_sub(self.active_value_count()) {
            self.active_values().extend(values.into_iter().take(remaining));
        }
    }
}

/// The route the engine took during compilation. This is used to detect
/// cyclic imports and excessive nesting.
pub struct Route<'a> {
    /// The parent route segment, if present.
    ///
    /// This is used when an engine is created from another engine.
    // We need to override the constraint's lifetime here so that `Tracked` is
    // covariant over the constraint. If it becomes invariant, we're in for a
    // world of lifetime pain.
    outer: Option<Tracked<'a, Self, <Route<'static> as Track>::Call>>,
    /// This is set if this route segment was inserted through the start of a
    /// module evaluation.
    id: Option<FileId>,
    /// This is set whenever we enter a function, nested layout, or are applying
    /// a show rule. The length of this segment plus the lengths of all `outer`
    /// route segments make up the length of the route. If the length of the
    /// route exceeds `MAX_DEPTH`, then we throw a "maximum ... depth exceeded"
    /// error.
    len: usize,
    /// The upper bound we've established for the parent chain length.
    ///
    /// We don't know the exact length (that would defeat the whole purpose
    /// because it would prevent cache reuse of some computation at different,
    /// non-exceeding depths).
    upper: AtomicUsize,
}

impl<'a> Route<'a> {
    /// Create a new, empty route.
    pub fn root() -> Self {
        Self {
            id: None,
            outer: None,
            len: 0,
            upper: AtomicUsize::new(0),
        }
    }

    /// Extend the route with another segment with a default length of 1.
    pub fn extend(outer: Tracked<'a, Self>) -> Self {
        Route {
            outer: Some(outer),
            id: None,
            len: 1,
            upper: AtomicUsize::new(usize::MAX),
        }
    }

    /// Attach a file id to the route segment.
    pub fn with_id(self, id: FileId) -> Self {
        Self { id: Some(id), ..self }
    }

    /// Set the length of the route segment to zero.
    pub fn unnested(self) -> Self {
        Self { len: 0, ..self }
    }

    /// Start tracking this route.
    ///
    /// In comparison to [`Track::track`], this method skips this chain link
    /// if it does not contribute anything.
    pub fn track(&self) -> Tracked<'_, Self> {
        match self.outer {
            Some(outer) if self.id.is_none() && self.len == 0 => outer,
            _ => Track::track(self),
        }
    }

    /// Increase the nesting depth for this route segment.
    pub fn increase(&mut self) {
        self.len += 1;
    }

    /// Decrease the nesting depth for this route segment.
    pub fn decrease(&mut self) {
        self.len -= 1;
    }
}

/// The maximum nesting depths. They are different so that even if show rule and
/// call checks are interleaved, for show rule problems we always get the show
/// rule error. The lower the max depth for a kind of error, the higher its
/// precedence compared to the others.
impl Route<'_> {
    /// The maximum stack nesting depth.
    const MAX_SHOW_RULE_DEPTH: usize = 64;

    /// The maximum layout nesting depth.
    const MAX_LAYOUT_DEPTH: usize = 72;

    /// The maximum HTML nesting depth.
    const MAX_HTML_DEPTH: usize = 72;

    /// The maximum function call nesting depth.
    const MAX_CALL_DEPTH: usize = 80;

    /// Ensures that we are within the maximum show rule depth.
    pub fn check_show_depth(&self) -> HintedStrResult<()> {
        if !self.within(Route::MAX_SHOW_RULE_DEPTH) {
            bail!(
                "maximum show rule depth exceeded";
                hint: "maybe a show rule matches its own output";
                hint: "maybe there are too deeply nested elements";
            );
        }
        Ok(())
    }

    /// Ensures that we are within the maximum layout depth.
    pub fn check_layout_depth(&self) -> HintedStrResult<()> {
        if !self.within(Route::MAX_LAYOUT_DEPTH) {
            bail!(
                "maximum layout depth exceeded";
                hint: "try to reduce the amount of nesting in your layout";
            );
        }
        Ok(())
    }

    /// Ensures that we are within the maximum HTML depth.
    pub fn check_html_depth(&self) -> HintedStrResult<()> {
        if !self.within(Route::MAX_HTML_DEPTH) {
            bail!(
                "maximum HTML depth exceeded";
                hint: "try to reduce the amount of nesting of your HTML";
            );
        }
        Ok(())
    }

    /// Ensures that we are within the maximum function call depth.
    pub fn check_call_depth(&self) -> StrResult<()> {
        if !self.within(Route::MAX_CALL_DEPTH) {
            bail!("maximum function call depth exceeded");
        }
        Ok(())
    }
}

#[comemo::track]
#[expect(clippy::elidable_lifetime_names, reason = "required for `comemo::track`")]
impl<'a> Route<'a> {
    /// Whether the given id is part of the route.
    pub fn contains(&self, id: FileId) -> bool {
        self.id == Some(id) || self.outer.is_some_and(|outer| outer.contains(id))
    }

    /// Whether the route's depth is less than or equal to the given depth.
    pub fn within(&self, depth: usize) -> bool {
        // We only need atomicity and no synchronization of other operations, so
        // `Relaxed` is fine.
        use Ordering::Relaxed;

        let upper = self.upper.load(Relaxed);
        if upper.saturating_add(self.len) <= depth {
            return true;
        }

        match self.outer {
            Some(_) if depth < self.len => false,
            Some(outer) => {
                let within = outer.within(depth - self.len);
                if within && depth < upper {
                    // We don't want to accidentally increase the upper bound,
                    // hence the compare-exchange.
                    self.upper.compare_exchange(upper, depth, Relaxed, Relaxed).ok();
                }
                within
            }
            None => true,
        }
    }
}

impl Default for Route<'_> {
    fn default() -> Self {
        Self::root()
    }
}

impl Clone for Route<'_> {
    fn clone(&self) -> Self {
        Self {
            outer: self.outer,
            id: self.id,
            len: self.len,
            upper: AtomicUsize::new(self.upper.load(Ordering::Relaxed)),
        }
    }
}
