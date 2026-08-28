The complete solution should be an exact shortest-path/DP optimizer over resumable flow-layout states, followed by one deterministic replay of the winning break plan. It should not construct or mutate a candidate-local introspector.

That differs from a literal Knuth–Plass port. Knuth–Plass can identify a state by its last breakpoint. Typst’s future layout also depends on spill state, queued floats and footnotes, insertion relayout, columns, balancing, and the exact remaining `Regions`. Those fields must be part of the DP state.

One complexity correction: \(O(\min(nw,n^2))\) is polynomial, not exponential. Typst can retain that bound in the insertion-free case where each breakpoint has one continuation state. With arbitrary floats, footnotes, breakable containers, and contextual layout, an exact optimizer can have exponentially many distinct continuation states in pathological documents. No exact implementation can promise \(O(n^2)\) while silently merging states that can have different futures. The practical goal should be the same as Typst’s line breaker: exact search, a good incumbent, aggressive safe dominance, a small active window, and near-linear behavior on normal documents.

## What is wrong with the current mechanism

The present pipeline is:

- [`layout_flow`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/mod.rs:194) collects children, then calls `compose` greedily once per outer region.
- [`Composer`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/compose.rs:65) owns the page/column insertion fixed points.
- [`Distributor`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/distribute.rs:35) walks children until the current region ends.
- Its sticky implementation stores one [`DistributionSnapshot`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/distribute.rs:79), then rolls back that one suffix at finalization.
- [`Work`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/mod.rs:301) contains much more continuation state than just the next child.
- [`MultiSpill`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/collect.rs:545) repeatedly relays out the whole breakable block and skips previously emitted frames. Its own comment correctly says this is not fully correct when later regions can alter earlier fragments.
- [`CachedCell`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/collect.rs:677) caches only by layout input and can bypass both comemo dependency replay and `Sink` effects during a later winning replay.

The one-snapshot scheme has three fundamental limitations:

1. It cannot retain and compare several possible sticky suffixes.
2. It cannot merge equivalent alternatives, so generalizing it recursively becomes exponential.
3. A snapshot of only `Work`, `items.len()`, and `used` is insufficient once floats, footnotes, balancing, and sticky-quality state are part of the decision.

The local `sticky` branch is a useful prototype: its whole-flow replay, reserved relayout locator, `LayoutAttempt`, and explicit sticky trace are all good ideas. But its recursive boolean-choice stream is still an exponential search, its child identity is pointer-based, and it cannot perform DP dominance over equivalent continuation states.

## Define “optimal” precisely

The optimizer needs a stable objective before its implementation can be judged.

A non-empty sticky block creates a keep edge from the region containing its last non-empty fragment to the region containing the first non-empty subsequent in-flow child:

```text
tail(sticky block) ──keep──> head(next non-empty in-flow child)
```

The following do not count as the target:

- Tags.
- Relative or fractional spacing.
- Absolute placed content.
- Floating content.
- Footnote insertions.
- Empty frames.

For consecutive sticky blocks `A`, `B`, and `C`, the edges are `A → B`, `B → C`, and `C → next`. A column boundary counts as a region boundary, matching current flow behavior.

A breakable sticky block creates its edge only when the block completes. Its tail is the last non-empty fragment, not its first fragment. An empty sticky block creates no edge. A sticky block at the end of the flow has no target and therefore no violation.

A strong explicit page/column break is a forced separation. Record it independently for diagnostics, but exclude it from the optimizable penalty because no sticky decision can remove it.

I would use this lexicographic quality:

```rust
struct StickyQuality {
    /// Keep edges separated without an explicit forced break.
    detached: u32,

    /// Number of outer regions/columns emitted.
    regions: u32,

    /// Deliberate optional breaks taken by the optimizer.
    optional_breaks: u32,
}
```

Comparison is lexicographic in that order. On a complete equality, keep the first-discovered plan, with natural/greedy continuation enumerated first. That gives deterministic, maximally packed output without inventing an arbitrary raggedness measure.

This means Typst will accept blank space or an extra region when that is necessary to satisfy more sticky edges, but it will not move content gratuitously when the number of satisfied edges is unchanged.

Paragraph widow/orphan behavior should remain governed by `LineChild::need`. It should not be silently converted into weighted sticky penalties; that would be an unrelated public behavior change.

## The high-level architecture

```text
                         previous complete introspector
                                      │
                                      │ read-only
                                      ▼
collect children ──> speculative exact optimizer ──> BreakPlan
                              │
                              │ tracked-read journal
                              ▼
                    global convergence constraint

same children + BreakPlan ──> committed deterministic replay ──> Fragment
                                      │
                                      ▼
                         warnings/errors/traces/output
```

The global introspection loop in [`typst/src/lib.rs`](/home/mkorje/Documents/repositories/typst/crates/typst/src/lib.rs:133) remains unchanged. Iteration \(k+1\) lays out every candidate against the complete introspector from iteration \(k\). The selected layout creates the document introspector used by iteration \(k+2\).

A partial candidate introspector must not be built. It would be missing:

- Future pages.
- Other page runs, which are currently parallel.
- Marginal/header/footer content.
- Locations not visited by that candidate yet.
- Current-iteration state and counter updates that are intentionally only visible on the following global iteration.

It would also make layout depend on candidate visitation order, defeating memoization and Typst’s pure-layout model described in the downloaded [layout-model article](</home/mkorje/Documents/repositories/typst/TeX and Typst_ Layout Models _ Laurenz's Blog.html>).

## Core identities and state types

The current shrinking child slice must become an indexed cursor. Pointer identity is not stable enough for replay or plan serialization.

```rust
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct ChildId(u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct FlowPathId(u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct FlowRegionId {
    outer: u32,
    column: u16,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct LayoutAttempt(u64);
```

`FlowPathId` identifies nested flows. A root flow gets one path; entering a breakable content block derives another path from the parent path and `ChildId`.

The prepared flow should own a child slice and expose stable lookup:

```rust
struct PreparedFlow<'a> {
    path: FlowPathId,
    children: &'a [Child<'a>],
    has_sticky: bool,
}
```

The initial and recursively realized children can use one flow-local arena:

```rust
struct FlowSession {
    realization: Arenas,
    prepared: Bump,
    next_path: Cell<u32>,
    next_attempt: Cell<u64>,
    states: RefCell<StateArena>,
}
```

The session lives for the full `layout_flow` call. Nested `BlockBody::Content` realizations allocate into the same session, so a resumable nested flow can safely outlive one call to `layout_multi_block` without self-referential owned structures or unsafe lifetime extension.

`Work` becomes:

```rust
#[derive(Clone)]
struct Work<'a> {
    flow: &'a PreparedFlow<'a>,
    next: usize,

    spill: Option<BlockSpill<'a>>,

    /// In source order, represented by stable child identities.
    floats: EcoVec<ChildId>,

    footnotes: EcoVec<QueuedFootnote>,
    footnote_spill: Option<FootnoteSpill>,

    tags: EcoVec<&'a Tag>,

    /// Ordered for canonicalization, plus a set for fast lookup.
    handled_insertions: HandledInsertions,
}

#[derive(Clone)]
struct QueuedFootnote {
    id: Location,
    elem: Packed<FootnoteElem>,
}

#[derive(Clone)]
struct FootnoteSpill {
    layout: FootnoteLayoutId,
    frames: Arc<[Frame]>,
    next: usize,
}
```

Replacing `std::vec::IntoIter<Frame>` with `Arc<[Frame]> + next` makes the spill clonable and canonically identifiable.

`HandledInsertions` should retain insertion order as well as set membership:

```rust
#[derive(Clone)]
struct HandledInsertions {
    order: EcoVec<Location>,
    set: Rc<FxHashSet<Location>>,
}
```

The order is deterministic and can be included in the continuation key; an unordered `FxHashSet` alone is unsuitable.

## Sticky tracking

Sticky quality must be updated from actual non-empty fragment placement, not from an estimated child height.

```rust
#[derive(Clone, Eq, PartialEq, Hash)]
struct StickyState {
    /// A completed sticky child waiting for its next non-empty in-flow child.
    pending: Option<PendingKeep>,

    /// A child currently being laid out, including a block spanning regions.
    active: Option<ActiveChild>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct PendingKeep {
    child: ChildId,

    /// Whether the sticky tail is in the region currently being distributed.
    tail_here: bool,

    /// A strong explicit break has made this edge unavoidably detached.
    forced: bool,
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct ActiveChild {
    child: ChildId,
    sticky: bool,
    saw_nonempty: bool,
    last_nonempty_here: bool,
}
```

Its transitions are exact:

- `begin_child(id, sticky)` creates `active`.
- On the active child’s first non-empty fragment, resolve `pending`:
  - `tail_here == true`: attached.
  - `tail_here == false && !forced`: add one avoidable detachment.
  - `forced`: add only the forced-detachment diagnostic count.
- Every non-empty fragment updates `active.last_nonempty_here = true`.
- On region advance, set both `pending.tail_here` and `active.last_nonempty_here` to false.
- On completion of a non-empty sticky child, replace `pending` with the active child’s tail.
- Completion of an empty child does not disturb an older pending edge.
- At flow completion, discard an unresolved pending edge because it has no target.

This compact `tail_here` representation is sufficient; the optimizer does not need an unbounded absolute region number in the continuation key.

The tracker must update only after footnote handling for a frame succeeds. A footnote can migrate the origin frame, so observing the frame before `Composer::footnotes` returns would record a placement that is subsequently rolled back.

Fractionally sized block frames are currently materialized during `Distributor::finalize`. Their `Item::Fr` entry must carry `ChildId`, and sticky observation must occur when that final frame is successfully laid out.

## Resumable breakable blocks

A complete solution cannot leave ordinary content blocks behind the present `MultiSpill` abstraction.

Consider a sticky breakable block whose greedy internal split fills a page exactly, leaving its following paragraph on the next page. A different legal internal split may put the block’s last few lines and the following paragraph together. Moving only the start of the entire block cannot represent that solution.

The replacement should be:

```rust
#[derive(Clone)]
enum BlockSpill<'a> {
    /// A normal `BlockBody::Content`, controlled by Typst flow layout.
    Flow(BlockContinuation<'a>),

    /// A native multi-layouter that only exposes Regions -> Fragment.
    Opaque(OpaqueSpill<'a>),
}

#[derive(Clone)]
struct BlockContinuation<'a> {
    child: ChildId,
    inner: FlowContinuation<'a>,
    regions: BlockRegionState,
    decoration: BlockDecoration<'a>,
    width_pass: WidthPass,
    saw_nonempty: bool,
}

#[derive(Clone)]
struct OpaqueSpill<'a> {
    child: ChildId,
    origin: &'a MultiChild<'a>,
    first: Abs,
    full: Abs,
    committed: EcoVec<Abs>,
    min_backlog_len: usize,
    next_frame: usize,
}
```

For `BlockBody::Content`, `layout_multi_block` should no longer produce the complete fragment immediately. It should:

1. Realize and collect the body into the flow session.
2. Build a nested `FlowContinuation`.
3. Map the outer region through width, height, and inset rules.
4. Ask that continuation for possible next-fragment outcomes.
5. Apply the block’s inset, clipping, fill/stroke, frame hardness, and label to each emitted fragment.

When an enclosing sticky block is still open, the nested flow exposes an additional class of legal breakpoint:

```rust
enum BreakClass {
    /// Existing break before a sticky child.
    StickyStart(ChildId),

    /// A legal internal break used only to shape the tail of an enclosing
    /// breakable sticky block.
    StickyTail(ChildId),
}
```

`StickyTail` opportunities are offered only at boundaries where the ordinary breaker is legally allowed to stop. They must respect widow/orphan grouping, unbreakable children, and explicit breaks. Their secondary optional-break cost ensures they are taken only when they improve a sticky edge or avoid an extra region.

For native `MultiLayouter` callbacks, `Regions -> Fragment` is the only behavior their API exposes. Treat that output as one opaque transition and retain an `OpaqueSpill`. The optimizer can move its enclosing block and correctly track its last fragment, but cannot invent internal alternatives the callback does not expose. That is a semantic API boundary rather than an optimizer approximation.

The current automatic-width consistency check in [`block.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/block.rs:100) becomes a fixed point on `BlockContinuation`:

- First obtain fragment outcomes without horizontal expansion.
- If their widths differ, restart that block continuation with the maximum width and `expand.x = true`.
- Roll back the block’s `Work`, sticky state, and visible effects.
- Continue only with the expanded outcome.

This removes the “relayout everything and skip committed frames” correctness problem for normal content blocks.

## Turn Distributor into a transition machine

The optimizer must use exactly the current layout operations. It should not build a TeX-like static list of boxes and estimated heights because Typst children are placement-aware functions of `Regions`.

Replace `Distributor::run` with a resumable kernel:

```rust
enum DistributionEvent<'a> {
    /// The distributor reached a legal optional breakpoint.
    Choice {
        id: BreakId,
        snapshot: DistributionState<'a>,
    },

    /// Ordinary overflow or end of available space.
    NaturalEnd(DistributionState<'a>),

    /// Explicit column/page break.
    ForcedEnd(DistributionState<'a>),

    /// A float or footnote changed an insertion area.
    Relayout {
        scope: PlacementScope,
        state: DistributionState<'a>,
    },

    /// All work completed in this region.
    Complete(DistributionState<'a>),

    Failed(EcoVec<SourceDiagnostic>),
}

#[derive(Clone)]
struct DistributionState<'a> {
    work: Work<'a>,
    items: PersistentItems<'a>,
    used: Size,
    sticky: StickyState,
    target: Option<Abs>,
    passed_choice: Option<BreakId>,
}
```

`run_until_event` executes the existing child logic until one event is reached.

When it reaches `Choice`, search has two successors:

- Finalize the current region at the snapshot.
- Mark that choice as passed and continue to the next event.

Render mode consults `BreakPlan` and follows exactly one successor.

The present `sticky` and `stickable` fields disappear. Natural finalization no longer performs an implicit sticky rollback. Every deliberate migration is an explicit chosen breakpoint.

The initial “all items are migratable” restoration remains, but its snapshot must include `StickyState` and any state changed by finalization. An empty early break is legal only if advancing the `Regions` sequence changes the available geometry. On an identical repeating region, it is rejected as a no-progress cycle.

`PersistentItems` can be a persistent vector or arena-backed predecessor chain. Search needs enough item state to preserve:

- Weak-spacing collapse.
- Used width and height.
- Fractional items that are resolved at finalization.
- The initial baseline.
- Whether all current items are migratable.

It does not need to retain rendered page frames after an outer-region outcome has been reduced to a continuation state. The winning plan is rendered once afterward.

## Make Composer state explicit

The hidden page/column fields in `Composer` must become cloneable search state:

```rust
#[derive(Clone)]
struct PageState<'a> {
    /// Restored when a parent-scoped insertion or balancing restarts the page.
    entry_work: Work<'a>,
    entry_sticky: StickyState,

    column: usize,
    work: Work<'a>,
    sticky: StickyState,

    page_insertions: Insertions<'a>,
    balancing_height: Option<Abs>,
    total_used_height: Abs,

    column_decisions: EcoVec<BreakChoice>,
}

#[derive(Clone)]
struct ColumnState<'a> {
    entry_work: Work<'a>,
    entry_sticky: StickyState,

    work: Work<'a>,
    sticky: StickyState,

    column_insertions: Insertions<'a>,
    footnote_queue: EcoVec<QueuedFootnote>,
    footnote_spill: Option<FootnoteSpill>,
}
```

This fixes the current awkward duplication where footnote queue/spill fields live partly in `Work` and partly in `Composer`.

The page enumerator is an internal worklist:

```rust
fn outer_region_outcomes(
    engine: &mut Engine,
    entry: FlowContinuation,
    region: Regions,
) -> SourceResult<Vec<RegionOutcome>>;
```

Its behavior is:

1. Start at column zero with empty page insertions.
2. Enumerate stable outcomes of that column.
3. For each stable column outcome, move to the next column.
4. A column float restarts the column from its entry checkpoint while retaining the enlarged column insertion state.
5. A parent float restarts the whole outer region from `entry_work` and `entry_sticky`, retaining the enlarged page insertion state.
6. After the last column, column balancing can restart the page with the computed target.
7. A stable final page produces `RegionOutcome`.
8. Deduplicate only states with exactly equal canonical keys.

Because each candidate follows an actual prefix, insertion causality is correct. If an early break occurs before a later float or footnote origin, that insertion is never discovered and cannot contaminate the earlier candidate. This is one of the main reasons snapshots taken after a global relayout are insufficient.

`Insertions::finalize` should be split into:

```rust
fn commit_state(&self, work: &mut Work);
fn render(self, config: &Config, inner: Frame, balance: Option<Abs>) -> Frame;
```

Search calls `commit_state` to update handled locations and queues without constructing the final visual page. Winning replay calls both.

Line numbers can be omitted during search because they do not alter available flow space. They are generated in committed replay and receive their normal introspection/effects there.

## The actual DP

The outer optimizer stores one node per exact continuation state at an outer-region boundary:

```rust
struct FlowNode {
    state: ContinuationId,
    quality: StickyQuality,
    predecessor: Option<Predecessor>,
}

struct Predecessor {
    node: NodeId,
    decision: RegionDecision,
}

struct RegionDecision {
    columns: EcoVec<BreakChoice>,
    expected_outcome: OutcomeFingerprint,
}

struct FlowOptimizer<'a> {
    arena: StateArena<'a>,
    table: FxHashMap<ContinuationKey, NodeId>,
    active: VecDeque<NodeId>,
    terminal: Option<NodeId>,
    incumbent: StickyQuality,
    transitions: TransitionCache,
}
```

A continuation key contains every field that can affect future layout:

```rust
struct ContinuationKey {
    region: RegionCursorKey,
    work: WorkKey,
    sticky: StickyState,
}

struct WorkKey {
    flow: FlowPathId,
    next: usize,
    spill: Option<BlockSpillKey>,
    floats: EcoVec<ChildId>,
    footnotes: EcoVec<Location>,
    footnote_spill: Option<(FootnoteLayoutId, usize)>,
    tags: EcoVec<TagKey>,
    handled_insertions: EcoVec<Location>,
}
```

Within the page worklist, the key additionally includes:

- Column number.
- Page-entry continuation.
- Page and column insertion keys.
- Balancing target.
- Total used height.
- Footnote queue/spill.
- Sticky state.

Insertion keys must include the stable origin location, alignment/scope, and the layout-result identity or exact region input that produced the frame.

Do not merge solely because two `u128` hashes match. Intern keys through hash buckets followed by exact equality. If an opaque native continuation cannot provide meaningful equality, assign it a fresh ID and do not merge it. Losing an optimization opportunity is safe; merging unequal futures is not.

The search is:

```text
1. Run the current greedy policy speculatively to obtain an incumbent plan.
2. Insert the initial continuation into the table.
3. Pop an active node.
4. Enumerate exact stable outcomes for the next outer region.
5. Add the outcome's sticky delta to the node's quality.
6. Reject no-progress repetitions.
7. Prune if the accumulated quality cannot beat the incumbent.
8. Intern the successor continuation.
9. If the key is new, add it.
10. If the key exists and the new quality is better, replace its predecessor.
11. At a complete-flow outcome, update the terminal/incumbent.
12. Reconstruct RegionDecisions through predecessor links.
```

This is the direct analogue of the table and predecessor chain in [`linebreak_optimized_bounded`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/inline/linebreak.rs:240), but its key is a full continuation rather than a text offset.

Safe pruning includes:

- The sticky-free fast path described below.
- Current detached count as a lower bound on every continuation.
- Region and optional-break counts once the primary cost equals the incumbent.
- Exact-key dominance.
- Transition caching by `(ContinuationId, RegionSignature, BreakId)`.
- Stopping breakpoint enumeration at the natural end of the region.
- Resetting the active window at strong explicit breaks.
- Rejecting an optional empty transition into an identical repeated region.
- Natural/greedy successors first, giving a strong incumbent early.

No beam width, candidate cap, timeout, or heuristic state merge should be part of the exact implementation.

## Introspection and transactional effects

The introspector itself remains fixed and read-only throughout one global iteration. However, there are two different classes of effects.

### Dependencies must include losing candidates

Suppose candidate A wins and candidate B loses because a query made B taller. On the newly produced document introspector, that query might make B shorter and optimal. If the query performed while evaluating B is discarded from the global constraint, Typst could falsely declare convergence.

Therefore, every tracked read that influenced the search—including reads in losing candidates—must be committed to the outer comemo dependency sink.

This should be intentional rather than an accidental consequence of sharing `Tracked` values. Add a replayable call journal:

```rust
struct CallJournal<C> {
    calls: Mutex<CallJournalRepr<C>>,
}

struct CallJournalRepr<C> {
    ordered: Vec<(C, u128)>,
    dedup: FxHashMap<C, u128>,
}

impl<C: comemo::Call> comemo::Sink for CallJournal<C> {
    type Call = C;

    fn emit(&self, call: C, result: u128) -> bool {
        // Deduplicate while preserving first-observation order.
    }
}
```

An optimizer transaction:

1. Uses `comemo::to_parts_ref` to get the raw `World`, `Introspector`, and `Traced` values plus their parent sinks.
2. Retracks those raw values into fresh call journals.
3. Uses a fresh Typst `Sink`.
4. Runs the complete search.
5. Replays the call journals into the parent comemo sinks.
6. Transfers the search sink’s high-level `Introspection` descriptors for non-convergence diagnostics.
7. Discards warnings, delayed errors, and traced values from search.

This means the global constraint accurately describes the entire argmin computation.

### User-visible effects come only from the winning layout

Warnings, delayed errors, and traced values from rejected candidates must not leak. Nor should they leak from a page/column attempt discarded because a float caused relayout.

The final deterministic replay should therefore use transactional visible effects:

- A page or column attempt gets an isolated visible-effect buffer.
- On `Relayout`, discard that buffer but keep its dependency journal and introspection descriptors.
- On stable finalization, merge the buffer into the parent `Sink`.
- If every candidate fails, reconstruct and replay the selected deterministic failing plan so the user sees the correct diagnostic from a real committed execution.

This is a cleaner version of the sink isolation already explored on the `sticky` branch.

### Fix local caches

`CachedCell` must distinguish effect epochs:

```rust
struct CachedCell<T> {
    slot: RefCell<Option<(LayoutAttempt, u128, T)>>,
}
```

`LayoutAttempt` is included only in this local one-entry cache key. It must not be added to the memoized `layout_*_impl` inputs, because the final replay should be able to hit comemo’s global memo cache and replay its dependencies and effects into the correct destination.

Allocate a new attempt ID for:

- Optimizer search.
- Committed replay.
- Any final-render attempt restarted after rolling back visible effects.

That guarantees a search-filled local cache cannot cause committed replay to bypass effect/dependency emission.

Stable locations are preserved by reserving one composition locator and using `relayout()` for every search/replay, as the prototype branch already does.

## Fast path

After collection:

```rust
if !prepared.has_sticky {
    return layout_flow_greedy(...);
}
```

Nested flows independently make the same decision. This preserves the present code path, output, and performance for the overwhelming majority of flows without sticky blocks.

A breakable sticky block activates the optimizer even if its nested body contains no sticky children, because its tail placement is itself an optimization obligation.

## File-by-file change plan

| File | Current role | Required change |
|---|---|---|
| [`flow/mod.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/mod.rs:194) | Collects and greedily composes regions; owns `Work` | Add `FlowSession`, stable IDs, indexed `Work`, sticky-free fast path, speculative optimization, committed replay, and plan validation |
| `flow/state.rs` (new) | — | Define continuations, canonical keys, state interning, insertion keys, sticky tracker, spill types, and attempt IDs |
| `flow/optimize.rs` (new) | — | Implement outer DP, page/column worklists, transition cache, incumbent/pruning, predecessor reconstruction, and exhaustive test mode |
| [`flow/distribute.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/distribute.rs:55) | Sequential distribution plus one sticky rollback | Remove `sticky`/`stickable`; accept `ChildId`; emit `DistributionEvent`; branch at `BreakId`; update `StickyState`; make snapshots complete |
| [`flow/compose.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/compose.rs:35) | Mutable page/column fixed points | Make page/column state explicit and clonable; expose outer-region outcomes; preserve insertion causality; split insertion state commit from rendering |
| [`flow/collect.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/collect.rs:355) | Builds child enum and local layout caches | Assign `ChildId`; record legal optional-break metadata; use attempt-aware cache; replace ordinary `MultiSpill` with resumable block preparation |
| [`flow/block.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/flow/block.rs:100) | Returns a complete fragment for breakable blocks | Add `BlockContinuation`, incremental region mapping and decoration, nested flow preparation, width-expansion restart, and opaque callback fallback |
| [`engine.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-library/src/engine.rs:18) | Owns tracked inputs and one push-only `Sink` | Add replayable analysis transactions and visible-effect transaction/checkpoint support |
| [`regions.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-library/src/layout/regions.rs:42) | Borrowed region sequence | Add an exact `RegionSignature`/cursor helper; do not change public region semantics |
| [`pages/run.rs`](/home/mkorje/Documents/repositories/typst/crates/typst-layout/src/pages/run.rs:57) | Invokes root flow for a page run | No algorithmic change; continue optimizing independently inside each explicit page run |
| [`typst/src/lib.rs`](/home/mkorje/Documents/repositories/typst/crates/typst/src/lib.rs:133) | Global introspection fixed point | No loop or iteration-count change initially; optimizer dependencies feed its existing constraint |

## Handling the difficult interactions

### Floats

Float order remains strict.

A float transition contains its `ChildId`, layout-result identity, alignment, scope, and location. Adding a column float restarts only the column checkpoint. Adding a parent float restarts the outer-region checkpoint. Insertion locations are monotonic within one fixed point, which gives termination.

Earlier-break candidates are finalized from states that never visited later float origins. They consequently cannot inherit those later insertions.

### Footnotes

Footnote origin and entry order remain unchanged.

The column state explicitly owns:

- Queued footnotes.
- Footnote spill.
- Separator state.
- Inserted footnote frames and their IDs.
- Locations already handled.

An unbreakable origin whose first note frame does not fit returns a natural-end candidate before the origin. A breakable origin queues/spills according to current rules. Every branch uses the existing footnote layout functions against its exact remaining regions.

A `FootnoteSpill` is identified by the pure footnote-layout input/result ID and its frame cursor. Independently produced spills that cannot be proven equivalent are not merged.

### Fractional spacing and blocks

Search finalization runs the same fractional allocation as committed rendering. A breakpoint does not use the pre-finalization `used` value as an estimate.

`Item::Fr` needs to carry `ChildId`, sticky metadata, and its cached layout input. Its eventual frame participates in footnote discovery and sticky observation.

### Column balancing

Balancing is part of `PageState`, not a post-optimization operation. When a completed-flow page produces a larger balancing target, restart that page state with:

- Page-entry `Work`.
- Page-entry `StickyState`.
- Retained parent insertions.
- New balancing height.
- Reset local quality and decisions from the discarded pass.

Different break plans can generate different balancing fixed points and must remain different states.

### Relative sizing and region backlog

Every block transition receives the complete actual `Regions` sequence from that candidate. `ContinuationKey` includes all committed region-map state needed by fixed-height and inset mapping.

No transition may be keyed only by current remaining height; `full`, `expand`, backlog, and `last` can all change layout.

### Errors

A fatal error makes that transition infeasible, not automatically the whole document. A different legal placement can legitimately avoid a region-dependent layout error.

If there is no successful terminal path, retain a deterministic failing predecessor chain and replay it with the real effect sink. This avoids exposing arbitrary errors from losing candidates.

### Explicit page/column breaks

Strong breaks are mandatory outcomes and terminate the local active set. Pending sticky edges become forced-detached.

Weak breaks retain their current “only if this region has content” behavior. Because `items` are part of the distribution state, their behavior remains path-specific and exact.

### Page-run boundaries

Page runs are split by explicit page configuration/break changes before flow layout and can continue to run in parallel. A sticky edge cannot be optimized across such a forced boundary; mark it forced-detached if applicable.

## Complexity and expected performance

Let:

- \(b\) be the number of reachable optional breakpoints.
- \(w\) be the maximum active breakpoint window that can fit into a region.
- \(s_i\) be the number of distinct continuation states at breakpoint \(i\).

In the simple case with no insertions, no nested alternative state, and one continuation per breakpoint:

\[
O(\min(bw,b^2))
\]

That is the Knuth–Plass-like behavior.

In the complete flow model:

\[
O(E), \qquad E \leq \sum_i s_i w
\]

The \(s_i\) term can be exponential in contrived input because different break choices can leave different queues, spills, and insertion fixed points. The implementation should say this honestly in its internal documentation.

Practical performance should still be good because:

- Sticky-free flows bypass the optimizer.
- Most headings create only one or two live alternatives.
- Float and footnote queues preserve order rather than arbitrary subsets.
- Exact-equivalent states merge.
- Parent/column insertions grow monotonically during relayout.
- The greedy pass supplies a strong upper bound.
- Natural overflow limits the breakpoint window.
- Block and transition layouts are memoized.
- Search discards finished page frames and retains only persistent predecessor/state data.
- Final rendering is one additional largely memo-hit pass.

Instrumentation should count visited states, transitions, dominance hits, transition-cache hits, maximum frontier size, and final replay time. Keep that instrumentation behind timing/debug support so adversarial regressions are visible.

## Correctness invariants

These should be documented beside the optimizer and asserted in debug builds:

1. Every candidate in one optimization reads the same previous global introspector.
2. Every tracked read used by candidate comparison reaches the enclosing dependency constraint.
3. Only the stable winning execution contributes warnings, delayed errors, and trace values.
4. Two nodes merge only when every future-affecting field is exactly equal.
5. Float and footnote source order never changes.
6. An insertion can occur only in a state that visited its origin.
7. Composer relayout restores `Work`, sticky state, local quality, plan cursor, and visible effects together.
8. A repeated-region transition must advance content/insertion state or be rejected.
9. Search and replay use the same locator hierarchy.
10. Replay must end with the `OutcomeFingerprint` recorded by the winning node.
11. No-sticky flows produce the same frames and sink effects as the current greedy path.
12. A normal content block never uses the old relayout-and-skip `MultiSpill` compatibility behavior.

A replay fingerprint mismatch should be an internal layout error in development, not silently fall back to a different plan. It indicates that search and render are not using the same transition kernel.

## Test plan

The current container tests around [`block-sticky`](/home/mkorje/Documents/repositories/typst/tests/suite/layout/container.typ:181) remain the baseline. The useful cases already developed on the `sticky-fix*` branches should be brought into the final test suite rather than discarded.

The semantic matrix should include:

- One sticky block fitting with its target.
- Consecutive chains where every edge fits.
- Chains where only an arbitrary middle suffix can be preserved.
- Chains larger than any region, proving there is no empty-region loop.
- Sticky at flow end.
- Empty sticky and empty target children.
- Tags, absolute placement, weak/strong spacing, and fractional spacing between sticky and target.
- Strong and weak column breaks.
- Finite region sequences with `last = None`.
- A smaller initial partial region followed by a larger full region.
- Zero, negative, oversized, fixed-relative, and fractional block heights.
- One, two, and many columns.
- Balanced and unbalanced columns.
- Column and parent floats at top/bottom/auto alignment.
- Queued floats and `place.flush`.
- Float containing a footnote.
- Footnote on the sticky block and on its target.
- First footnote frame migration.
- Nested footnotes, queued notes, spilled notes, and a separator that does not fit.
- A breakable sticky block whose optimal result requires a different internal tail split.
- Empty first fragments and later non-empty fragments.
- Nested sticky blocks and nested columns.

Introspection/effect tests are especially important:

- A losing candidate uses `counter(page)` and becomes the winner when the next introspector changes. The global loop must perform another iteration.
- A rejected candidate emits a warning; the final compilation must not.
- A rejected candidate emits a traced value; trace output must contain only the winning placement.
- A search cache entry is reused during final replay; the final global constraint must still contain the memoized introspection calls.
- `here().page()`, state, queries, citations, and page counters change a candidate’s size.
- A parent float relayout discards a pass containing a warning or delayed error.
- Stable locations remain identical across search and replay.

For algorithmic optimality, add a test-only exhaustive oracle:

- Disable dominance/pruning.
- Enumerate every legal optional choice for flows with at most roughly ten breakpoints.
- Compare its `StickyQuality` to the optimized DP.
- Run randomized combinations of lines, blocks, spaces, floats, footnotes, explicit breaks, and varying region heights.
- Separately compare the merged and unmerged state machines.

For performance:

- Benchmark a long document without sticky blocks; it must stay on the old path.
- Benchmark thousands of ordinary headings.
- Benchmark every child sticky.
- Benchmark long sticky chains with impossible attachments.
- Benchmark float/footnote-heavy page runs.
- Track state and transition counts, not only wall-clock time.

## Implementation order for one integrated change

I would implement this as one complete behavior change, with the following dependency order rather than shipping partial algorithms:

1. Introduce stable IDs, indexed `Work`, `FootnoteSpill`, canonical state keys, `StickyState`, and attempt-aware local caches.
2. Add dependency journaling and visible-effect transactions to `Engine`/`Sink`.
3. Refactor Distributor into the shared event-producing transition kernel while retaining a natural-only mode for equivalence testing.
4. Move page, column, insertion, footnote, and balancing checkpoints into explicit state structs.
5. Replace normal-content `MultiSpill` with `BlockContinuation` and add tail-shaping break opportunities.
6. Implement outer-region outcome enumeration and exact state interning.
7. Implement the bounded DP, greedy incumbent, predecessor reconstruction, and nested `BreakPlan`.
8. Add committed replay and outcome-fingerprint validation.
9. Enable the sticky-free fast path.
10. Land the exhaustive oracle, semantic fixtures, introspection/effect tests, and performance instrumentation.
11. Remove the obsolete one-snapshot sticky machinery and ordinary-content `MultiSpill`.

The most important design decisions are therefore:

- Use a generalized exact DP, not recursive whole-flow boolean replay.
- Treat the actual Composer/Distributor as the edge evaluator.
- Keep the introspector global and immutable during a compilation iteration.
- Journal dependencies from all evaluated alternatives.
- Commit user-visible effects only from the winning stable replay.
- Make normal breakable content resumable so sticky-tail optimization is genuine.
- Represent every future-affecting field in the continuation key.
- Retain a strictly unchanged fast path for flows without sticky content.

That produces a complete optimizer consistent with Typst’s placement-aware layout philosophy, rather than grafting a TeX-style box model onto code whose children can legitimately change when their region sequence changes.
