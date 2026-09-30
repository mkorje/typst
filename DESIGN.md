# Lazy Tracked Regions and Resumable Multi-Layout

## 1. Scope and goals

Typst currently has a mismatch between two layout models.

Flow layout is already incremental internally: it realizes and collects content once, keeps a `Work` value representing the remaining work, and repeatedly composes regions. Breakable child layouters, however, still receive an eager `Regions` value containing the complete known future region sequence and return an entire `Fragment`.

This mismatch is bridged by `MultiSpill`. A `MultiChild` first lays itself out completely, returns the first frame, and, if more frames exist, creates a `MultiSpill`. On each later region, `MultiSpill` reconstructs a larger historical `Regions`, reruns the complete child layout, and skips the frames which have already been emitted.

The source already notes that this compatibility mechanism is not fully correct: later regions can affect earlier frames. Issue #8487 provides a real example involving a broken table cell.

The proposed refactor has three goals:

1. `Regions` becomes a lazy, immutable, tracked description of the layout environment.
2. Breakable layouters become resumable computations which produce one stable frame plus semantic continuation state.
3. Layouters which genuinely need future-to-past computation, initially grid/table, hold provisional region drafts behind an internal stabilization frontier until those drafts are safe to publish.
4. Frames emitted against *predicted* future regions are verified once the actual regions are known, and flow recomposes earlier regions when they would change (§36). This is the correctness layer; it is prototyped and measured (§43) and does not depend on goals 1–3.

Flow itself will **not** become a resumable multi-layouter in this work. It continues to realize once, collect once, lay out the complete flow synchronously, and return a `Fragment`.

This is deliberate. It avoids changing the ownership model of `Arenas`, `Child<'a>`, `StyleChain<'a>`, `Locator<'a>`, and related data merely to allow `Work` to escape its current stack frame.

The four relevant concepts are therefore:

```text
Regions
    external geometry available to layout

MultiState
    semantic progress of one breakable child

LayoutFrontier
    internally computed output which is not safe to publish yet

Work
    flow's existing, synchronous state while composing regions
```

They should remain separate.

---

# 2. Current architecture being replaced

The current public region representation is approximately:

```rust
#[derive(Debug, Copy, Clone, Hash)]
pub struct Region {
    pub size: Size,
    pub expand: Axes<bool>,
}

#[derive(Copy, Clone, Hash)]
pub struct Regions<'a> {
    pub size: Size,
    pub expand: Axes<bool>,
    pub full: Abs,
    pub backlog: &'a [Abs],
    pub last: Option<Abs>,
}
```

and `Regions` is both:

* a description of the layout environment; and
* a mutable cursor through that environment.

For example:

```rust
regions.next();
```

mutates the same value from one region into the next.

A breakable block currently ultimately reaches:

```rust
pub fn layout_multi_block(
    elem: &Packed<BlockElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Regions,
) -> SourceResult<Fragment>;
```

Custom multi-layouters use:

```rust
pub fn multi_layouter<T: NativeElement>(
    captured: Packed<T>,
    f: fn(
        content: &Packed<T>,
        engine: &mut Engine,
        locator: Locator,
        styles: StyleChain,
        regions: Regions,
    ) -> SourceResult<Fragment>,
) -> Self;
```

and flow stores:

```rust
pub struct MultiChild<'a> {
    pub align: Axes<FixedAlignment>,
    pub sticky: bool,
    alone: bool,
    elem: &'a Packed<BlockElem>,
    styles: StyleChain<'a>,
    locator: Locator<'a>,
    cell: CachedCell<SourceResult<Fragment>>,
}
```

with continuation represented by:

```rust
pub struct MultiSpill<'a, 'b> {
    pub(super) exist_non_empty_frame: bool,
    multi: &'b MultiChild<'a>,
    first: Abs,
    full: Abs,
    backlog: Vec<Abs>,
    min_backlog_len: usize,
}
```

The central architectural change is that `MultiSpill` disappears.

---

# 3. New `Region` representation

A future region needs slightly more information than the current `Region`.

In particular, `full` belongs to the logical region itself. It is not always identical to the currently available height: fixed-height blocks can distribute one logical block height over several physical regions while retaining the fixed height as the basis for relative sizing.

The proposed type is therefore:

```rust
#[derive(Debug, Copy, Clone, Hash)]
pub struct Region {
    /// Available size in this region.
    pub size: Size,

    /// Whether content should expand along each axis.
    pub expand: Axes<bool>,

    /// Base height for relative sizing.
    ///
    /// For an ordinary fresh region this is normally `size.y`.
    /// For transformed regions it can differ.
    pub full: Abs,
}
```

The normal constructor retains today's behaviour:

```rust
impl Region {
    pub fn new(size: Size, expand: Axes<bool>) -> Self {
        Self {
            size,
            expand,
            full: size.y,
        }
    }

    pub fn with_full(
        size: Size,
        expand: Axes<bool>,
        full: Abs,
    ) -> Self {
        Self {
            size,
            expand,
            full,
        }
    }

    pub fn base(self) -> Size {
        Size::new(self.size.x, self.full)
    }
}
```

Existing single-region callers using:

```rust
Region::new(size, expand)
```

do not need to care about the additional field.

---

# 4. `Regions` becomes an immutable lazy view

`Regions` should no longer expose:

```rust
backlog
last
next()
```

as its fundamental representation.

Instead it is a view into a private region source.

A concrete proposed representation is:

```rust
#[derive(Copy, Clone)]
pub struct Regions<'a> {
    source: RegionSource<'a>,
    index: usize,
}
```

with a private source:

```rust
#[derive(Copy, Clone)]
enum RegionSource<'a> {
    /// Exactly one region.
    One(Region),

    /// One region repeated indefinitely.
    Repeat(Region),

    /// A finite sequence, optionally followed by an infinite repeat.
    Slice {
        regions: &'a [Region],
        repeat: Option<Region>,
    },

    /// A lazily computed source.
    Dynamic(&'a dyn RegionProvider),
}
```

A provider returns both geometry and whether that geometry represents a finite progression or an indefinitely repeated tail:

```rust
#[derive(Debug, Copy, Clone, Hash)]
pub(crate) enum RegionSlot {
    /// A concrete finite position in the region sequence.
    Finite(Region),

    /// A region repeated indefinitely from this position onward.
    Repeat(Region),

    /// No region exists at this position.
    End,
}

pub(crate) trait RegionProvider: Sync {
    fn region(&self, index: usize) -> RegionSlot;
}
```

`RegionSource` implements the same private operation:

```rust
impl RegionSource<'_> {
    fn region(self, index: usize) -> RegionSlot {
        match self {
            RegionSource::One(region) => {
                if index == 0 {
                    RegionSlot::Finite(region)
                } else {
                    RegionSlot::End
                }
            }

            RegionSource::Repeat(region) => {
                RegionSlot::Repeat(region)
            }

            RegionSource::Slice { regions, repeat } => {
                if let Some(&region) = regions.get(index) {
                    RegionSlot::Finite(region)
                } else if let Some(region) = repeat {
                    RegionSlot::Repeat(region)
                } else {
                    RegionSlot::End
                }
            }

            RegionSource::Dynamic(provider) => {
                provider.region(index)
            }
        }
    }
}
```

The distinction between `Finite` and `Repeat` is required for the existing semantics of `may_progress()`.

Moving to another finite region counts as progress even when its geometry happens to be identical. An indefinitely repeated region only provides progress if a fresh repeated region improves on the currently available geometry.

---

# 5. Tracked `Regions` API

The layout-facing methods are tracked:

```rust
#[comemo::track]
impl<'a> Regions<'a> {
    pub fn current(&self) -> Region {
        self.slot(0)
            .into_region()
            .expect("Regions must contain a current region")
    }

    pub fn size(&self) -> Size {
        self.current().size
    }

    pub fn expand(&self) -> Axes<bool> {
        self.current().expand
    }

    pub fn full(&self) -> Abs {
        self.current().full
    }

    pub fn base(&self) -> Size {
        self.current().base()
    }

    /// Obtain the region `offset` breaks from the current one.
    ///
    /// `peek(0)` is the current region.
    pub fn peek(&self, offset: usize) -> Option<Region> {
        self.slot(offset).into_region()
    }

    pub fn may_break(&self) -> bool {
        !matches!(self.slot(1), RegionSlot::End)
    }

    pub fn may_progress(&self) -> bool {
        let current = self.current();

        match self.slot(1) {
            RegionSlot::End => false,

            // Moving through a finite sequence is itself progress.
            RegionSlot::Finite(_) => true,

            // An infinite repeat only improves a lack of space if a
            // fresh repeated region has different available geometry.
            RegionSlot::Repeat(next) => {
                next.size != current.size
            }
        }
    }

    pub fn is_full(&self) -> bool {
        Abs::zero().fits(self.size().y)
            && self.may_progress()
    }
}
```

The private helper is not tracked independently:

```rust
impl Regions<'_> {
    fn slot(&self, offset: usize) -> RegionSlot {
        self.source.region(self.index + offset)
    }
}
```

Normal layout code receives:

```rust
Tracked<Regions<'_>>
```

rather than `Regions` directly.

For example:

```rust
#[comemo::memoize]
fn some_layout(
    ...,
    regions: Tracked<Regions<'_>>,
) -> SourceResult<...>
```

A layouter which only calls:

```rust
regions.size()
```

depends only on the current size.

A grid simulation which explicitly calls:

```rust
regions.peek(1);
regions.peek(2);
regions.peek(3);
```

records exactly that lookahead.

---

# 6. Region transformation is lazy

Current operations such as:

```rust
Regions::map(...)
```

and `breakable_pod` eagerly construct complete backlogs.

They should instead create private providers which map another tracked `Regions` lazily.

For example, padding can have:

```rust
struct PaddedRegions<'a> {
    outer: Tracked<
        'a,
        Regions<'a>,
        <Regions<'static> as Track>::Call,
    >,
    inset: Sides<Abs>,
}
```

with:

```rust
impl RegionProvider for PaddedRegions<'_> {
    fn region(&self, index: usize) -> RegionSlot {
        let Some(region) = self.outer.peek(index) else {
            return RegionSlot::End;
        };

        let mapped = Region {
            size: shrink(region.size, self.inset),
            expand: region.expand,
            full: shrink_height(region.full, self.inset),
        };

        // In the real implementation the finite/repeat distinction is
        // propagated from the outer source as well.
        ...
    }
}
```

The important property is not the exact provider names.

It is:

> A transformed region source delegates its future queries to the outer `Tracked<Regions>`, so accesses remain visible to Comemo.

This is analogous in purpose to `LocatorLink`: information across a boundary is only pulled in when it is actually required.

Specific providers will be required for:

```text
padding/inset
fixed-height breakable blocks
column expansion
flow's current remaining region
other existing Regions::map-style transformations
```

---

# 7. Mutable remaining space stays outside `Regions`

Flow currently mutates `Regions.size.y` as it consumes the current region.

That responsibility moves into `Distributor`.

Conceptually:

```rust
struct Distributor<'r, ...> {
    /// Immutable external region environment.
    regions: Tracked<'r, Regions<'r>>,

    /// Remaining portion of the current region.
    remaining: Region,

    used: Size,

    // existing fields...
}
```

Consumption becomes:

```rust
fn use_height(&mut self, amount: Abs) {
    self.remaining.size.y -= amount;
    self.used.y += amount;
}
```

When a child is laid out, flow constructs a child-facing lazy region source whose region zero is `remaining` and whose later regions delegate to the enclosing tracked source.

Thus:

```text
Regions
    describes the environment

remaining
    describes this particular distribution attempt
```

No `RegionCursor` abstraction is introduced.

---

# 8. Multi-layout continuation state

The continuation is not a callable object and does not own the layout environment.

The callback/block already identifies **what program should run**.

The continuation stores only:

> Where is that program up to?

The type-erased state is:

```rust
#[derive(Clone, Hash)]
pub struct MultiState(Arc<dyn MultiStateBounds>);
```

with:

```rust
trait MultiStateBounds:
    Debug
    + Send
    + Sync
    + Any
    + 'static
{
    fn dyn_hash(&self, state: &mut dyn Hasher);
}
```

and:

```rust
impl<T> MultiStateBounds for T
where
    T: Debug + Hash + Send + Sync + 'static,
{
    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        TypeId::of::<T>().hash(&mut state);
        self.hash(&mut state);
    }
}

impl Hash for dyn MultiStateBounds {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dyn_hash(state);
    }
}
```

Construction and downcasting are private to the callback implementation:

```rust
impl MultiState {
    fn new<S>(state: S) -> Self
    where
        S: Debug + Hash + Send + Sync + 'static,
    {
        Self(Arc::new(state))
    }

    fn downcast_ref<S>(&self) -> Option<&S>
    where
        S: 'static,
    {
        let inner: &dyn MultiStateBounds = &*self.0;
        (inner as &dyn Any).downcast_ref()
    }
}
```

Using `Arc` makes cloning continuation state cheap and avoids requiring a dynamic clone operation.

Concrete state types should normally derive:

```rust
#[derive(Debug, Clone, Hash)]
```

although `Clone` is required by the concrete algorithm rather than by the erased wrapper itself.

---

# 9. Strongly typed and erased step results

Concrete callbacks use:

```rust
pub struct MultiStep<S> {
    pub frame: Frame,
    pub next: Option<S>,
}
```

The erased layer uses:

```rust
pub struct MultiLayoutResult {
    pub frame: Frame,
    pub next: Option<MultiState>,
}
```

The semantic transition is:

```text
callback/program
      +
state S_i
      +
Tracked<Regions_i>
      ↓
Frame_i
      +
state S_{i+1}
```

or mathematically:

$$
(P,S_i,R_i)\longmapsto(F_i,S_{i+1}).
$$

The public contract is:

> A frame returned from a multi-layout step is stable with respect to that layouter's continuation. The continuation must never later mutate the returned frame.

This is the contract grid's stabilization frontier exists to enforce.

---

# 10. New `BlockElem::multi_layouter` signature

The strongly typed constructor becomes:

```rust
pub fn multi_layouter<T, S>(
    captured: Packed<T>,
    f: fn(
        content: &Packed<T>,
        engine: &mut Engine,
        locator: Locator,
        styles: StyleChain,
        regions: Tracked<Regions<'_>>,
        state: Option<&S>,
    ) -> SourceResult<MultiStep<S>>,
) -> Self
where
    T: NativeElement,
    S: Debug + Hash + Send + Sync + 'static,
{
    Self::new().with_body(Some(BlockBody::MultiLayouter(
        callbacks::BlockMultiCallback::new(captured, f),
    )))
}
```

`None` means the initial state.

A continuation call receives the state returned from the previous step.

This means the grid callback eventually has the shape:

```rust
pub fn layout_table(
    elem: &Packed<TableElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Tracked<Regions<'_>>,
    state: Option<&GridState>,
) -> SourceResult<MultiStep<GridState>>;
```

and similarly:

```rust
pub fn layout_grid(
    elem: &Packed<GridElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Tracked<Regions<'_>>,
    state: Option<&GridState>,
) -> SourceResult<MultiStep<GridState>>;
```

The `GridState` contains no `Locator`, `StyleChain`, `CellGrid` reference, or arena-backed data. Those are supplied afresh to every callback invocation.

---

# 11. `BlockMultiCallback` type erasure

The existing callback macro can continue handling inline and single-region callbacks.

The multi callback should become a specialised implementation because it now has an additional generic state type.

Conceptually:

```rust
#[derive(Clone)]
pub struct BlockMultiCallback {
    captured: Content,
    f: Arc<dyn MultiCallback>,
}
```

The erased function interface is:

```rust
trait MultiCallback: Send + Sync {
    fn call(
        &self,
        captured: &Content,
        engine: &mut Engine,
        locator: Locator,
        styles: StyleChain,
        regions: Tracked<Regions<'_>>,
        state: Option<&MultiState>,
    ) -> SourceResult<MultiLayoutResult>;
}
```

The typed adapter is:

```rust
struct TypedMultiCallback<S> {
    f: fn(
        content: &Content,
        engine: &mut Engine,
        locator: Locator,
        styles: StyleChain,
        regions: Tracked<Regions<'_>>,
        state: Option<&S>,
    ) -> SourceResult<MultiStep<S>>,
}
```

with:

```rust
impl<S> MultiCallback for TypedMultiCallback<S>
where
    S: Debug + Hash + Send + Sync + 'static,
{
    fn call(
        &self,
        captured: &Content,
        engine: &mut Engine,
        locator: Locator,
        styles: StyleChain,
        regions: Tracked<Regions<'_>>,
        state: Option<&MultiState>,
    ) -> SourceResult<MultiLayoutResult> {
        let state = state.map(|state| {
            state
                .downcast_ref::<S>()
                .expect("invalid multi-layout state type")
        });

        let result = (self.f)(
            captured,
            engine,
            locator,
            styles,
            regions,
            state,
        )?;

        Ok(MultiLayoutResult {
            frame: result.frame,
            next: result.next.map(MultiState::new),
        })
    }
}
```

As today, the first parameter can be changed from:

```rust
&Packed<T>
```

to:

```rust
&Content
```

when the callback is constructed because `Packed<T>` is a transparent wrapper over `Content`.

`BlockMultiCallback` retains today's equality/hash semantics based primarily on the captured content rather than function-pointer identity.

---

# 12. `MultiChild` and `MultiSpill`

`MultiChild` retains its current borrowed fields:

```rust
pub struct MultiChild<'a> {
    pub align: Axes<FixedAlignment>,
    pub sticky: bool,
    alone: bool,
    elem: &'a Packed<BlockElem>,
    styles: StyleChain<'a>,
    locator: Locator<'a>,
}
```

The fragment `CachedCell` is removed. Caching is handled by the memoized step function.

Its new layout method is:

```rust
impl<'a> MultiChild<'a> {
    pub fn layout(
        &self,
        engine: &mut Engine,
        regions: Tracked<Regions<'_>>,
        state: Option<&BlockState>,
    ) -> SourceResult<BlockStep> {
        layout_multi_impl(
            engine.world,
            engine.library,
            engine.introspector.into_raw(),
            engine.traced,
            TrackedMut::reborrow_mut(&mut engine.sink),
            engine.route.track(),
            self.elem,
            self.locator.track(),
            self.styles,
            regions,
            state,
            self.alone,
        )
    }
}
```

The block wrapper uses its own concrete state:

```rust
#[derive(Debug, Clone, Hash)]
pub(super) struct BlockState {
    body: BlockBodyState,

    /// Remaining explicit block height, if this block has fixed height.
    remaining_height: Option<Abs>,
}
```

with:

```rust
#[derive(Debug, Clone, Hash)]
enum BlockBodyState {
    /// Normal content was eagerly laid out once and these frames remain.
    Content(FragmentCursor),

    /// A custom resumable multi-layouter.
    Callback(MultiState),

    /// Explicit empty block continuation.
    Empty,
}
```

The wrapper result is:

```rust
#[derive(Clone)]
pub(super) struct BlockStep {
    pub frame: Frame,
    pub next: Option<BlockState>,
}
```

`MultiSpill` is completely removed.

Its replacement inside `Work` is:

```rust
#[derive(Clone)]
pub struct PendingMulti<'a, 'b> {
    multi: &'b MultiChild<'a>,
    state: BlockState,
}
```

This is safe precisely because `PendingMulti` remains inside the existing synchronous `layout_flow` call. It never has to outlive the collection `Bump`.

---

# 13. `Work` remains borrowed

The current lifetime/arena structure is retained.

Only the spill field changes:

```rust
#[derive(Clone)]
struct Work<'a, 'b> {
    children: &'b [Child<'a>],

    spill: Option<PendingMulti<'a, 'b>>,

    floats: EcoVec<&'b PlacedChild<'a>>,
    footnotes: EcoVec<Packed<FootnoteElem>>,
    footnote_spill: Option<std::vec::IntoIter<Frame>>,
    tags: EcoVec<&'a Tag>,
    skips: Rc<FxHashSet<Location>>,
}
```

There is deliberately no conversion of:

```text
Child<'a>
BumpBox
StyleChain<'a>
Locator<'a>
Pair<'a>
```

into owned equivalents.

`layout_flow` still:

```text
realizes once
collects once
creates Work once
composes all regions synchronously
returns Fragment
```

---

# 14. `layout_multi_impl`

The memoized implementation becomes a state transition rather than a whole-fragment layout:

```rust
#[comemo::memoize]
#[expect(clippy::too_many_arguments)]
fn layout_multi_impl(
    world: Tracked<dyn World + '_>,
    library: &LazyHash<Library>,
    introspector: Tracked<dyn Introspector + '_>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    elem: &Packed<BlockElem>,
    locator: Tracked<Locator>,
    styles: StyleChain,
    regions: Tracked<Regions<'_>>,
    state: Option<&BlockState>,
    alone: bool,
) -> SourceResult<BlockStep> {
    let introspector = Protected::from_raw(introspector);
    let link = LocatorLink::new(locator);
    let locator = Locator::link(&link);

    let mut engine = Engine {
        library,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    layout_and_modify(styles, |styles| {
        layout_multi_block(
            elem,
            &mut engine,
            locator,
            styles,
            regions,
            state,
            alone,
        )
    })
}
```

The state is part of the memoization key.

The tracked regions are not hashed wholesale; only observed tracked calls constrain cache reuse.

---

# 15. `layout_multi_block`

The signature becomes:

```rust
pub fn layout_multi_block(
    elem: &Packed<BlockElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Tracked<Regions<'_>>,
    state: Option<&BlockState>,
    alone: bool,
) -> SourceResult<BlockStep>;
```

Its responsibilities are now:

```text
construct the lazy transformed child-region view
        ↓
obtain exactly one raw body frame
        ↓
post-process that frame
        ↓
construct next BlockState, if any
```

It no longer iterates over and modifies a complete `Fragment`.

---

# 16. Normal content does not require resumable flow yet

This is important.

`BlockBody::Content` currently calls:

```rust
crate::layout_fragment(...)
```

which realizes and lays out the complete nested flow.

Since flow is not being made resumable in this refactor, this branch initially remains eager.

On the first step:

```rust
let fragment =
    crate::layout_fragment(
        engine,
        body,
        locator.relayout(),
        styles,
        pod,
    )?;
```

The existing auto-width consistency logic can therefore remain unchanged:

```rust
if !pod.expand().x
    && fragment
        .as_slice()
        .windows(2)
        .any(|w| !w[0].width().approx_eq(w[1].width()))
{
    // Determine max width and relayout exactly as today.
}
```

After that one eager layout, the frames are stored in:

```rust
#[derive(Debug, Clone, Hash)]
struct FragmentCursor {
    frames: EcoVec<Frame>,
    next: usize,
}
```

The first frame is returned immediately.

The continuation contains:

```rust
BlockBodyState::Content(FragmentCursor {
    frames,
    next: 1,
})
```

Subsequent regions simply retrieve the next frame.

This means nested content is still eagerly laid out once, but unlike `MultiSpill`, it is **not repeatedly laid out from the beginning**.

A later refactor may make nested flow resumable, but this is not required for the current work.

**Caveat.** The stored frames were laid out against *predicted* later regions. If the actual region differs (a footnote or float lands there), returning the stored frame unchecked would overflow where `MultiSpill` adapts today. So a `FragmentCursor` continuation must go through the verification of §36: re-run the eager layout with the actual region, accept if the emitted frames are unchanged, otherwise restart or fall back.

---

# 17. Custom multi-layouters

For:

```rust
Some(BlockBody::MultiLayouter(callback))
```

the initial call is:

```rust
let result = callback.call(
    engine,
    locator,
    styles,
    pod,
    None,
)?;
```

and continuation calls use:

```rust
let Some(BlockBodyState::Callback(state)) = ...;

let result = callback.call(
    engine,
    locator,
    styles,
    pod,
    Some(state),
)?;
```

The wrapper stores:

```rust
result.next.map(BlockBodyState::Callback)
```

inside the next `BlockState`.

This is the path grid/table ultimately uses.

---

# 18. Explicit/fixed-height block regions

Today's `breakable_pod` eagerly distributes a fixed height over the whole `Regions` backlog.

That becomes a lazy block-specific region provider.

Conceptually:

```rust
struct BlockRegions<'a> {
    outer: Tracked<
        'a,
        Regions<'a>,
        <Regions<'static> as Track>::Call,
    >,

    width: Sizing,
    inset: Sides<Abs>,

    /// `None` for auto-height blocks.
    remaining_height: Option<Abs>,

    expand: Axes<bool>,
}
```

For automatic height, region `n` is simply a mapped outer region.

For explicit height, `region(n)` computes how much of the remaining fixed block height belongs in outer region `n`, stopping once the block height is exhausted.

The continuation updates:

```rust
BlockState::remaining_height
```

after each produced region.

Thus no fixed-height backlog needs to be allocated eagerly.

---

# 19. Per-frame block post-processing

The existing fragment-wide postprocessing becomes a per-frame function:

```rust
fn finish_multi_frame(
    elem: &Packed<BlockElem>,
    styles: StyleChain,
    frame: &mut Frame,
    region: Region,
    inset: Sides<Rel<Abs>>,
    decorate: bool,
    is_explicit: bool,
) {
    ...
}
```

It performs the same operations as today:

```text
set FrameKind::Hard where appropriate
enforce expansion
grow by inset
clip
fill/stroke
label
```

but only for the current frame.

---

# 20. Empty first frames

Current block and flow code contain a special rule:

> If the first frame is empty but a later frame is non-empty, do not decorate the empty frame and move the child to the next region where appropriate.

A resumable API no longer automatically knows whether a later non-empty frame exists.

This should not be encoded as a permanent `future_non_empty` field which every layouter must eagerly compute.

Instead provide a rare-path probe.

Conceptually:

```rust
fn has_non_empty_continuation(
    multi: &MultiChild,
    engine: &mut Engine,
    regions: Tracked<Regions<'_>>,
    state: &BlockState,
) -> SourceResult<bool>;
```

It advances the continuation speculatively through future regions until either:

```text
a non-empty frame is found
or
the continuation ends
```

This probe is only needed when the current first frame is empty.

All region accesses made during the probe remain tracked.

For eager `BlockBody::Content`, the answer is already available directly from the stored fragment.

---

# 21. New distributor path

Current:

```rust
let (frame, spill) = multi.layout(engine, pod)?;
```

becomes:

```rust
let result = multi.layout(
    self.composer.engine,
    pod,
    None,
)?;
```

then:

```rust
if result.frame.is_empty()
    && result.next.is_some()
    && self.regions.may_progress()
    && has_non_empty_continuation(
        multi,
        self.composer.engine,
        pod,
        result.next.as_ref().unwrap(),
    )?
{
    return Err(Stop::Finish(Finish::Soft));
}
```

otherwise:

```rust
self.frame(
    result.frame,
    multi.align,
    multi.sticky,
    true,
)?;
```

and:

```rust
if let Some(state) = result.next {
    self.composer.work.spill = Some(PendingMulti {
        multi,
        state,
    });

    self.composer.work.advance();
    return Err(Stop::Finish(Finish::Soft));
}
```

Continuation handling becomes:

```rust
fn continue_multi(
    &mut self,
    pending: PendingMulti<'a, 'b>,
) -> Result<(), Stop> {
    let pod = self.child_regions();

    if pod.is_full() {
        self.composer.work.spill = Some(pending);
        return Err(Stop::Finish(Finish::Soft));
    }

    let result = pending.multi.layout(
        self.composer.engine,
        pod,
        Some(&pending.state),
    )?;

    self.frame(
        result.frame,
        pending.multi.align,
        false,
        true,
    )?;

    if let Some(state) = result.next {
        self.composer.work.spill = Some(PendingMulti {
            multi: pending.multi,
            state,
        });

        return Err(Stop::Finish(Finish::Soft));
    }

    Ok(())
}
```

No previously emitted frame is recomputed or skipped.

---

# 22. Grid's continuation state

The current `GridLayouter<'a>` mixes two categories of information:

```text
borrowed environment
    grid
    styles
    cell locators
    header references
    Regions

semantic progress
    current row
    resolved columns
    rowspans
    current region rows
    finished region frames
    repeated-header state
```

Only the second category belongs in the continuation.

The new callback reconstructs an ephemeral `GridLayouter<'a>` each step from:

```text
elem/grid
locator
styles
Tracked<Regions>
GridState
```

The persistent state is approximately:

```rust
#[derive(Debug, Clone, Hash)]
pub struct GridState {
    /// Next normal table row to process.
    y: usize,

    /// Resolved column widths.
    rcols: Vec<Abs>,

    /// Sum of resolved column widths.
    width: Abs,

    /// Number of remaining rows in the current unbreakable group.
    unbreakable_rows_left: usize,

    /// Rowspans which cannot yet be rendered.
    rowspans: Vec<Rowspan>,

    /// Current-region layout state.
    current: GridCurrent,

    /// Region drafts not yet all publishable.
    regions: LayoutFrontier<GridRegionDraft>,

    /// Active repeated-header indices into `grid.headers`.
    repeating_headers: Vec<usize>,

    /// Pending repeated-header indices.
    pending_headers: Vec<usize>,

    /// Index of the next unprocessed header.
    upcoming_header: usize,

    row_state: RowState,

    /// Whether the logical end of the table has been reached.
    done: bool,
}
```

Where useful, existing grid types such as `Current`, `Row`, `RowState`, `Rowspan`, and `FinishedHeaderRowInfo` can simply acquire `Clone`/`Hash` derives rather than being fundamentally redesigned.

Borrowed header references are replaced with indices into the current `CellGrid`.

---

# 23. Grid region drafts

Current grid stores:

```rust
finished: Vec<Frame>,
rrows: Vec<Vec<RowPiece>>,
finished_header_rows: Vec<FinishedHeaderRowInfo>,
```

but these regions are not actually finished: `layout_rowspan` can later mutate their frames.

Replace those parallel structures with:

```rust
#[derive(Debug, Clone, Hash)]
struct GridRegionDraft {
    frame: Frame,
    rows: Vec<RowPiece>,
    header: FinishedHeaderRowInfo,
}
```

A draft remains private to grid until it becomes stable.

---

# 24. Generic stabilization frontier

The generic helper can initially be very small:

```rust
#[derive(Debug, Clone, Hash)]
pub(crate) struct LayoutFrontier<T> {
    drafts: VecDeque<T>,

    /// Number of drafts at the front which are known stable.
    stable: usize,
}
```

with:

```rust
impl<T> LayoutFrontier<T> {
    pub fn push(&mut self, draft: T) {
        self.drafts.push_back(draft);
    }

    pub fn stabilize_prefix(&mut self, count: usize) {
        self.stable = self.stable.max(count);
        debug_assert!(self.stable <= self.drafts.len());
    }

    pub fn pop_stable(&mut self) -> Option<T> {
        if self.stable == 0 {
            return None;
        }

        self.stable -= 1;
        self.drafts.pop_front()
    }

    pub fn pending_mut(&mut self) -> &mut VecDeque<T> {
        &mut self.drafts
    }
}
```

The helper does not decide what is stable.

The algorithm using it decides that.

Grid is simply its first user.

---

# 25. Where the frontier sits

The frontier is entirely internal to a layouter.

For grid:

```text
rows / headers / rowspans
          │
          ▼
   GridRegionDrafts
          │
          │
   LayoutFrontier
          │
          ▼
   stable draft
          │
          ▼
 fills / strokes
          │
          ▼
 returned Frame
          │
          ▼
        flow
```

It is **not**:

```text
flow → frontier → child
```

and it is not a replacement for `Work`.

Its sole question is:

> Could future execution of this same layouter still modify this draft?

If yes, the draft stays behind the frontier.

If no, it may be returned.

---

# 26. Grid frontier rule

The primary blocker is a pending rowspan.

Suppose drafts 2–4 exist and an unresolved rowspan began in draft 2.

Then:

```text
D0 D1 | D2 D3 D4
      ^
      frontier
```

`D0` and `D1` can already have escaped.

`D2` cannot.

When the rowspan's final spanned row becomes known:

```text
layout_rowspan(...)
```

writes its cell fragments into the private drafts.

After that, if no other unresolved rowspan can touch `D2`, it becomes stable.

A simple conservative test is:

```text
draft i is blocked if any unresolved rowspan has
first_region <= i
```

The implementation can later make that test more precise if necessary.

---

# 27. Grid `finish_region`

Current `finish_region` eventually pushes into:

```rust
self.finished
self.rrows
self.finished_header_rows
```

New `finish_region` pushes one draft:

```rust
self.state.regions.push(GridRegionDraft {
    frame,
    rows: resolved_rows,
    header: FinishedHeaderRowInfo {
        repeated_amount,
        last_repeated_header_end,
        repeated_height,
    },
});
```

It then updates rowspans and determines how far the stable prefix can advance.

Nothing is returned merely because a region was geometrically completed.

---

# 28. Grid step algorithm

The new grid step has the shape:

```rust
pub fn layout_table(
    elem: &Packed<TableElem>,
    engine: &mut Engine,
    locator: Locator,
    styles: StyleChain,
    regions: Tracked<Regions<'_>>,
    state: Option<&GridState>,
) -> SourceResult<MultiStep<GridState>> {
    let grid = elem.grid.as_ref().unwrap();

    let mut layouter = GridLayouter::restore(
        grid,
        regions,
        locator,
        styles,
        elem.span(),
        state,
    )?;

    loop {
        if let Some(draft) = layouter.state.regions.pop_stable() {
            let frame = layouter.finalize_region(draft)?;

            return Ok(MultiStep {
                frame,
                next: (!layouter.state.is_done())
                    .then(|| layouter.into_state()),
            });
        }

        layouter.plan_more(engine)?;
    }
}
```

The initial call performs column measurement before normal row planning.

`plan_more` may:

```text
lay out more rows
finish regions
simulate future regions
process headers/footers
complete rowspans
advance the frontier
```

It runs until a frame can safely be returned or the table is complete.

As written, the loop pops stored drafts regardless of the region it is given. Drafts beyond the first were planned against predicted regions, so each must record the geometry it was planned with and go through §36's verification when the actual region differs.

---

# 29. Rowspan rendering changes

Current `layout_rowspan` mutates:

```rust
self.finished.iter_mut()
```

which is no longer allowed.

Instead it accesses private drafts:

```rust
self.state
    .regions
    .pending_mut()
```

and inserts the cell frames into those drafts.

The actual rowspan geometry algorithm can remain largely unchanged initially.

This is deliberately a conservative migration: change **where** the output is stored before changing **how** rowspans are calculated.

---

# 30. Fills and strokes

`render_fills_strokes` currently runs over the complete table at the end.

It becomes region finalization:

```rust
fn finalize_region(
    &self,
    mut draft: GridRegionDraft,
) -> SourceResult<Frame> {
    let is_last =
        self.state.done
        && self.state.regions.drafts.is_empty();

    render_fills_strokes_for_region(
        &mut draft.frame,
        &draft.rows,
        &draft.header,
        &self.state.rcols,
        is_last,
        ...,
    )?;

    Ok(draft.frame)
}
```

Most line/fill logic is already region-local.

The main whole-table-looking condition is whether this is the final region.

Therefore the last pending draft cannot cross the frontier until either:

```text
a later region exists
```

so it is definitely not last, or:

```text
table layout has completed
```

so it is definitely last.

This is effectively a one-region lastness dependency.

---

# 31. Headers and footers

Correction: header/footer simulation does not actually look ahead. `simulate_header`/`simulate_footer` only use the current region, and the region-skipping loops (`layout_active_headers`, `prepare_footer`) just emit empty frames, which a streaming layouter can do too. The genuine grid lookahead is in breakable auto-row measurement (cells laid out into all future regions), its empty-first-frame skip, rowspan measurement joining past and future heights, the gutter rowspan simulation, and the assumption that future regions carry the same header/footer heights (`TODO(layout model)` in `repeated.rs`).

Repeated header/footer simulation remains legal.

Instead of mutating a copied `Regions`:

```rust
regions.next();
```

simulation uses an offset:

```rust
let region = regions.peek(offset);
```

When simulation crosses a region:

```rust
offset += 1;
```

Thus a simulation which only needs two future regions records exactly:

```text
peek(1)
peek(2)
```

rather than depending on the entire remaining sequence.

No attempt should initially be made to remove this lookahead.

---

# 32. Auto rows

The current `measure_auto_row` can require the complete future fragment of a cell.

Eventually this can probably become a lockstep continuation algorithm, but that is not part of the first migration.

Initially preserve the existing behaviour.

A temporary coarse helper may materialize exactly the future region data required by the existing code:

```rust
fn materialize(
    regions: Tracked<Regions<'_>>,
) -> MaterializedRegions;
```

or an internal iterator can repeatedly call:

```rust
regions.peek(offset)
```

until the relevant cell finishes.

This may create a broad dependency for auto rows, but it is sound.

After the architecture is stable, auto-row layout can be optimized separately.

---

# 33. External speculative lookahead

The grid frontier handles one problem:

> Future grid computation might still modify this grid draft.

There is a second, independent problem:

> A frame may have been computed using predicted future regions which later turn out to differ.

It is not specific to grid. Whenever a flow lays out a breakable child in region `k`, the regions `k+1, k+2, ...` it passes along are *predictions*. The flow does not yet know which floats and footnotes will end up in them. A float that did not fit on page `k` is queued for page `k+1`; a footnote whose marker lands on page `k+1` shrinks it. Only when the child's continuation is laid out in region `k+1` is its actual size known.

Today, `MultiSpill` then lays the child out again with the actual size and skips the frames it already emitted. That silently assumes frame `k` does not depend on region `k+1`. When it does, frames from two different layouts are glued together.

Minimal reproducer (on `main`, "CCC" is lost):

```typst
#set page(width: 200pt, height: 200pt, margin: 10pt)
#set text(size: 10pt)
#for i in range(10) [Filler #i.\ ]
#place(auto, float: true, rect(width: 100%, height: 150pt, fill: aqua))
#block(breakable: true, stroke: red)[AAA.\ BBB.\ CCC.\ DDD.]
```

The float does not fit on page 1 and is queued for page 2. The block is laid out on page 1 assuming a full page 2, so widow prevention moves "CCC" and "DDD" to page 2 together. On page 2, only 15pt remain next to the float. Laid out again with 15pt for page 2, the widow pair no longer fits there, so "CCC" stays on page 1. But page 1's frame, which lacks "CCC", was already emitted.

Giving "CCC" a footnote (and making the float 138pt tall) turns the lost line into issue #8487's crash:

```text
error: internal error: parent group (occurred at crates/typst-pdf/src/tags/tree/build.rs:206:14)
```

The same happens inside table cells, nested blocks and columns (see §43).

What matters for correctness is whether an *emitted frame* would change, not whether the child *observed* the mispredicted region. This distinction drives the design in §36.

---

# 34. Nested memoization already composes with tracked regions

Comemo's `Input` implementation for:

```rust
Tracked<T>
```

uses a merged sink when another memoization boundary is entered. Calls performed inside a memoized child are propagated to the outer tracked sink, including when the child itself is served from its cache (`Tracked::call` emits to the outer sink during call-tree traversal). Comemo 0.5.1 also exposes:

```rust
pub struct Constraint<C>(...);

impl<C> Constraint<C> {
    pub fn validate<T>(&self, value: &T) -> bool
    where
        T: Track<Call = C> + ?Sized;
}
```

together with `Track::track_with(&value, &constraint)`.

So a flow could record the region calls a child made and later validate them against the actual regions. This is **not** used as the correctness mechanism (see §36.8), because observations over-approximate dependencies. It remains useful as a fast path (§39).

---

# 35. Flow remains synchronous

For this work the outer flow signature remains conceptually:

```rust
pub fn layout_flow<'a>(
    engine: &mut Engine,
    children: &[Pair<'a>],
    locator: &mut SplitLocator<'a>,
    shared: StyleChain<'a>,
    regions: Tracked<Regions<'_>>,
    column: ColumnOptions,
    mode: FlowMode,
) -> SourceResult<Fragment>;
```

Its lifetime structure remains:

```text
layout_fragment_impl
    creates realization Arenas
        ↓
realize
        ↓
layout_flow
    creates Bump
        ↓
collect
        ↓
Child<'a>[]
        ↓
Work<'a, 'b>
        ↓
compose all regions (possibly restarting earlier ones, §36)
        ↓
Fragment
```

Nothing in `Work` needs to become `'static`. Restarting earlier regions only needs clones of `Work` from earlier region starts, which stay inside `layout_flow`.

---

# 36. Verify, restart, fall back

This is the correctness layer. It is implemented and measured on top of the *current* `MultiSpill` (see §43); it does not depend on the lazy `Regions` or `MultiState` work and can land first.

It is the "every move requires a relayout" idea from <https://laurmaedje.github.io/posts/layout-models/>, applied only where it is needed for consistency. That post's "tracking regions" (asking "are there at least 4cm left?" rather than for exact sizes) is the later fast path (§39).

## 36.1 Who owns predictions

Each flow owns the predictions for its own future subregions. Subregions are numbered consecutively across columns and regions:

```rust
subregion = region_index * column_count + column
```

Only the owner of a region sequence can learn its actual geometry. A nested flow (a block's body) receives its future regions from its parent pod, and a mismatch caused by the *parent's* insertions shows up as a changed frame of the block in the *parent's* flow. So every flow runs the same logic for its own children, at every nesting depth. No mechanism has to cross a memoization boundary.

## 36.2 Predictions

```rust
struct Predictions {
    /// Learned heights, keyed by subregion.
    heights: FxHashMap<usize, Abs>,
    /// How often each subregion triggered a restart.
    restarts: FxHashMap<usize, usize>,
}
```

Initially empty: the prediction for a subregion is simply the flow's raw region. Learned heights are applied (`min`) to the backlog every child sees. Predictions live in `layout_flow`, outside `Work`, so they survive restarts.

## 36.3 Verification by output

A pending continuation records:

```rust
struct MultiSpill<'a, 'b> {
    ...
    /// Heights of the regions the emitted frames were laid out with.
    backlog: Vec<Abs>,
    /// The upcoming heights (and repeated region) of the layout the emitted
    /// frames were taken from, i.e. the child's predictions.
    predicted: Vec<Abs>,
    predicted_last: Option<Abs>,
    /// The frames emitted so far (cheap `Arc` clones).
    emitted: Vec<Frame>,
    /// Subregion of the first emitted frame.
    origin: usize,
    /// Whether frames went to consecutive subregions (§36.6).
    aligned: bool,
}
```

When the continuation is offered actual height `A` in subregion `s`:

1. Build the candidate pod `committed ++ [A] ++ upcoming` (exactly what `MultiSpill` does today) and lay the child out.
2. If the candidate pod equals the pod the emitted frames came from, the frames are identical by determinism. Accept without checking. This is the common case: uniform regions, and every nested flow without insertions.
3. Otherwise, compare the already-emitted frames with the candidate's frames structurally (`Frame::identical`: `Arc` pointer shortcut, early exit, tags compared by location rather than by element).
4. If they are identical, accept and commit `A`. This is today's adaptive behaviour, now checked.

## 36.4 Restart

If an emitted frame differs, `A` is smaller than the prediction the child used, and the restart budget allows it, the spill returns:

```rust
struct Restart {
    /// Subregion of the first inconsistent frame.
    from: usize,
    /// What was learned (see also §36.10).
    lesson: Lesson,
}

enum Lesson {
    /// The subregion `at` actually has only `height` available.
    Height { at: usize, height: Abs },
    ...
}
```

It travels as a new variant through the existing control flow:

```text
MultiSpill::layout → Stop::Restart → RelayoutStop::Restart
    → Composer::column → Composer::page → compose → layout_flow
```

`layout_flow` keeps, per region, the `Work` and `Regions` at its start and the region's `Locator`:

```rust
Err(RelayoutStop::Restart(restart)) => {
    predictions.learn(&restart); // e.g. heights[at] = min(heights[at], height)
    let target = restart.from / config.columns.count;
    (work, regions) = checkpoints[target].clone();
    finished.truncate(target);
    continue; // recompose from `target`, reusing its locator via `relayout()`
}
```

Reusing the per-region locator keeps locations stable across restarts. Realization and collection are not repeated.

## 36.5 Fallback

If a restart is not possible (the region is larger than predicted, the budget is exhausted, or the spill is unaligned), the spill continues with the layout the emitted frames were taken from (`committed ++ predicted`). This is consistent by construction, because it is the same computation.

* If `A` is larger than predicted, some space in `s` stays unused.
* If `A` is smaller (only once restarts are exhausted), the frame overflows. It is visually wrong but never loses content or corrupts tags.

(In the step-based design, the fallback continues with the *actual* regions instead; see §53.)

## 36.6 Keeping regions aligned

`MultiSpill` skips a subregion that is already full (`pod.is_full()`), for example because queued floats fill it. The child then never sees that subregion, so its frame indices stop lining up with the flow's subregions, and a learned height would be applied to the wrong child region.

The spill therefore lays the child out into the full subregion too (height clamped to zero), through the same verification. If the child continues and the frame has no height and nothing of the child's body (at most its fill and stroke), it is dropped and alignment is kept. Otherwise the spill is marked unaligned and restarts are disabled for it. (Originally, any frame without height and with nothing but shapes was dropped, which lost content; see §49.1.)

Before this rule, all overflowing fallbacks in the stress tests came from unaligned spills. After it, there were none.

## 36.7 Termination and convergence

A fixpoint (prediction equals reality) need not exist. Example: with the full-page prediction, widow prevention moves a line *with a footnote* to page `k+1`; the footnote shrinks page `k+1`; with that smaller prediction the line stays on page `k`, so page `k+1` has no footnote and is larger again. Neither layout is self-consistent. Any scheme must therefore settle on a layout whose predictions are wrong. The question is only which way it errs.

The two rules have different jobs:

* **The cap guarantees termination.** `MAX_RESTARTS = 3` per subregion bounds total restarts by 3 × subregions. Monotonicity alone does not give a useful bound: each restart must lower `heights[at]` by more than the `fits` tolerance, which still allows about 10⁶ restarts for a 180pt region.
* **Monotone predictions decide how non-convergence is resolved.** A restart is only requested when `A` is *smaller* than the prediction, and learning takes the minimum (§49.4 relaxes this without losing the property below):
  * A flip-flop between two layouts (the example above) ends after one restart. The second attempt finds the region *larger* than the lowered prediction, and the fallback (§36.5) keeps the prediction-consistent layout, which fits with some space unused.
  * Unless the cap is exhausted, a fallback therefore always fits. It only overflows (consistent, but overlapping e.g. the footnote area) when the cap is hit.

What monotonicity does *not* rule out is a strictly decreasing chain: each lowered prediction adds more insertions to the same region, and an emitted frame changes again each time. For example, a line kept on page `k` has a footnote entry that no longer fits there and is deferred to `k+1`, crossing yet another widow/orphan threshold. The usual dynamic is the opposite, since a smaller prediction moves content, and with it footnotes, *away* from `k+1`. But nothing forbids such a chain, and after 3 steps it hits the cap.

Measurements:

* Adversarial document where the flip-flop occurs about 300 times (§43): non-monotone predictions hit the cap in 4 subregions (52 restarts). Monotone predictions need 44 restarts and 4 underfull fallbacks, and never hit the cap.
* 3200 fuzzed small documents (random floats, fillers, footnotes of random length; blocks, table cells and nested blocks): 293 had restarts, 624 restarts in total, at most 2 logged for any subregion index, and zero cap hits, overflows or errors. The logging merges introspection passes and nested flows, so 2 is an upper bound.
* Realistic documents: no restarts at all.

A possible hardening, not implemented: when the cap is reached, restart one last time with prediction 0 for that subregion. Then `A ≥ prediction` always holds, the fallback always fits, and the worst case becomes a region the child leaves empty instead of an overlap.

## 36.8 Why output verification rather than input constraints

Validating recorded region calls (`Constraint::validate`) would restart whenever a child *observed* a mispredicted region, even if its frame did not change. That over-approximation is large:

* Lazy region providers (§6) implement slots via `outer.peek(n)`, so a child's `may_progress() -> bool` becomes an exact `peek(n) -> Region` dependency in every outer constraint. Nearly every breakable child (through `BlockRegions` and its nested flow's `is_full`/`may_progress`) would depend on the exact next region.
* Eager nested flows (§16) observe every region they lay out into.

Output verification restarts only when an emitted frame would actually change. That is rare: zero times across the whole test suite and all realistic benchmarks.

The cost is one relayout per changed pod, which is exactly what `MultiSpill` already pays today. Input constraints remain valuable as a way to *skip that relayout* when observations validate (§39).

## 36.9 Soundness versus quality

Only frames that were already emitted by a continuing child can become inconsistent. The flow's own lookahead only affects quality, never consistency:

* the widow/orphan `need` check against the next region (`distribute.rs`, `line()`);
* parent-scoped float space estimates over the following columns;
* `exist_non_empty_frame`.

After a restart, these see the learned predictions too, but they never trigger a restart themselves. If desired, the same mechanism could be extended to them later (for example, verifying the flow's own predicate decisions), but that is not needed to fix #8487.

## 36.10 Beyond heights: learning decisions (sticky blocks)

The same machinery lets flow make decisions *with* lookahead, without a separate speculative pass. The next region's real composition serves as the speculation. If it shows that a decision in an earlier region didn't pay off, flow restarts there and learns the better decision. `Restart` therefore carries a `Lesson`:

```rust
enum Lesson {
    /// A subregion is smaller than predicted (§36.4).
    Height { at: usize, height: Abs },
    /// Migrating the sticky group starting at this child index didn't attach
    /// it to anything in the next region.
    KeepSticky(usize),
}
```

**Sticky today.** When a region ends on a group of sticky blocks, the group migrates to the next region. The only guard is a proxy for "migrating can't help": the group is not migrated if it starts at the top of the region (`may_progress()`). The proxy misses the case where the group migrates but what follows doesn't fit on the next region either. The group is then alone on that region, and the space it left behind is wasted. On `main`, a heading followed by a block that doesn't fit on a page together with it produces a page containing only the heading.

**With lookahead.**

1. When a region ends by migrating a group, `Work::migrated` records the group (index of its first child) and the subregion it came from.
2. The record is cleared once a non-sticky, non-empty frame follows the placed group.
3. If a region finishes with the group placed but nothing attached, finalization requests `Restart { from, KeepSticky(group) }`.
4. With the lesson learned, the group is never snapshotted as stickable again, so it stays.

If the group migrates *again* (e.g. insertions left it no space), the record keeps the original origin, so the whole chain is judged by whether the group finally attaches.

**Termination.** A lesson only turns "migrate" into "keep", never back, so there is at most one restart per sticky group.

**What this makes optimal and what not.** Flow never migrates a sticky group unless the migration attaches it. That's the local criterion that removes regions containing nothing but an orphaned heading. It is not a global page-breaking optimization: it doesn't consider breaking earlier to make room, and it doesn't trade one heading's placement against another's.

**Policy question (not lookahead).** There is a second kind of "empty region": a *successful* migration that leaves nothing but spacing or insertions behind. The `issue-5296` references deliberately render an empty first page (only `v(2cm)`) so that the heading stays with its content. Avoiding such pages means orphaning the heading instead. That is a preference, not missing information. If preferred, it can be implemented with a purely local check at migration time ("does the region keep any in-flow content?") and needs no restart.

**Other candidates for learned decisions.** Flow's own lookahead (§36.9) could use the same pattern:
* widow/orphan `need` checks against the next region;
* float placement;
* column balancing.

Each needs a monotone lesson to keep termination simple.

## 36.11 What this does not need

* Flow does not become a multi-layouter; `Work` does not escape `layout_flow`.
* No new region representation; the prototype uses today's `Regions` backlog.
* No comemo changes.
* One small library addition: `Frame::identical`.

---

# 37. Why flow does not need a `LayoutFrontier`

Grid needs a frontier because it literally has incomplete output:

```text
frame exists
but
future rowspan will mutate it
```

Flow, by contrast, returns only after the whole synchronous `layout_flow` operation is complete. Its "frontier" is simply the list of region checkpoints in §36.4: any region before the current one may still be recomposed until `layout_flow` returns.

---

# 38. Relationship between the mechanisms

```text
MultiState
    Where does this child resume?

LayoutFrontier
    Is this child's internally generated output complete?

Output verification + restart (§36)
    Is emitted output consistent with the actual regions? If not,
    recompose from the region of the first inconsistent frame.

Tracked<Regions> / Constraint (§39)
    Optional fast path: can the verification relayout be skipped
    because nothing the child observed changed?
```

For a grid:

```text
GridState
    contains rows, rowspans, draft regions, etc.

LayoutFrontier<GridRegionDraft>
    prevents unfinished rowspan output from escaping.

Output verification
    a returned grid frame planned against predicted regions is
    re-checked when the actual next region is known.
```

---

# 39. Caching model

The memoized unit becomes a state transition. For a callback with concrete state `S`:

```rust
#[comemo::memoize]
fn layout_step(
    ...,
    state: Option<&S>,
    regions: Tracked<Regions<'_>>,
) -> SourceResult<MultiStep<S>>;
```

Conceptually:

```text
S0 + R0 → F0 + S1
S1 + R1 → F1 + S2
S2 + R2 → F2 + S3
```

Verification in the resumable model: when step `k+1` receives an actual region different from the one step `k` planned with, re-run step `k` with a view whose later slots are the actual ones. Compare `F_k`; if identical, continue with the new `S_{k+1}`, otherwise restart.

Tracked regions make this cheaper. If step `k`'s recorded observations validate against the actual view, `F_k` and `S_{k+1}` are provably unchanged and the re-run is skipped entirely. Today every changed pod costs a full relayout of the child (O(regions × body) for a child spanning many regions with footnotes on every page); with this fast path it would mostly cost nothing.

This is the main performance argument for the lazy `Regions` work. For it to pay off, providers must forward predicate-shaped queries (`may_progress()`, "do at least `h` fit?") rather than raw `peek(n)` (§36.8).

---

# 40. Migration strategy

1. **Restart layer (prototyped).** Output verification, `Restart`, predictions, per-region checkpoints and the aligned skip, on top of today's `MultiSpill`. Fixes the #8487 class independently of everything below.

2. Introduce the richer `Region` and lazy tracked `Regions` representation with provider-based transformations. Providers forward predicate queries. Preserve coarse compatibility helpers where necessary.

3. Introduce `MultiState`, `MultiStep<S>`, `MultiLayoutResult`, and the state-aware `BlockMultiCallback`.

4. Change `MultiChild` from whole-fragment replay to `BlockState` transitions, keeping the §36 rule: a continuation whose actual region differs from the one it planned with must verify its emitted frames (re-run the previous step) or restart. Stored frames must never be returned unchecked; that would overflow wherever `MultiSpill` adapts today.

5. Keep `BlockBody::Content` eager (`FragmentCursor`), with the same rule.

6. Convert layouters without nested flows to real continuation state. Equation is the first candidate; pad, columns, stack and lists wrap nested flows and would only become `FragmentCursor`s.

7. Grid: `GridState`, `LayoutFrontier`, drafts, rowspans into drafts, per-region fills/strokes. Drafts record the geometry they were planned against and go through verification like any other stored frame.

8. Use observation constraints to skip verification relayouts (§39).

9. Optimize broad lookahead such as `measure_auto_row` separately.

---

# 41. Explicit non-goals

This change does **not** initially:

```text
make flow itself a MultiLayout

make Work escape layout_flow

change Child<'a> into an owned representation

replace BumpBox

make StyleChain owned

change Locator ownership

rerun realization for each region

eliminate all table lookahead

rewrite auto-row measurement from scratch

require every layouter to use LayoutFrontier
```

Those would unnecessarily enlarge the initial change.

---

# 42. Resulting architecture

The complete design is:

```text
                         external geometry
                                │
                                ▼
                      Tracked<Regions<'_>>
                                │
                ┌───────────────┴───────────────┐
                │                               │
                │                               │
          observed lazily                 peek(n) when needed
                │                               │
                └───────────────┬───────────────┘
                                ▼
                       multi-layout callback
                                +
                           MultiState
                                │
                                ▼
                     algorithmic computation
                                │
                 ┌──────────────┴──────────────┐
                 │                             │
          ordinary layouter                grid/table
                 │                             │
                 │                       region drafts
                 │                             │
                 │                     LayoutFrontier
                 │                             │
                 └──────────────┬──────────────┘
                                ▼
                          stable Frame
                                +
                         next MultiState
                                │
                                ▼
                         synchronous flow
                                │
                      existing Work state
                                │
                 output verification + restart
                    if lookahead is provisional
                                │
                                ▼
                            Fragment
```

The three central invariants are:

```text
1. Regions is environment, not computation state.

2. MultiState is computation state, not environment.

3. A returned frame is internally stable:
   later continuation of that layouter may never mutate it.
```

Output verification with restarts (§36) then supplies the final external guarantee:

```text
An emitted frame is only kept if laying the child out with the actual
regions reproduces it; otherwise the flow recomposes from that frame's region.
```

This removes the fundamental unsoundness of `MultiSpill` without requiring flow realization, collection, or locator ownership to be redesigned.

---

# 43. Prototype results

Implemented on top of today's `MultiSpill`: `crates/typst-layout/src/flow/{mod,compose,distribute,collect}.rs`, plus `Frame::identical` in `typst-library`. Release builds, same machine, `hyperfine` means (10–30 runs).

## 43.1 Correctness

Adversarial documents: 300 repetitions of filler, a queued float of varying height, and a breakable block with four lines. `C`/`D` count distinct "CCC n"/"DDD n" lines found in the PDF (out of 300).

| Document | `main` | Prototype | Restarts | Fallbacks |
|---|---|---|---|---|
| block | C = 279 | C = 300 | 21 | 0 |
| block in table cell | C = 279 | C = 300 | 21 | 0 |
| block in breakable block | C = 296 | C = 300 | 9 | 0 |
| two columns | C = 279 | C = 300 | 21 | 0 |
| "CCC" has a footnote | **crash** (#8487 error) | all 300 | 44 | 4 (underfull) |
| same, in table cell | **crash** (#8487 error) | all 300 | 44 | 4 (underfull) |

Page counts match `main` where `main` succeeds. Counts cover both introspection iterations.

Sticky decisions (§36.10), same restart machinery:

| Document | `main` | Prototype | Restarts |
|---|---|---|---|
| heading + block too tall to fit with it | 4 pages, one with only the heading | 3 pages | 1 |
| heading + table whose first row doesn't fit with it | 4 pages, one with only the heading | 3 pages | 1 |
| same in two columns | column 2 holds only the heading | heading stays in column 1 | 1 |
| same inside a breakable block | 4 pages | 3 pages | 1 per nested layout |
| queued float, then the above | 5 pages | 4 pages | 1 |
| 300 sections, some followed by tall blocks | 166 pages, 129 ms | 140 pages, 127 ms | 26 per pass |

A realistic document with 120 headings has no failed migrations: no change in output or time. Three regression tests were added (`block-sticky-useless-migration{,-table,-columns}` in `tests/suite/layout/container.typ`). No existing reference changes.

Three regression tests were added to `tests/suite/layout/flow/flow.typ` (`flow-spill-restart-{widow,footnote,table}`). All three fail on `main`: two lose "CCC", and the `pdftags` test crashes.

The full suite (3787 tests) passes with identical output and **zero** mismatch, restart or fallback events.

## 43.2 Performance

Realistic documents: no restart events, identical text output.

| Document | Pages | `main` | Prototype | Ratio |
|---|---|---|---|---|
| plain (headings, paragraphs, footnotes) | 50 | 178 ms | 176 ms | noise |
| long table, footnotes in cells | 71 | 5799 ms | 5918 ms | 1.02 |
| breakable block, footnote per paragraph | 37 | 892 ms | 920 ms | 1.03 |
| tables + auto-placed figures | 63 | 155 ms | 156 ms | 1.01 |
| whole document in a grid cell | 38 | 1921 ms | 1946 ms | 1.01 |
| rowspans across pages + footnotes | 29 | 1717 ms | 1742 ms | 1.01 |
| two columns, tables, footnotes | 23 | 149 ms | 149 ms | 1.00 |

Adversarial documents, layout only (`typst query`, since `main` crashes during PDF export): 1–8% slower, with a restart on about 4% of pages. That is roughly 0.2–0.4 ms per restart, since a restart recomposes about one page.

The remaining overhead on realistic documents is verification, not restarts. Two lessons from getting there:

* Verifying by **hashing** frames made the whole-document-in-a-cell benchmark 23% slower. Fresh frames are hashed in full: soft frames are inlined into their parents, so cached `LazyHash`es are lost. Every nesting level paid it.
* Skipping verification when the pod is unchanged, and comparing structurally with early exit instead of hashing, brought it down to the numbers above.

## 43.3 What this says about B's concern

The performance worry about restarting earlier pages does not materialize, for three reasons:

* Emitted frames rarely depend on the exact size of the next region, so restarts are rare: none across the test suite and realistic benchmarks.
* A restart usually recomposes only the one page before the mispredicted one.
* The verification relayout already exists today in `MultiSpill`; it is now checked rather than trusted.

Convergence needs the monotone rule (§36.7), but it is cheap in practice.

---

# 44. Open questions

* **Sticky policy for successful migrations (§36.10).** Keep the heading with its content at the cost of a region with only spacing or insertions (current behaviour and `issue-5296` references), or orphan it to avoid that region?
* **`MAX_RESTARTS` value.** 3 was never hit with monotone predictions. Should exhausting it warn?
* **Overflow on exhausted restarts.** The fallback is consistent but may overlap footnotes. Alternatively, fall back to today's adaptive glue when the frame would overflow, trading soundness for looks.
* **Column balancing.** Balancing shrinks the pod (`balancing_target`), and a mismatch during a balancing pass learns the balanced height. This is bounded by the same rules, but it is untested with balanced columns.
* **Sink side effects.** Diagnostics and other sink output from discarded attempts accumulate. This is the same situation as the existing column/page relayout loops, but restarts discard more work.
* **Invisible-frame rule (§36.6).** It drops zero-height frames containing only shapes. Frames with tags keep the old unaligned behaviour.
* **Backlog extension.** Today's `Regions` has only two ways to describe the future: an explicit `backlog: &[Abs]` and a repeated `last: Option<Abs>`. `may_progress()` treats them differently:
  * any explicit backlog entry counts as progress;
  * a repeated `last` region only counts if the current region is already partly used (`size.y != last`).

  The root flow's regions are `Regions::repeat(page)`, so a learned prediction for a later region can only be expressed by writing explicit entries (`Predictions::apply` resizes the backlog with `last`). After a restart, a child at the top of a fresh, full region therefore gets `may_progress() == true` where it used to get `false`. This holds even when the next region is predicted to be *smaller*, and for the padded entries between the current region and the learned one.

  Consequences, reasoned from the code and not reproduced:
  * Something that does not fit even into a full page is deferred one region instead of being placed with overflow, leaving a (mostly) empty region. Examples: an oversized unbreakable block, an oversized float, a footnote whose first line does not fit, a sticky group at the top of a page (`stickable`), grid rows via `could_progress_at_top`.
  * Expanded flows drain explicit backlog entries (`layout_flow`'s loop condition), so an expanded child could emit an extra empty frame for a predicted entry it no longer needs.

  All of this is bounded: only in a flow that restarted, only for regions composed before the learned index, and no loops, since past that index the old semantics resume.

  Fix: predictions should change the sizes children plan with, not the progress semantics. In today's struct this needs an extra field (touching every `Regions { .. }` literal). In the lazy design (§4) it is natural: a provider returns the predicted size but keeps the slot kind (`Repeat` stays `Repeat`), and `may_progress` is defined on the unpredicted sequence.
* **Footnote spill.** A footnote entry that doesn't fit is laid out *once* (`layout_footnote`) into the column's regions. Only the first region is reduced (by flow need, separator and gap); later regions are the raw backlog. The remaining frames are stored in `Work::footnote_spill` and pushed onto later columns/pages unchecked (`Composer::footnote_spill`). This is the `FragmentCursor` pattern: sound, because all frames come from one computation, but never fitted to the actual space. Measured on `main` and the prototype alike: on every page a long footnote continues onto, the entry frame is sized for the whole page, so with separator, clearance and gap the footnote area exceeds the page. Because it is anchored to the bottom, the excess is pushed upward, about 8.5pt into the top margin with default settings. It is cosmetic unless something else occupies that page's insertion area. There is also an interaction with the prototype: after a restart, footnote entries read the learned predictions from the same backlog, although those describe the space *for flow content*, so a continuation may break earlier than necessary.

  Fixes, in order of effort:
  1. Reduce the later regions of the footnote pod by separator, clearance and gap, which covers the common case.
  2. Keep predictions out of the footnote pod.
  3. Treat the entry like a `MultiSpill`: re-lay it with the committed prefix plus the actual space, and verify emitted frames (§36.3).

---

# 45. Global page breaking: plan

Status: plan only, nothing implemented beyond §36. The goal is a Knuth–Plass-style global optimization of where regions end, together with a new meaning of stickiness that may *break earlier* in order to still stick.

## 45.1 Motivation: the open issues

Surveyed from GitHub (September 2026), with reproducers run against `main` and the §36 prototype.

| Issue | Problem | Status |
|---|---|---|
| #8890, #8487 | lost line / tag crash when a float is deferred | fixed by §36 |
| #7623 | sticky run pushed to the last column | fixed by §36.10 |
| #6388 | tall sticky block in a list leaves a page empty | fixed by §36.10 |
| #6924 | sticky breakable block can't break, moves whole | needs the new sticky semantics |
| #6546, #7229 | quote and its attribution split | needs breaking early inside a block |
| #5357 | figure body and caption split | needs stickiness between body and caption, plus early breaks |
| #5376 | footnote-induced widow | needs costs traded off globally |
| #5259 | sticky towards the previous block | feature: a cost at the cut before a block |
| #5931, #3442 | minimum fill, vertical stretching | features: region cost terms |
| #8050, #4841 | grid header / row orphans | grid-internal; later |

## 45.2 Sticky, redefined

**Definition.** A sticky block's *last* frame must end up in the same region as the first non-empty frame of the in-flow content that follows it (its *attachment*). Consecutive sticky blocks form a chain; the whole chain's end must share a region with the attachment.

**Consequences:**
* A breakable sticky block may break freely before its last frame (#6924).
* To satisfy the constraint, layout may:
  1. move the chain to the next region (today's behaviour);
  2. break *earlier inside* a breakable member of the chain, so that its tail moves with the attachment (#6546);
  3. with sticky-to-previous (#5259), break earlier in the *preceding* content.
* If no layout satisfies the constraint, it is violated at a cost rather than failing. This replaces today's "give up at the top of the region" rule.

**Hard or soft?** Proposal: costs are compared *lexicographically*: sticky violations first, then empty regions, then widow/orphan costs, then region count, then badness. Stickiness then behaves as a hard constraint whenever it can be satisfied, and degrades predictably when it can't. A weighted sum is the alternative if users should be able to trade stickiness against other costs. This is an open question (§45.10).

## 45.3 Page breaking as a search

The graph is the one the restart driver already walks (§36):

* **Node:** the start of a region, identified by `(region class, work key, knowledge key)`.
  * The work key hashes the child index, the spill's progress, queued floats and footnotes, the footnote spill, and pending tags.
  * The region class is the exact index within the finite part of the region sequence, and "repeat" beyond it. As with line classes in Knuth–Plass, paths that reach the same point on different page numbers merge.
* **Edge:** composing one region from a node, ending at a chosen *cut*. It yields the frame, the successor node, and a cost vector.
* **Drivers:**
  * the greedy/restart driver of §36: depth-first, backtracking with lessons;
  * the DP driver: expands several edges per node, keeps the cheapest path per node, and prunes to a beam.

**Soundness per path.** Composing a region uses predictions of later regions. When expanding a successor reveals a frame mismatch (§36.3), the driver re-derives *that* node with the lesson, as a new node whose knowledge key includes it. Other paths are unaffected. Lessons are monotone per path, so this terminates.

## 45.4 What makes branching affordable

1. **Branch safety (done, §36).** Every path is internally consistent, so branches may share child layouts after a cheap check instead of relaying them defensively.
2. **Predicate-shaped tracked regions (§4–§7, §36.8).**
   * Layouters ask "does `h` fit in the current region?" (`fits(h) -> bool`), "may I progress?", and only exceptionally the exact size.
   * A memoized layout is then valid for the whole interval of heights that gives the same answers. DP branches mostly shift heights by a line or two, so most child layouts in a branch are cache hits; only children whose break actually moves are relaid.
   * The same constraints give the verification fast path (§39): a continuation whose observations validate against the actual region needs no relayout at all.
   * Providers must forward predicates, not raw `peek`s (§36.8).
3. **Resumable continuations (§8–§21) for big breakable children.** Two branches that break a table at the same row share everything after it. With eager fragments, each branch re-lays the whole child. Nested *flows* can stay eager: they are memoized per interval, and a branch that changes their break relays only that flow.

Measured so far (§43, prototype): lookahead decisions occur on about half of the pages of text-heavy documents. Composing every root region 1 or 3 extra times with warm caches costs +1–4% and +2–9%. What is *not* yet measured is the hit rate when children start at shifted heights. That is experiment E1 (§45.8), and the plan depends on it.

**Exact-height observers.** Fractional spacing, `expand`, bottom alignment, and fractional rows depend on the exact height and will not reuse across branches. That is correct but makes the benefit document-dependent.

## 45.5 Candidate cuts

The distributor still fills greedily, but it records the legal cuts it passes, each with a penalty:

* **Between in-flow children and between lines**, with widow/orphan costs from `text.costs`.
* **Before a sticky chain**, which moves it.
* **Inside breakable children**, at breakpoints the child reports for its current frame: the height of each legal earlier end, with a penalty.
  * A nested flow reports its own recorded cuts.
  * A grid reports row boundaries (respecting headers/footers).
  * An equation reports row boundaries.
* **Footnotes:** migrating a footnote's origin vs queueing the footnote.

Candidate edges are generated from these cuts within a tolerance window, plus any cut a constraint asks for. For example, when greedy ends a region right after a sticky chain, the candidates are: move the chain, or break inside its last breakable member at each of the last few breakpoints.

A non-greedy edge is a composition with a *forced* cut, not a truncation of the greedy composition. That way, insertions of the cut-off content (footnotes, floats) are never placed. This also addresses #5314-style "footnote precedes its reference" effects.

## 45.6 Costs

The cost vector per edge, compared lexicographically in this order:

1. sticky violations;
2. empty regions (nothing but spacing or insertions);
3. widow/orphan and footnote-migration penalties;
4. number of regions;
5. badness: unused fraction cubed, not counted for the last region or at forced breaks.

Later terms: sticky-to-previous (#5259), minimum fill (#5931), stretch/shrink (#3442).

Ties are broken deterministically in favour of the greedy edge, so that output only changes where it improves, and introspection iterations don't oscillate between equally good layouts.

## 45.7 Search strategy

* **Merging** by node key is exact but rarely happens between page-breaking paths offset by a line; they typically only rejoin at hard breaks.
* **Beam:** keep at most B nodes per region index, ranked by cost; B = 4 to start.
* **Lazy branching:** expand non-greedy edges only where the greedy edge has a non-zero penalty, or within a small window before a penalized region. This is the generalization of the restart driver's lessons.
* **Scope:** first the root flow, then flows in breakable blocks (they run their own DP and report breakpoints upward), then columns (each column is a stage; parent-scoped floats couple columns within a page).

## 45.8 Milestones, experiments and gates

| | Work | Output | Gate |
|---|---|---|---|
| M0 | §36 restart driver with `Height` / `KeepSticky` (done) | fixes #8890, #8487, #7623, #6388 | — |
| M1 | Specify sticky semantics and costs; write the expected outputs of §45.1 as failing tests | test suite for the new behaviour | agree with B on semantics |
| E1 | Measure reuse potential: relay each root-level breakable child with its first region shrunk by 1–3 lines and count how often its frames are unchanged (≈ predicate-tracked cache hit); same for later regions | hit rates per document class | reuse must be high for text and block content |
| M2 | Lazy tracked `Regions` with predicate queries; memoized layouts keyed by observations; verification fast path | faster footnote-heavy docs; measured cross-branch hit rates | no regression on normal docs |
| M3 | Distributor records cuts + penalties, supports forced cuts; nested flows report breakpoints | greedy driver reproduces today's output exactly | zero reference diffs |
| M4 | DP driver for the root flow behind a flag | quality metrics (sticky violations, widows/orphans, empty and underfull pages, page count) and time on tests + corpus + fuzzing | B's call on perf/complexity |
| M5 | Nested flows, columns, resumable steps for grid; sticky semantics on by default; remove superseded heuristics (`need` lookahead, sticky snapshot/restore, `KeepSticky`) | fixes #6924, #6546, #7229, #5357, #5376 | — |
| M6 | Features as cost terms: sticky direction, minimum fill, stretching | — | — |

## 45.9 Risks

* **Blow-up.** Branching × footnote relayout loops. An earlier sticky-optimizer experiment hit 24 s / 4.5 GB on a 200-section document with footnotes, because decisions were re-derived inside relayout loops. Decisions must live at edge granularity; a composition with a fixed cut must be deterministic, with no nested search.
* **Nested DPs.** Nested flows run their own DP per parent branch. Memoization by observation intervals is what keeps this bounded (E1/M2 must confirm).
* **Memory.** More memoized entries (per interval) and more live `Work` checkpoints.
* **Complexity for maintainers.** The distributor becomes "fill and record" rather than "fill with heuristics". Net code may shrink once heuristics are removed, but the driver and cost model are new concepts.
* **Behaviour change.** The new sticky semantics intentionally changes output. Existing references (e.g. `issue-5296`, `block-sticky-breakable`) need review.

## 45.10 Open questions

* **Lexicographic or weighted costs?** And user-configurable page-level costs, analogous to `text.costs`?
* **Empty regions:** should they be an explicit penalty (the `issue-5296` references render one deliberately today)?
* **Scope of v1:** root flow only, or also breakable blocks? Breaking early *inside* a sticky block (#6546) requires at least nested breakpoints.
* **Floats:** keep greedy float placement initially, or make it part of the search?
* **Grid-internal stickiness** (headers, #8050, #4841): its own DP or breakpoints only?

---

# 46. Lazy multi-region layout: implementation

Status: implemented on top of §36 (uncommitted). Every multi-region layouter now produces one frame per call. `Regions` is **not** tracked yet (§46.7).

## 46.1 Protocol

```rust
BlockElem::multi_layouter(captured, f)

f: fn(&Packed<T>, &mut Engine, Locator, StyleChain, Regions,
      Option<&MultiState>) -> SourceResult<MultiStep>

struct MultiStep { frame: Frame, next: Option<MultiState> }
struct MultiState(Arc<dyn Any + Send + Sync>);
```

* A layouter receives the current region followed by *predictions* of the upcoming ones, plus the state it returned for the previous region (`None` for the first). It returns this region's frame and, if it continues, the state for the next region.
* States are immutable. A step can be re-run from the same state with different regions. Verification (§36.3) depends on this: it re-runs the previous step with the actual future.
* The old eager `multi_layouter` (regions → `Fragment`) is gone. `layout_steps` drives a layouter over known regions; it is only used for single-region callers (unbreakable blocks).
* `layout_fragment_step` steps arbitrary content. `layout_fragment` (eager) remains for callers that need all frames at once: footnote entries (root flow, known regions), grid rowspans (laid out once their heights are final), unbreakable and non-lockstep grid rows.
* Deviations from §8–§11: `MultiState` is type-erased and not `Hash`, so only first steps are memoized (§46.6). There is no per-layouter associated state type.

## 46.2 Content: resumable flows

Content is stepped by `prepare_flow` (memoized realization + collection, keyed on content, locator, styles, column width, full height, `expand.x`) and `layout_flow_step` (one `compose` call, restarts disabled).

* Collected children are owned (§33 revision "C2"). Styles are stored relative to the flow's styles (only the links added inside the flow are cloned) and locators by their local hash, re-linked with `Locator::with_local`.
* A flow is only resumable if every child's style chain has the flow's styles as a suffix. This held for all 23,066 nested flows measured. A non-resumable flow falls back to `ContentState::Replay`, which re-lays out in full per region (never observed).
* Per-region locators are derived with `SplitLocator::nth`, so stepped and eager layouts assign identical locations.

## 46.3 Breakable blocks

`layout_multi_block` is now the only implementation for breakable blocks. The eager version, `MultiChild::layout_full`, `layout_multi_impl` and the old replay `MultiSpill` were deleted. The new `MultiSpill` holds a `BlockState`:

```rust
struct BlockState {
    body: MultiState,          // where the body continues
    history: RegionHistory,    // heights of the regions already laid out
    relayout: Option<Abs>,     // width after the auto-width consistency relayout
    skip_first: bool,          // first frame left undecorated
}
```

* `RegionHistory` rebuilds the regions the whole block would have been laid out in (committed heights + current + predictions). That is what keeps fixed-height blocks (`distribute`) identical.
* Two lookaheads remain. Both lay the rest of the body out into the *predicted* regions, and both are covered by verification:
  * the auto-width consistency check, for content bodies with `!expand.x`;
  * the empty-first-frame checks (`skip_first`, `exist_non_empty_frame`), only when the first frame is empty.
* The first step is memoized (`layout_multi_first_impl`). Later steps are not.

## 46.4 Converted layouters

| Layouter | State | Remaining lookahead |
|---|---|---|
| pad, columns, `layout()` | content state (+ the function's result) | — |
| stack | `{ child, Resume::Start \| Continue(MultiState) }` | — |
| list / enum | items and measured widths + stack state; per item: marker, offsets, body state | empty-first-frame skip and the baseline of the first non-empty frame (only if the first frame is empty) |
| block equation | math layout + number laid out once (`PreparedEquation`) + row cursor | — |
| grid / table | `GridSnapshot` (see below) | rowspan measurement and simulation, non-lockstep auto rows, header/footer assumptions (§31) |
| test harness `bounds` | content state | — |

**Grid.** Instead of §23–§28's drafts, grid became *resumable*:

* `GridLayouter::advance` processes one row (or header or footer) at a time.
* The layouter can be snapshotted between rows (`GridSnapshot`: header references become indices) and restored.
* A step lays out rows until the requested region's frame is **final**, i.e. no pending rowspan starts at or before it. It then renders that region's fills and strokes (`in_last_region` is known then) and hands the frame out.
* It keeps two snapshots:
  * a *live* one, reused when the next regions equal the prediction;
  * a *base* one, taken at the start of the next region, from which it re-lays out otherwise.
* Frames of regions already handed out are discarded. A rowspan finishing later skips them, which is sound because a frame is only handed out once no pending rowspan touches it.

**Lockstep auto rows.** A breakable auto row whose cells all start in and only span that row is laid out one region at a time. Each step does two things:

* it measures every cell's next region, with the same pods `measure_auto_row` uses;
* it lays every cell out into that region's resulting row height, predicting the upcoming ones.

This is the grid part of the "cells in lockstep" idea (§32). Rows with rowspans, repeated header/footer rows and unbreakable groups keep the eager measurement against predicted regions. The predicted final region is passed as a finite entry, matching the eager `layout_multi_row` pod, which has no repeated region.

## 46.5 Results

**Correctness.**

* Test suite: 3790/3790 pass, no reference changes (the §36 tests included).
* Corpus: all **5,240** documents under `~/Documents` that compile on `main` (mostly the Typst Universe packages repository: templates, manuals, examples) produce byte-identical PDFs on `main`, the §36 build and this build.
* Adversarial documents keep the §36 fixes: output equals the §36 build and differs from `main` exactly where `main` loses content.

**Performance** (release, `hyperfine` means, 5 runs, ms):

| Document | `main` | §36 | lazy (this) | Speedup |
|---|---|---|---|---|
| long table, footnotes in cells (71 p.) | 5891 | 5900 | 307 | 19× |
| whole document in a grid cell (38 p.) | 1990 | 1929 | 147 | 13.5× |
| rowspans across pages + footnotes | 1717 | 1707 | 197 | 8.7× |
| breakable block, footnote per paragraph | 908 | 924 | 128 | 7.1× |
| pad, footnote per paragraph | 815 | — | 121 | 6.7× |
| `layout()`, footnote per paragraph | 677 | — | 127 | 5.3× |
| stack of breakable blocks + footnotes | 622 | — | 164 | 3.8× |
| list, footnote per item | 420 | — | 137 | 3.1× |
| tables + auto-placed figures | 157 | 178 | 123 | 1.3× |
| two columns, tables, footnotes | 146 | 165 | 110 | 1.3× |
| template (thesis-like) | 156 | 156 | 137 | 1.1× |
| plain text | 177 | 176 | 178 | 1.0× |
| nested enum, block equations, sticky stress | ≈ | ≈ | ≈ | 1.0× |

**Incremental** (`typst watch`, median of 5 edits, ms; "top"/"inside" insert a paragraph at the start of the document / of the big container):

| Document | edit | `main` | lazy |
|---|---|---|---|
| long table | trivial / end / top | 54 / 2055 / 2400 | 28 / 142 / 155 |
| document in a grid cell | end / top / inside | 52 / 1485 / 1515 | 38 / 46 / 45 |
| breakable block | end / top / inside | 246 / 791 / 753 | 22 / 49 / 48 |
| pad | end / top / inside | 582 / 707 / 540 | 23 / 25 / 30 |
| list | end / top | 20 / 132 | 24 / 26 |
| template, plain | all | 29–55 | 27–54 |

**Corpus timing** (the 5,240 documents above, serial, one run each, alternating order):

* Total: 189.8 s on `main` vs 188.5 s here. Geomean ratio 0.998, median 1.000.
* Peak memory: geomean ratio 1.004.
* Of the 63 documents above 300 ms, one regressed by more than 5%: `lacy-ubc-math-project` `test/math100-p3.typ`, 760 → 910 ms and 332 → 495 MB. It comes from lockstep rows. With lockstep disabled, the same build takes 697 ms / 258 MB, faster and smaller than `main`, with identical output.
  * The document uses `equate`, which turns each multi-line equation into `block(grid(..))` with one auto row per line.
  * With lockstep, grids take 2.7× as many steps (1,382 vs 517), and 506 rows "continue" into another region on an 8-page document. Simple `equate` and grid reproducers don't show this.
  * The likely cause: a cell's measurement step reports a continuation where the eager measurement fits in one region, so the grid emits extra (invisible) steps. See §46.8.

Where does the time go on `main`? A spilling child is laid out in full once per region, plus once per footnote relayout, which is quadratic in pages. With steps, each region is composed once and later regions are never laid out early.

## 46.6 Caching

* Memoized: `prepare_flow_impl` (realization + collection), every step of a breakable block (`layout_multi_step_impl`, see §48), `layout_single_impl`, `layout_fragment_impl` (eager callers).
* The within-run `Caches` were removed (§47.5).

## 46.7 Tracked regions: not done, measured instead

`Regions` is still a plain value, hashed in full at memoization boundaries.

I instrumented the two memoization boundaries that take `Regions`: first block step and eager fragment. For each memo miss, the analysis asks whether an earlier miss of the same element, with the same styles, differed only in some part of the regions:

| Document | misses | future-only diff, same output | height-only diff, same output | other / first / tracked deps |
|---|---|---|---|---|
| plain | 1,134 | 0% | 34% | 66% |
| template | 4,575 | 0% | 17% | 83% |
| long table | 10,367 | 16% | 12% | 72% |
| document in a grid cell | 2,984 | 74% | 2% | 24% |
| list | 657 | 0% | 0% | 100% |
| adversarial floats | 560 | 9% | 6% | 85% |

* Varying futures come from grid measurement vs. output pods and from footnote relayouts. Tracked future queries (§4–§5) would turn those misses into hits. Blocks that fit only ask `may_break`/`may_progress` on the future, if at all.
* Height-only misses with identical output need *predicate* tracking of the current height: "does `h` fit?" rather than "what is `size.y`?". That is what E1 (§45.8) asks, and these numbers are a first answer. 12–34% of first-step misses in realistic documents would be hits. Branching in a page-breaking DP produces exactly these shifted-height calls.

What implementation needs (M2 in §45.8):

* `Regions`' future becomes a lazy source (§4), queried through a comemo-tracked `slot(i)`.
* Memoized functions take the future as `Tracked`.
* The within-run `Caches` and `RegionsDesc` must stop hashing the future in full, or they re-record everything.
* Flow's height arithmetic (`regions.size.y -= h`) has to be restated as `fits` queries against the region's original height before a height-predicate API pays off.

## 46.8 Open questions

* **Grid base snapshot.** Restoring from the base snapshot drops the parts of pending rowspans that fall into regions already handed out. This is only reachable when a grid step's regions differ from both its live prediction and the verified state. Every stepper sits directly under a flow's `MultiSpill`, which verifies first, so it should not happen. It is not asserted.
* **Lockstep extra steps (§46.5 outlier).** Find why lockstep rows in `lacy-ubc-math-project` continue into further regions. The output is identical but time is +30% and memory +90%.
  * Candidates: a cell flow whose `done` condition differs between measurement and eager layout, or rows whose last region is mispredicted (predicted full, actually shorter), which forces verification re-runs in every spilled cell child.
  * A one-region-lookahead variant would give the output pod the exact next height: measure region k+1 before laying out region k, and reuse that measurement at the next step.
  * Memoizing the first content step (tried) made it worse: 1047 ms, 629 MB.
* **Lockstep coverage.** Rows with rowspans still measure against predicted regions. Extending lockstep to rowspans needs per-rowspan measurement state across rows.
* **API naming.** `MultiState` / `MultiStep` versus §8's typed `MultiStep<S>`: type erasure keeps the callback macro simple, at the cost of a downcast per step.

---

# 47. Tracked regions: implementation

Status: implemented on top of §46 (uncommitted). Output is unchanged: 3790/3790 tests, and all 5,240 corpus documents are byte-identical.

## 47.1 Questions instead of fields

`Regions`' fields are private. Layout asks the most specific question available:

| Question | Replaces |
|---|---|
| `fits(h)`, `fits_next(h)` | `size.y.fits(h)`, `iter().nth(1)…fits` |
| `limited(h)`: `min(h, remaining)`, which reveals the height only on overflow | `used.min(size)` |
| `is_full()`, `may_break()`, `may_progress()`, `has_backlog()`, `has_region(i)`, `is_finite()` | backlog / last inspection |
| `consume`, `limit`, `at_least`, `with_height`, `with_width`, `with_expand`, `with_full`, `with_future`, `shrink(inset)` | mutating `size.y`, struct literals, `map` |
| blunt: `height()`, `size()`, `backlog()` (iterator), `last()`, `iter()`, `map`, `materialize` | — |

Width, `full` and `expand` are plain values. They don't change when content shifts, so reading them costs no reuse.

Remaining blunt reads:

* expanded or fr-sized frames, which need the exact height;
* fixed-height breakable blocks (`distribute`);
* multi-column and parent-scoped floats;
* auto float alignment;
* spill bookkeeping (`RegionsDesc`, `RegionHistory`);
* grid (below).

## 47.2 Derived regions

```rust
enum Kind<'a> { Explicit { height, full, backlog, last }, Derived(Derived<'a>) }
struct Derived<'a> {
    outer: &'a dyn Outer,        // a RegionsLink around Tracked<Regions>
    index: usize,                // which outer region is the first one
    shift, lower, upper: Abs,    // first height = clamp(outer − shift, lower, upper)
    inset: Abs,                  // subtracted from all heights and full
    full: Option<Abs>,
    future: Option<(&'a [Abs], Option<Abs>)>,
}
```

* The first region's height is `clamp(outer − shift, lower, upper)`. That normal form represents any sequence of `consume` / `limit` / `at_least` / `with_height` / absolute `shrink`. Every question is forwarded to the outer regions with the transformation applied, e.g. `fits(h)` becomes `outer.fits(index, shift + h)` unless the bounds decide it.
* Tracked questions (`#[comemo::track]`) take the region index: `slot`, `fits`, `fits_into`, `below`, `height`, `full`, `finite`, `may_progress(i, used)`, `last`, `width`, `expand`.
* `Tracked<'a, Regions<'a>>` is invariant in `'a`, because its sink type is a projection through `Track`. A `RegionsLink` stores it behind `&dyn Outer`, like `LocatorLink`, so derived regions stay covariant.
* `Hash` of derived regions uses the outer link's address. That is only meaningful within one layout run, and derived regions are never used as memoization keys.
* **Floating point.** Derived heights are `x − Σhᵢ` instead of `((x − h₁) − h₂) …`. `fits` decisions are unaffected (EPS is 1e-4 pt). Grid, however, compares heights for exact equality (`height() != initial_after_repeats`), so it `materialize`s its regions at the start of each step. That was the only place where this mattered.

## 47.3 Where regions are tracked

Tracking everything was a net loss (full compiles +2% to +20%). Grid cells are laid out into exact heights computed by the grid: measured with the remaining height, then relaid out into the expanded row height. The answers differ anyway, so recording every line's `fits` question was pure overhead, and memo misses were identical with and without tracking.

Tracking is therefore per call site, not per region kind:

* **Tracked:** the first step of breakable blocks (`layout_multi_first_impl`) and footnote entries (`layout_fragment_tracked`). Both lay out into regions that depend on the content's position.
* **Hashed:** eager layout (`layout_fragment`: grid cells, rowspans, replay). It materializes derived regions first.

## 47.4 Invalidation by constraint (§34 revived)

Every spill step now records the questions it asked into a `RegionsConstraint` (`Track::track_with`; this also works through the memoized first step, since hits replay into the sink). On the next region, the flow validates the constraint against the previous region followed by the actual future:

1. Valid: the emitted frame and continuation are unchanged, by determinism. No re-run.
2. Invalid: re-run and compare frames (§36.3). If the frame changed, restart or fall back (§36.4–36.5).

This replaces the exact-equality `predicts()` pre-check. The §36.8 objection (validation over-approximates) is largely gone, because `may_progress`, `fits` and so on are recorded as yes/no answers, not as heights.

Two pitfalls:

* **Normalized future.** The verification regions must leave trailing entries equal to the repeated height to the repeated region. An explicit entry changes `may_progress` (explicit regions count as progress), fails validation, and forces a re-run. That re-run exposed a latent grid panic: `repeating_header_heights.drain` out of range in `place_new_headers` when an explicit backlog entry equals the repeated height (`grid-subheaders-repeat-replace-short-lived`). It is reachable whenever the regions have that shape, e.g. after restart predictions.
* **Same pod.** Validation must use the same adjusted regions as the step (`expand.y &= alone`). Otherwise it fails for every root-level block.

## 47.5 The flow's per-run caches are gone

`Caches` (main's `CachedCell`) deduplicated identical consecutive layouts of a child within one flow run. Disabling them changed full-compile time by −1.1% to +1.7% on six documents, i.e. noise. They were removed.

## 47.6 Results (release, vs §46)

Full compile (20 runs for the noisier ones):

| Document | §46 | tracked |
|---|---|---|
| plain, template, block, list, pad, rowspans, doc-in-cell, columns | — | 0.96–1.02× |
| long table | 307.8 ms | 315.3 ms (1.02×) |
| tables + floats | 126.0 ms | 131.4 ms (1.04×) |

Fewer layouts on tables + floats: block steps 3380 → 2452, pad 3178 → 2250, grid steps unchanged at 214.

Incremental (median of 5):

| Document | edit | §46 | tracked |
|---|---|---|---|
| breakable block + footnotes | top / inside | 45.8 / 48.9 ms | 31.7 / 36.6 ms |
| columns | top | 29.4 | 27.1 |
| long table | end / top | 131.7 / 144.1 | 144.9 / 154.3 |
| tables + floats | top | 45.5 | 51.1 |
| others | — | — | ±5% |

## 47.7 Open

* **Grid (done, §47.8).**
* **Continuation steps** are not memoized (`MultiState` isn't `Hash`). A failed validation re-runs a step in full.
* **Predicate granularity.** Lines ask `fits(used + h)` one by one, so a block with n lines records n questions. Asking for larger totals first would reduce the recorded calls without losing reuse, since answers are monotone.

## 47.8 Grid asks questions

* **Bookkeeping by consumed amounts.** `Current` stores the heights used up in the region (`consumed`, a log) instead of `initial: Size` and `initial_after_repeats: Abs`. "Was anything placed since the repeats?" compares consumed amounts, not remaining heights. The regions at region start are kept (`GridLayouter::initial`), so frame sizing uses `initial.limited(used)`.
* **Replayable snapshots.** `GridSnapshot` holds the consumption log instead of an absolute size, and restoring replays it onto the step's regions.
* **Derived measurement pods.** Cells in the current region are measured with pods derived from the grid's regions (`with_width`, plus followup regions if headers and footers are subtracted), in both the lockstep and the `measure_auto_row` path.
* **Blunt reads that remain:** rowspans (`height_after_repeats` replays the log), rows spanning several regions, fr rows.
* **No second-guessing of predictions.** `step_grid` no longer compares the live snapshot's predicted regions with the actual ones and has no "base" fallback snapshot. Flow only continues a step with regions that validate the previous step's constraint, or with the predicted regions (§47.4), so the live snapshot is consistent with them. This removes the rowspan-dropping base path (§46.8).

**Results.**

* All 5,240 corpus documents and all benchmark PDFs are byte-identical.
* One test differs: `grid-subheaders-too-large-repeating-orphan-before-auto`.
  * Main produces 3 pages there by floating-point accident. The cell's `may_progress()` compares the remaining height `((L − h₀) − h₁) − h₂` with the next region's `L − (h₀ + h₁ + h₂)`. These differ in the last bit, so the row moves to a new page with the same space, twice.
  * Consistent arithmetic answers "no progress" and places the row with overflow on page 1. That is exactly what the sibling test with a relative row (`…-before-relative`) renders.
* Performance: rowspans 0.96× full / −8% incremental; others within noise. Many small tables with floats are +12% (tracking overhead without reuse).
* The dominant cost left in table documents is unmemoized continuation steps: an edit at the end of a long table re-runs every page's grid step.

---

# 48. Memoized continuation steps

Status: implemented (uncommitted). All 3790 tests pass, with one reference updated (§47.8). All 5,240 corpus documents are byte-identical.

## 48.1 Mechanism

* **States are identified, not hashed.** `MultiState` gets a unique ID from a global counter when it is created. `Hash` / `Eq` use the ID. States are immutable and layout is deterministic, so a state ID plus the other inputs determines the step. IDs are never reused, so there are no false hits. Content-hashing states would be expensive, since they contain frames and grid snapshots (see `frame-hashing-is-expensive`).
* **One memoized step function.** `layout_multi_step_impl(.., regions: Tracked<Regions>, state: Option<&BlockState>)` handles every region of a breakable block. `BlockState` derives `Hash`.
* **Reuse chains.** When a step is a cache hit, comemo returns the cached output, including the *same* next state with the same ID. The next step can then hit too, so an unchanged table is reused page after page. After a change, new states get new IDs, and everything downstream of the change is recomputed.
* **Every flow, not just the root.** Memoizing only in root flows was tried; with the glyph fix below there is no reason to restrict it.

## 48.2 The memory cost was frame copy-on-write, not comemo

Measured with perf and DHAT (from nix-shell) on the long table, memoizing steps before the fix:

* CPU instructions +0.2%, page faults +36%, peak heap +39 MB (107.7 → 146.5 MB), total allocation +4 MB.
* 34 of the 39 MB are copy-on-write clones: `Frame::insert` / `prepend` / `inline` call `Arc::make_mut` on frames the cache also holds, and `TextItem.glyphs: Vec<Glyph>` makes each clone deep.
  * Grid's `layout_cell` prepends introspection tags to cached cell fragments: 33.7 MB.
  * `Composer::column` inlines soft child frames: about 11 MB.
* comemo's recorded calls: about 1 MB. The boxed flow children (C2) are not a factor.

**Fix:** `TextItem.glyphs: EcoVec<Glyph>`. Cloning a frame now copies item headers only. Output is unchanged. It also lowers memory without any memoization change (plain 91 → 62 MB, long table 136 → 99 MB, rowspans 99 → 78 MB), so it is worth upstreaming on its own.

## 48.3 Results

Compared with §46 (lazy, untracked) = "e".

**Full compile**, with every step memoized: parity or better, and memory at or below e.

| Document | e | now |
|---|---|---|
| plain | 180 ms / 91 MB | 161 ms / 61 MB |
| template | 137 ms / 107 MB | 131 ms / 93 MB |
| tables + floats | 127 ms / 72 MB | 121 ms / 59 MB |
| long table | 313 ms / 136 MB | 318 ms / 115 MB |
| doc in grid cell | 154 ms / 70 MB | 153 ms / 71 MB |
| 300 small tables + floats | 113 ms | 127 ms (§48.4) |

**Incremental** (median of 5, ms):

| Document | end edit | top edit |
|---|---|---|
| long table | 136 → 36 | 144 → 154 |
| doc in grid cell | 39 → 19 | 44 → 50 (inside 45 → 53) |
| breakable block | 23.5 → 21.6 | 48.5 → 31.6 |
| rowspans | 55 → 19 | 114 → 102 |
| tables + floats | 44 → 25 | 48 → 30 |
| list | 22.5 → 13.5 | ≈ |
| plain | 49 → 37 | 51 → 40 |

**Where it still costs: top edits of one giant grid.** Everything shifts, so every step misses. Each miss records its tracked calls, including replays of every nested cache hit (every cell), and builds a cache entry.

## 48.4 The small-tables regression

Document: 300 small tables, each holding a breakable block, interleaved with auto-placed floats.

| Build | instructions | page faults | block steps |
|---|---|---|---|
| e | 886 M | 11.2 k | 2419 |
| tracked regions (§47) | 949 M | 12.2 k | 2403 |
| + grid questions (§47.8) | 974 M | 12.3 k | 2403 |
| + memoized steps | 976 M | 11.5 k | 2387 |
| + cached `full` | 965 M | — | — |

It is CPU overhead per question, not memory and not extra layout. Floats keep changing the available space, so every relayout asks again and nothing is reused. The overhead (callgrind, inclusive):

* SipHash of recorded calls and memo keys: +19 M.
* `Constraint::emit` (the spill-validation sink): +13 M. Every region question is recorded twice, once for the memo and once for the spill constraint.
* Forwarding cell-measurement questions through derived pods: roughly +30–45 M.
* `MultiSpill::layout`: +23 M.

`Regions::link` now reads `full` once instead of forwarding every `base()` call (−11 M).

## 48.5 Options

1. **Nested hits by reference in comemo.** Today an outer memoized function's constraint contains all calls of nested memoized functions, flattened. On a nested hit, comemo replays the nested entry's calls into the outer sink.
   * Referencing the nested entry instead would shrink outer constraints and avoid the replay cost.
   * This only works for tracked values that the outer and inner call share (world, introspector, route).
   * Derived arguments (`RegionsLink`, `LocatorLink`) already record *translated* questions at the outer level. Their inner calls can't be validated without re-running the outer code that built them.
   * It is a comemo design change; measure first which tracked types dominate the flattened calls.
   * For mutable calls (the `Sink`), see §54.3: they are only replayed, never validated, so references are simpler there.
2. **Fewer questions.** Ask about runs of lines in one question (`fits(used + total)`), and binary-search the break point only when the run doesn't fit. Answers are monotone, so the recorded questions still determine the break point exactly: reuse is unchanged, and recording drops from one call per line to O(1) or O(log n) per run.
3. **Drop the separate spill constraint (done, §48.6).** Now that every step is memoized, re-invoking the previous step with the actual future *is* the validation. comemo walks the call tree with the new regions and returns the same output on a hit, so `Frame::identical` short-circuits on pointer equality. On a miss it recomputes, exactly as the explicit re-run does. `RegionsConstraint`, `track_with` and `MultiSpill.constraint` become unnecessary, which removes the double recording (§48.4).

## 48.6 Spill validation by memoization

§47.4's `RegionsConstraint` is gone. `MultiSpill::layout` always calls the (memoized) previous step with the actual future and compares frames. This is exactly the "let comemo decide what is invalidated" idea of §34: a cache hit returns the same frame; a miss is the §36.3 relayout.

**Full compile, ms / MB:**

| Document | main | e | now | now / e |
|---|---|---|---|---|
| long table | 5755 / 2509 | 309 / 135 | 299 / 114 | 0.97 |
| doc in grid cell | 1860 / 1936 | 148 / 70 | 147 / 72 | 1.00 |
| breakable block | 883 / 1042 | 131 / 57 | 124 / 57 | 0.95 |
| pad | 787 / 878 | 122 / 61 | 120 / 56 | 0.98 |
| list | 423 / 321 | 138 / 60 | 138 / 59 | 1.00 |
| rowspans | 1655 / 668 | 201 / 100 | 184 / 85 | 0.91 |
| tables + floats | 156 / 78 | 127 / 72 | 113 / 60 | 0.89 |
| 300 small tables + floats | 119 / 65 | 112 / 66 | 120 / 67 | 1.07 |
| columns | 148 / 69 | 111 / 62 | 108 / 54 | 0.97 |
| plain | 178 / 90 | 176 / 90 | 159 / 61 | 0.90 |
| template | 147 / 115 | 134 / 107 | 126 / 93 | 0.94 |

**Incremental, e → now (ms):**

| Document | end | top | inside |
|---|---|---|---|
| long table | 135 → 36 | 145 → 143 | — |
| doc in grid cell | 37 → 18 | 42 → 48 | 45 → 49 |
| breakable block | 22 → 21 | 48 → 30 | 49 → 32 |
| pad | 24 → 20 | 25 → 22 | 30 → 28 |
| list | 22 → 13 | 28 → 24 | — |
| rowspans | 54 → 17 | 108 → 98 | — |
| tables + floats | 45 → 25 | 47 → 28 | — |
| small tables | 31 → 30 | 32 → 33 | — |
| plain | 50 → 37 | 56 → 38 | — |

**Small tables:** 886 M → 950 M instructions (+7%, down from +10%).

## 48.7 Where the questions come from

Tracked questions executed in one compile (a question from a nested view executes once per tracked level it passes):

| Question | small tables | long table | doc in cell | plain |
|---|---|---|---|---|
| `fits` | 26.9k | 54.6k | 8.1k | 4.0k |
| `width` + `expand` + `full` (at `Regions::link`) | 14.2k | 54.7k | 23.8k | 6.2k |
| `below` (`limited`) | 6.3k | 10.7k | 5.8k | 4.0k |
| `slot` / `may_progress` / `last` | 10.5k | 9.5k | 6.0k | 1.8k |
| `height` (blunt) | 2.9k | 7.5k | 5.7k | 0.2k |
| lines placed | 9.6k | 10.8k | 18.5k | 6.3k |

Each executed question costs roughly 500–1000 instructions: SipHash of the call and its answer, sink emission, and a call-tree node on insertion. That is the remaining small-tables overhead.

## 48.8 One question for the base properties

`Regions::link` now asks a single tracked question, `tracked_base() -> (width, expand, full)`, instead of three. These properties don't change when content shifts. The principled alternative would be a comemo extension: methods in a `#[track]` impl marked as *key methods*, whose results are hashed into the memo key like hashed arguments. comemo already separates `Input::key` (hashed parts) from `Input::call` (tracked parts); for `Tracked<T>` the key part is currently empty.

Results. Tests pass and all 5,240 corpus documents are byte-identical.

**Instructions, single-threaded (M):**

| Document | e | before | now |
|---|---|---|---|
| small tables | 886 | 950 | 942 |
| long table | 2335 | 2345 | 2316 |
| doc in cell | 1274 | 1266 | 1252 |

**Full compile, now / e:**

* long table 0.94, doc in cell 0.97, breakable block 0.96, pad 0.96, list 0.98
* rowspans 0.90, tables + floats 0.87, columns 0.95, plain 0.90, template 0.93
* small tables 1.05

**Incremental, e → now (ms):**

| Document | end | top | inside |
|---|---|---|---|
| long table | 136 → 37 | 143 → 142 | — |
| doc in cell | 37 → 18 | 43 → 46 | 43 → 47 |
| breakable block | 22 → 20 | 49 → 31 | 51 → 32 |
| list | 22 → 13 | 26 → 25 | — |
| rowspans | 55 → 18 | 109 → 98 | — |
| tables + floats | 45 → 26 | 46 → 29 | — |
| small tables | 33 → 31 | 33 → 32 | — |
| plain | 51 → 37 | 53 → 38 | — |

Remaining regressions: small-tables full compile (+5%) and doc-in-cell top/inside edits (+6% / +9%). Asking about runs of lines at once (§48.5, option 2, via knowledge plus a probe on `Regions`) targets both.

# 49. Review fixes

Two code reviews of §46–§48 (`REVIEW.md`, `REVIEW2.md`) reported bugs, a crash, inefficiencies and cleanups. All are addressed. Tests pass (3,793, three of them new, plus unit tests for `Regions`), all 5,240 corpus documents are byte-identical, and the restart stress documents (§43) lose or duplicate no content and are byte-identical to before.

## 49.1 Content lost when skipping a full subregion

§36.6 dropped the frame a spill produced in a full subregion if it had no height and only shapes. If the block *ended* there, that frame held its last content: in `flow-spill-skip-full-region`, a red `line` after a page break and the block's final stroke, which `main` draws on the page after the float.

Dropping only frames without any content fixed that, but made every stroked block that merely continues through a full page lose alignment. In the stress documents, the resulting fallbacks left gaps that added up to eight pages per document. The distinction that matters is the block's body versus its decoration: `BlockStep::decoration_only` reports that nothing of the body was placed. A frame is dropped only if the block continues and the frame is decoration-only with no height. `main` never lays the block out into the full subregion, so it doesn't draw that decoration either.

## 49.2 Consistent widths after mispredictions

The first region decides whether an auto-width body is relaid at a fixed width, using predictions of the upcoming regions (§46.3). Verification only re-checks the previous step. So if the third region or a later one was mispredicted, a later frame could get a different width: 0pt instead of 150pt in `flow-spill-block-width-consistent`, where a float leaves no room for the widow pair.

`BlockState::width` keeps the width of the frames so far. If a continuation frame's width differs, the body is laid out again at `max(width, frame width)` from that region on. That matches `main`, which relays out the whole block for every spill.

## 49.3 Predictions as an overlay

`Predictions::apply` padded the backlog with copies of `last`, turning repetitions of the final region into finite backlog regions. That changed `may_progress` (oversized unbreakable content was deferred instead of placed with overflow), `has_backlog` (nested expanding flows kept emitting frames), and `full`.

The explicit future of `Regions` now has three parts: `backlog`, `last`, and `predicted`, the remaining heights of the first repetitions of the final region (`Regions::with_predicted`). They stay `Slot::Repeat`, `may_progress` compares against the unpredicted `last`, and their `full` is `last`. Unit tests in `regions.rs` cover this and the end-of-regions guards (§49.7).

**Pitfall: blunt copies.** The first version kept the blunt `backlog()` limited to finite regions. Code that copies regions from `backlog()` and `last()` (grid measurement, rowspans) then silently assumed full pages for predicted repetitions. Verification couldn't notice, since the copy never asked about predictions: `flow-spill-restart-table` lost its restart and produced a row whose content overflowed the cell. Therefore:

* `backlog()` includes the predicted repetitions, so any copy has the right heights and merely treats them as finite.
* `predicted()` says how many there are, for copies that preserve the kind: spill descriptions, `RegionHistory`, `predict` in compose, and `Regions::map`/`materialize`.
* For derived regions, this is a tracked `len` question, so it's sound.

Spill verification (`RegionsDesc::followed_by`) now gives the actual regions the kind the previous step saw them as (finite or repetition), instead of trimming trailing entries equal to `last`. This also made three column variants of grid tests identical to `main` again, where the trimming had turned finite column regions into repetitions.

## 49.4 Stale predictions

A learned prediction was never raised, even when a later restart moved insertions out of the subregion. The block then broke early in front of it and left space unused.

Now, a mismatch with *more* space than predicted also restarts, and learning replaces the height. To keep §36.7's property (after an oscillation, the fallback leaves space unused instead of overflowing), raising requires two remaining restarts and lowering one. The last restart for a subregion therefore always lowers its prediction. In the adversarial document, the four oscillating subregions each take one raise and one more lowering (52 restarts instead of 44) and end in the same fallback. The output is byte-identical and the time unchanged. The other stress documents never raise.

The height a restart learns is now the subregion's available height before column balancing limits it (R2#4), so balancing no longer teaches a prediction that is too small.

## 49.5 Smaller correctness fixes

* When `finalize` restores the initial state because all items are migratable, a frame placed by the spill is discarded. The spill is now marked unaligned, since it skipped the subregion (R2#6).
* Grid headers: `place_new_headers` drained the heights of pending headers along with those of conflicting repeating ones. When a short-lived header followed a pending one, the pending header became repeating without a height, and the next header's `drain` panicked. This also crashes `main` (with `page(columns: 2)`). Test: `grid-subheaders-repeat-replace-short-lived-columns`.
* Regions past the end: `fits` and `fits_into` are false, `below` is `None`, and `height` is zero, instead of treating them as zero-height regions (REVIEW §2.2).
* Hashing derived regions panics. They are only meaningful through their questions; memoized functions get materialized or tracked regions.

## 49.6 Efficiency

**Locators.** Grid cells, list items and stack children computed their locators for the whole element at every continuation step: for a long table, every cell was hashed into a map on every page. The local hashes are now computed once and kept in the state. Continuations reattach them with `Locator::with_local`.

**Auto-height blocks.** Continuations of auto-height blocks no longer rebuild their regions from the history (REVIEW §2.5). Only fixed heights, which are distributed over all regions, need it. The only blunt question left is the current height, which becomes `full` (as for every region after the first, also on `main`).

This is a trade-off, not a pure win. With forwarded regions, the body's questions (for a table: every row and line) are recorded on the block's step and validated on every cache hit. The rebuilt regions answered them locally. Measured, ms:

| Document | full: forwarded / rebuilt | end edit | top edit |
|---|---|---|---|
| long table | 276 / 269 | 42 / 36 | 129 / 112 |
| doc in grid cell | 136 / 145 | 19 / 18 | 37 / 46 |
| rowspans | 153 / 152 | 19 / 17 | 81 / 75 |
| breakable block | 121 / 126 | 20 / 21 | 29 / 29 |

Forwarding is kept, as it matches the design. The long table's end edit is its cost, and batched questions (§48.5, option 2) are its remedy.

**Spill descriptions** still read the regions bluntly when a spill is created (REVIEW2). The next region needs the previous region and its future, but that region no longer exists by then. For the root flow the regions are explicit, so this costs nothing. For nested flows, it makes a step with a spilling child depend on its exact regions. Avoiding this would need a "prepend" view: regions whose first region is explicit and whose rest forwards to the current regions.

**Full compile, ms (before → now):** long table 291 → 276, rowspans 183 → 153, doc in grid cell 145 → 136, tables + floats 113 → 110, breakable block 124 → 121, list 139 → 136. Plain, template, small tables, pad and columns are unchanged.

**Incremental, before → now (ms):**

| Document | end | top | inside |
|---|---|---|---|
| long table | 36 → 42 | 140 → 129 | — |
| doc in grid cell | 20 → 18 | 47 → 37 | 48 → 42 |
| breakable block | 21 → 21 | 31 → 29 | 33 → 33 |
| list | 13 → 13 | 25 → 21 | — |
| rowspans | 19 → 19 | 98 → 79 | — |

## 49.7 Cleanup

* The `TYPST_FLOW_STATS` instrumentation (`log_event`) is gone.
* `layout_and_modify` is implemented with `layout_with_modifiers`.
* `ColumnOptions::resolve` is shared by `configuration` and `ColumnOptions::width`.
* `flow::layout_remaining` replaces four loops that laid out the remaining steps.
* One `grid::is_empty_frame` replaces three copies.
* The always-`None` `CellMeasurementData::backlog` is gone.

# 50. Benchmarks against `main` (2026-09-28)

43 documents: everyday ones, size series of the pathological cases (long tables, rowspans, whole documents in a grid cell, nested breakable blocks) and large stress documents of up to 1,745 pages. Four builds:

* `main`: the branch point.
* `cur`: this branch.
* `hist`: `cur` with §49.6's change reverted, so all block continuations rebuild their regions from the history (REVIEW §2.5 "off").
* `lazy`: `cur` with a prototype of lazy spill descriptions (below).

The builds produce byte-identical PDFs on every document that `main` compiles, and `lazy` passes the test suite. Every run was a single process in a cgroup capped at 8 GB without swap, so "> 8 GB" means it was killed at the cap. Full compiles are `hyperfine` means of 5–10 runs. Incremental times are medians over 3–5 rounds of edits in `typst watch`, using typst's own reported compile time. The documents, scripts and raw results are in `~/.cache/typst-lm`.

## 50.1 Full compiles

| Document | pages | main ms / MB | cur ms / MB | cur / main |
|---|---|---|---|---|
| plain | 50 | 175 / 91 | 155 / 61 | 0.89 |
| thesis template | 124 | 141 / 106 | 136 / 89 | 0.97 |
| breakable block + footnotes | 37 | 888 / 1042 | 120 / 55 | 0.14 |
| pad + footnotes | 35 | 809 / 879 | 116 / 56 | 0.14 |
| list + footnotes | 27 | 434 / 321 | 134 / 58 | 0.31 |
| tables + floats | 63 | 157 / 77 | 110 / 59 | 0.70 |
| table, 100 / 400 rows + footnotes | 19 / 74 | 572 / 253, 8168 / 3322 | 102 / 58, 319 / 132 | 0.18, 0.04 |
| table, 800 / 6400 rows + footnotes | 148 / 1181 | > 8 GB | 591 / 230, 4512 / 1587 | — |
| table, 30,000 rows, no footnotes | 527 | 3220 / 1142 | 3080 / 1213 | 0.96 |
| rowspans, 60 / 1000 groups | 29 / 475 | 1667 / 667, > 8 GB | 151 / 77, 2855 / 1051 | 0.09, — |
| document in a cell, 150 / 2400 sections | 38 / 600 | 1861 / 1936, > 8 GB | 133 / 63, 1780 / 558 | 0.07, — |
| 2,000 list items | 375 | > 8 GB | 944 / 306 | — |
| 2,000 plain chapters | 834 | 2524 / 1035 | 2236 / 532 | 0.89 |
| stack of 30 long blocks | 110 | 3414 / 4386 | 349 / 122 | 0.10 |
| tables in blocks in cells | 58 | 2638 / 1069 | 621 / 237 | 0.24 |
| 400 floats | 312 | 654 / 232 | 459 / 155 | 0.70 |
| 60 long equations | 216 | 1492 / 208 | 918 / 157 | 0.62 |
| footnote storm | 165 | 530 / 173 | 536 / 161 | 1.01 |
| 2,000 small tables + floats | 1696 | 793 / 288 | 803 / 302 | 1.01 |
| sticky headings | 477 | 598 / 250 | 552 / 158 | 0.92 |
| restart stress, 1,000 blocks | 1745 | error (PDF tags) | 533 / 217 | — |

`main` is quadratic whenever page heights vary during a long breakable element, since each spill relays out the whole element with different regions. Without that (30,000 rows, no footnotes), `main` is as fast as `cur`.

## 50.2 Regression: nested breakable blocks with footnotes

| nesting depth | 1 | 4 | 6 | 7 | 8 | 9 | 10 | 16 |
|---|---|---|---|---|---|---|---|---|
| main, ms / MB | 466 / 640 | 492 / 663 | 563 / 751 | 574 / 762 | 613 / 795 | 753 / 977 | 810 / 989 | 890 / 882 |
| cur, ms / MB | 80 / 45 | 140 / 82 | 246 / 175 | 407 / 347 | 806 / 867 | 2050 / 2608 | > 8 GB | > 8 GB |

The cost grows by about 2.5× per level. It needs both nesting and mispredicted regions:

* **No footnotes:** depth 10 takes 60 ms / 43 MB on both builds.
* **`width: 100%` blocks:** these skip the first-step width lookahead (§46.3). Depth 10 then takes 250 ms / 207 MB, still growing about 1.45× per level.

Suspected mechanism: each level lays out its body with predicted regions (the lookahead and the steps), and again when footnotes shrink the actual pages. Verification that re-executes a step produces fresh continuation-state IDs, which invalidate the memoized steps of the next level. So the work compounds per level. Possible fixes:

* A cheaper width check that doesn't lay out the whole body.
* Keeping the original continuation state when a re-executed step yields an identical frame and an equivalent state (structural state comparison, REVIEW §7.1).
* Restricting the lookahead to the outermost block.

## 50.3 Incremental compiles (ms; watcher peak MB)

| Document | end: main / cur | top: main / cur | inside: main / cur | peak: main / cur |
|---|---|---|---|---|
| plain | 49 / 36 | 52 / 37 | — | 435 / 126 |
| thesis template | 49 / 51 | 48 / 50 | — | 191 / 152 |
| breakable block | 41 / 21 | 725 / 29 | 638 / 31 | 7144 / 131 |
| pad | 42 / 19 | 616 / 23 | 409 / 26 | 5951 / 125 |
| list | 18 / 13 | 121 / 21 | — | 846 / 104 |
| columns | 17 / 19 | 43 / 23 | — | 139 / 89 |
| table, 100 rows | 11 / 12 | 178 / 37 | — | 718 / 116 |
| rowspans, 30 groups | 10 / 10 | 348 / 37 | — | 660 / 115 |
| document in a cell, 150 | > 8 GB | > 8 GB | > 8 GB | > 8 GB / 168 |
| nested blocks, depth 4 | 17 / 17 | 372 / 70 | 289 / 73 | 4184 / 354 |
| tables in blocks in cells | 26 / 29 | 1330 / 299 | — | 2211 / 452 |
| 2,000 plain chapters | 888 / 714 | 912 / 723 | — | 5796 / 1416 |
| 30,000 rows | 599 / 608 | 1710 / 1470 | — | 3531 / 3106 |
| stack of long blocks | > 8 GB / 41 | > 8 GB / 180 | — | > 8 GB / 305 |
| footnote / float / small-table storms | 144 / 139, 115 / 114, 192 / 197 | 156 / 141, 121 / 115, 201 / 203 | — | 583 / 404, 591 / 373, 914 / 785 |

Branch-only (`main` over the cap or too slow): table with 6,400 rows, end 769 / top 2410; 1,000 rowspan groups, 385 / 2070; document in a cell with 2,400 sections, 281 / 593 / inside 718; nested depth 7, 91 / 293.

Top-of-document edits in long tables still recompute every later step, because continuation-state IDs change (§48). They cost about half a full compile. "Trivial" edits on large documents (≈ 600 ms for 800+ pages) cost the same on `main`: this is realization, validation and export, not layout.

## 50.4 REVIEW §2.5 off (`hist`) and lazy spill descriptions (`lazy`), relative to `cur`

**`hist`:**
* Long tables: full compiles 3–6% faster, end edits 10–12% faster (6,400 rows: 687 vs 769 ms), memory 5–7% lower.
* Nested blocks and tables in cells: 2–7% faster.
* Documents in cells: 5–14% slower, with 38–45% more memory (2,400 sections: 2029 vs 1780 ms, 811 vs 558 MB), and top/inside edits 25–45% slower (853 vs 593 ms, watcher 3.6 vs 2.1 GB).
* Elsewhere: within ±2%.

The trade-off from §49.6 holds at scale; the table side is the validation cost of forwarded questions.

**`lazy`** prototype: for derived regions, a spill keeps only its first region and the kind and height of the next one. Verification and fallback then use a view that forwards the remaining regions to the live regions (`Regions::prepended`); explicit regions keep the full copy. Results:
* No gain anywhere: within ±1%.
* Nested blocks: 37–94% slower full compiles, 1.7–2.2× slower top/inside edits, 30–60% more memory.
* Tables in cells: 8% slower.

In nested flows the future is almost always stable, so there's nothing to save. Continuations already ask for the exact current height (for `full`). The forwarding view adds a level of tracked questions to every nested verification. Not adopted.

# 51. Fix: nested breakable blocks

## 51.1 Cause

comemo records two kinds of calls on tracked arguments:

* **Immutable calls** are deduplicated per memoized call.
* **Mutable calls** on `TrackedMut<Sink>` are appended to the memoized call's constraint and to every executing ancestor's constraint, without deduplication. They are replayed on every cache hit.

`Engine::introspect` records each introspection (e.g. a footnote's counter query) as such a mutable call.

On this branch, a parent step executes its child's current step and also re-invokes the child's previous step to verify it (§48.6). So a parent step's recorded side effects hold its own plus two children's worth:

M_L(j) = m_L(j) + M_{L+1}(j) + M_{L+1}(j−1)

This grows like 2^d with nesting depth d. The first-step width lookahead (§46.3) added a third copy for first steps. Time and memory followed: 2.5× per level, over 8 GB at depth 10, and one real package document (`lacy-ubc-math-project` test `math100-p3`) was killed at 8 GB. `main` has no such repeated invocations within one parent execution.

## 51.2 Fix

`Engine::isolate(f)` runs `f` with a separate sink and returns it; `Engine::commit(sink)` applies it. Layouts that aren't emitted drop their side effects:

* spill verification;
* lookahead passes: the block width check, list marker placement, grid cell skipping;
* a first frame that is laid out again at a fixed width;
* the trial layout into a full subregion, unless it is kept.

The layout that emits a frame records its side effects exactly once, so nothing is lost. Tracked dependencies (introspector, world, regions, locator) are still recorded, so memoization stays sound. Introspections only feed the non-convergence analysis, whose warnings are deduplicated by span and message. Recurrence afterwards: M_L(j) = m_L(j) + M_{L+1}(j) = Θ(m·d).

## 51.3 Verification

* All tests pass (3,793).
* All 15,382 corpus documents that `main` compiles (out of 20,507 `.typ` files) produce byte-identical PDFs to `main`. Their PDFs and stderr (warnings and errors) are also identical to before the fix, except `math100-p3`, which previously ran out of memory. On that document the fix takes 0.68 s / 194 MB (`main`: 0.76 s / 340 MB), with PDF and stderr identical to `main`.
* All 43 benchmark documents are byte-identical.

**Nesting, before → after (ms / MB):**

| depth | 4 | 7 | 9 | 10 | 16 |
|---|---|---|---|---|---|
| before | 140 / 82 | 407 / 347 | 2050 / 2608 | > 8 GB | > 8 GB |
| after | 138 / 78 | 261 / 146 | 405 / 202 | 500 / 256 | 1170 / 530 |
| main | 492 / 663 | 574 / 762 | 753 / 977 | 740 / 989 | 710 / 882 |

**Depth-7 incremental, ms:** end edit 87 → 19, top edit 295 → 182; watcher peak 2386 → 683 MB.

**Everywhere else:**
* Full compiles: 0.99–1.02× on all other documents.
* Instructions: +0.05–0.2%, so the remaining wall-clock differences are code layout.
* Incremental: within ±3%.

The branch is faster than `main` up to about depth 13. Beyond that, a quadratic term remains (§52.2 B5).

# 52. Asymptotic complexity of layout: `main` vs this branch

Notation:

| symbol | meaning |
|---|---|
| S | subregions (pages × columns) |
| k | content per subregion, bounded by the page size |
| N ≈ kS | content size |
| E | a breakable element (block, table, list, …) spanning p subregions, of size n = Θ(kp) |
| d | nesting depth of breakable elements |
| F | insertions (floats and footnotes) |
| R | restarts |

Costs count layout work (lines, rows, cells laid out) and the bookkeeping that grows with these parameters.

## 52.1 `main`

**M1. Spills (the dominant term).** On subregion j ≥ 2 of E, `MultiSpill::layout` builds

pod_j = [first; h_2, …, h_j; predicted…]

with h_i the actual heights of the earlier subregions, trims trailing entries equal to `last` (down to the longest backlog seen), and calls the memoized `layout_full(pod_j)` for the *whole* element. A per-child cell short-circuits an identical pod.

The pods are identical exactly when every committed height equals the prediction. Let D be the set of subregions whose actual start height differs from the prediction (footnotes, floats). Each j ∈ D is a cache miss and recomputes E in full:

* time Θ(|D|·n) = Θ(k·|D|·p);
* memory Θ(|D|·n), since every distinct pod's fragment stays cached;
* plus O(p²) to build and hash the growing committed backlog.

With footnotes on a constant fraction of pages, |D| = Θ(p), so time and memory are Θ(k·p²): quadratic in the length of a single element. Examples: a 400-row table with footnotes takes 8 s / 3.3 GB, and 800 rows exceed 8 GB. Without deviations, |D| = 0 and E is laid out once.

**Nesting on `main`.** Inside one layout of the outer element, inner elements see explicit, exact heights, so their pods coincide and they're laid out once per outer layout. The total stays Θ(|D|·n) with a constant factor per level. That's why `main` is nearly flat in d.

**Incremental on `main`.** An edit in or before E changes E's memo key or first region, which costs Θ(|D|·n) again.

**M2. Insertions.** A region can be relaid out once per insertion that changes it: O(m_r·k) for m_r insertions in region r, so O(F·k) overall. That's linear in N, since m_r ≤ k.

**M3. Skip set.** `Work::skips` is shared with the per-region checkpoint, so each region that adds skips copies the whole set: O(S·F) in the worst case. The per-element cost is tiny (a `Location`).

## 52.2 This branch

**B1. Steps.** E is laid out one subregion at a time: step j costs O(k) plus per-step bookkeeping, so n(E) plus bookkeeping in total. A memoized step hit costs O(q_j), the questions it asked. No step lays out other subregions' content.

**B2. Verification.** One extra call of step j−1 per spill subregion j:
* a hit costs O(q_{j−1});
* a miss costs O(k).

Total O(n). The constraint is local to one step: a changed height at j only affects steps that asked about j.

**B3. Width lookahead.** One extra pass over the body per auto-width content block: O(n), and O(d·n) over d nesting levels.

**B4. Restarts: linear, not quadratic.**
1. Each subregion can trigger at most `MAX_RESTARTS = 3` restarts, so R ≤ 3S.
2. A restart requested at subregion `at` rewinds to `from = origin + count − 1`, the subregion of the spill's previous frame, i.e. `at − 1` for aligned spills. The flow restarts from `from / columns`.
3. Only the compositions of `from`, and of `at` (partial or complete), are discarded: at most 3 subregion compositions per restart. Everything before `from` is kept, and nothing after `at` exists yet.
4. A chain of restarts, each raised while recomposing the rewound subregion, costs O(1) per link.

Hence the extra layout work is O(R) ≤ O(S). Raising predictions (§49.4) doesn't change this, because the per-subregion cap covers both directions.

Restarts would be quadratic if they rewound to the element's origin (Θ(p) per restart, Θ(p²) per element) or weren't capped. Neither is the case. The one superlinear piece is bookkeeping: `Predictions::affects` and `apply` scan all learned keys for each composed subregion, O(S·R) ≤ O(S²). The constant is tiny, and an ordered map would reduce it to O(S log R).

**B5. Nesting depth.**
* **Before the fix:** Θ(n·2^d) time and memory (§51.1).
* **After the fix:** let c_L be the number of distinct region contexts in which a level-L step for a given subregion runs. Every context of its parent that changes its answers gives one, plus its own verification context, so c_L ≤ c_{L−1} + O(1) = O(L). Measured: about 5L per page, from instrumented execution counts.

So step executions total Σ_{L≤d} c_L·p = O(d²·p), each O(k) plus validation, and time is O(n·d²). The cause is that a verification miss re-executes the step and mints fresh continuation-state IDs, which invalidate the cached steps below it. Removing this term would need structural comparison of continuation states (REVIEW §7.1). Each question also passes through at most d levels of forwarded regions, recorded once per level.

**B6. Lists and stacks.** Each continuation step rebuilds the list of all children (locators, `StackLayoutChild`s), O(#children), so O(#children·p) per list or stack. That's quadratic with a tiny per-child constant; storing the resume position would remove it.

**B7. Fixed-height breakable blocks.** `RegionHistory` grows by one height per subregion, and each step clones and hashes it: O(p²) per block. This is the same class as `main`'s growing committed backlog. Auto-height blocks keep no history (§49.6).

**B8. Grid.** A step restores a snapshot of the pending regions only (`discard_until`), O(1) normally. A rowspan spanning r subregions keeps r regions pending: O(r) per step, O(r²) for that rowspan.

**B9. Skip set.** Same as M3.

**B10. Incremental.**
* An edit inside E invalidates E's steps, since E is part of their memo key; its children still hit, so the cost is O(n(E)).
* An edit before E shifts its first region and mints new state IDs for all later steps: O(n(E)).
* `main`: Θ(|D|·n(E)) in both cases.

## 52.3 Summary

| | `main` | branch |
|---|---|---|
| breakable element, page heights as predicted | Θ(n) + O(p²) tiny | Θ(n) |
| … with deviating heights (footnotes, floats) | Θ(k·p²) time and memory | Θ(n) + O(p) verification |
| nesting depth d | Θ(|D|·n), ≈ flat in d | Θ(n·d²) (was Θ(n·2^d)) |
| restarts | — | O(S) layout, O(S·R) bookkeeping |
| lists, stacks | inside M1 | + O(#children·p) bookkeeping |
| fixed-height blocks | O(p²) regions | O(p²) history |
| edit in or before E | Θ(|D|·n(E)) | O(n(E)) |

## 52.4 Follow-ups

**Ordered prediction map (done).** `Predictions::heights` is a `BTreeMap`. `affects` and `apply` are range queries, so B4's bookkeeping is O(S log R) instead of O(S·R). All 51 benchmark documents are byte-identical, all tests pass, and there's no measurable time difference (R is small in practice).

**B5 investigation.** An instrumented build classified each executed block step by invocation context (real step, verification, lookahead) and by whether its continuation state had been seen before. At depth 9, a step at level L runs about 6 + 6(L − 1) times per page (level 8: 50). Nearly all of the growth is in executions *inside an ancestor's verification* with a *never-seen continuation state* (level 2: 176, level 8: 1,740). Re-executions of known states in new region contexts stay at about 3 per step at every level.

So verification re-runs step k−1 and mints fresh continuation IDs, and every deeper level then misses the cache in that context, once per ancestor.

An experiment that made nested spill descriptions record only the next region, instead of all future regions, made things worse: re-executions of known states doubled per level again (level 8: 301 per step, depth 16 > 8 GB), with identical output. So which verification contexts coincide depends on subtle details of what continuation states record. The trigger is not simply "reads the whole future".

A real fix needs continuation states that are identified by content: re-executions that produce an equivalent state would reuse its identity, so deeper cached steps still hit. This requires conservative structural equality (or hashing) for every layouter's state: flows with their spills, grids with their snapshots, lists, stacks, pads, equations, columns, blocks. Frames must be compared structurally, because re-executions allocate new ones. States produced with predicted and with actual regions also differ in the predictions they record, so the best achievable is O(d) contexts per page rather than O(1).

**Restart cap under monotone predictions (commit "Soundness fix").** That build still logs restart events (`TYPST_FLOW_STATS`). Measured on:
* all benchmark documents;
* the 15,382 corpus documents;
* 3,000 newly generated random stress documents (floats, filler, footnotes of random length, breakable blocks, table cells, nested blocks).

Results:
* 873 random documents had restarts: 2,383 restarts in total, with 173 fallbacks, all underfull.
* The 1,000-block restart stress document had 172 restarts and 12 fallbacks, all underfull.
* No benchmark document other than the restart stress document, and no corpus document, had a single mismatch.

No subregion ever logged more than 2 restarts. The logs merge nested flows and introspection passes, so that's an upper bound. The cap of 3 was therefore never reached, and no fallback overflowed.

With the raise rule of §49.4, an oscillating subregion takes exactly 3 restarts (lower, raise, lower) and then falls back underfull. That happened in 4 subregions of the adversarial document and nowhere else.

**Resume position in lists and stacks (done).** `layout_stack_internal` takes a function from the index to resume at, to the children from that index onwards. Only the first region gets all children, to prepare their locators once. Lists create each item's locators lazily for the children the stack asks for. That removes B6's O(#children · regions) term:
* Instructions saved grow quadratically: 24M at 8,000 items, 396M (2.9%) at 32,000 items (654 pages).
* Wall clock: −1.4% for the 32,000-item list; top edits on it 583 → 548 ms. Everything else is unchanged; the 1–3% differences are code layout, as instruction counts are identical.
* All tests pass, and all 15,382 corpus documents are identical in PDF, stderr and exit code.

**B5, traced.** An instrumented build logged, per root composition attempt, every block step that ran, with its continuation-state IDs. Depth 3 (outer O, middle M, inner I), page 10, after the page's footnote shrinks it:
1. The root verifies O's step 9 against the actual page 10, which re-runs it.
2. Inside, M verifies its step 8, which re-runs too, although page 10 is two pages ahead. `followed_by` reads the current regions' whole future bluntly, so every step with a spill inside depends on all future pages.
3. The re-runs mint fresh states. O adopts one, so O step 10 and M step 10 run with never-seen states.
4. Only I step 10 re-runs because its own region changed.

Every ancestor's verification creates another chain of fresh states below it. That's the O(d) growth per level: the innermost level runs about 39 times per page at depth 9, 32 of them inside verifications with fresh states.

Porting the lazy prototype (descriptions record only the next region, verification forwards to the live regions) onto the current code removes step 2 exactly as predicted: at depth 3, 3 runs per page instead of 4. But its forwarding views make every question more expensive, so it's 1.7–6.6× slower overall (depth 16: 7.75 s vs 1.17 s). The innermost level's fresh-state executions don't change (1,338 in both).

**Why state identity alone doesn't fix it.** Reusing old state IDs when a re-run produces an equal state would need the states to *be* equal. But the re-run happens precisely because the actual next region differs from the prediction, and every nested spill records that prediction (for its fallback). So the old and new states differ exactly in these cases. A fix therefore needs continuation states that don't record predictions in nested flows. Nested flows can't restart, so the prediction only serves the fallback, and an ancestor's verification already covers a nested child's dependence on the next region: an ancestor that verifies adopts a state computed with the actual regions, and one that falls back continues with the predicted ones. The only exception is a nested flow's own insertions (floats placed at the top of its next region), which the ancestor doesn't see. Together with structural state identity, this would make each level run a constant number of times per page, O(n·d) in total. It's a redesign of verification for nested flows, not a local change.

## 52.5 Measured scaling (after §56)

These are the series in `docs/cx-*`, with and without footnotes. Without footnotes, `main` is linear, so those series show where the branch could lose. Each probe is one full compile, capped at 8 GB and 4–5 minutes.

| Series | Branch | `main` |
|---|---|---|
| Content size, depth 8, footnotes (150 → 2,400 paragraphs) | linear: 0.16 → 2.16 s | quadratic: 0.22 → 2.35 s at 600, over 8 GB from 1,200 |
| … without footnotes | linear: 0.05 → 0.33 s | linear: 0.04 → 0.29 s |
| Depth, 300 paragraphs, footnotes (d = 2, 8, 16, 24) | 0.11, 0.31, 0.90, 2.01 s | 0.53, 0.69, 0.82, 1.32 s |
| … without footnotes (d = 1 → 24) | 0.06 → 0.10 s | 0.06 s throughout |
| Fixed-height block, footnotes (10 → 160 pages) | linear: 0.04 → 0.35 s | quadratic: 0.08 → 2.69 s at 80 pages, over 8 GB at 160 |
| … without footnotes | linear: 0.04 → 0.23 s | linear: 0.04 → 0.24 s |
| Table without footnotes (400 → 6,400 rows) | linear: 0.10 → 1.10 s | linear: 0.13 → 1.07 s |
| One rowspan over all rows, no footnotes (100 → 1,600) | linear: 0.03 → 0.14 s | linear: 0.03 → 0.18 s |
| … with footnotes (100 → 1,600, then 3,200) | quadratic: 0.05 → 6.74 s (2.4 GB), over 5 minutes | quadratic: 0.12 → 14.5 s (5.8 GB), over 5 minutes |

* **Nesting depth** is the one parameter in which the branch grows faster than `main`. From d = 12 to 24 it takes 3.7× longer, close to the d² of B5, while `main` is nearly flat. With footnotes, `main` becomes faster from about d = 16 on. Without them, the difference is 0.06 vs 0.10 s at d = 24.
* **A rowspan over many pages that footnotes shorten** is quadratic in both. The grid's first frame is only final once the rowspan is laid out, so the first step lays out every page ahead. Each shortened page then lays the steps out again from the first one: 24, 60, 128 and 266 recomputations for 200 to 1,600 rows. Each recomputation lays out the whole table again, and restores a snapshot with all pending regions per step. `main` lays out the whole table again on each such page (M1).
* B6 is gone (§52.4). B7 and B8's bookkeeping isn't visible up to 160 pages or 1,600 spanned rows.

# 53. Soundness of region-by-region layout

This section states what "sound" means for breakable layout, what it relies on, and why the protocol has it. It also records the violations found so far.

## 53.1 Setting

**Flows.** A flow composes its subregions (pages or columns) one after another. Composing a subregion can take several attempts: a float or footnote found during distribution shrinks it, and distribution runs again. Only the last attempt counts. A restart rewinds the flow to the start of an earlier region and composes again with learned predictions.

**Steps.** A breakable child C is laid out in steps:

  (f_k, s_{k+1}) = step(s_k, R_k),  with s_0 = none.

R_k is C's view of the regions: the first region is the space of the subregion f_k goes into, followed by predictions of later subregions (base heights, lowered or replaced by learned predictions). A step is deterministic in its inputs and in the answers to the questions it asks about R_k; memoization relies on this.

**The spill protocol.** At the subregion σ where C continues after emitting f_{k−1}, with f_{k−1} laid out with R_{k−1}:
1. **Verify.** Compute V = step(s_{k−1}, R'_{k−1}). R'_{k−1} is R_{k−1}'s first region followed by the actual regions from σ on, with the slot kinds R_{k−1} gave them.
2. If V's frame is identical to f_{k−1}, adopt V's state as s_k.
3. Otherwise, request a restart if allowed. It is allowed when restarts are left for σ, the spill is aligned, and the prediction was wrong in a direction a restart may fix. The restart learns σ's actual height and rewinds to the region of f_{k−1}.
4. Otherwise, keep s_k, the state produced together with f_{k−1}.
5. Emit f_k = step(s_k, R_σ), where R_σ are the actual regions at σ.

Until this change, step 5 of the fallback (4) used R_{k−1} advanced by one region, i.e. the predicted regions.

## 53.2 What sound means

* **(C) Coherence.** C's frames, in order, contain each part of C's content exactly once, in order. Violating it loses or duplicates content and can split PDF tags.
* **(F) Fit.** Every frame of C is laid out with the actual first region of the subregion it is placed into.
* **(P) Accurate lookahead.** Decisions a frame makes about later subregions (widows, orphans, grid rows) match their actual space. This is not needed for soundness; restarts improve it.
* **(T) Termination.**

`main` has (F) but not (C): each spill lays out the whole child again with the actual heights so far and glues the new frame to frames from differently predicted layouts.

## 53.3 Assumptions

* **(A1) Identical frames consume the same content.** Frame identity compares every item, including introspection tags by location. Two layouts from the same state whose frames are identical therefore continue at the same point. Content that produces nothing in a frame, like collapsed weak spacing, can't be told apart, but also can't be lost.
* **(A2) Continuing a state is independent of the regions.** A state produced by a step says exactly where the content continues. step(s, R) continues from there for *any* regions R; the regions only decide how much goes into which frame. Checked per layouter:
  * **Flows:** the cursor, queued floats, footnotes and tags, and a nested spill (sound by induction; its region description only feeds its own verification).
  * **Lists, enumerations, stacks:** the child to continue with and its state.
  * **Equations:** the prepared rows and the next row.
  * **`layout`:** the callback's content and its state.
  * **Blocks with a fixed height:** the heights already emitted. Redistributing the height keeps the shares of the earlier regions, as distribution is greedy from the first region.
  * **Grids:** the snapshot, consisting of the rows so far, the pending regions, rowspans and the lockstep row. The rows in pending regions are content laid out ahead, which the step declares, and the spill checks (§55).
  * ~~**Exception: content replay.**~~ Removed: all content is prepared for stepping (§54.1).
* **(A3) Determinism,** as in §53.1.

## 53.4 Coherence

*Claim:* every emitted frame is produced from a state that continues exactly where the previously emitted frame ended. By (A2), the frame then continues the content exactly, so (C) follows by induction over the emitted frames.

* **First frame:** from no state.
* **Verified:** V's frame is identical to f_{k−1}, and V's state continues V's frame. By (A1), it continues f_{k−1}.
* **Not verified:** s_k was produced together with f_{k−1}.
* **Full subregions:** the trial frame is dropped only if nothing of the body was placed into it and the child continues, so its state continues where the previous one did. Otherwise, the spill is deferred unchanged.
* **Discarded frames:** when `finalize` restores its initial snapshot, the spill returns to its state before the region, and the dropped frame's content is laid out again.
* **Restarts:** the flow restores the work (including spill states) checkpointed at the start of the target region and discards every frame from there on. What remains is an earlier, coherent prefix.

Coherence doesn't depend on which regions step 5 uses.

## 53.5 Fit

Step 5 always uses the actual regions, so every frame computed by the step that emits it fits. The remaining exceptions:
* Content that doesn't fit even into a fresh region (as on `main`).
* Content laid out ahead whose recomputation with the actual region changes an emitted frame, if no restart is left for it (§55). This never happened in validation.

## 53.6 Termination

* Each restart increments the counter of the subregion it learns about. A subregion allows at most `MAX_RESTARTS = 3`; raising a prediction needs two left, so the last restart for a subregion always lowers.
* A restart only rewinds to the region of the spill's previous frame, which lies before the learned subregion.
* Between restarts, the flow composes as it does without them, which terminates.

So there are at most 3 restarts per subregion that is ever reached, each rewinding at most one region.

## 53.7 The fix (V1) and what it changes

**V1: frozen predictions.** A fallback used to continue with the predicted regions of the last frame, including its predictions of the subregions after the next one. A restart requested there learned the right height, but when the flow reached the fallback again, it reused the same old predictions. So the restarts couldn't take effect: three identical restarts, then a fallback with the old prediction, which overflowed into the footnotes (§52.4 search, document `c09032`).

**Why the predicted regions were used.** Commit 1's model laid the child out in full for every region. Staying consistent with the emitted frames meant reproducing that layout, which needed the old predictions. With steps, the kept state already pins what was emitted (§53.4), so the regions don't matter for (C). The predicted first region only broke (F), and the predicted later regions broke restarts.

**Fix:** step 5 uses the actual regions in the fallback, too.
* (C) and (T) are unchanged.
* (F) now holds for fallbacks, except for rows a grid had already laid out into the next region. §54.2 fixes those.
* A fallback's lookahead uses every prediction learned so far, so a restart for the next subregion takes effect.
* The monotone and raise rules were chosen so that fallbacks leave space unused rather than overflow. Now fallbacks do neither. The rules still bound restarts and are kept.

**Validation** (23,060 stress and benchmark documents, logging builds before and after):
* **Overflowing spill frames:** 4 before (all from fallbacks, in 3 documents), 0 after.
* **Coherence:** no document loses or duplicates a line.
* **Output:** changes in 1,505 documents. 1,068 got shorter, 436 kept their page count, and 1 got a page longer. In that one, a page alternating between 0pt and 61pt was left empty before; filling it moves footnotes onto the next page, which then alternates too.
* **Corpus:** all 15,382 documents are identical in PDF, stderr and exit code (real documents don't reach a fallback).
* **Test:** `flow-spill-fallback-actual-regions` fails before the fix in all four outputs, including PDF tags. `main` drops two lines of it and can't export tagged PDF.

## 53.8 Open violations

All three are fixed (§54):
* **V2: grid work ahead of the next region (REVIEW.md, confirmed).** A grid step can finish several regions at once (e.g. for a rowspan), computing the frames of later regions with predicted heights. Verification only re-runs the previous step against the next region. The step that hands out a pending frame doesn't compute it, so a frame computed two or more regions ahead is never checked against its actual region, which breaks (F). Rows then overlap the footnotes. Fixed by the grid itself (§54.2) instead of the restart proposed here, which would have needed a new signal through every nested layouter.
* **V3: side effects of adopted states (REVIEW.md).** A state adopted from a verification that re-ran may carry work done ahead (e.g. pending grid rows) whose warnings, errors and introspections went to the discarded sink. This affects diagnostics only, not output. Fixed in §54.3.
* **Content replay:** (C) held only as on `main` (§53.3). Removed in §54.1.

# 54. Fixes for REVIEW.md

The review in REVIEW.md found one confirmed regression (V2), two further soundness concerns (V3 and block widths), and cost, duplication and cleanup items. All are addressed here. The content replay path (§53.3) is removed as well.

## 54.1 Content replay removed

A nested flow could only be stepped if all children's styles extended the flow's styles. The children's styles are stored relative to a base chain, and a base that isn't the flow's own styles (the shared trunk of the children) doesn't outlive the realized content. Otherwise, `ContentState::Replay` laid out the whole content for each region and skipped the frames already emitted, which violates (C). It was never observed (§46.2), but it was reachable.

`InnerStyles` already had an `absolute` mode that stores a child's complete chain. `prepare_flow` now always uses the flow's styles as the base and stores the (in practice nonexistent) children that don't extend them in full. The base selection moved into `base_styles`, which only eager `layout_flow` uses, where the trunk lives long enough. `ContentState::Replay`, `replay_content_step` and `PreparedFlow::resumable` are gone, so (A2) now holds without exception.

The suffix check no longer collects the links of each child into a vector (review item 9). It counts the links and compares the tail in place. The dedup key for relative styles is built in a `SmallVec` and only allocated on insertion.

## 54.2 Grid work ahead of the next region (V2)

*Superseded by §55, which moves the check into the step protocol. The analysis and the dead ends below still apply.*

A grid step lays out until the frame of its region is final. Rowspans are laid out when the region with their last row is *finished*, so a frame whose rowspan crosses into the next region is only final once the layouter has entered the region after that. Rows spanning several regions do the same. Any such step leaves rows in later regions, laid out with their predicted heights. Flow layout verifies the region after the step's own by laying out the step again with its actual height. Nothing checked the regions after that, and nothing checked the next region either when verification failed and the spill continued from the step's original state (the fallback of §53.1). Both let rows overlap footnotes.

**Fix: the grid checks what its snapshot laid out ahead.** Each grid state records the pass that computed its snapshot (`GridPass`):
* the snapshot it resumed from (or none, at the grid's start) and the index of the region it started in,
* the remaining and full height of that region, the remaining height of each region it entered afterwards when it entered it, whether that region was a repetition of the final region, and the final region's height,
* the frames handed out since it started.

A step that continues a snapshot beyond its own region extends the record. If it lays out more rows into the snapshot's region, that region's entry becomes the step's own view of it, since the rows continue with that view.

**When a step redoes the pass.** A step checks its region against the record. If the pass predicted a different remaining height (`Regions::has_height`, two tolerance-based `fits` questions) *and* what the pass laid out into the region doesn't fit into it (`fits` of the used height), the step lays out the pass again:
* from the recorded snapshot, with regions rebuilt from the record (same heights, same kinds) up to the step's region,
* with the actual regions from the step's region on (`GridLayouter::splice` switches to them when the layouter enters that region).

It continues from the new layout if every frame handed out since the pass started comes out the same, up to floating-point error (`Frame::approx_identical`: the earlier regions were laid out with derived regions, the redo uses explicit ones, and the two only agree up to rounding). Otherwise, it continues from the old layout, as before the fix.

* **(C):** The new layout reproduces the handed-out frames, so by (A1) it continues exactly where they ended. The earlier regions see the recorded heights, including in their lookahead, so their decisions are reproduced unless a rowspan that ends in the step's region changes them.
* **(F):** The step's region is laid out with the actual regions whenever the old layout doesn't fit. When the old layout fits, it is the layout any greedy row placement would produce in the smaller space, too. (If a region has more space than predicted, some of it may be left unused, as before.)
* **(T):** A redo is one extra pass, without loops.

**Dead ends measured on the way** (`results/`):
* **Exact frame comparison:** every redo failed, since explicit regions reproduce derived heights only up to rounding (163.27799999999996 vs 163.278).
* **Rebuilding the earlier regions as plain backlog regions:** changes `may_progress` (which uses exact `!=` for repetitions), so the grid's first-region decisions (orphan headers, footer widows) differed. The record keeps the kinds.
* **Redo on any height difference:** 2.3–2.9× slower on `rowspan-240/1000` (286 redos, all reproducing the old frame). Pages with footnotes are shorter than predicted, yet the rows still fit.
* **Redo on full height differences:** the full height of a region after the first is its remaining height in a continuing block (as on `main`), but the final region's height for a predicted repetition. It differs on every page with footnotes, without any effect on fit. The same cost as above.
* **Keeping the old start after a redo:** every redo went back to the grid's first region, so the cost grew quadratically (`rowspan-240`: 9 s, 1.8 GB).
* **Recording only snapshots two or more regions ahead:** missed the fallback case above (`rowspan2-fn`: rows over footnotes on page 4).
* **Not updating the entry of a continued region:** a redo rebuilt it with the old pass's view, so the frames differed and the redo failed (`rowspan2-fn` again).

**Cost:** recording asks for the exact heights of the step's region and the regions it entered, which are blunt questions. On the benchmark documents, time and memory stay within noise of the previous build (§54.7). Redos are rare: none on `rowspan-240`, which has footnotes on every page.

**Test:** `grid-rowspan-footnotes-ahead` (fails without the check). Of the review's reproducers, 20 of 21 now match `main` pixel for pixel. `tallrow-fn` has no overlap anymore but differs in footnote migration: `main` moves line 28 to the next page with its footnote, while here the footnote migrates alone. That comes from how the flow migrates footnotes out of a spill's frame, not from the grid. *(Wrong: it came from the re-run's view of the regions, see §56.4. Since then, `tallrow-fn` matches `main`.)*

## 54.3 Side effects exactly once (V3 and review item 7)

Verification runs isolated. Before, its sink was dropped even when its state was adopted, so work it did ahead (pending grid rows) never recorded its warnings, delayed errors and introspections.

**Fix: the spill holds the side effects of its last emitted step (`MultiSpill::pending`) until the step is verified.**
* Verified: the verification's sink is committed and the pending one dropped. The verification's state is kept, so its side effects are the ones that belong to the emitted content.
* Mismatch with fallback: the pending sink is committed, since that state is kept.
* Restart: both are dropped. The restart lays the region out again.
* The last step of a spill commits immediately.

Each step's side effects are thus recorded once, from the layout that is kept. This doesn't bring back §51's blowup: that came from recording both.

**Review item 7** (no per-child caches, so each relayout attempt replays child side effects into the parent's constraint). The caches can't simply come back: a prepared flow is shared through comemo, so it can't hold per-run cells. The measurements (below) show the growth is small, so two changes suffice:
* **Empty commits:** `Engine::commit` skips empty sinks. Every commit was a mutable call recorded by all memoized callers and replayed on each hit, even when empty. This was the bulk of the overhead (e.g. `table-huge`: 466k calls without any payload).
* **Root flow restarts:** each region's sink is held until the flow is done and dropped with the region if a restart discards it. A restart restores all work from its checkpoint, so nothing laid out in a discarded region survives.

**Tried and reverted: isolating compose attempts.** Each attempt of the page and column loops ran isolated and was only committed if kept. The corpus showed that this *loses* diagnostics (two font warnings in 6 of 15,382 documents): attempts are not independent. Insertions laid out in a discarded attempt (floats in `column_insertions`, footnote frames in `footnote_spill` and `footnote_queue`) survive into the next attempt, and their side effects went down with the discarded sink. So side effects of compose attempts are recorded at least once, as on `main`.

The rule behind all of this: side effects may only be dropped together with *everything* the dropped layout produced. That holds for the spill's pending sink (the kept state comes entirely from one layout), inspection layouts (`layout_rest`, `lockstep_rest_non_empty`, list bodies) and flow restarts, but not for compose attempts.

**What eliminating the rest would take: side effects by reference in comemo.** With comemo as the only cache, a relayout attempt that re-hits a child can't avoid the replay. Today, a mutable call on a tracked `Sink` inside a memoized function is:
1. stored in that function's cache entry,
2. forwarded, as a copy, to the constraint of every memoized function that is executing around it, and
3. on a cache hit, replayed call by call into the caller's sink, where each replayed call is forwarded again, as in step 2.

So every entry holds a flattened copy of all side effects of its subtree, and every repeated hit of a child during one execution (one per attempt) adds another copy to all enclosing entries.

The change: keep a function's mutable calls once, in its entry (behind an `Arc`). On a hit inside an executing memoized function, forward a *reference* to that entry's calls instead of copying them. Replaying an entry walks its own calls and the references in order.
* Recording and replaying a hit becomes O(1) per hit instead of O(side effects of the subtree), at every level.
* Repeated references to the same entry within one execution could be recorded once, if the tracked type declares that replaying its calls twice has the same effect as once. That holds for `Sink` (warnings and errors are deduplicated, introspections only serve diagnostics), but comemo can't know it in general, so it would be opt-in.
* It is simpler than §48.5's proposal for immutable calls, since mutable calls are only replayed, never validated. There is no need to re-run the code that derived the arguments.
* Entries must stay alive while referenced (eviction), and the replay order must be preserved.

This would remove the remaining overhead of item 7 and the per-level copying measured below (the items in `nest-16`).

**Measurement** (`scripts/add-sink-count.py`, Sink method calls including comemo replays, and the items they carry):

| Document | `main` | before | after |
|---|---|---|---|
| plain | 1,474 | 9,439 | 1,494 |
| thesis | 2,268 | 4,266 | 2,592 |
| block-fn | 36,075 | 2,915 | 2,561 |
| float-storm | 2,452 | 48,136 | 4,517 |
| float-table | 63 | 17,569 | 227 |
| fn-storm | 7,283 | 7,283 | 7,591 |
| columns-storm | 803 | 30,161 | 959 |
| restart-storm | 8,169 | 18,561 | 17,745 |
| sticky-storm | 18,649 | 35,184 | 19,379 |
| table-huge | 2 | 466,267 | 1 |
| small-tables | 2 | 35,155 | 1 |
| nest-8 | 6,753 | 32,295 | 2,433 |
| nest-16 | 6,753 | 129,081 | 5,681 |
| rowspan-60 | 25,557 | 138,197 | 6,173 |
| stack-huge | 72,237 | 5,985 | 3,873 |

Calls are what memoized callers record and replay. "After" is at or below `main` except where relayouts and restarts replay child side effects (`float-storm`, `restart-storm`: up to 2.2×), which is the remaining cost of item 7. The items carried grow with nesting (`nest-16`: 209k, `main`: 17k), since each level's commit forwards its subtree's side effects once. That is storage `main` has in its constraints too, just without the copies.

## 54.4 Block widths in continuations (review item 3)

A breakable block with an automatically sized content body keeps its frames at one width. Its first step checks the widths of all frames in the predicted regions. Before, a continuation whose frame had another width relaid out the body at the larger width with expansion and then stopped checking. That had two problems:
* It stepped a body prepared at the pod's width with other regions, against `layout_flow_step`'s documented contract.
* Later frames were no longer checked, so one wider than the new width would overflow the stroke and fill.

**Fix:** the state keeps the target width for all continuations. Each one first lays out naturally (isolated).
* Same width: keep it.
* Narrower: lay it out again at the target width with expansion, like `main`.
* Wider: keep the natural frame, since the content wouldn't fit the target, and make its width the target for the later frames. The earlier frames can't change anymore, so their width differs. Only a restart of the whole block could avoid that. `main` lays out all frames at once, so it relays them out together.

The contract is now documented as what happens: in regions of another width, the prepared paragraph lines keep their width and are aligned, and everything else is laid out at the new width. That's how the block uses it, and it matches `main`'s relayout except for line breaking, since `main` breaks the lines again at the new width. The review couldn't reproduce an overflow, and a reproducer needs content whose width depends on the region's height. So there is no new test. The existing block width tests pass unchanged.

## 54.5 Predicted regions and owned regions (review items 4, 11, 12)

**`Followup`** (typst-library) is an owned copy of the regions after the first one: backlog, predicted repetitions and the final region. `Regions::followup` reads it (blunt), `Regions::with_followup` applies it, and it has `map` and `prepend`. It replaces the six copies of `backlog()` + `split_off(len - predicted())` (the flow's `RegionsDesc` and `followed_by`, `RegionHistory`, column subregions, `Composer::predict`, `Regions::map`) and is the buffer of `shrink`, `materialize` and `map`.

**Grid pods keep the kinds of the regions (item 4).** The eager measurement (`measure_auto_row`), the lockstep measurement (`lockstep_pod`) in the first region and in continuations all build their regions with `Followup` now. Before, predicted repetitions became plain backlog regions in all but the first lockstep measurement, which changes `may_progress` for predictions equal to the final region. So break decisions for oversized content could differ between lockstep and eager rows.

A pitfall found while doing this: subtracting the header and footer heights as `h - (header + footer)` instead of `h - header - footer` changed `grid-header-too-large-repeating-orphan-with-footer` from four pages to two. `may_progress` compares heights exactly (`!=`), so the last bit decides whether a region break helps. The code keeps the original order of operations.

**Hashing derived regions (item 11)** no longer panics. It hashes the materialized regions, which is a blunt question about all of them. Tracking remains the precise alternative.

**Measurement pods (item 5).** The `height: None` branch of `CellMeasurementData`, which asked about the current region instead of reading it, is gone. `layout_cell` materializes the regions anyway.

## 54.6 Other items

* **Item 6:** `SpillTarget::available` is only read if a restart may be requested. In nested flows, it was a blunt question about the exact height on every spill.
* **Item 8:** `layout_cell_step` generates the cell's tags in the first region only and keeps their location in its state (`CellState`). The body's locator doesn't depend on them (`next_location` and `next` use different keys).
* **Item 10:** `GridLayouter::snapshot` finds a repeating header's index from its offset in the grid's headers instead of scanning them.
* **Item 13:** The lockstep path shares `filled_height` (a continuing row's height in a region, minus repeated headers and footers) and `row_pod` (the regions of a cell in a row with known heights) with `layout_auto_row` and `layout_multi_row`.
* **Items 14–16:** `fits_next` documents that it checks the remaining height of the next region, `with_height` is removed, and so is the redundant `headers.is_empty()` guard in `discard_until`.

## 54.7 Validation

All against the tree before these fixes (the fallback fix of §53.7, `fb`), and against `main` where noted. Raw results in `~/.cache/typst-lm/results/`.

* **Tests:** 3,796 pass, including the new `grid-rowspan-footnotes-ahead` and `flow-spill-restart-cap-nested` (nested blocks whose footnotes make a page alternate between two heights until the restart cap is reached; `main` drops a line of it). New unit tests cover `Followup`, hashing derived regions and `Frame::approx_identical`.
* **Review reproducers** (21 documents, pixel comparison with `main`): 20 identical, including all four confirmed V2 cases. `tallrow-fn` has no overlap but migrates a footnote without its line (§54.2).
* **Grid stress documents** (2,000 generated tables with rowspans, tall rows, repeated headers and footers, footnotes in cells, floats, nested in blocks and columns; `scripts/gridgen.py`, checked with `grid-check.py`):
  * Before: rows overlap footnote entries in 182 documents, and 212 PDFs differ from `main`.
  * After: no overlaps, and all 2,000 PDFs are byte-identical to `main`. 1,148 recomputations, none of which changed an emitted frame.
* **Stress documents** (23,060: the adversarial restart and footnote set, fuzz documents and benchmarks): all PDFs are identical to before. There are no overflowing spill frames, and restarts and mismatches are identical. Grid redos never trigger: where rows were laid out ahead, they still fit.
* **Corpus** (15,382 real documents): PDFs and exit codes are identical. One document lists two adjacent warnings in the opposite order, since a spill's side effects are now recorded when its step is verified.
* **Full compiles** (45 benchmark documents, one capped run each): the same PDFs, time within noise, peak memory within ±2%. Hyperfine on the 11 documents with the largest single-run differences: 0.98–1.02× (the largest, `rowspan-1000`, is +1.6%).
* **Incremental compiles** (33 documents, trivial edit / edit at the end / at the top / inside, `scripts/inc-pair.sh`): geometric mean ratios 0.995 / 0.999 / 0.997 / 0.984, all within 0.93–1.08. Peak memory within 5%, except `table-400` (+6%, 344 → 364 MB), probably the pass records in cached grid states.

# 55. Content laid out ahead, in the step protocol

§54.2 made the grid check the rows it laid out ahead itself. That works, but the soundness of the protocol then depends on each such layouter knowing about it and keeping books. This section moves the check into the protocol. Layouters only declare what they laid out ahead.

## 55.1 The invariant

The protocol assumes that a step's state says *where to continue*: the next step lays out its region from scratch, with the actual regions (§53.3, A2). A state may carry more than that, namely content already laid out into later regions with their predicted heights. It is then only valid in regions that this content fits into, and whoever continues the state must check that.

Of the current layouters, only grids carry such content: a frame is final only once the region after it is finished, since rowspans are laid out when the region with their last row is finished. Flows carry spills, but those check their own children (below). Footnote entries spilled to later pages (`footnote_spill`) are laid out ahead too, but they are insertions of the flow, identical to `main`, and placed before the body on the next page, which adapts to them.

## 55.2 Declaring

`MultiStep::ahead` lists, for each region after the step's one, how much height the returned state already used up in it. It is empty for layouters whose state only says where to continue. `step_grid` fills it from its snapshot (`GridLayouter::used_in`). A block forwards its body's declaration and adds its vertical insets (`BlockStep::ahead`). Flows don't forward anything: every multi-region child of a flow is a breakable block, whose spill does the checking.

## 55.3 Checking

`MultiSpill` keeps a window of emitted steps: for each, the state it was laid out from, its regions, its frame and its declaration. The window holds the last step and all steps whose declarations reach the next region. At each region:

1. Verify the last step as before (§53.1). If it verifies, its entry takes the verification's regions and declaration.
2. If the latest state declares content for this region that doesn't `fit` its actual height, lay out the steps since the earliest one whose declaration reaches this region again.
   * Each step's regions are its recorded ones, with this region *spliced* to the actual one (`Regions::spliced`). Its lookahead still sees the predictions it was laid out with, but once layout advances into this region, it lays out into the actual one. That's what reproduces the emitted frames.
   * The frames are compared with the emitted ones up to floating-point error (`Frame::approx_identical`), since the recorded regions are explicit while the original ones may have been derived.
   * If they are all the same, the spill continues from the new state.
   * Otherwise, it requests a restart at the region of the earliest step, learning this region's height, if the spill is aligned and a restart is left. Failing that, it continues from the old state, whose content then overflows.
3. Lay out the next step with the actual regions, add it to the window and drop the steps whose declarations no longer reach the next region.

*Splicing was replaced by plain regions in §56.4: each step is laid out again with its recorded regions up to this region and the actual ones from there on. The lookahead no longer sees the old predictions of this region.*

**Splicing through tracking.** Steps are memoized with tracked regions, and layout inside a step advances *derived* regions, which only ask the tracked regions questions by index. So each tracked question now also carries the index of the region that the asking derived regions are in. The tracked regions answer questions from derived regions that advanced to the spliced region or beyond from the spliced-in regions, and all others from their own. Regions with nested splices (from repeated recomputations) resolve recursively.

## 55.4 Soundness

* **(C):** as for verification. A recomputation is only used if all frames since its start come out the same, so by (A1) its state continues exactly where the emitted frames end.
* **(F):** content laid out ahead that doesn't fit its actual region is laid out again with it, or the flow restarts with the region's height learned. Only if neither is possible, it overflows. That was never observed.
* **(T):** at most one recomputation per spill step, and restarts are counted as in §53.6.
* **What layouters must do:** declare content they laid out ahead, and forward the declarations of bodies they wrap without being a flow. They don't check anything.

## 55.5 Compared with §54.2

* **The grid** is back to a plain step plus its declaration: `GridPass`, the recomputation, the splice in the layouter and the recording of entered regions are gone.
* **Fewer recomputations:** they start at the earliest step whose content reaches the region, instead of at the start of the grid's pass (grid stress documents: 725 instead of 1,148; `rowspan2-fn`: 2 instead of 6).
* **Failures can restart**, since the spill knows the flow's subregions. The grid couldn't.
* **Generic:** any future layouter that lays out ahead only has to declare it.

## 55.6 Validation

Against the tree before the fixes of §54 (`fb`) and against `main`:
* **Tests:** 3,796 pass. A new unit test covers splicing, directly and through tracking.
* **Review reproducers:** 20 of 21 match `main` pixel for pixel, as with §54.2 (`tallrow-fn` as described there).
* **Grid stress documents** (2,000): no overlaps, all PDFs byte-identical to `main`. There were 725 recomputations, none of which changed an emitted frame.
* **Stress documents** (23,060): all PDFs identical to before, with identical restarts and mismatches, and no overflowing spill frames. No recomputation is needed in them.
* **Restart cap with nested blocks** (317 documents that hit it, §55.7): identical to §54.2, and to `main` except for 2 where `main` drops a line.
* **Corpus** (15,382): PDFs and exit codes identical. One document lists two warnings in swapped order (§54.7).
* **Full compiles** (34 documents): identical PDFs, time within 3%, peak memory −5% to +2%. Hyperfine: `rowspan-240` 1.000×, `rowspan-1000` 1.004× (§54.2: 1.016×), `table-1600` 1.001×, `nest-16` 0.976×.
* **Incremental compiles** (10 documents): geometric mean ratios 0.994 (trivial edit), 1.007 (end), 0.977 (top).

## 55.7 The restart cap with nested blocks and footnotes

Question: what happens when a page uses up its restarts, compared with `main`? Documents with 2–4 levels of nested breakable blocks, footnotes on many lines and floats (`scripts/nestgen.py`, checked with `nest-check.py`: every line exactly once, no line overlapping a footnote entry or below the margin):

| Set | Hit the cap | Same as with a cap of 20 | Same as `main` | `main` drops a line |
|---|---|---|---|---|
| 3,000 moderate | 52 | 52 | 52 | 0 |
| 4,000 harsh | 265 | 265 | 263 | 2 |

None of them loses, duplicates or overlaps a line. In the adversarial stress set, 1,563 documents hit the cap, and all of them are byte-identical with a cap of 20 (40.5 instead of 7.4 restarts per document on average). So the cap only saves work: the layouts oscillate between two heights of a page, and the fallback lands where more restarts would.

`flow-spill-restart-cap-nested` is a reduced example: footnotes make the space on page 7 alternate between 19.3pt and 29.1pt. The page uses its 3 restarts (lower, raise, lower), and the block continues from the state of its last frame, into the actual space. `main` drops a line there, since it glues together frames of layouts with different breaks, and puts a footnote entry on the page before its line. That misplaced entry also happens on this branch in a slightly different variant of the document, just as on `main`, so it's a pre-existing issue of footnote placement, not of restarts.


# 56. Review round 2

A second review (REVIEW.md: reuse, simplification, efficiency and altitude agents) came back with an outcome per finding, and the user fixed a part of it. This section covers:
* checking those fixes;
* the items addressed on top;
* the items left, and why.

## 56.1 The user's fixes

The user's fixes pass all 3,796 tests and are fmt- and clippy-clean. Their output matches the previous build:
* the review reproducers;
* the grid stress documents (2,000);
* the restart-cap documents (317);
* the stress documents (23,060), with identical restarts, mismatches and fallbacks.

## 56.2 Skipping verification of correct predictions (efficiency #4)

If the actual regions after the last frame's region equal the ones it was laid out with, verification would lay out the same step with the same regions again. For every frame but the first, that's a cache hit with the same result, so skipping it only saves the lookup: hashing the arguments, validating every recorded question and replaying the sink.

The first frame is the exception, since it's laid out with the flow's regions, which may be derived. Laying it out again with regions recreated from their description can then change it by floating-point error. That was why the review skipped the item: such a drift used to go to the restart or fallback branch, and would now continue silently. But:
* Continuing from the frame's own state *is* the fallback.
* The only difference is the restart, and that restart would be spurious. The prediction was right, so the restart would learn a height equal to it, or, with column balancing, the unbalanced height.
* With a single comparison policy (§56.3), verification accepts such a drift anyway.

`RegionsDesc::followed_by` now takes the described actual regions, so the followup is computed once per region instead of twice. That saves one blunt `followup()` question per region.

## 56.3 One comparison policy (altitude #1, part)

Verification now also compares frames with `Frame::approx_identical`. `Frame::identical` and the `Precision` enum are gone.

## 56.4 Laying out steps again without splices (altitude #1)

The reviewer proposed replacing splices by laying out the steps again with plain regions. A variant with the actual regions from each step's region on showed:
* **Grid stress documents:** all 725 recomputations still reproduce their frames, and all PDFs are identical.
* **`tallrow-fn` now matches `main`.** It was the last reproducer that didn't.

**Why the splice was worse.** Printing the regions of each re-run explains `tallrow-fn`:
* The splice keeps the old prediction of page 3 (180pt) for the lookahead of the steps laid out on pages 1 and 2. In fact, the page has 145.7pt.
* The frames of pages 1 and 2 are reproduced either way. But the grid's decisions about page 3 that are made from the earlier pages, like the lockstep measurement of the tall row across its regions, use the stale height.
* So line 28 stays on page 3, and its footnote migrates alone. With the actual height, line 28 moves to page 4 together with its footnote, as on `main`.
* §54.2 blamed footnote migration for this difference. That was wrong.

**What the splice was good for.** After a failed verification, the recorded regions keep the prediction for the region after a step. These reproduce its frame, while the actual region wouldn't.

**Chosen: recorded regions, then actual ones.** Each step is laid out again with the regions it was laid out with up to the current region, and with the actual ones from there on (`RegionsDesc::followed_by(at, regions)`, the reviewer's "generalise `followed_by`").
* If the step was verified, its recorded regions after its own are the actual ones.
* The replaced region's kind (backlog or repetition) determines the kind of the regions that replace it, as for verification.

**Removed:**
* `Regions`' `splice` field, `spliced` and `resolve`;
* the `from` argument of the ten tracked questions and of the `Outer` trait;
* `StepRegions` with its recursive `build`;
* the splice unit test.

A unit test for `followed_by` covers both kinds.

**Soundness.**
* **(C):** unchanged. A recomputation is used only if all its frames come out the same (A1).
* **(F):** content in the current region is laid out into the actual region.
* **(P):** better than before: every re-run step sees the actual current region.
* **(T):** unchanged.

## 56.5 Grid snapshots (simplification #5, altitude #2)

`GridLayouter` isn't split into a context and a progress struct. That would touch nearly every field access of the grid code for two concrete risks, which are fixed directly:
* **Forgotten fields:** `snapshot()` destructures the layouter and `restore()` destructures the snapshot, without `..`. So a field that is added to one of them but not handled fails to compile.
* **Unchecked header indices:** `header_index` searches the headers with `ptr::eq` instead of computing the index from pointer offsets. `subslice_range` asserts that it found the subslice, also in release builds.

## 56.6 Smaller items

* **Reuse #5:** `followup_without_repeats` replaces the three copies of the mapping. It keeps the subtraction as `h - header - footer`, since `h - (header + footer)` rounds differently (§54).
* **Simplification #6:** `measured_height` replaces the duplicated maximum.
* **Altitude #3(a):** `cell_x` is the RTL cell position. All four sites computed `width - (dx + w)`, and it does the same.
* **Efficiency #6:** the measurement followup is computed once per row (`LazyCell`), and only for breakable rows.
* **Altitude #1, wrappers:**
  * Stack, lists, pad (`grow_ahead`), the layout callback and grid cells forward their body's `ahead` instead of dropping it with `..`.
  * Flows are the documented exception (`MultiStep::ahead`).
* **Altitude #4:** `SimulatedRegions` shares the layouter's `used`/`used_after_repeats` bookkeeping with the rowspan simulator. The 2,000 rowspan simulation documents (`gridsim/`) come out the same.

## 56.7 Measured and rejected: memoizing content steps (efficiency #3, #7)

A variant memoized `layout_content_step`, for first and later steps, with tracked regions. It was slower everywhere:

| Document | Change |
|---|---|
| `fn-storm` | 0% |
| `blockbody-huge` (efficiency #3's case) | +3% |
| `list-huge` | +4% |
| `rowspan-240` | +5% |
| `listn-8000` | +9% |
| `bigcell-600` | +13% |
| `table-huge` | +22% |
| `stackn-8000` | +24% |
| `nested-mix` | +36% |
| `nest-8` | +37% |

The duplicated work that #3 describes is cheap: the body's lines are cached anyway, and composing them is not the cost. The memo lookups are.

The variant also broke `table-400`: footnote numbers jumped to the final counter value. A memoized wrapper links its locator once more, which changes the locations of everything beneath it. The eager cell layout gives the same cells the old locations, so introspection doesn't converge. So a memo boundary on one layout path must match the other paths. This agrees with §46.8.

## 56.8 Left as is

* **Reuse #2 (skip predicate helper):** both callers pass `!is_empty_frame`.
* **Reuse #4 (grid and block variants of `fit`):** they compute something else.
* **Reuse #8 (`FlowCx` accessors):** each is used once.
* **Reuse #9 (realize prologue):** because of the arena lifetime, sharing it would need a callback threaded through two memoized functions.
* **Reuse #10 (`Regions::from_followup`):** it would save a line per site.
* **Simplification #1 (finish/label/modify helper):** it would need about 9 parameters, and the sites guard the finish call differently.
* **Simplification #6 (merging `header_height` and `footer_height`):** it changes rounding and breaks a test.
* **Simplification #12 (one tracked `ask`):** the three lists of the ten questions remain. Without the `from` argument and `resolve` (§56.4), they are one-line forwarders again. A query enum would trade them for unpacking an answer at each question.
* **Simplification #13 (one-pass `followup`):** it assumes that predicted slots come last.
* **Altitude #3(c) (one auto row implementation):** a redesign with its own performance risks (§46.5).

## 56.9 Validation

Compared with the build of the user's fixes, and with `main` where noted:
* **Tests:** 3,796 pass. There are unit tests for `followed_by` and `approx_identical`.
* **Review reproducers:** all 20 match `main` pixel for pixel, including `tallrow-fn` (§56.4).
* **Grid stress documents** (2,000): all PDFs byte-identical to before and to `main`. There were 725 recomputations, and none of them changed a frame.
* **Rowspan simulation documents** (2,000): identical to before. 1,252 recomputations, and none of them changed a frame. They differ from `main` in 20 documents, where `main` duplicates content.
* **Restart cap with nested blocks** (317): identical.
* **Stress documents** (23,060): identical PDFs, with identical restarts (23,546), mismatches (29,121) and fallbacks (5,575), and no overflowing frames.
* **Corpus** (15,382): identical PDFs, exit codes and diagnostics.
* **Full compiles against the commit before this round** (hyperfine, 19 documents):
  * nested blocks are faster: `nest-8` 0.846×, `nest-16` 0.725×, `nested-mix` 0.969×, mostly from §56.2;
  * tables are faster: `table-1600` 0.976×, `table-huge` 0.974×;
  * everything else is within 1.5% (`restart-storm` 1.015×, ±1%).

# 57. Toward linear scaling: rowspans and nesting depth

§52.5 left two superlinear cases: a rowspan over pages that footnotes shorten (quadratic, like `main`), and nesting depth with footnotes (about d², where `main` is nearly flat). This section has prototypes for both and plans. The prototype patches are in `~/.cache/typst-lm/patches/proto-*.patch`.

## 57.1 Rowspans laid out one region at a time (prototype)

**Change:**
* When a region is finished, the part of each rowspan that continues after it is laid out right away (`layout_cell_step`), and the rowspan keeps its state.
* The lookahead of these parts sees one predicted region after the current one, as a finite backlog, like the finite regions of the eager layout. The part in the rowspan's last region is laid out into exactly its height, as before.
* The last spanned auto row is measured by continuing from the state, instead of laying out the whole cell again and skipping the frames of earlier regions.
* So every frame is final once its region is finished.

**Bug found on the way:** a cell whose content ended but that expanded into more regions continues with empty frames. When measured, such a frame is only the cell's insets, and the row grew by them. Continuations that yield only empty frames now count as ended, like when measuring from scratch.

**Output:**
* Tests: 3 of 3,796 change.
  * `grid-rowspan-split-9`: the last two lines of a paragraph now stay together across a sliver region, instead of leaving a widow. Arguably better.
  * `grid-rowspan-cell-coordinates` and `grid-rowspan-excessive-gutter`: invisible differences (PDF tags, and at most 1/255 in a few pixels) for rowspans that extend past the table's end.
* Corpus: 0 of 15,382.
* Grid stress documents: 4 of 2,000, all pixel-identical.
* Rowspan simulation documents: 39 of 2,000.
  * Of 15 sampled, 10 look identical.
  * Others: a line of a rowspan moves to the next page, footnotes shift, a sliver row disappears. 6 page counts change.
  * 2 documents get more checker findings; both were already broken in the build before.

**Performance:** one rowspan over 1,600 rows with footnotes takes 0.52 s and 193 MB instead of 6.74 s and 2.4 GB, and it's now linear.

**What's left:** a row with a rowspan cell that breaks across regions is still laid out eagerly, several regions at once, so steps can still carry content ahead (§55). Extending lockstep to these rows would remove that:
* the rowspan's continuation becomes a lockstep cell in its last row;
* this might replace the rowspan simulation;
* then `MultiStep::ahead`, the step window and the recomputation could go.

**Output-preserving alternative:** memoize grid layout per region: a snapshot and tracked regions in, a snapshot out. A recomputation would then only redo regions whose answers changed. But lockstep measurement and the rowspan simulation read the future in full, so every region would depend on all later ones. This needs the views of §57.3 in the grid, plus cheap snapshots with an identity. Much larger.

## 57.2 Nesting depth: the trace

An instrumented build logged every executed block step, with its level, the chain of contexts that caused it (first step, next step, verification, recomputation, width lookahead) and its state:
* **Without footnotes:** exactly one execution per step and level (16 levels × 43 pages = 688).
* **With footnotes:** about 2.9·d² per page. Almost all of the growth is inside verifications: each ancestor level's verification re-executes the chain below it with fresh states.

There are two causes, and each one alone gives d².

**1. Reading the whole future.** A spill describes its regions with `RegionsDesc::new`, whose `followup()` reads all upcoming heights. In a nested flow, these reads are forwarded into the constraints of every enclosing step, so a page that footnotes shorten invalidates every ancestor step laid out before it. Verifications at nested levels whose regions differ only two or more regions ahead missed 464 of 532 times. The hits were innermost steps, which have no spill.

**2. The width lookahead.** The first step of an auto-width breakable block lays out its body into all remaining regions, just to compare frame widths. At depth 8 that's about 100,000 `fits` questions per future region. So first steps depend on every future page. Re-running an enclosing first step creates the nested first steps afresh, and each looks ahead again.

The §50.4 lazy prototype was slow because it forwarded the lookahead's questions through its views. Reproduced: 7× slower at depth 16.

## 57.3 Nesting prototypes

**A. Views.**
* A spill in a nested flow records only its first region.
* Its steps, and their verification, see `Regions::prepended`: the recorded first region, followed by the live regions. Questions about later regions go to the live regions. If there's no next region, a step sees the live regions with the first region's height known (`Regions::overridden`).
* Explicit regions (the root flow) keep full descriptions and restarts.
* The width lookahead runs on a materialized copy of the regions.

Results of A:
* **Output:** identical in all 15,382 corpus documents and 3,796 tests.
* **Full-width blocks:** linear. At depth 16, 2,752 executions: 4 per page and level, all from the root laying out pages again for footnotes. That's 0.60× the time of the build before and 0.20× of `main`.
* **Auto-width blocks:** still about d² (depth 24: 1.09×), because of the lookahead.
* **Regression:** `bigcell-600` is 44% slower, with 4.3× as many executions. Verifications in the cell's nested flows now miss where the explicit path skipped them. The suspected cause is that the view answers the full height of the next region differently when the step runs and when it's verified. Open.

**B. A without the width lookahead.** The first frame's natural width becomes the target. Narrower later frames are laid out again at it, and wider ones keep their width.
* **Scaling:** linear. Depth 8, 16 and 24 take 0.54×, 0.22× and 0.14× the time of the build before. Depth 24 takes 255 ms, where `main` takes 949 ms.
* **Output:** 0 test changes, and 17 of 15,382 corpus documents change. 15 of them look identical. The other 2 are one template in two versions: a figure moves to the next page, which adds a page.
* **Regressions:**
  * `bigcell-600`: +45%, as in A.
  * `rawblock-huge`: +42%, since continuations narrower than the target are laid out twice.
  * `nested-mix`: +15%.

**Dead end:** verifying a first step with the width it decided on, instead of looking ahead again. This gave no scaling gain, since nested first steps are created afresh rather than verified, and it changed 3 corpus documents.

## 57.4 Plan: nesting depth

1. Views for nested spills (A). A nested flow that can restart (footnote entries) needs a prediction for the next region, so record just that one there.
2. Run every multi-region lookahead on materialized regions, so that its many questions are answered locally:
   * the width check;
   * the peek for an empty first frame;
   * the peek in lists;
   * the grid's `lockstep_rest_non_empty`.
3. Make the view answer the same questions in the same way when a step runs and when it is verified, as long as nothing changed. Then bring back a cheap equality check for the common case. Target: `bigcell-600` within 2%.
4. Decide on the width lookahead:
   * **Drop it (B):** linear in depth; 17 corpus documents change, one template visibly. The double layout of narrower continuations must be avoided, for example by laying them out at the target width directly.
   * **Keep it:** depth is then linear for full-width blocks, and about d² only for auto-width blocks with footnotes, where `main` is faster from about 16 levels on.
5. Validate with all corpora and the `cx-*` series. Target: linear in depth, and no regression on everyday documents.

## 57.5 Plan: rowspans

1. Rowspans laid out one region at a time (§57.1).
2. Lockstep for rows with rowspan cells, with the rowspan's continuation as a cell of its last row. Replace the rowspan simulation where possible.
3. Once every frame is final when its region is finished, remove `MultiStep::ahead`, the step window and the recomputation (§55).

# 58. What tracked regions buy (benchmark)

**Variant:** block steps are memoized on the materialized regions (hashed like any argument, as on `main`), instead of tracking the questions asked about them. Footnote entries use `layout_fragment` instead of `layout_fragment_tracked`. Nothing else changes.

**Output:**
* Corpus: identical in all 15,382 documents.
* Tests: 1 of 3,796 changes. In `grid-subheaders-too-large-repeating-orphan-before-auto`, too-large repeating headers spill over three pages instead of one. Materializing changes how the repeated final region is represented, and the grid's exact `may_progress` then sees progress.

**Full compiles** (hyperfine, untracked ÷ tracked):

| Documents | Time | Peak memory |
|---|---|---|
| Nested blocks (`nest-*`, depth series) | 0.49–0.76× | 0.54–0.63× |
| Tables | 0.91–0.95× | 0.89–0.98× |
| Lists, stacks, small tables, mixed nesting | 0.87–0.98× | 0.88–0.95× |
| Everyday documents | 0.99–1.00× | 0.98–1.02× |
| Whole document in a grid cell (`bigcell-*`) | 1.08–1.09× | 1.21–1.25× |
| `restart-storm`, `float-storm`, rowspans | 1.02–1.09× | 1.01–1.05× |

**Incremental compiles** (34 documents, `typst watch`, geometric mean of untracked ÷ tracked):
* By edit: trivial 1.00, end 0.91, top 0.95, inside 0.84; peak memory 0.96.
* Tracking is faster for long footnoted content in blocks and cells:
  * `block-fn`: top 1.64, inside 1.47;
  * `bigcell-*`: top 1.12–1.42, memory 1.33–1.39;
  * `fn-storm` and `list-fn`: top 1.11–1.12.
* It's slower for nesting (top and inside 0.47–0.56) and tables (end 0.79–0.84).

**Why.** Counting block step calls and executions shows that tracking does what it was designed for. Regions that differ but give the same answers are frequent: tracking avoids 38–77% of step executions (`table-400`: 5,673 instead of 10,349; `bigcell-600`: 3,655 instead of 15,787). But a step execution is cheap, since it composes one region whose lines are memoized separately. Tracking pays on every question instead:
* each question is a recorded comemo call;
* in nested content, it is forwarded and recorded again at every enclosing level;
* every cache hit asks all recorded questions again to validate them.

`nest-8` forwards 440,000 questions to save 544 executions. Tracking only wins where re-executing is expensive and frequent: long content with footnotes inside a cell or block, laid out again at slightly different heights.

# 59. Floating-point exactness dropped, tracked regions removed

The user decided that output doesn't need to be byte-identical down to floating-point rounding, and that tracked regions go (§58).

## 59.1 Floating-point exactness

Code that existed only to reproduce `main`'s arithmetic bit for bit:
* **`Regions::may_progress`** compared the remaining height of a repetition with the final region's height with exact `!=` (inherited from `main`). It now uses `approx_eq`.
* **The grid's log of consumed heights** was replayed when restoring a snapshot and for `height_after_repeats`, so that the heights were subtracted in the same order. Now the grid keeps only the sum `used`: restoring consumes it at once, and `height_after_repeats` is `initial - used_after_repeats`.
* **Header and footer heights were subtracted one after the other,** since subtracting their sum rounded differently. Lockstep rows now keep their sum (`PendingRow::repeats`), `followup_without_repeats` takes it, and `repeats_height()` computes it in one place.

Output:
* Corpus (15,382), stress documents (23,060) and grid stress documents (2 × 2,000): all identical.
* One test changes: in `grid-header-too-large-repeating-orphan-with-footer`, a repeating header that's too large filled 4 pages, because an exact comparison kept reporting progress until the rounding happened to match. It now fills 2 pages, and the extra pages had only repeated the header.

## 59.2 Tracked regions removed

`Regions` is a plain value again: the width, the first region's remaining and full height, and the regions after it. Those are a backlog, then predicted repetitions of the final region (§49, needed for restarts), then the final region, repeated. Removed:
* the derived kind with its interval bounds;
* `Outer`, `RegionsLink`, `Regions::link` and the eleven tracked questions;
* `materialize`, and the special case of `shrink` for derived regions;
* `layout_fragment_tracked`, whose only caller was footnote entries, which use `layout_fragment` now.

Consequences:
* **Block steps are memoized on their regions,** hashed like any other argument (`layout_multi_step_impl` takes `Regions`).
* **Frames are compared exactly again** (`Frame::identical`). The tolerance only absorbed rounding differences between derived regions and their explicit copies, which no longer exist, and the `approx` helpers are gone.
* **`SpillTarget::available` is always set.** It was only computed when a restart was possible, since reading the height used to be a "blunt question".
* **The public methods of `Regions` stay** (`fits`, `limited`, `fit`, `fits_next`, `may_progress`, …), so callers didn't change. They're now plain computations on the fields. The distinction between "blunt" and precise questions is gone from their documentation.

`regions.rs` shrinks from 1,096 to 557 lines. Together with §59.1, that's 899 lines removed and 160 added in the crates.

Output:
* Corpus (15,382), stress documents (23,060), grid stress documents (2 × 2,000), restart cap documents (317) and review reproducers (20): all identical to the build before §59.
* All 3,796 tests pass. `grid-subheaders-too-large-repeating-orphan-before-auto`, which changed with the untracked variant of §58, is unchanged, because the tolerance of §59.1 covers it.

**Performance against the build before §59:**
* **Full compiles** (hyperfine, 40 documents):
  * nesting: 0.49–0.78×;
  * tables: 0.90–0.96×;
  * stacks, lists, `nested-mix`: 0.88–0.99×;
  * everyday documents: within ±1.5%;
  * slower: `bigcell-*` 1.08–1.10×, one rowspan over 400 rows 1.10×, `rowspan-30` 1.05×, `float-storm` 1.04×.
* **Peak memory:** nesting 0.54–0.62×, tables 0.89–0.98×, `bigcell` 1.22–1.25×, others within ±4%.
* **Incremental compiles** (34 documents, geometric means): trivial 1.00, end 0.93, top 0.96, inside 0.85; peak memory 0.96.
  * Best: nesting, 0.48–0.67×.
  * Worst: long footnoted content in blocks and cells: `block-fn` top 1.59 and inside 1.45; `bigcell-*` top 1.20–1.30; `fn-storm` and `columns-fn` top 1.13.
  * This is the pattern of §58: pages are laid out again at slightly different heights, which tracked steps survived.

## 59.3 Review fixes

* **Content laid out ahead under relative insets.** `pad::grow_ahead` resolved a relative vertical inset against the current region and added it to every height laid out ahead. But `grow` sizes each frame with the inverse `(h + abs) / (1 - rel)`, so a child using 180pt of a later 200pt region under 10% padding was declared as 190pt instead of 200pt. The spill could then accept a region that doesn't fit. Both now use the same function (`grown`), which also no longer needs the regions. A unit test checks that a declared height grows exactly like the frame.
* **Side effects of a rejected recomputation.** `MultiSpill::redo` laid the steps out again directly into the engine. When the recomputation was rejected and the spill continued from the old state (no restart possible), its introspections, delayed errors and values were recorded anyway. The recomputation now runs isolated, and its sink is committed only if the spill continues from the new state, following §54.3. If it does, some side effects of the replaced layout may be recorded twice, as before.

Output is identical in the corpus (15,382), stress documents (23,060) and grid stress documents (4,000). All 3,796 tests pass.

# 60. Simplification pass

The user asked for the code of the last two commits to get the same treatment as `block.rs`: fewer functions, simpler models, less code. Every change here was validated to leave output unchanged, except where noted.

## 60.1 Blocks

`layout_multi_block` is one step function for every region. It:
* reconstructs the regions of a block with a fixed height from its `RegionHistory`;
* lays out the body with `step_body`;
* for auto-width content bodies, looks ahead once with `peek_remaining` to find a consistent width;
* detects orphans with the same peek.

Removed: `layout_multi_block_first`, `layout_rest` and `finish_multi_frame`, plus the separate first-region and continuation paths. `finish_frame` decorates both single and multi-region frames. `block.rs` went from 749 to 562 lines.

## 60.2 Flow driver

Eager layout and one-region-at-a-time layout now share the prepared flow:
* `PreparedFlow::new` collects realized children.
* `prepare` realizes content and calls it. It backs both `layout_fragment_impl` and the memoized `prepare_flow` of content steps.
* `layout_flow` (the root flow) calls `PreparedFlow::new` directly.
* `layout_prepared_flow` runs the restart loop.
* `layout_flow_step` lays out one region and can't restart.
* Both lay out a region with `compose_region`. Region locators come from `SplitLocator::nth`.

Removed: `layout_steps`, `layout_remaining`, `layout_fragment_tracked`, `layout_fragment_inner` and the `prepare_flow` wrapper. `peek_remaining` is a plain loop.

Two bugs came up while validating, and both are fixed:
* **Footnote styles.** The root flow built its configuration from the children's base styles instead of the page's styles. So footnote entries inherited a document-wide `#show: align.with(center)`: 9 corpus documents (touying) changed. Callers now build the `Config` once, as before. Test: `footnote-entry-outside-show-everything`.
* **Restart target.** The restart loop divided the subregion by the requested column count, instead of the resolved one, which is 1 in infinite width.

**Performance pitfall.** Merging the preparation code made `prepare_flow` take the whole `Regions` as a memo key. The key then included the predicted regions after the first one, so the first step of almost every cell missed the cache. Output was identical, but tables, rowspans and big cells were 1.6–2.4× slower. The key is the column base size and horizontal expansion again, which is all that `collect` reads.

## 60.3 Regions

`Regions` is `main`'s struct again: public `size`, `expand`, `full`, `backlog` and `last`, plus one new field, `predicted: usize`. It counts the entries at the end of the backlog that are predicted repetitions of `last`:
* their full height is `last`;
* `has_backlog()` doesn't count them;
* `may_progress` only reports progress into them if the current height differs from `last` (§49.3).

Readers of the heights see predictions without special handling. Code that creates predictions (`Predictions::apply`, `RegionsDesc::followed_by`) calls `trim_predicted`, so that a prediction equal to `last` isn't one.

What went:
* the accessor API from before §59: `width`, `height`, `consume`, `limit`, `limited`, `fit`, `fits`, `fits_next`, `at_least`, `with_*`, `shrink`, `backlog()`, `followup`;
* the `Future` and `Slot` machinery;
* the `Followup` type in the library. The grid keeps a small owned copy for its measurement regions.

Most call sites read as on `main` again, and `pad` and `breakable_pod` use `main`'s `map` and `shrink_multiple`. `regions.rs` went from 557 to 238 lines.

## 60.4 Spills

Verifying the last step and recomputing the steps that laid out content ahead were two code paths with two restart rules. Now both are `settle(start)`: it lays out the steps from `start` again with the actual regions, unless none of them changes. If a frame changes, it restarts or keeps the old state. A spill settles:
1. the last step;
2. then, if the content laid out ahead into the region still doesn't fit, the steps from the earliest one that laid it out.

There's one restart rule. The prediction of the region from the first re-laid step is compared with the available height, as verification did before.

**Rejected:** settling all unsettled steps whenever the regions differ. This also lays out content ahead again when it fits, which changes the grid's decisions for that region. 7 of 4,317 grid stress documents changed. One got a page holding only a footnote entry, whose reference had moved to the next page after the region was laid out again for footnotes.

## 60.5 Stacks, lists, grid

* **Stacks:** the `Resume` enum is gone. The state holds `inner: Option<MultiState>`, which is `None` when the child starts afresh after the spacing was already laid out.
* **Lists:** `BodyStart` is gone. `layout_body` returns the first step and the frame the marker is placed on, keeping `main`'s sticky `first_frame`.
* **Grid:** the seven lockstep functions are down to four. `lockstep_row` prepares a row. `layout_lockstep_row` lays out its part in the current region, whether it's the first region, with the skip, or a continuation. `step_lockstep_cell` and `is_lockstep_row` stay. The grid's architecture is unchanged (§60.7).

## 60.6 Validation and performance

**Output:**
* Corpus (15,382), stress documents (23,060) and grid stress documents (4,317): identical to the build before this pass, after the fixes of §60.2.
* All 3,797 tests pass (one new).

**Full compiles against the build before this pass** (hyperfine, 23 documents):
* within ±3% for everyday documents, tables, big cells, lists, stacks and footnote and float stress documents;
* faster: `nest-12` 0.87–0.94×, `cx-span-fn-400` 0.94×, `rowspan-*` 0.97×;
* slower: `nested-mix` 1.02–1.05×, with 5% more peak memory, which was already the case after the block rewrite.

**Lines in the crates since the build before this pass:** 1,733 removed and 1,053 added. Against `main`, the branch now adds 4,097 lines and removes 1,469, down from 4,912 and 1,604.

## 60.7 What's left: the grid's architecture

The grid remains the least uniform layouter. Its steps resume from snapshots taken between rows, and a region's frame is only final once its rowspans end. So a step can lay out rows into the next region, which is why `MultiStep::ahead` and the spill's window of unsettled steps exist. Removing that needs three changes:
1. **Rowspans laid out one region at a time** (§57.1), so that frames are final when their region is finished.
2. **Steps that end at a region break.** `finish_region` stops the step instead of starting the next region. The interrupted row is laid out again from its start in the next region, so the work done before the break must be repeatable (for example, registering rowspans).
3. **Lockstep for rows with rowspan cells.** Otherwise their later parts are content laid out ahead. This includes measuring a rowspan that ends in the row by continuing its state, and it touches the rowspan simulation.

Then `MultiStep::ahead`, `grow_ahead`, the window in `MultiSpill`, and the grid's `offset`, `discard_until`, `is_final` and `used_in` would go. The output of rowspan-heavy tables would change (§57.1: 39 of 2,000 rowspan simulation documents, mostly identical-looking).
