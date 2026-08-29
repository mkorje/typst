Right — the findings went through the `ReportFindings` tool, so the UI renders them but they aren't in my message text, which is all `/copy` sees. Here they are as plain markdown:

---

## Flow layout optimizer review

Scope: `git diff main...HEAD` (~5.6k lines) — `crates/typst-layout/src/flow/{mod,collect,compose,distribute,block}.rs` and `crates/typst-library/src/engine.rs`. `cargo check -p typst-layout` passes.

### 1. HIGH — `crates/typst-layout/src/flow/mod.rs:310` — Positional break-choice replay is not stable across insertion-driven relayout → `unreachable!` panic

`StickyChoices` matches recorded decisions to observations *by cursor position*, and `decide` panics if the id at that position differs. But `Composer::column` (compose.rs:442) rewinds only `cursor` when a float/footnote forces a column relayout, while the *sequence* of observations is not invariant across that relayout: `Distributor::can_break_before_sticky` (distribute.rs:286–300) falls back to `next_region_is_taller()` when nothing in-flow has been placed yet, and the column insertion shrinks `regions.size.y` below `regions.last`, flipping that gate from `false` to `true`.

```typst
#set page(height: 200pt)
#block(sticky: true)[A]      // at region top: gate false on pass 1
Some text
#block(sticky: true)[B]      // gate true -> first observation, id = B
Text with a note#footnote[x] // discovered only on the next run
```

Run 1 records `values = [id_B]`. Run 2: pass 1 consumes `id_B` at cursor 0, then the footnote triggers `RelayoutStop::Relayout(Column)`; cursor is rewound to 0; pass 2 now runs with `pod.size.y = 200pt - footnote_height`, so block **A** is observed first and `decide` compares `id_A` against `values[0] == id_B` → `unreachable!("flow optimizer replay encountered a different breakpoint")`. Same hazard exists for `Composer::page`'s rewind (compose.rs:291) on parent floats and column balancing.

### 2. HIGH — `crates/typst-layout/src/flow/mod.rs:1203` — Node relaxation leaves stale predecessor `PagePlan`s, producing a plan that cannot be replayed

`nodes[existing] = successor;` overwrites a node in place. Successors that were already created from the *old* node still hold `predecessor: Some((existing, old_plan))`, whose `expected: OutcomeFingerprint` was derived from the old node's `trace` (note `SearchKey` deliberately excludes `trace.score`, so two states with different `detached`/`forced_detached` share a key). The node is re-queued and re-expanded, but a re-derived successor is only written back when `quality < nodes[existing].quality` *strictly* — so a successor whose new `detached` is no better keeps its stale plan. If such a stale node was already latched as `terminal` (mod.rs:1222), `BreakPlan` reconstruction (mod.rs:1244–1248) splices a plan computed from state A onto a chain that now passes through A′, and `render_flow` hits `unreachable!("flow optimizer replay diverged from its continuation")` (mod.rs:965) or `"...encountered a different breakpoint"`. Even when it doesn't panic, `nodes[s].quality` is left stale, so `incumbent` can be worse than the true optimum.

### 3. MEDIUM — `crates/typst-layout/src/flow/mod.rs:923` — Optimizer invariants are enforced with `unreachable!()`, turning search bugs into compiler panics on user documents

There are ~12 of these across the diff (mod.rs:923, 945, 960, 965, 972, 1238, 1311; collect.rs:528, 546, 788, 793, 823; distribute.rs:929, 987, 1134; compose.rs:921). None is a language-level impossibility — they are assertions about a large, freshly written search whose replay determinism depends on findings 1 and 2. Since `render_greedy_fallback` already exists and reproduces the pre-existing behaviour, replay divergence should degrade to that path (or to a `bail!`) rather than abort the process. A panic here takes down `typst compile`, the language server, and any embedding host.

### 4. MEDIUM — `crates/typst-layout/src/flow/mod.rs:1324` — `enumerate_page` enumerates break-choice *combinations* across the columns of a page; cost is multiplicative in the column count

A `migrate: true` decision only ends the current *column* (`Stop::Finish(Finish::Soft)` → `column()` returns Ok → `page_contents` proceeds to column `i+1`, which emits fresh observations against the same `StickyChoices`). So the worklist leaf count is roughly `∏_columns (breakpoints_in_column + 1)`, and every leaf is a full page recomposition. For `#set page(columns: 3)` with ~10 sticky blocks per column that is ~10³ page compositions *per DP node*, before the outer table multiplies by the number of surviving states. Related: `transitions` (mod.rs:1125) and `nodes` retain a cloned `Work` (with `MultiSpill` decision vectors) for every visited state and are never pruned, so memory grows with document length × alternatives. Note Typst headings are sticky by default, so `config.optimize` is true for essentially every real document.

### 5. MEDIUM — `crates/typst-layout/src/flow/collect.rs:524` — Nested controlled flows receive the outer flow's *entire* remaining decision list

`MultiChild::layout` passes `choices.remaining()` (and `MultiSpill::layout` at collect.rs:779 appends it to the committed prefix) into `layout_full_controlled`. Those trailing entries belong to *later outer* children, not to the nested flow. If the nested flow ever observes one more breakpoint than the search recorded for it — which is exactly what finding 1 makes possible — it consumes an outer decision and `StickyChoices::decide` panics on the id mismatch. Symmetrically, `output.consumed.checked_sub(old)` (collect.rs:787) and `frames.next()` (collect.rs:822) panic if a continuation replay ever produces a shorter prefix than what was already emitted, which is precisely the "not 100% correct" region-juggling the retained comment warns about. A length-bounded slice (or an explicit end-of-plan sentinel) would make this a recoverable condition instead of an abort.

### 6. MEDIUM — `crates/typst-layout/src/flow/distribute.rs:525` — Footnotes inside fractional blocks are discovered at a different point depending on whether the flow happens to contain a sticky block

When `config.optimize` is false, `single()` calls `composer.footnotes(&regions, &frame, Abs::zero(), false, ALLOW)` on the *preliminary* full-height frame. When `config.optimize` is true it rolls that back and instead calls `fractional_footnotes` from `finalize` (distribute.rs:913) with the *allocated* frame and `flow_need = frame.height()` instead of `Abs::zero()`. Different `flow_need` changes the footnote pod (`pod.size.y -= flow_need + separator_need + gap`), so it changes whether an entry fits and therefore the page break. Since `config.optimize` is enabled purely by `children.iter().any(Child::sticky)` (mod.rs:766), adding an unrelated `#block(sticky: true)` elsewhere in the same flow silently changes footnote placement for `#block(height: 1fr)[…#footnote[…]]`.

### 7. LOW — `crates/typst-layout/src/flow/collect.rs:938` — `CachedCell` keyed by `LayoutAttempt` defeats the cache exactly where it used to pay off

`attempt` is bumped on every column advance (compose.rs:380) and every page/column relayout (compose.rs:297, 443), i.e. on every repeat of a distribution pass with unchanged inputs — the only case the cell was designed for. Every float/footnote insertion now re-drives `layout_single_impl`/`layout_multi_impl` for every already-laid-out child. comemo still absorbs most of the cost, but the local cache is now effectively dead. If the intent was to force sink effects to be re-emitted, note that comemo already replays recorded `TrackedMut<Sink>` mutations on a cache hit.

### 8. LOW — `crates/typst-layout/src/flow/mod.rs:783` — Discarded candidates' introspection descriptors are merged into the real sink

`search_sink.introspections()` is forwarded wholesale. These feed `typst_library::introspection::analyze` (typst/src/lib.rs:174), which produces the "layout did not converge" hints — so a non-convergence report can now cite a `counter`/`query` read that only ever happened in a rejected candidate layout, pointing the user at content that isn't in the output.

### Non-issues checked and cleared

- Effect-transaction begin/commit/rollback pairing on all early-return paths (including `inspect_err` in block.rs:260 and `promote_effect_transaction` popping the buffer in `float`/`footnotes`).
- `Sink` warning dedup across nested/promoted buffers — `merge_effect_buffer` re-checks the target's set, so no duplicates leak; the root `MAX_VALUES` cap still holds.
- `CallJournal` dedup vs. comemo's `Constraint::emit`: comemo never dedups *mutable* calls, but `World`/`Introspector`/`Traced` expose only `&self` tracked methods, so the journal's dedup + `assert_eq!` is currently sound.
- `MultiSpill` backlog/skip arithmetic (`limit = backlog.len()` ↔ `render_flow_controlled` emitting `limit + 1` regions) is consistent.
- `PlanQuality.regions` accounting agrees between `greedy_flow` and `optimize_flow`; the `quality >= incumbent` prune is admissible (all three components are monotone along a path).
- Predecessor chains cannot cycle (`outer` strictly increases and is part of `SearchKey`).
- `finalize`'s ordering (`trim_spacing` → `used_height_without_fr` → fr resolution → positioning) matches the original.

### Separately

The diff also commits `PLAN.md`, `DEVIATIONS.md`, and some downloaded HTML assets — probably not intended for the branch.
