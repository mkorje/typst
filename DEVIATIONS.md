# Deviations from `PLAN.md`

- Optimizer/state types remain colocated in `flow/mod.rs` instead of new
  `flow/state.rs` and `flow/optimize.rs` modules. `RegionsKey` is likewise local
  to flow layout rather than a new helper in `regions.rs`. This is source
  organization only.

- Composer and Distributor expose optional-break control events and restore
  complete transactional checkpoints, but page-local alternatives are
  evaluated by deterministic replay from the exact page-entry continuation
  instead of retaining cloneable `PageState`, `ColumnState`, and
  `DistributionState` values. This preserves insertion causality and exact
  outcomes, at the cost of recomputing page prefixes. The outer continuation
  table still performs exact-key dominance and predecessor reconstruction.

- Normal `BlockBody::Content` continuations use a controlled `MultiSpill` with
  stable paths, committed nested decisions, a current-fragment limit, and
  prefix-effect suppression rather than the proposed shared `FlowSession` and
  arena-resident `BlockContinuation`. Prefix layout is reconstructed to recover
  nested `Work`. Because an already emitted fragment cannot be retroactively
  replaced, controlled continuations use incremental automatic widths instead
  of running the complete-fragment automatic-width fixed point. Native
  `MultiLayouter` callbacks retain the planned opaque compatibility behavior.

- Insertion state is committed by the existing `Insertions::finalize` path
  rather than a separate `commit_state`/`render` API, so speculative search
  still constructs and discards page frames. Search does not yet have a
  timing-counter export; exact state dominance, transition caching, the greedy
  incumbent, and the sticky-free optimizer bypass are implemented.

- The exhaustive oracle, semantic fixtures, and performance benchmarks from
  the plan were not added because the implementation request explicitly
  prohibited adding or updating tests. No existing tests were changed, and the
  implementation agent did not run them; the user independently ran the suite
  and supplied its regression report for diagnosis.
