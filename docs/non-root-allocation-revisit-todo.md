# Non-Root Allocation Revisit Todo

Status: Long-term follow-up.
Date: 2026-03-23

## Why This Is Still On The List

Root-only allocation was not the right long-term answer, and the old root stratification experiment was removed for good reason. But that does not mean deeper allocation is unimportant.

The still-interesting question is whether Hansei should sometimes reallocate effort after the search has already uncovered a meaningful deeper frontier split.

That matters when:

1. the hard evidence is not visible at the root,
2. downstream branches have very different continuation cost or variance,
3. a fixed local mass rule under-invests in low-mass but high-importance continuations,
4. future streaming drivers need to rebalance effort online rather than once at startup.

## Long-Term Questions

1. Can beam occupancy be reallocated at deeper frontier splits using downstream difficulty signals rather than only local cull weights?
2. Can such reallocation be done without smuggling mutable policy state into `ProbabilitySpace` or resumable beam snapshots?
3. What statistics are actually worth using for allocation:
   downstream survivability,
   local branching factor,
   observed historical variance,
   or a bounded lookahead value estimate?
4. Is the right abstraction still beam occupancy, or should deeper allocation be phrased as stratified budget slices over frontier families?

## Deferred Experiment Ideas

1. Occupancy floors for selected deeper frontier families after a branch point is discovered.
2. Per-family retained-width quotas that are computed after shallow lookahead rather than at the root.
3. Two-level culling: first choose family budgets, then resample within families.
4. Sequential adaptive allocation where later rounds reassign occupancy based on evidence surprise or posterior collapse.

## Ordering Recommendation

Do not resume this immediately.

Revisit it after:

1. the Phase 1 DAG/DP experiment establishes how much repeated-work sharing can buy us on structured sequential models,
2. we have a clearer picture of whether the remaining beam bottleneck is allocation quality or raw frontier-advancement cost,
3. a benchmark emerges where beam accuracy is limited by poor deeper allocation rather than by computational budget alone.

## Current Conclusion

Non-root allocation remains a good beam-side idea, but it is no longer the next experimental priority.