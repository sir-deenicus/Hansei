# Nested Unbounded Inference Note

Status: Long-term research note.
Date: 2026-03-23

## Motivation

One difficult future direction for Hansei is support for nested unbounded inference where:

1. an outer inference process invokes an inner inference process,
2. either or both processes may be unbounded,
3. naive depth-first execution can starve later work forever.

This is exactly the kind of setting where fair interleaving matters.

## Why The Backtracking Work Is Relevant

The backtracking monad already contains a useful conceptual pattern:

1. maintain a frontier of suspended work,
2. expose work in a fair interleaving order rather than exhausting one branch first,
3. represent deferred computation explicitly through thunks,
4. preserve progress for multiple infinite or very deep branches.

That does not solve probabilistic nested inference directly, but it does suggest a scheduling model.

## Possible Direction

For nested unbounded probabilistic inference, one plausible approach is to represent each active inference task as a resumable suspended computation and interleave them in bounded quanta.

Conceptually:

1. outer inference produces requests for inner inference work,
2. inner inference tasks are treated as resumable jobs rather than immediately forced to completion,
3. a fair scheduler advances all active jobs incrementally,
4. partial results are exposed upward in a monotone or refinement-friendly way.

## Key Technical Questions

1. What is the correct notion of partial answer for an unfinished probabilistic subproblem?
2. How do we normalize or combine unfinished nested masses without violating semantics?
3. Can we define a scheduler that is fair enough to avoid starvation but still efficient on finite jobs?
4. What form of resumable state is safe under Hansei's reversibility and backtracking constraints?
5. Which parts of the current beam-state machinery can be reused for this, and which parts are too tied to finite frontier rounds?

## Likely Building Blocks

1. explicit resumable inference state values,
2. external fair scheduling over suspended jobs,
3. bounded per-step advancement rather than full recursive forcing,
4. monotone diagnostics such as lower and upper mass envelopes or progressively refined approximations,
5. careful separation between scheduler state and monadic semantic state.

## What Not To Do

1. Do not bury scheduler mutation inside `ProbabilitySpace` or subtree thunks.
2. Do not rely on ordinary depth-first recursion for nested unbounded cases.
3. Do not assume closure identity is sufficient to detect equivalent nested tasks.

## Near-Term Recommendation

Do not implement this yet.

Instead:

1. keep it as a design direction informed by the backtracking fair-stream work,
2. reuse the lesson that interleaving and explicit suspension are central,
3. return to it only after the DAG/DP and current beam experiments have clarified the more immediate performance envelope.