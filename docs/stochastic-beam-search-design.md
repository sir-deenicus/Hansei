# Stochastic Beam Search Design For Hansei

Status: Active experimental direction.
Date: 2026-03-20

## Goal

Design a bounded stochastic frontier search method that fits Hansei's current weighted continuation-tree representation better than generic SMC.

The method should:

1. operate directly on Hansei's suspended frontier structure,
2. keep memory bounded by an explicit beam width,
3. reuse forced-prefix collapse aggressively,
4. remain competitive on hard discrete evidence problems,
5. admit future streaming/incremental driver support,
6. support future non-root stratified allocation without retaining the whole tree.

## Why This Fits Hansei Better Than SMC

Hansei natively exposes:

1. weighted branching frontiers,
2. suspended continuations,
3. deterministic forced prefixes,
4. exact and approximate local branching operations.

That is exactly the substrate a bounded frontier search wants.

What Hansei does not expose naturally is:

1. semantic time,
2. explicit factor events,
3. explicit sequential state transitions,
4. native stage boundaries for particle filtering.

That mismatch is why generic SMC needed either tree-shape heuristics or manual stage markers. By contrast, stochastic beam search can work directly with the frontier Hansei already has.

## Core Idea

Maintain a bounded pool of live frontier items.

Each round:

1. collapse forced singleton continuations,
2. accumulate any completed answers,
3. expand one frontier layer for the remaining live items,
4. combine all resulting candidates into a temporary pool,
5. keep the pool if it is already within beam width,
6. otherwise cull it stochastically back down to the beam width.

This is a stochastic beam search rather than a stage-aware particle filter.

## Recommended State Representation

Conceptually:

```fsharp
type FrontierItem<'T> =
    {
        Frontier: ProbabilitySpace<'T>
        Occupancy: int
        Weight: float
    }

type BeamState<'T> =
    {
        Live: FrontierItem<'T> list
        Answers: Map<'T, float>
        Rounds: int
    }
```

The live frontier is the bounded working set. The important difference from the earlier sketch in this note is that the first sketch only tracked one weighted live frontier per entry and did not represent replicated beam slots explicitly. The current prototype adds `Occupancy`, so many logical beam slots can share one compact frontier representative without losing multiplicity.

## Round Semantics

For each live item:

1. collapse forced prefixes,
2. if dead, discard it,
3. if terminal, add its mass into `Done`,
4. if branching, expand one local layer.

The expansion result contributes to a temporary candidate pool.

That candidate pool contains weighted live frontier items ready for the next round.

## Forced Prefix Collapse

This is essential.

The search should never waste beam slots on long deterministic chains. Every live item should be normalized by pushing through singleton continuation paths before it enters the next beam round.

This keeps:

1. the beam focused on real choices,
2. the live pool smaller,
3. memory retention lower,
4. the search less sensitive to administrative tree structure.

## Resampling / Culling Policy

When the temporary candidate pool exceeds `beamWidth`, reduce it stochastically.

Recommended default:

1. use systematic resampling over candidate weights,
2. clone high-mass candidates proportionally,
3. reset offspring to a common mass scale after resampling.

This gives a bounded stochastic frontier without the high variance of naive repeated multinomial draws.

The prototype now does one more important thing: when many resampled offspring select the same frontier representative, it preserves that multiplicity as an explicit occupancy count instead of silently collapsing it away. The live frontier is therefore represented compactly as:

1. a unique frontier representative,
2. an occupancy count for how many beam slots it owns,
3. a total retained weight for that representative.

That fixes the earlier failure mode where a nominal beam width of 2500 was accidentally behaving like a beam of size 10 because replicated occupancy was not being carried forward correctly.

## Why This Is Memory Efficient

Memory is driven by:

1. current live frontier,
2. temporary candidate pool for the current round,
3. any memoized subtree structure reachable from those items,
4. answer accumulator.

So the practical memory profile is:

1. bounded by beam width in the persistent live set,
2. bounded by one expansion round in the temporary pool,
3. sensitive to memoization retention, but still much easier to control than deep multi-stratum tree retention.

This is a better fit for Hansei than deeper importance-sampling stratification because we do not need to retain explicit strata across the whole tree. We only keep the current frontier.

After the occupancy-preserving fix, the space picture is more precise:

1. `live-slots` is the logical beam occupancy,
2. `unique-live` is the compact persistent frontier actually retained in memory,
3. `unique-candidates` is the transient frontier before culling,
4. `selected-reps` is the compact live set after culling.

In the current prototype runs with `beamWidth = 2500`, the compact frontier is much smaller than the logical beam:

1. hard evidence: `unique-live=10`, `live-slots=2500`,
2. Oleg rare evidence: `unique-live=34`, `live-slots=2500`,
3. Oleg with exact local likelihood: `unique-live=8`, `live-slots=2500`,
4. sequential HMM: `unique-live=94`, `live-slots=2500`.

So the persistent space cost is mostly proportional to unique frontier representatives, not raw particle multiplicity. That is the core space advantage of this representation.

## Memoization Policy

Default should still be memoized.

Why:

1. multiple survivors may share the same suspended continuation,
2. recomputing every retained local frontier can be expensive,
3. memoized sharing often improves speed.

But a low-memory mode should exist eventually:

1. if beam items retain too much subtree structure,
2. if recomputation is cheaper than retention,
3. if we want a strict low-memory search mode.

So the design recommendation is:

1. default memoized,
2. expose optional `NoMemoize` for frontier search later,
3. decide by profiling.

## Resumable State Machine

The prototype now has an explicit resumable state machine rather than only a one-shot `stochasticBeam` runner.

The key operations are:

1. initialize a beam state from a `ProbabilitySpace`,
2. advance one round,
3. advance many rounds,
4. snapshot the current posterior approximation,
5. run to completion as a thin wrapper over repeated advancement.

That matters for two reasons:

1. it makes incremental advancement a real execution mode instead of a future idea,
2. it separates the search engine from any later driver that wants to decide when to pause, resume, or consume new evidence chunks.

The current prototype is still round-driven rather than evidence-chunk-driven, but the core resumable state is now in place.

## State Safety And Reversibility

This needs a careful distinction.

Hansei's reversible search semantics apply inside the probabilistic program representation itself: suspended continuations, memoized subtree forcing, and branching histories must not be polluted by mutable host-language state that is supposed to rewind under backtracking.

The stochastic beam state is only safe if it remains external inference-driver state.

That means the following are safe:

1. mutable or ephemeral buffers used inside one beam-advance step,
2. accumulated driver statistics,
3. explicit beam frontier state held outside `ProbabilitySpace` and outside subtree thunks.

The following would not be safe:

1. storing mutable beam-driver state inside model closures,
2. allowing mutable driver state to escape into `ProbabilitySpace` nodes,
3. relying on host-language mutation that is expected to rewind when Hansei backtracks.

The current prototype is compatible with Hansei's reversibility in that restricted sense:

1. the beam state lives outside the model,
2. live frontier entries point to ordinary Hansei frontiers,
3. temporary mutation is only used in local driver buffers,
4. no mutable beam state is captured by user continuations.

One subtlety mattered for resumability: a resumable beam state must also be persistent-safe, not just externally scoped. The earlier state-machine refactor stored a mutable `Random` inside the beam state, which meant advancing one state also mutated the RNG seen by older snapshots. That was not safe for branching or independent resumption from an old checkpoint.

The prototype now uses explicit immutable RNG state instead. That makes old and new beam states independent snapshots again, which is the right design if we want resumable advancement to compose cleanly with Hansei's backtracking model and with future driver branching.

## Streaming Support

This design is now partially implemented for incremental advancement, but true streaming still needs an explicit driver boundary.

The right direction is:

1. keep an explicit search state,
2. allow advancing it one round or one evidence chunk at a time,
3. allow reading the current posterior approximation without restarting from scratch.

The first half of that now exists in the prototype: the beam can be resumed round by round from the same retained frontier state. The remaining step for genuine streaming inference is a model interface that can expose evidence chunks incrementally instead of requiring the full observation sequence up front.

## Non-Finite Sources

This approach can support future non-finite sources better than whole-model exact or importance inference, but only if the API is incremental.

The key is not a lazy source by itself. The key is a resumable search state that can consume evidence chunks lazily.

That means:

1. finite lists work now,
2. lazy sequences could work later,
3. open-ended streams become possible when the driver decides when to pause or stop.

## Non-Root Stratified Allocation

Yes, this approach can support non-root stratified allocation naturally.

The basic stochastic beam already allocates effort beyond the root because each round culls and replicates candidates from the current live frontier, not just from the root.

If stronger guarantees are needed later, we can add bucketed culling:

1. assign each candidate to a bucket by a user-provided or inferred key,
2. reserve a small quota for each nonempty bucket,
3. fill the rest of the beam proportionally by weight.

That gives a practical form of non-root stratified sampling without needing to retain the full deeper tree structure.

## Proposed API Shape

Eventually, if the prototype works well, the public interface should look like bounded frontier search rather than particle filtering.

Conceptually:

```fsharp
type StochasticBeamConfig<'T> =
    {
        BeamWidth: int
        MaxRounds: int
        EliteCount: int
        Subsample: ProbabilitySpace<'T> -> ProbabilitySpace<'T>
        SubtreeMemoization: SubtreeMemoization
    }

type Model with
    static member StochasticBeamSearch(distr, config)
```
```

Possible later additions:

1. optional bucket function for non-root stratified allocation,
2. optional low-memory no-memo mode,
3. resumable search state for streaming use,
4. search diagnostics.

## Accuracy Improvement Split

The accuracy work now falls into two logical parts.

Part 1 is variance reduction within the current beam semantics.

This keeps the current frontier-search structure, but makes culling less noisy. The first step is deterministic elite retention before stochastic culling: keep a small number of highest-mass candidates exactly, then resample the remaining beam slots over the rest of the candidate pool.

The prototype now starts this part with an `EliteCount` knob in the beam config.

Why this belongs in part 1:

1. it does not change the model interface,
2. it does not require new bucket keys or heuristics,
3. it directly reduces accidental loss of obviously strong candidates,
4. it can be benchmarked against the current stochastic beam with minimal structural churn.

Part 2 is better allocation across distinct hypothesis families.

That is where bucketed culling, local lookahead scores, or other diversity-preserving allocation policies belong. Those changes are more structural because they decide not just how much mass to keep, but which qualitatively different regions of the frontier must remain represented.

## Experimental Plan

Phase 1:

1. prototype a bounded stochastic beam search in an `.fsx`,
2. compare it against exact, prepared importance, no-pre importance, and path sampling,
3. reuse the current hard-evidence and Oleg-style benchmarks.

Phase 2:

1. measure accuracy and speed,
2. inspect peak live width and candidate width,
3. compare behavior on sequential evidence models,
4. decide whether this direction is more promising than generic SMC.

## Current Prototype Results

After fixing occupancy preservation, accumulated-weight propagation, and the resumable-state RNG persistence issue, the current prototype is still accurate on the existing benchmark suite.

An elite-retention variant is now also being prototyped as the first part of the accuracy work. On the sequential HMM case it improves accuracy substantially, but it also keeps many more distinct frontier representatives alive, so that case becomes heavier by design. Full benchmark-table refresh for the elite-retention variant is still pending.

With `beamWidth = 2500`:

1. hard evidence posterior: beam `L1 = 0.003547`,
2. Oleg rare evidence: beam `L1 = 0.000267`,
3. Oleg with exact local likelihood: beam `L1 = 0.000291`,
4. sequential HMM evidence: beam `L1 = 0.011316`.

Compared with other methods on the same runs:

1. prepared importance is still best or tied on the hard-evidence and exact-local cases,
2. stochastic beam is dramatically better than path on all current tests,
3. stochastic beam is competitive with prepared importance on the HMM stability run,
4. stochastic beam strongly outperforms no-pre importance on the buried-evidence cases.

Stability over 12 runs:

1. hard evidence: beam mean `L1 = 0.003453`,
2. Oleg rare evidence: beam mean `L1 = 0.000508`,
3. Oleg exact-local case: beam mean `L1 = 0.000282`,
4. sequential HMM: beam mean `L1 = 0.019847` versus importance mean `L1 = 0.010866`.

Timing is now part of the prototype output as well:

1. hard evidence: beam about `1.607 ms/run`, importance about `0.804 ms/run`, path about `57.519 ms/run`,
2. Oleg rare evidence: beam about `5.934 ms/run`, importance about `154.298 ms/run`,
3. Oleg exact-local case: beam about `0.786 ms/run`, importance about `0.168 ms/run`,
4. sequential HMM: beam about `5.817 ms/run`, importance about `212.642 ms/run`, path about `103.712 ms/run`.

The internal timing split still suggests that frontier advancement dominates cost more than culling does. On the HMM run, advancement is about `2.292 ms` and culling about `1.348 ms`.

## Sequential HMM Prefix Study

The prototype now includes a prefix study on the same HMM observation stream.

Across prefixes of length 1 through 8:

1. beam `L1` stays small, from about `0.000400` to `0.027084`,
2. beam time rises gradually from about `0.160 ms` to about `1.517 ms`,
3. importance time rises sharply on deeper prefixes, reaching about `194.758 ms` by prefix length 8,
4. path accuracy degrades much more sharply than beam on longer prefixes.

This is the strongest current argument for bounded frontier search as a future streaming-oriented inference mode in Hansei.

## Incremental Resume Study

The prototype now also includes an explicit incremental-resume demonstration on the HMM case.

The same beam state is advanced through checkpoints at rounds `1, 2, 4, 8, 12, 17` without restarting from scratch.

Observed behavior:

1. the pending live mass shrinks steadily as more of the frontier resolves,
2. the final resumed posterior matches the one-shot beam result,
3. incremental resume costs about `1.491 ms/run` versus about `1.506 ms/run` for one-shot execution in the current script.

That overhead is small enough that resumable execution looks practical as a foundation for a later streaming driver.

## Current Recommendation

This is the next experimental search direction most compatible with Hansei's current representation.

If one inference idea should be explored after generic SMC, it should be bounded stochastic beam / frontier search, not deeper stratified importance sampling and not further attempts to infer SMC stages from generic tree structure.

The occupancy-preserving version is now a credible experimental direction rather than just a sketch.

The next architectural step should be an evidence-chunk driver layered on top of the existing resumable beam state, not another rewrite of the core beam loop.