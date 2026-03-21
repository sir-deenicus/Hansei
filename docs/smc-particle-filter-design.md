# Sequential Monte Carlo Design For Hansei

Status: Failed experiment as a general Hansei inference direction.
Date: 2026-03-20

## Goal

Design a proper Sequential Monte Carlo (SMC) or particle-filter inference method for Hansei's `ProbabilitySpace<'T>` representation.

The design should:

1. fit Hansei's weighted continuation tree,
2. respect the semantics of `observe`, `soft_observe`, and `factor`,
3. avoid tying resampling decisions to arbitrary syntactic tree depth,
4. keep memory bounded in practice,
5. outperform naive path sampling on models with incremental evidence,
6. complement rather than replace prepared importance sampling.

## Recommendation

The right SMC design for Hansei is not "resample every time a subtree is forced."

The right design is a stage-aware particle filter:

1. advance each particle until the next meaningful branching or weighting boundary,
2. accumulate all deterministic and local weight changes along the way,
3. stop at a stage boundary shared by the current live particles,
4. resample only when the effective sample size (ESS) drops enough,
5. use systematic resampling rather than multinomial resampling,
6. treat memoization as a performance policy, not as part of the SMC semantics.

This is the design that best matches Hansei's current execution model.

## Why A Naive Tree-Step Particle Filter Is Not Enough

A naive sketch for Hansei often looks like this:

1. keep `K` particles,
2. force each `ContinuedSubTree` once,
3. expand its children,
4. resample immediately,
5. repeat until all particles are `Value`s.

That approach is easy to prototype, but it has several problems.

### Problem 1: Tree Depth Is Not Semantic Time

In Hansei, one extra `Delay`, one extra singleton continuation, or one extra local factoring rewrite can change how many subtree forces occur before the same logical event.

If resampling is triggered by subtree forcing itself, then the particle filter becomes sensitive to representation details rather than model meaning.

That is not a sound SMC architecture.

### Problem 2: Forced Prefixes Should Not Consume Particle Stages

Hansei already has explicit machinery to collapse deterministic singleton chains and forced prefixes.

A good SMC method should reuse that idea. If a particle is moving through a deterministic or effectively forced prefix, that is not a useful resampling point.

### Problem 3: Resampling Every Step Is Too Aggressive

Resampling every stage increases variance and causes unnecessary particle impoverishment.

Standard SMC practice is to:

1. propagate,
2. update weights,
3. measure ESS,
4. resample only when ESS falls below a threshold.

### Problem 4: Multinomial Resampling Is The Wrong Default

Repeatedly drawing particles independently from the weighted pool is the highest-variance common resampler.

For Hansei, systematic resampling is a better default because it is:

1. lower variance,
2. simple,
3. linear time,
4. easy to implement over arrays.

## The Core Idea: Stage-Aware Particle Advancement

The design should revolve around a particle step function that advances one particle to the next semantic stage.

The stage is not "one thunk force." The stage is the next point where the particle has a real choice or has accumulated meaningful new weight.

Each particle should advance through three kinds of structure without stopping:

1. deterministic singleton continuations,
2. exact local likelihood factors,
3. zero-cost administrative tree structure.

It should stop only when it reaches one of these:

1. a real branching point with multiple live continuations,
2. a weighting boundary that should synchronize the particle population,
3. a terminal `Value`,
4. a dead branch.

That means the main abstraction is not a sampled subtree. It is a suspended particle state that can be advanced to the next stage.

## Proposed Particle State

The particle state should be represented explicitly.

Conceptually:

```fsharp
type Particle<'T> =
    {
        Frontier: ProbabilitySpace<'T>
        Weight: float
        Status: ParticleStatus<'T>
    }

and ParticleStatus<'T> =
    | Live
    | Done of 'T
    | Dead
```
```

In implementation, it is likely better to normalize this further so that a live particle is always stored in a form already collapsed through forced prefixes.

The important invariant is:

1. `Weight` is the cumulative linear importance weight,
2. `Frontier` is the next unresolved live distribution for this particle,
3. `Done` particles keep their final value and their current weight,
4. `Dead` particles are ignored after normalization or removed before resampling.

## Proposed Advance Semantics

The central routine should be something like:

```fsharp
advanceParticle : Particle<'T> -> AdvancedParticle<'T>
```

where the result is one of:

1. terminal value,
2. dead particle,
3. staged frontier ready for selection or exact local branching.

### Advance Rules

While advancing a live particle:

1. collapse any singleton continuation chain,
2. multiply through the accumulated path probability,
3. force memoized subtrees as needed,
4. if the frontier becomes empty, mark the particle dead,
5. if the frontier becomes a single `Value`, mark it done,
6. if the frontier becomes a small exact local model, optionally integrate it exactly,
7. if the frontier becomes a genuine multiway branching frontier, stop.

This is where Hansei's existing forced-path collapse logic is directly relevant.

## Stage Boundaries

A Hansei SMC needs a rule for what counts as a shared stage.

The cleanest design is:

1. advance every live particle until it reaches its next branching or weighting boundary,
2. collect the resulting staged frontiers,
3. perform one local proposal step from each staged frontier,
4. update weights,
5. compute ESS,
6. resample if needed.

This still does not expose full semantic time in the way a factor graph would, but it is far better than resampling on raw subtree forces.

## Proposal Mechanism

At a staged frontier, the particle must choose how to advance through multiple live branches.

For Hansei, the best initial proposal is simple:

1. use the branch weights already present in the frontier,
2. sample one branch proportionally to those weights,
3. multiply the particle weight by the normalizing mass correction if needed.

This is essentially the same local proposal logic already used by Hansei's weighted tree samplers.

Later improvements could include:

1. local lookahead proposals,
2. user-provided proposals,
3. proposals informed by exact local likelihood helpers.

But the baseline SMC does not require those.

## Exact Local Likelihood Integration

The new `factor`, `exact_local_likelihood`, and `exact_local_observe` helpers are directly useful for SMC.

When a local submodel can be integrated exactly, the particle filter should prefer that representation because it:

1. removes local rejection,
2. reduces particle variance,
3. shortens the effective trace length,
4. reduces sensitivity to resampling schedule.

This does not mean the SMC engine must automatically discover such factorizations. It means the engine should work well when the model provides them.

## Resampling Policy

Resampling should be triggered by effective sample size.

Given normalized particle weights $w_1, \dots, w_K$, define:

$$
ESS = \frac{1}{\sum_{i=1}^{K} w_i^2}
$$

Recommended default:

1. resample when `ESS < 0.5 * K`

This is a strong default for Hansei because it avoids unnecessary resampling on easy stages while still correcting weight degeneracy when evidence becomes sharp.

## Resampler Choice

Use systematic resampling.

Given cumulative normalized weights and one random offset $u \sim \text{Uniform}(0, 1/K)$, take positions:

$$
u, u + \frac{1}{K}, u + 2\frac{1}{K}, \dots, u + (K-1)\frac{1}{K}
$$

and clone the particles hit by those positions.

This has three advantages over naive repeated sampling:

1. lower variance,
2. exactly `K` offspring,
3. linear-time implementation with one sweep through cumulative weights.

After resampling, reset all live particle weights to the common average mass represented by the resampled population.

## Memoization Policy

Memoization is not required for SMC correctness.

However, memoization is still the right default.

### Why Memoization Is Fine

In Hansei, a memoized subtree represents a deterministic continuation of the probabilistic program, not an independently resampled host-language random event.

If multiple offspring particles land on the same suspended subtree, sharing the forced result is usually desirable.

### When Unmemoized Might Help

An SMC-specific `NoMemoize` mode may reduce memory retention when:

1. many particles clone large forced frontiers,
2. recomputation is cheaper than retention,
3. we want a strict low-memory mode.

So the design recommendation is:

1. default to memoized subtrees,
2. expose an opt-in `NoMemoize` or local policy for SMC,
3. profile before deciding that unmemoized should be the default.

## Memory Model

A well-engineered SMC for Hansei should keep memory bounded by:

1. the current particle array,
2. the temporarily expanded staged frontier,
3. any memoized subtree sharing still reachable from live particles.

This is not literally $\mathcal{O}(K)$ in the strongest formal sense, because frontier expansion can still create temporary wide pools.

But it is practically bounded when implemented carefully with:

1. arrays or `ResizeArray`s for live particles,
2. in-place overwrite after resampling,
3. forced-prefix collapse before frontier materialization,
4. optional local exact integration.

## Relationship To Existing Hansei Samplers

### Compared To Path Sampling

SMC should dominate plain path sampling when evidence is spread across stages, because it can periodically cull dead or low-weight particles and redistribute effort.

### Compared To Prepared Importance Sampling

Prepared importance sampling remains strong when:

1. shallow pre-exploration reveals useful structure cheaply,
2. evidence is not naturally stagewise,
3. exact local likelihood factors are already available.

SMC is most promising when:

1. there is sequential evidence,
2. weights collapse progressively rather than only at the very end,
3. resampling can rescue good partial trajectories before total degeneracy.

That means SMC should be a complementary inference mode, not a replacement for the current importance sampler.

## Proposed API Shape

The first public API should stay small.

Conceptually:

```fsharp
type SMCConfig<'T> =
    {
        NumParticles: int
        ResampleThreshold: float
        MaxStageDepth: int
        Subsample: ProbabilitySpace<'T> -> ProbabilitySpace<'T>
        SubtreeMemoization: SubtreeMemoization
    }

type Model with
    static member ParticleFilter(distr, config)
```
```

Possible later additions:

1. custom resampler,
2. custom local proposal selector,
3. tracing hooks for ESS and resampling diagnostics,
4. user-declared stage markers.

## Implementation Strategy

The implementation should be staged.

### Phase 1

Build an internal experimental SMC with:

1. forced-prefix collapse,
2. weighted local branching proposal,
3. ESS-triggered systematic resampling,
4. default memoization.

Do not expose it publicly yet.

### Phase 2

Benchmark it against:

1. path sampling,
2. prepared importance,
3. factored Oleg-style examples,
4. any genuinely sequential evidence models.

Only keep it if it clearly wins on a meaningful class of models.

### Phase 3

If Phase 2 is promising, add:

1. lightweight diagnostics,
2. optional user stage markers,
3. optional low-memory `NoMemoize` mode,
4. documentation about when SMC is the right choice.

## Phase 1 And Phase 2 Execution

Phase 1 and Phase 2 were executed in the experimental script:

1. [Hansei/smc-particle-filter-prototype.fsx](Hansei/smc-particle-filter-prototype.fsx)

The prototype implements:

1. forced-prefix collapse,
2. one-step local survival lookahead before proposal,
3. ESS-triggered systematic resampling,
4. memoized subtrees by default.

It is still intentionally an experiment rather than public library API.

### Benchmark Summary

The prototype was benchmarked against:

1. prepared importance,
2. no-pre-explore importance,
3. path sampling,
4. exact inference as ground truth.

#### Hard Evidence Posterior

Result:

1. SMC clearly outperformed path and no-pre-explore importance,
2. but it was still worse than prepared importance,
3. and substantially slower than prepared importance.

Interpretation:

1. the stage-aware SMC is no longer degenerating into plain path sampling,
2. but Hansei's prepared importance sampler is still the stronger tool for this kind of small discrete hard-evidence posterior.

#### Oleg-Inspired Rare Evidence

Result:

1. SMC improved dramatically over path sampling,
2. but it remained much worse than prepared importance,
3. and its stability was still poor on the unfactored buried-evidence case.

Interpretation:

1. stage-aware resampling alone is not enough to solve deeply buried local rejection,
2. the real issue remains proposal quality and local factoring, not just particle management.

#### Oleg-Inspired With Exact Local Likelihood

Result:

1. once the local rejection was rewritten with `exact_local_observe`, the model became easy for all methods,
2. SMC performed reasonably well,
3. but prepared importance remained the most accurate and fastest strong method.

Interpretation:

1. exact local likelihood factoring is more important than SMC on this class of example,
2. SMC benefits from the factoring, but it is not the main reason the model becomes tractable.

#### Sequential HMM Evidence

Result:

1. this was the most promising case for SMC,
2. SMC strongly outperformed path sampling,
3. SMC was competitive with importance in accuracy,
4. and faster than importance in the current experiment.

Interpretation:

1. Hansei SMC is most promising when evidence arrives incrementally across many stages,
2. that matches the intended use case for particle filtering.

### Current Conclusion

The experimental result is:

1. SMC is worth keeping as an experimental direction,
2. but not yet as a public core inference API,
3. because it does not beat prepared importance on the current hard buried-evidence benchmarks,
4. while it does look genuinely promising on sequential evidence models.

So the design judgment after Phase 1 and 2 is:

1. do not replace prepared importance,
2. keep SMC experimental,
3. push it further only if we care about genuinely sequential models enough to justify the extra complexity.

### Explicit Stage Markers Experiment

An explicit user-stage variant was also prototyped in:

1. [Hansei/smc-particle-filter-prototype.fsx](Hansei/smc-particle-filter-prototype.fsx)

That variant uses an explicit staged model interface rather than discovering stage boundaries from the generic continuation tree.

The result was mixed:

1. it reduces the number of resampling stages,
2. it gives the user a semantically meaningful notion of "time",
3. but the first implementation was much slower because each user-marked stage was exact-explored before sampling.

So the conclusion is not that user stage markers are a bad idea. The conclusion is that user stage markers are useful, but they should be integrated into the execution model directly rather than implemented as repeated exact exploration of each whole stage block.

That makes explicit stage markers a promising next design step, especially for sequential and streaming evidence models.

## Memory Use

The rough memory profile of Hansei SMC versus prepared importance is different.

### Prepared Importance Sampling

Prepared importance tends to spend memory on:

1. shallow explored frontiers,
2. memoized subtrees,
3. retained branch structures used across many samples.

Its memory cost is driven more by frontier width and retained subtree structure than by the sample count itself.

### SMC

SMC tends to spend memory on:

1. the current particle population,
2. the next staged population during propagation,
3. any shared memoized subtrees still reachable from those particles.

That means SMC memory is more directly tied to particle count, while prepared importance memory is more directly tied to retained explored structure.

In practice, a careful SMC can often be kept from exploding memory because resampling discards most of the old particle population and keeps only the current live set. But it is not automatically cheap:

1. if each live particle points into a large memoized frontier, memory can still grow,
2. if branching is wide before resampling, temporary staged pools can also be large.

So SMC is not a magic memory fix. It is just easier to keep approximately bounded by particle count when the implementation is careful.

## Streaming Evidence And Non-Finite Sources

SMC is the most natural Hansei inference direction for streaming evidence.

Why:

1. particles can be updated incrementally as new evidence arrives,
2. resampling naturally operates stage by stage,
3. we do not need the whole future evidence stream up front.

This suggests a future design where user stage markers or a staged model interface consume a source lazily.

That could support:

1. finite observation lists,
2. lazy sequences,
3. potentially non-finite evidence streams with explicit stopping conditions.

## CE `for` And Non-Finite Iteration

The current computation expression `for` support in Hansei assumes a finite sequence because it materializes the input sequence before iterating.

That is acceptable for the current exact and importance machinery, but it is not the right shape for streaming inference.

A lazy list or stage-stream interface would help here because it would:

1. avoid requiring the whole input up front,
2. align better with SMC's sequential update model,
3. make non-finite or open-ended evidence sources possible in principle.

The real benefit would not be the lazy list by itself. The real benefit would be pairing a lazy source with an inference method, such as stage-aware SMC, that can consume evidence incrementally.

## What Would Make This Much Better Later

The current Hansei tree is expressive enough for a good SMC, but not ideal.

The following additions would improve it substantially:

1. explicit stage markers,
2. explicit factor events,
3. a first-order suspended-state representation rather than only opaque closures,
4. richer local proposal interfaces,
5. optional particle rejuvenation moves.

Those are later improvements. They are not prerequisites for a useful first implementation.

## Final Recommendation

If Hansei gets an SMC implementation, it should be:

1. stage-aware,
2. forced-prefix-collapsing,
3. ESS-triggered,
4. systematically resampled,
5. memoized by default,
6. designed to cooperate with exact local likelihood factors.

It should not be a naive "resample every subtree force" algorithm. That design is too sensitive to the surface shape of the continuation tree and too disconnected from the actual flow of information in Hansei models.