# Hansei

Hansei is an F# codebase for representing branching computations as lazy weighted search trees and interpreting those trees in different ways.

On the surface, that yields probabilistic programs with exact inference, conditioning, and approximate search. Look deeper and it also gives you fair nondeterministic search, logic-programming-style relations, max-product reasoning, semiring-based dynamic programming, quantum-mechanical amplitude calculations, and tensor or differentiable interpretations. A single question runs through the entire repository: once a computation has been reified as a weighted tree of alternatives, what can you do with it?

Three layers organize the code:

1. **`Hansei.Continuation/*`** — The substrate: continuations, lazy streams, fair backtracking, and search utilities.
2. **`Hansei/*`** — A weighted search-tree language built on that substrate, where a probabilistic model becomes a lazy tree of choices, values, and suspended subtrees.
3. **`Hansei.Quantum/genericprobtest.fsx`** — Despite the folder name, not a standalone product. Rather, a large idea notebook demonstrating that the same weighted-tree machinery can be interpreted over many semirings beyond ordinary probabilities.

Approach the repository as a research codebase rather than a polished package. Its value lies in the ideas and representations it works through: continuations, fair search, weighted branching, exact and approximate inference, algebraic interpretations, and specialized submodels.

## The Core Idea

A computation can branch into alternatives. Those alternatives can carry weights. Exploration can be exact, approximate, shallow, greedy, random, or structured. And the same branching structure admits interpretation as probability, logic, counting, max-product, tropical algebra, complex amplitudes for quantum mechanics, or differentiable/symbolic computation — all depending on the weight type.

In the main Hansei layer, these ideas take concrete form as `ProbabilitySpace<'T>`, `WeightedTree<'T> = Value of 'T | ContinuedSubTree of ...`, the `dist { ... }` computation expression, and primitives like `observe`, `factor`, and `exact_local_likelihood`, backed by exact exploration and multiple approximate inference procedures.

A Hansei model is therefore not best thought of as "a sampler." It is first a lazy weighted search tree. Exact inference, rejection, importance sampling, beam methods, and specialized exact submodels are all different ways of traversing or collapsing that tree.

## `Hansei.Continuation/*`

The continuation project provides the machinery underpinning the later Hansei layers.

### Continuations

`Continuations.fs` defines ordinary and delimited continuations. Continuations grant explicit control over the remainder of a computation, allowing probabilistic and nondeterministic branching to be reified rather than immediately executed. Suspension and re-entry become natural operations. Everything that follows — where a computation is treated as an explicit tree of suspended futures rather than something that simply runs to completion — rests on this foundation.

### Lazy and fair nondeterminism

`Backtracking.fs` and `LazyList.fs` address search spaces that may be large or infinite. Three levels of laziness come into play: plain lists are exhaustive but eager; lazy lists defer work but can still exhibit depth-first bias; fair streams interleave branches so that no single infinite subtree starves the rest.

`Backtracking.fs` devotes considerable attention to `Choice`, `Thunk`, interleaving, and stack-safe bind variants — far from incidental implementation detail. These mechanisms determine which infinite or highly branching spaces can be searched faithfully.

### Search as an interface

`TreeSearch.fs` offers several search views: exhaustive list search, sequence-based search, fair backtracking search, and lazy-list-based search. The same high-level logical structure thereby acquires different operational behavior depending on the underlying search container.

## `Hansei/*`

The weighted probabilistic layer, built atop the continuation substrate.

### `ProbabilitySpace<'T>`

`ProbabilitySpace.fs` defines the core representation. Values inhabit a weighted tree rather than being returned bare; subtrees can be suspended and forced later; `dist { ... }` composes weighted choices lazily. The key primitives include `distribution` for weighted alternatives, `always`/`exactly` for deterministic values, `fail` for impossible branches, `observe` for hard constraints, `factor` for soft weighting, and `exact_local_likelihood` for analytically collapsing a local submodel into a single factor.

That last primitive deserves emphasis. When a local latent region can be integrated exactly, `exact_local_likelihood` avoids expanding it naively into the outer tree, instead condensing it into a local likelihood factor. Much of the more interesting work in the repository moves in precisely that direction.

### Exact exploration

`Exploration.fs` provides exact traversal with depth control. The explorer walks the weighted tree, accumulates weights for equal terminal values, optionally stops descending after a chosen depth, and returns both discovered answers and the suspended residual computation. This reveals the overall design intent: the tree is primary, and inference is a controlled projection of that tree into a distribution over answers.

### Approximate inference

`Hansei.fs`, `IncrementalSamplers.fs`, and `StochasticBeam.fs` supply approximate procedures over the same tree structure — path sampling, greedy path selection, importance sampling with pre-exploration, ordinary beam search, stochastic beam search, and incremental sampler states that can be advanced in chunks and checkpointed. Crucially, these are all different traversals of one shared representation, not separate model languages.

### Distribution helpers

`Distributions.fs` provides model-building helpers: Bernoulli and categorical choices, uniform distributions, and recursive examples like geometric and Polya/Dirichlet-style urn processes. These examples make the intended style clear — models are ordinary recursive F# code inside `dist { ... }`, not a separate external DSL.

### Structured exact subproblems

Some of the most compelling work in `Hansei/*` concerns identifying subproblems that should bypass naive recursive expansion: finite DAG inference in `DynamicProgrammingDag.fs`, trie-based segmentation in `TrieSegmentation.fs`, and local exact likelihood collapsing in `ProbabilitySpace.fs`. A recurring theme emerges: write a generic probabilistic outer model, detect inner regions with exploitable structure, solve those regions exactly or more efficiently, and feed the result back into the larger model. That story is far richer than "a sampler library."

## Inference Algorithms

Several inference procedures operate over the same underlying weighted tree, differing mainly in how aggressively they explore, approximate, collapse, or retain frontier structure. The main public entry points live in `Hansei/Model.fs`:

- `Model.ExactInfer`
- `Model.PathSample`
- `Model.GreedySample`
- `Model.ImportanceSamples`
- `Model.BeamSearch`
- `Model.StochasticBeam`

### Exact exploration

Exact inference serves as the semantic reference point for the repository. `Exploration.fs` walks the entire reachable weighted tree, merges equal terminal answers, and returns the exact resulting distribution. For small or well-structured discrete models, nothing captures the meaning of a model more cleanly. Every approximate algorithm that follows is best understood as a controlled compromise against this exact traversal.

### Path sampling and greedy sampling

The simplest approximate strategy in `Hansei.fs` is path sampling: repeatedly start at the root, collapse forced singleton prefixes, choose one branch at each real choice point, accumulate the resulting answer weight, and average over many runs. `PathSample` chooses branches randomly; `GreedySample` uses a max selector. Both are conceptually straightforward reference procedures, though they can waste considerable work when evidence is buried deep in the tree.

### Importance sampling

Importance sampling occupies a more central place in Hansei than naive path sampling. The distinctive idea is not merely "draw weighted samples" but rather to prepare a better sampling frontier before committing the main sample budget.

Concretely, the importance sampler collapses forced singleton prefixes aggressively, collects already-exposed terminal mass exactly, optionally shallow-pre-explores the tree before sampling, prepares a frontier once and reuses it across many samples, and maintains correction factors for retained frontier mass. Shallow preparation pays off whenever it can expose deterministic structure, collapse forced prefixes, prune dead mass early, or build a reusable frontier. Buried evidence remains fundamentally hard, however, when the decisive constraint surfaces only deep in the tree and cannot be factored into an exact local likelihood. Two strategies help more directly in those situations: local likelihood factoring, which analytically collapses the hard local subproblem instead of discovering it late by sampling; and stochastic beam search, which maintains a bounded population of live weighted frontier hypotheses rather than repeatedly committing to a single long trajectory.

This is precisely why the repository includes `factor` and `exact_local_likelihood` — they let the model author move evidence handling closer to where it matters, rather than forcing the sampler to discover it only after a long rollout. The design notes in `docs/importance-sampling-stratification-notes.md` record the current stance explicitly: shallow preparation is broadly useful; buried evidence is the real hard case; root-only stratification was tried and removed; prepared importance sampling earned its place more clearly than stratified variants.

### Beam search

Ordinary beam search retains only the highest-scoring frontier items at each expansion layer — a deterministic bounded-width search that expands one layer, scores candidates by weight, keeps the top `beamWidth`, and repeats until only values remain or a limit is reached. Because it maximizes rather than preserving the posterior, beam search suits tasks where the goal is to recover high-probability explanations or MAP-like structures rather than full distributional fidelity.

### Stochastic beam search

Stochastic beam search is one of the most active ideas in the repository. As `docs/stochastic-beam-search-design.md` describes, Hansei's natural exposure of weighted branching frontiers, suspended continuations, deterministic forced prefixes, and exact local frontier operations makes bounded stochastic frontier search a far better fit than generic stage-based particle filtering.

Each round collapses forced prefixes, discards dead frontiers, accumulates completed answers, expands one local layer for remaining live frontiers, pools all resulting candidates, and stochastically culls back down to the beam width. A key implementation idea is occupancy-preserving compaction: when many logical beam slots select the same frontier representative, the implementation stores a compact unique representative alongside an occupancy count and retained total weight. Persistent memory cost thus scales with unique live frontier representatives rather than raw particle multiplicity.

Beyond the core round structure, the stochastic beam code incorporates bounded beam width and round limits, elite retention, diversity buckets, lightweight lookahead scoring, resumable round-by-round state, and immutable RNG state so that resumed snapshots remain independent. Conceptually, this is a stochastic bounded frontier search over Hansei's native suspended search tree — not a generic SMC implementation retrofitted onto a model that does not naturally expose stages.

### Incremental and checkpointed inference

`IncrementalSamplers.fs` adds resumable state objects for path sampling, importance sampling, and stochastic beam search. By separating the model representation, the inference driver, and current search state, it allows a sampler or beam search to be advanced in chunks, snapshotted, resumed, or run to completion — inference becomes an incremental process rather than a single all-or-nothing call.

## `genericprobtest.fsx`

`Hansei.Quantum/genericprobtest.fsx` is the clearest statement of the repository's broader ambition. The file argues, in code, that the central object is not specifically a probability monad but a generic weighted branching computation over a semiring. It defines `GenericProbabilitySpace<'T,'W>` with semiring-constrained weights, generic `distribution`, `observe`, `explore`, and builders, and multiple interpretations driven by the algebra of `'W`. The abstraction crystallizes neatly: sequencing corresponds to multiplication, branching to addition, `observe` prunes branches by returning zero weight, and `explore` collapses equal outcomes by summing their weights. From that foundation the file develops several interpretations.

### Ordinary probabilities

With `float` weights, the generic machinery behaves as a probabilistic program. Alternatives add probabilities, sequential choices multiply them, and normalization yields a posterior distribution.

### Logic programming

Under a logical semiring, the same computation becomes a logic program. The file works through relational examples — parent, sibling, cousin, unification, reversible list relations, and Prolog-like goals — demonstrating one of the strongest ideas in the repository: the same branching language acts as probability or logic depending solely on the weight algebra.

### Quantum mechanics

When weights are read as complex amplitudes, branches carry amplitudes, identical outcomes combine by amplitude addition, and interference emerges when `explore` collapses equal outcomes. Far from a loose analogy, this is ordinary quantum mechanics expressed through the same weighted branching machinery: superposition via additive complex amplitudes, sequential evolution via multiplicative composition, interference when equal basis states collapse. The formalism is expressive enough for arbitrary quantum-circuit algorithms, a direction sketched directly in `Hansei.Quantum/Quantum.fs` with symbolic gates and constructions — Hadamard, phase shifts, rotations, CNOT, Bell states, and measurement.

### Max-product and shortest-path readings

Once weights function as semiring elements rather than probabilities, the same program can behave as Viterbi-style max-product search, tropical/shortest-path aggregation, counting, or provenance tracking. The script's comments are explicit: multiple `return` lines are additive alternatives, not imperative early exits.

### Tensor and linear algebra interpretations

The script expresses dot products, matrix-vector multiplication, matrix-matrix multiplication, and tensor contraction / einsum-style computations through the same machinery. The trick is elegant: indices are sampled as weighted alternatives, equality constraints are imposed with `observe`, and `explore` performs the summation over matching indices. The system thereby doubles as a compact language for semiring-valued sum-product programs.

### Differentiable weights

A dual-number weight type lets the same model structure support both value computation and derivative propagation, reusing the identical search and combination operations. Once more, the branching language stays fixed while the algebra of the weights changes.

## Reading The Repository

To follow the ideas in the order they build on each other:

1. `Hansei.Continuation/Continuations.fs`
2. `Hansei.Continuation/Backtracking.fs`
3. `Hansei.Continuation/TreeSearch.fs`
4. `Hansei/ProbabilitySpace.fs`
5. `Hansei/Exploration.fs`
6. `Hansei/Distributions.fs`
7. `Hansei/Hansei.fs`
8. `Hansei/IncrementalSamplers.fs`
9. `Hansei.Quantum/genericprobtest.fsx`

After that, `DynamicProgrammingDag.fs` and `TrieSegmentation.fs` show the "specialized exact submodel inside a larger weighted framework" direction developed more concretely.

## Small Examples

### 1. Exact finite probabilistic model

```fsharp
open Hansei
open Hansei.Probability
open Hansei.Distributions

let coin = bernoulli 0.5

let twoCoins =
    dist {
        let! a = coin
        let! b = coin
        return (a, b)
    }

let exact = Model.ExactInfer twoCoins
```

Builds a weighted tree and explores it exactly.

### 2. Conditioning with a hard constraint

```fsharp
open Hansei
open Hansei.Probability
open Hansei.Distributions

let posterior =
    dist {
        let! a = uniform [1..6]
        let! b = uniform [1..6]
        do! observe (a + b = 7)
        return (a, b)
    }
    |> Model.ExactInfer
```

`observe` removes inconsistent branches from the tree.

### 3. Soft weighting with `factor`

```fsharp
open Hansei
open Hansei.Probability
open Hansei.Distributions

let biasedPosterior =
    dist {
        let! x = uniform [1..6]
        do! factor (if x = 6 then 5.0 else 1.0)
        return x
    }
    |> Model.ExactInfer
```

Rather than pruning branches, `factor` reweights them.

### 4. A simple causal reasoning pattern

Hansei has no special causal DSL, but causal reasoning follows naturally when the generative structure is written explicitly.

```fsharp
open Hansei
open Hansei.Probability

type Health = Healthy | Sick
type Treatment = Treat | NoTreat
type Outcome = Recover | NotRecover

let priorHealth =
    distribution [ Healthy, 0.7; Sick, 0.3 ]

let treatmentPolicy health =
    match health with
    | Healthy -> distribution [ Treat, 0.2; NoTreat, 0.8 ]
    | Sick -> distribution [ Treat, 0.8; NoTreat, 0.2 ]

let recoveryModel health treatment =
    match health, treatment with
    | Healthy, Treat -> distribution [ Recover, 0.98; NotRecover, 0.02 ]
    | Healthy, NoTreat -> distribution [ Recover, 0.95; NotRecover, 0.05 ]
    | Sick, Treat -> distribution [ Recover, 0.70; NotRecover, 0.30 ]
    | Sick, NoTreat -> distribution [ Recover, 0.35; NotRecover, 0.65 ]

let observedTreatmentPosterior =
    dist {
        let! health = priorHealth
        let! treatment = treatmentPolicy health
        do! observe (treatment = Treat)
        let! outcome = recoveryModel health treatment
        return outcome
    }
    |> Model.ExactInfer

let intervenedTreatmentPosterior =
    dist {
        let! health = priorHealth
        let treatment = Treat
        let! outcome = recoveryModel health treatment
        return outcome
    }
    |> Model.ExactInfer
```

The first model conditions on observing treatment; the second intervenes by replacing the treatment-assignment mechanism with a fixed value. That distinction — seeing `Treat` versus doing `Treat` — is the causal one.

### 5. A simple counterfactual pattern

Counterfactuals follow the standard abduction-intervention-prediction pattern: infer a posterior over the latent state from factual evidence, then rerun downstream mechanisms under a different intervention.

```fsharp
open Hansei
open Hansei.Probability

let inferredHealthFromFactual =
    dist {
        let! health = priorHealth
        let factualTreatment = NoTreat
        let! factualOutcome = recoveryModel health factualTreatment
        do! observe (factualOutcome = NotRecover)
        return health
    }
    |> Model.ExactInfer

let counterfactualOutcomeIfTreated =
    dist {
        let! health = inferredHealthFromFactual
        let treatment = Treat
        let! outcome = recoveryModel health treatment
        return outcome
    }
    |> Model.ExactInfer
```

The factual statement: "this patient was not treated and did not recover." The counterfactual question: "what would have happened under treatment?" The latent `health` variable ties the factual and counterfactual worlds together.

### 6. Causal reasoning with cycles

Feedback loops are best represented in Hansei by unrolling over time. The graph is cyclic at the system level, but each step of the generative program remains acyclic and executable.

```fsharp
open Hansei
open Hansei.Probability

type Stress = Calm | Stressed
type Sleep = GoodSleep | BadSleep

let sleepModel stress usedSleepAid =
    match stress, usedSleepAid with
    | Calm, false -> distribution [ GoodSleep, 0.85; BadSleep, 0.15 ]
    | Calm, true -> distribution [ GoodSleep, 0.95; BadSleep, 0.05 ]
    | Stressed, false -> distribution [ GoodSleep, 0.30; BadSleep, 0.70 ]
    | Stressed, true -> distribution [ GoodSleep, 0.65; BadSleep, 0.35 ]

let nextStressModel stress sleep =
    match stress, sleep with
    | Calm, GoodSleep -> distribution [ Calm, 0.90; Stressed, 0.10 ]
    | Calm, BadSleep -> distribution [ Calm, 0.45; Stressed, 0.55 ]
    | Stressed, GoodSleep -> distribution [ Calm, 0.60; Stressed, 0.40 ]
    | Stressed, BadSleep -> distribution [ Calm, 0.10; Stressed, 0.90 ]

let rec stressSleepLoop day maxDay stress =
    dist {
        if day > maxDay then
            return stress
        else
            let useSleepAid = false
            let! sleep = sleepModel stress useSleepAid
            let! nextStress = nextStressModel stress sleep
            return! stressSleepLoop (day + 1) maxDay nextStress
    }

let rec stressSleepLoopWithIntervention day maxDay stress =
    dist {
        if day > maxDay then
            return stress
        else
            let useSleepAid = (day = 1)
            let! sleep = sleepModel stress useSleepAid
            let! nextStress = nextStressModel stress sleep
            return! stressSleepLoopWithIntervention (day + 1) maxDay nextStress
    }

let baselineAfterThreeDays =
    dist {
        let! initialStress = distribution [ Calm, 0.6; Stressed, 0.4 ]
        return! stressSleepLoop 1 3 initialStress
    }
    |> Model.ExactInfer

let interventionAfterThreeDays =
    dist {
        let! initialStress = distribution [ Calm, 0.6; Stressed, 0.4 ]
        return! stressSleepLoopWithIntervention 1 3 initialStress
    }
    |> Model.ExactInfer
```

Stress affects sleep, and sleep feeds back into later stress. The intervention changes one link in that loop on day 1 and lets the consequences propagate through subsequent days.

### 7. The generic semiring reading

From `genericprobtest.fsx`, the same shape reappears:

```fsharp
let inline dot (a: GenericProbabilitySpace<int,'W>) (b: GenericProbabilitySpace<int,'W>) : 'W =
    dist {
        let! i = a
        let! j = b
        do! observe (i = j)
        return ()
    }
    |> explore None
    |> List.fold (fun acc (_, w) -> acc + w) 'W.Zero
```

A dot product, expressed as weighted branching plus an equality constraint — a concise distillation of the file's central message.

## What This Repository Is About

Hansei explores how far a lazy weighted search-tree representation can be pushed. That sometimes means ordinary probabilistic programming, sometimes fair nondeterministic search, sometimes exact sum-product or max-product inference, sometimes logic programming, sometimes quantum-mechanical amplitude manipulation and interference, sometimes tensor contraction or differentiable weighted computation. In every case the recurring idea is the same: represent branching computations explicitly, defer work when useful, choose a weight algebra and an exploration strategy, and collapse equal outcomes when the time is right.

That thread connects `Hansei.Continuation/*`, `Hansei/*`, and `genericprobtest.fsx`.

## Current Status

The repository reads better as a source archive of ideas and implementations than as a cleanly packaged public library. Approach the files as a sequence of representations and experiments rather than expecting a polished, stable API.
