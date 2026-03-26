# Hansei

Hansei is an F# codebase about representing branching computations as lazy weighted search trees, and then interpreting those trees in different ways.

At one level, that gives you probabilistic programs with exact inference, conditioning, and approximate search. At another, it gives you fair nondeterministic search, logic-programming style relations, max-product style reasoning, semiring-based dynamic programming, and even tensor or differentiable interpretations. The unifying question running through the repository is simple: once a computation has been reified as a weighted tree of alternatives, what can you do with it?

The repository is easiest to understand as three layers:

1. `Hansei.Continuation/*`
   This is the substrate: continuations, lazy streams, fair backtracking, and search utilities.
2. `Hansei/*`
   This builds a weighted search tree language on top of that substrate. A probabilistic model is represented as a lazy tree of choices, values, and suspended subtrees.
3. `Hansei.Quantum/genericprobtest.fsx`
   Despite the folder name, this file is not "a product called Hansei.Quantum". It is a large idea notebook showing that the same weighted-tree machinery can be interpreted over many semirings, not just ordinary probabilities.

This is best approached as a research codebase rather than a polished package. The value of the repository is in the ideas and representations it works through: continuations, fair search, weighted branching, exact and approximate inference, algebraic interpretations, and specialized submodels.

## The Core Idea

At the center of the codebase is this idea:

- A computation can branch into alternatives.
- Alternatives can carry weights.
- Exploration can be exact, approximate, shallow, greedy, random, or structured.
- The same branching structure can be interpreted as probability, logic, counting, max-product, tropical algebra, or even differentiable / symbolic computation depending on the weight type.

In the main Hansei layer this appears as:

- `ProbabilitySpace<'T>`
- `WeightedTree<'T> = Value of 'T | ContinuedSubTree of ...`
- the `dist { ... }` computation expression
- `observe`, `factor`, `exact_local_likelihood`
- exact exploration and multiple approximate inference procedures

So a Hansei model is not best thought of as "a sampler". It is first a lazy weighted search tree. Exact inference, rejection, importance sampling, beam methods, and specialized exact submodels are then different ways of traversing or collapsing that tree.

## `Hansei.Continuation/*`

The continuation project contains the machinery that makes the later Hansei layers possible.

### Continuations

`Continuations.fs` defines ordinary and delimited continuations.

Why this matters here:

- continuations give explicit control over the rest of a computation,
- probabilistic and nondeterministic branching can be reified instead of immediately executed,
- suspension and re-entry become natural operations.

This is the conceptual floor under the later style where a computation is treated as an explicit tree of suspended futures rather than something that simply runs to completion once.

### Lazy and fair nondeterminism

`Backtracking.fs` and `LazyList.fs` are about search spaces that may be large or infinite.

The important distinction in the code is:

- plain lists are exhaustive but eager,
- lazy lists are deferred but can still bias toward depth-first behavior,
- fair streams interleave branches so that one infinite branch does not starve the others.

That is why `Backtracking.fs` spends so much effort on `Choice`, `Thunk`, interleaving, and stack-safe bind variants. This is not incidental implementation detail. It determines what kinds of infinite or highly branching spaces can still be searched faithfully.

### Search as an interface

`TreeSearch.fs` presents several search views:

- exhaustive list search,
- sequence-based search,
- fair backtracking search,
- lazy-list-based search.

This matters because the same high-level logical structure can be given different operational behavior depending on the underlying search container.

## `Hansei/*`

This is the weighted probabilistic layer built on top of that substrate.

### `ProbabilitySpace<'T>`

`ProbabilitySpace.fs` defines the main representation:

- a value is not just returned, it sits in a weighted tree,
- a subtree can be suspended and forced later,
- `dist { ... }` composes weighted choices lazily.

The key constructors and ideas are:

- `distribution` for weighted alternatives,
- `always` / `exactly` for deterministic values,
- `fail` for impossible branches,
- `observe` for hard constraints,
- `factor` for soft weighting,
- `exact_local_likelihood` for analytically collapsing a local submodel into a single factor.

That last operation is especially important. It says: if a local latent part can be integrated exactly, do not expand it naively into the outer tree. Collapse it into a local likelihood factor instead. A lot of the more interesting work in this repository moves in exactly that direction.

### Exact exploration

`Exploration.fs` provides exact traversal with depth control.

The exact explorer:

- walks the weighted tree,
- accumulates weights of equal terminal values,
- can stop descending after a chosen depth,
- returns both discovered answers and suspended residual computation.

This is a strong clue about the overall design. The tree is primary; inference is a controlled projection of that tree into a distribution over answers.

### Approximate inference

`Hansei.fs`, `IncrementalSamplers.fs`, and `StochasticBeam.fs` add approximate procedures over the same tree structure.

The notable families are:

- path sampling,
- greedy path selection,
- importance sampling with pre-exploration,
- ordinary beam search,
- stochastic beam search,
- incremental sampler states that can be advanced in chunks and checkpointed.

The important design choice is that these are not separate model languages. They are different traversals of the same underlying representation.

### Distribution helpers

`Distributions.fs` contains simple model-building helpers such as:

- Bernoulli and categorical choices,
- uniform distributions,
- recursive examples like geometric and Polya / Dirichlet-style urn processes.

Those examples matter because they show the intended style clearly: models are ordinary recursive F# code inside `dist { ... }`, not a separate external DSL.

### Structured exact subproblems

Some of the most interesting work in `Hansei/*` is about identifying subproblems that should not be handled by naive recursive expansion.

Examples in this repository:

- finite DAG inference in `DynamicProgrammingDag.fs`,
- trie-based segmentation in `TrieSegmentation.fs`,
- local exact likelihood collapsing in `ProbabilitySpace.fs`.

This points to a recurring theme in the codebase:

- write a generic probabilistic outer model,
- detect inner regions with exploitable structure,
- solve those regions exactly or more efficiently,
- feed the result back into the larger model.

That is a much richer story than "this is a sampler library".

## `genericprobtest.fsx`

`Hansei.Quantum/genericprobtest.fsx` is the clearest statement of the broader ambition of the repository.

The file argues, in code, that the central object is not specifically a probability monad. It is a generic weighted branching computation over a semiring.

That script defines a generic version of the Hansei machinery:

- `GenericProbabilitySpace<'T,'W>`
- semiring-constrained weights,
- generic `distribution`, `observe`, `explore`, and builders,
- multiple interpretations based on the algebra of `'W`.

The script is valuable because it makes the abstraction explicit:

- sequencing corresponds to multiplication,
- branching / alternatives correspond to addition,
- `observe` prunes branches by returning zero weight,
- `explore` collapses equal outcomes by adding their weights.

From there the file explores several interpretations of the same structure.

### Ordinary probabilities

With `float` weights, the generic machinery behaves like a probabilistic program. Alternatives add probabilities, sequential choices multiply probabilities, and normalization gives a posterior distribution.

### Logic programming

With a logical semiring, the same computation becomes a logic program. The file includes relational examples such as parent, sibling, cousin, unification, reversible list relations, and Prolog-like goals.

This is one of the strongest ideas in the repository:

- the same branching language can behave like probability,
- or like logic,
- depending on the weight algebra.

### Max-product / shortest-path style readings

Once the weights are not read as probabilities but as semiring elements, the same program can act like:

- Viterbi-style max-product search,
- tropical / shortest-path aggregation,
- counting,
- provenance tracking.

The comments in that script are very direct about this. Multiple `return` lines are additive alternatives, not imperative early exits.

### Tensor and linear algebra interpretations

The script also uses the same machinery to express:

- dot products,
- matrix-vector multiplication,
- matrix-matrix multiplication,
- tensor contraction / einsum-style computations.

The conceptual trick is simple and elegant:

- indices are sampled as weighted alternatives,
- equality constraints are imposed with `observe`,
- `explore` performs the summation over matching indices.

So the system is not just "for probability". It can also act as a compact language for semiring-valued sum-product programs.

### Differentiable weights

The file introduces a dual-number weight type and computes derivatives through the weighted computation.

That means the same model structure can support:

- value computation,
- derivative propagation,
- algebraic reuse of the same search and combination operations.

Again, the branching language stays the same. What changes is the algebra of the weights.

## Reading The Repository

If you want to understand the project in the order its ideas build on each other, this path works well:

1. `Hansei.Continuation/Continuations.fs`
2. `Hansei.Continuation/Backtracking.fs`
3. `Hansei.Continuation/TreeSearch.fs`
4. `Hansei/ProbabilitySpace.fs`
5. `Hansei/Exploration.fs`
6. `Hansei/Distributions.fs`
7. `Hansei/Hansei.fs`
8. `Hansei/IncrementalSamplers.fs`
9. `Hansei.Quantum/genericprobtest.fsx`

Read `DynamicProgrammingDag.fs` and `TrieSegmentation.fs` after that if you want to see the "specialized exact submodel inside a larger weighted framework" direction developed more concretely.

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

This builds a weighted tree and then explores it exactly.

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

Here `observe` removes inconsistent branches.

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

This does not prune branches. It reweights them.

### 4. The generic semiring reading

In `genericprobtest.fsx`, the same shape appears again:

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

This is a dot product expressed as weighted branching plus an equality constraint. It captures the file's central message very well.

## What This Repository Is About

The shortest honest summary is that Hansei explores how far one can push a lazy weighted search-tree representation.

Sometimes that means ordinary probabilistic programming.
Sometimes it means fair nondeterministic search.
Sometimes it means exact sum-product or max-product inference.
Sometimes it means logic programming.
Sometimes it means tensor contraction or differentiable weighted computation.

The recurring idea is the same in all cases:

- represent branching computations explicitly,
- delay work when useful,
- choose a weight algebra,
- choose an exploration strategy,
- collapse equal outcomes when the time is right.

That is the thread connecting `Hansei.Continuation/*`, `Hansei/*`, and `genericprobtest.fsx`.

## Current Status

This repository currently reads better as a source archive of ideas and implementations than as a cleanly packaged public library. The best way to approach it is to read the files as a sequence of representations and experiments rather than expect a polished, stable API.
