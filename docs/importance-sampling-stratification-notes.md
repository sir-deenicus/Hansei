# Importance Sampling Stratification Notes

## Status

Root-level importance stratification has been removed from this repo.

The implementation only split the initial frontier into two buckets, `heavy` and `light`, using an `epsilon` threshold. It then ran the existing importance sampler inside each bucket. In practice that did not produce a robust benefit over prepared importance sampling with shallow pre-exploration, and it added extra public API and benchmark complexity.

## Why It Was Removed

The main reasons were:

1. The current implementation was narrow.
   It only stratified the root frontier. It did not stratify deeper decisions, and it did not build a better proposal for buried evidence.

2. It was already layered on top of pre-exploration.
   The public stratified path always shallow-pre-explored each stratum before sampling. That means it was not isolating a different source of benefit from prepared importance sampling; it was mostly reusing the same machinery with an extra root split.

3. The benchmarks did not show a convincing win.
   On the rare-root benchmark, stratification occasionally improved coarse rare-bucket mass error a little, but it usually did not improve total distribution error. On the Oleg-inspired deep-evidence benchmark, it added essentially no benefit. A simple two-bucket benchmark was removed because it was too easy and therefore not informative.

4. It complicated the code and API surface.
   The feature required extra internal helpers, extra public entry points, and extra benchmark/reporting branches, without earning its place in the current codebase.

## What The Existing Results Suggest

The dominant hard case in this codebase is not root-mode coverage. It is buried evidence.

When evidence is checked late, a sampler can spend a long sequence of random choices walking toward a final contradiction or a tiny surviving weight. In that regime:

1. Path sampling wastes budget on dead or near-dead trajectories.
2. Importance sampling helps because it keeps weighted contributions instead of insisting on complete successful paths.
3. Shallow pre-exploration can help a lot when it exposes deterministic structure, collapses forced prefixes, and prepares a better frontier once for many samples.
4. Root stratification helps only if the main failure mode is under-coverage of an early coarse mode, which was not the main failure mode in the useful benchmarks.

## Is There A General Case Where Root Stratification Helps But Pre-Explore Does Not?

Possibly, but it is narrow.

The best candidate would look like this:

1. There is a very wide early mixture of coarse modes.
2. One low-mass root bucket matters disproportionately for the posterior.
3. The shallow pre-explore budget is too small to reveal that structure reliably.
4. Once inside each bucket, the remaining inference problem is not especially hard.

That is a real pattern in principle, but it is not broad, and the current Hansei implementation did not make a strong case for keeping dedicated support for it.

## Why Prepared Importance Can Be Faster

The occasional speedup from prepared importance is more general than stratification.

It happens when shallow pre-exploration does useful work that can be amortized across many samples, for example:

1. collapsing forced prefixes,
2. pruning dead mass early,
3. normalizing a reduced frontier once rather than re-discovering it per sample,
4. reusing memoized subtree structure.

This is not a theorem that pre-exploration is always faster. It depends on the model.

Prepared importance can be slower when shallow pre-exploration expands lots of branches that later turn out not to matter, or when the frontier is cheap to resample directly and expensive to prepare.

## Why The Old Stratification Speedups Were Incidental

The old root-stratified path was sometimes faster, but that speedup was incidental rather than architectural.

What that means here is:

1. the speedup depended on the particular root layout of a benchmark,
2. it depended on the `epsilon` split point,
3. it depended on how much shallow preparation work happened to become cheaper after splitting the root,
4. it did not come from a broadly better proposal for the real hard part of the model.

In the removed implementation, stratification only split the initial frontier into two buckets and then ran the same prepared importance machinery inside each bucket. That can be faster in some cases because each bucket may have a smaller frontier, slightly cheaper selector work, or slightly better cache behavior. But those are secondary effects of how the root is laid out, not a stable answer to buried evidence.

That is why the speedups were not a strong architectural reason to keep the feature. They were not tied to a general inference principle that consistently improved the hard cases we care about. They were a side-effect of a particular decomposition of the root frontier.

## What Hansei Already Does

Some of the ideas that matter for buried evidence are already present in limited form, and some are not.

### Shallow Lookahead

Yes, in a limited and practical sense.

The current sampler already performs bounded forward lookahead by:

1. shallow-pre-exploring the distribution before sampling,
2. collapsing forced single-branch prefixes,
3. preparing a frontier of remaining live choices,
4. continuing to do a bounded single-branch pre-explore inside sampling.

This is a forward-only structural lookahead. It helps when shallow expansion exposes dead branches or deterministic structure early.

### Local Exact Elimination

Only in a weak sense, not in the full probabilistic-inference sense.

What the current code does exactly is:

1. accumulate exact mass for already-exposed `Value` leaves,
2. collapse deterministic or singleton continuation chains,
3. normalize retained local frontiers before sampling from them.

What it does not do is general local exact elimination of latent variables, such as summing out a hidden variable analytically because its local factor is available. There is no general variable-elimination pass or factor algebra in the current engine.

### Delayed Sampling

No, not in the standard PPL sense.

The current code delays execution of continuations using thunks and memoization, but that is not delayed sampling as used in systems that postpone sampling a variable until observations or conjugate structure allow an exact update or Rao-Blackwellization.

So:

1. execution is delayed,
2. continuation expansion is lazy,
3. but random variables are not represented symbolically and analytically updated later.

That means Hansei currently does not implement delayed sampling in the usual inference-engine sense.

## Oleg's Observation About Bringing Evidence Closer

Oleg's note on buried evidence is correct: if the model must make a long series of choices before a hard test can fail, naive importance sampling can still collapse because it does not discover the constraint early enough.

In general, bringing evidence closer is only partly automatic.

On the modeling side, it is sometimes possible to:

1. reorder independent choices,
2. expose latent summaries earlier,
3. replace repeated simulation with an exact local likelihood,
4. introduce soft or incremental evidence rather than a single late hard check.

On the inference-engine side, more automation is possible, but not in full generality. To do it well, the engine needs some combination of:

1. symbolic reasoning,
2. lightweight constraint propagation,
3. delayed sampling or exact elimination,
4. model-specific proposal construction,
5. lookahead or backward information.

That is fundamentally problem-specific on both sides:

1. The model must expose enough structure for earlier reasoning to be legal.
2. The engine must know how to exploit that structure without changing semantics.

So the short answer is: partly general, but not fully automatic. The more buried and combinatorial the evidence is, the more model-specific the solution usually becomes.

## Factor And Exact Local Likelihood

Hansei now has two small helpers for this pattern:

1. `factor weight`
2. `exact_local_likelihood likelihood localModel`

There is also a convenience wrapper:

1. `exact_local_observe predicate localModel`

These are intentionally small. They do not make the engine symbolic. They give the model a way to expose a local likelihood exactly when the model author already knows it.

### What `factor` Means

`factor weight` multiplies the current execution weight by a finite non-negative likelihood term.

In the current Hansei implementation this is the right primitive shape because the model already carries ordinary linear weights rather than log-weights. In fact, `factor` is just a clearer alias for `soft_observe`.

That is why a plain factor is appropriate here.

### Why Not `log_factor`

Suggesting `log_factor` would only make sense if:

1. the engine internally accumulated weights in log space, or
2. we wanted a second helper that immediately exponentiates back to linear space.

Hansei currently does neither. So for this codebase, a plain `factor` is the minimal correct helper.

The earlier mention of log-space was about a common design in larger inference systems for numerical stability, not about what Hansei currently uses. For Hansei as it stands, `factor` is the right interface.

### What `exact_local_likelihood` Requires

This helper is general as an interface, but it still requires model structure.

It works when the model can isolate a small local submodel whose contribution can be summed exactly. The helper then explores that local model exactly and turns the result into one likelihood factor.

That means the local model should be:

1. finite or otherwise exactly tractable,
2. small enough that exact exploration is cheap,
3. conditionally independent enough to isolate from the rest of the program.

This is model-side factoring rather than automatic inference-engine discovery.

The current implementation computes the local likelihood directly as a scalar total rather than routing through `explore` first. That choice is intentional: for this helper we only need one number, not a reconstructed explored tree. A direct summation avoids building an intermediate `ProbabilitySpace` value and avoids the extra answer/frontier bookkeeping that `explore` performs.

An `explore`-based implementation is a bit more flexible if we later want to inspect or reuse the explored local tree, but it is not the right default for a helper whose only contract is to produce a single likelihood factor.

### Oleg-Style Example

In the original Oleg-inspired example, each gate samples a position and only then checks whether it equals the required coordinate. That means the sampler walks into a local rejection step repeatedly.

With `exact_local_observe`, the model can instead write the position check as an exact local likelihood over the local position model. That removes one buried rejection step per gate without changing the outer warmup structure.

This does not solve buried evidence in full generality. It just gives the model author a small, explicit tool for the cases where an exact local likelihood is available.

## Can Lightweight Constraint Propagation Be Done Generally?

Yes, but only in a restricted sense.

There is no fully general way to take an arbitrary higher-order F# probabilistic program, inspect an arbitrary late predicate, and always push that evidence backward automatically in a cheap and semantics-preserving way.

What can be done generally is lightweight propagation over recognized structure.

Typical examples are:

1. finite-domain equalities and inequalities,
2. simple deterministic transformations,
3. conjunctions of small local predicates,
4. local support filtering for discrete choices.

The general recipe is:

1. represent some choices with explicit domains or support sets,
2. record simple constraints as evidence arrives,
3. propagate those constraints through recognized deterministic relations,
4. prune any branch whose domain becomes empty,
5. optionally reweight or renormalize the surviving support.

For example, if later evidence says a pair `(x, y)` must equal `(3, 5)`, then a system with explicit domains could immediately reduce `x` to `{3}` and `y` to `{5}` instead of waiting to sample many incompatible values first. Likewise, if evidence says `x + y = 4` and both are small finite discrete variables, the system can prune unsupported values before full rollout.

That is general over a restricted language of constraints. It is not general over arbitrary user code.

In practice, to make it work well an engine usually needs one of these:

1. a restricted DSL for constrained variables,
2. symbolic representations for selected primitives,
3. an effect or factor layer that exposes local likelihoods and deterministic relations,
4. specialized propagators for common distributions and observations.

So the real answer is:

1. generally possible for recognized finite or symbolic structure,
2. not generally possible for arbitrary opaque computations,
3. most effective when the model is written in a way that exposes the structure the engine knows how to propagate.

## Remaining Benchmarks

The benchmark script keeps the cases that are still informative:

1. hard evidence, where prepared importance clearly beats path sampling,
2. a rare-root limitation case, which shows why root-level stratification was not enough,
3. the Oleg-inspired deep-evidence case, which shows the real failure mode more clearly.