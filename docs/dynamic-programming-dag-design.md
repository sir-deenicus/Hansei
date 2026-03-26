# Dynamic Programming DAG Experiment For Hansei

Status: New experimental direction.
Date: 2026-03-22

Phase 1 status: started with a minimal HMM-chain DAG prototype in `Hansei/dynamic-programming-dag-prototype.fsx`.

That prototype now has two front ends into the same evaluator:

1. a direct hand-built finite-chain IR path,
2. a minimal opt-in structured subset that compiles into that same IR.

It also now includes a small branching finite DAG example, not only linear chains.

## Goal

Evaluate whether Hansei can speed up inference on specific graph-shaped models by switching from frontier exploration to dynamic programming over an explicitly shared DAG.

The question is not whether every Hansei model can be turned into a DAG automatically. The practical question is narrower:

1. can we identify a useful restricted class of models where shared subproblems are stable and finite,
2. can we exploit that sharing to avoid repeated continuation-tree exploration,
3. can we do so without breaking Hansei's existing semantics or forcing a full redesign of the core system.

## Short Answer

Yes, this can be done for specific model classes.

For a narrow proof of concept, we do not need new public hooks immediately. We can hand-build a small DAG IR for one or two target families such as HMMs, finite-state DBNs, or other acyclic factor graphs and run a specialized dynamic-programming evaluator over that IR.

For a reusable, highly optimized pipeline that starts from ordinary Hansei programs, we probably will need explicit hooks or an opt-in compilation surface. The current continuation-tree interface does not expose enough stable semantic identity to reconstruct an optimized shared DAG reliably after the fact.

So the practical answer is:

1. no new hooks are required for the first experiment,
2. some hooks will likely be required if we want this to become a general or semi-general inference mode.

## Motivation And Use Cases

The point of this module is not to become a smaller copy of Infer.NET, Figaro, or a generic graphical-model toolkit.

The point is to give Hansei an explicit structural inference layer for the classes of models where we already know more than the generic continuation-tree search can exploit.

That matters for four reasons:

1. it keeps the modeling experience Hansei-adjacent instead of forcing a separate graph-toolkit mindset,
2. it allows exact or near-exact specialized inference for recurring finite structured families,
3. it creates a place to attach multiple optimized evaluators to one explicit representation,
4. it is broad enough to support non-toy standalone tasks while still being narrow enough to optimize aggressively.

The intended value is therefore not maximal expressiveness.

The intended value is a better performance and clarity tradeoff on structured finite problems that Hansei users are likely to care about.

Representative use cases for this family include:

1. HMMs and other finite-state sequential latent-variable models,
2. tagging, decoding, and simple finite-state dynamic Bayesian networks,
3. rooted local-sensor trees and finite hierarchical diagnosis models,
4. exact collapsed submodels embedded inside a larger Hansei computation,
5. stand-alone dynamic-programming style problems such as finite-state segmentation and decoding.

So the role of this module is:

1. keep Hansei good at general probabilistic programming,
2. add an explicit compiled-inference path for restricted structured models,
3. avoid broadening so far that we are effectively rebuilding a general graph-probabilistic system.

## Why Dynamic Programming Can Help

The current stochastic beam driver operates on Hansei's weighted continuation frontiers. That is a good fit when:

1. evidence is hard or buried,
2. the live frontier must be bounded,
3. we want resumable streaming-friendly search.

But beam search still pays for repeated exploration when many distinct histories lead to the same future subproblem.

Dynamic programming can remove that redundancy when we have:

1. a finite or at least aggressively hashable latent state summary,
2. stable transition structure,
3. shared downstream suffixes,
4. local factors or observations that can be attached to graph nodes or edges.

This is exactly why forward-backward works well for HMMs and why junction-tree style methods work well on suitable finite graphical models.

## Where Hansei Helps And Where It Does Not

Hansei already gives us:

1. exact local branching structure,
2. suspended subtrees,
3. weighted alternatives,
4. a clean external-driver boundary for experiments.

Hansei does not currently give us:

1. canonical node identity for semantically equivalent subproblems,
2. explicit factor nodes,
3. explicit variable scopes and elimination order,
4. a graph IR separate from the continuation tree,
5. a generic notion of finite state summary that the runtime can rely on.

That means a post hoc DAG recovery pass over arbitrary continuations is unlikely to be robust. Two continuations that are semantically equivalent will usually not look identical by pointer identity or local tree shape alone.

## Feasible First Experiment

The first experiment should be intentionally narrow.

Recommended target:

1. hidden Markov models or small finite-state dynamic Bayesian networks,
2. finite discrete hidden state,
3. local observation likelihoods,
4. fixed left-to-right topology.

Recommended method:

1. define a small DAG or chain IR outside the current Hansei continuation runtime,
2. compile or hand-encode one benchmark family into that IR,
3. run a specialized dynamic-programming evaluator,
4. compare against exact Hansei on small instances and against beam or importance on larger instances.

This gives us a clean answer to the real question: how much speedup comes from graph sharing alone, before we spend effort on making the interface generic.

## Do We Need Hooks?

### For The First Proof Of Concept

No, not necessarily.

We can do the first experiment with no core runtime changes by introducing a separate experimental module that defines something like:

```fsharp
type DagNodeId = int

type DagVar<'State> =
    {
        Id: DagNodeId
        States: 'State[]
    }

type DagFactor<'State> =
    {
        Parents: DagNodeId[]
        Child: DagNodeId
        Score: 'State[] -> float
    }
```

That would be enough to build a hand-authored HMM or finite factor graph and benchmark a specialized evaluator.

### For A Reusable Optimized Inference Mode

Yes, probably.

If we want users to write ordinary Hansei models and then obtain a high-performance DAG backend, we will need some way to expose structure the continuation tree currently hides.

The likely hook categories are:

1. explicit finite random variables with named domains,
2. explicit factor or observe nodes,
3. stable semantic keys for subproblems or state summaries,
4. an opt-in graph builder or compilation DSL,
5. possibly explicit stage or layer boundaries for sequential graphs.

Without these, any automatic DAG extraction would be forced to guess semantic equivalence from runtime artifacts, which is fragile and unlikely to be correct in general.

## Likely Hook Design

If we decide to expose hooks later, they should be explicit and opt-in rather than implicit global magic.

A reasonable direction is a small graph-building layer that lives beside the ordinary Hansei API.

For example:

```fsharp
type FiniteVar<'State> = ...
type FactorGraphBuilder = ...

val finiteVar: name:string -> states:'State[] -> FiniteVar<'State>
val transition: parent:FiniteVar<'A> -> child:FiniteVar<'B> -> score:('A -> 'B -> float) -> unit
val observeLocal: var:FiniteVar<'A> -> score:('A -> float) -> unit
val compileDag: FactorGraphBuilder -> CompiledDag
```

That keeps the structure explicit, which matters for both correctness and optimization.

## Why Not Reuse Continuation Pointer Identity?

Because pointer identity is not semantic identity.

Two subproblems may be semantically equal while being represented by:

1. different closures,
2. different memo cells,
3. different suspended frontiers with the same future distribution,
4. different histories that collapse to the same sufficient state.

A generic optimizer based on closure identity would therefore miss real sharing and may also merge things that should not be merged once local environment differences matter.

## Safety And Semantic Boundaries

The external-driver safety rule still applies.

Any DAG caches, message tables, or mutable work arrays used by the experiment should remain:

1. external to `ProbabilitySpace`,
2. external to suspended subtree thunks,
3. external to resumable beam state,
4. scoped to the evaluator rather than injected into the monad.

If we later add opt-in hooks, they should expose structure, not shared mutable runtime state.

## Expected Benefits

If the graph family really has strong repeated subproblems, dynamic programming should give:

1. much lower repeated work than beam or path sampling,
2. exact answers on finite acyclic models within the supported class,
3. cleaner scaling with sequence length on HMM-like models,
4. a useful reference backend for benchmarking approximate methods.

## Expected Limits

This will not replace beam search as the general next step for Hansei.

The likely limits are:

1. only a restricted model class will fit the DAG backend cleanly,
2. the compilation surface may be more explicit and less automatic than ordinary Hansei modeling,
3. higher-order stochastic control flow will not map naturally to a static DAG,
4. non-finite latent state summaries will still push us back toward search or particle-style methods.

## Recommended Plan

Phase 1:

1. build a minimal experimental DAG IR for finite acyclic graphs,
2. implement one specialized evaluator for HMM-style chains,
3. compare against the current beam benchmark on both the existing short HMM and a larger sequential case.

Current progress:

1. a first explicit chain-DAG prototype now exists for the HMM family,
2. a first opt-in structured subset now also exists and compiles into the same chain IR,
3. the prototype now also covers a small branching weather DAG case,
4. the direct IR path and the opt-in structured subset currently agree exactly on all benchmarked cases,
5. it is intentionally narrow and does not change Hansei core APIs,
6. it provides the initial concrete substrate needed to measure graph-sharing style evaluation before any broader hook design work.

Phase 2:

1. decide whether the speedup is large enough to justify a reusable interface,
2. if yes, design explicit opt-in hooks for finite variables, local factors, and stable state keys,
3. keep the DAG backend separate from the ordinary continuation-tree path until the semantics are clear.

## Closeout Note

The prototype phase is effectively complete. The system now supports unified graph IR (chains and trees), log-space exact/max inference, structural sharing, and a specialized trie-based segmentation engine with prepared contexts and OOV fallback.

Specific performance refinements for the beam-search path remain as optional "Pass 3" candidates, to be pursued only if larger-trie benchmarks justify the additional complexity:

1. **Bounded Partial Selection**: Replace the full `System.Array.Sort` in each beam step with a partial-selection strategy (e.g., Quickselect or a min-priority queue) to find the top $K$ candidates in $O(N)$ instead of $O(N \log N)$.
2. **Frontier Buffer Reuse**: Switch from per-iteration `ResizeArray` cycles to a pair of pre-allocated, reusable array buffers for the current and next frontiers to eliminate allocation churn.
3. **In-place Pruning**: Implement the beam pruning directly on these shared buffers to minimize data movement between steps.

### Syntax Adjustment Plan

The next syntax pass should make the opt-in structured subset feel more like Hansei without pretending it is ordinary unrestricted Hansei.

Goals:

1. keep the surface recognizably close to Hansei's modeling style,
2. preserve an explicit compilation boundary into finite graph IR,
3. avoid hiding graph-structure commitments behind implicit runtime inference.

Near-term design direction:

1. keep `chain { ... }` and `dag { ... }` as explicit opt-in computation expressions,
2. make variable introduction and observation forms shorter and more declarative,
3. use names and combinators that mirror Hansei where possible,
4. reserve direct IR construction for tests and reference comparisons rather than normal modeling.

Concretely, the next pass should explore forms like:

```fsharp
chain {
    let! weather = finite "Weather" [| Sunny; Cloudy; Rainy |] prior
    do! observe weather umbrellaLikelihood true
    let! nextWeather = transition weather weatherTransition
    return! query nextWeather label
}

dag {
    let! weather = finite "Weather" [| Sunny; Cloudy; Rainy |] prior
    let! umbrellaSensor = child weather "UmbrellaSensor" sensorTransition
    let! trafficSensor = child weather "TrafficSensor" sensorTransition
    do! observe umbrellaSensor umbrellaLikelihood true
    do! observe trafficSensor trafficLikelihood true
    return! query weather label
}
```

This would keep the structured subset close to Hansei syntax while still making the graph-building commitment explicit.

The prototype now does the first concrete cleanup step in code:

1. the syntax helpers are ordinary public modules rather than hidden aliases,
2. structured examples can use `open ChainDsl` or `open DagDsl` in scope,
3. DAG children can inherit their parent state space by default, so common branching cases do not have to repeat the domain on every edge.

### Near-Term Stabilization Target

We should not keep proliferating intermediate abstractions for too long.

For the current phase, the target should be:

1. one core explicit graph representation,
2. one core general evaluator for that representation,
3. `chain { ... }` retained only as ergonomic sugar for the linear special case,
4. `dag { ... }` treated as the real general surface.

To avoid losing the advantages of chains during this convergence, chain lowering should preserve shape information rather than erase it. In practice that means:

1. `chain { ... }` lowers into the same graph family as `dag { ... }`,
2. the lowered graph carries enough metadata to say it is still a linear chain,
3. the evaluator can therefore keep a chain fast path instead of forcing all lowered chains through the most generic DAG routine.

That means:

1. we do not need both `chain` and `dag` as independent long-term semantic foundations,
2. we probably do want both as surface syntax, because chains are common and deserve a lightweight notation,
3. but the implementation should converge toward `chain` lowering into the same underlying graph machinery as `dag`.

So the answer for "finalish for now" is:

1. keep both surface forms for ergonomics,
2. stop adding new parallel cores,
3. converge on a single DAG-oriented internal model as soon as the syntax pass settles.

### Consolidation Plan While Keeping Chains Fast

The consolidation target should be explicit and mechanical rather than aspirational.

Planned direction:

1. keep `dag { ... }` as the single general modeling surface,
2. keep `chain { ... }` only as a compact front end for the linear case,
3. lower both into one shared finite-graph family,
4. attach shape metadata so the evaluator can still tell whether a lowered graph is a chain.

Concretely, that means:

1. the builder-facing syntax converges first: both surfaces use the same modeling verbs where possible (`finite`, `observe`, `query`, plus `transition` for chains and `child` for DAGs),
2. the compiled representation converges second: chain models lower into the same graph family as DAG models instead of maintaining an unrelated long-term semantic core,
3. the evaluator remains split by shape, not by surface syntax: lowered chains dispatch to the tight forward array pass, while genuinely branching graphs use the generic DAG evaluator,
4. any future internal cleanup should replace ad hoc flags with an explicit shape classification once the surface syntax settles.

The important constraint is that consolidation must not erase the information that makes chains cheap:

1. linear parent/child order,
2. shared state space per step,
3. single forward sweep with no general graph bookkeeping.

So the intended end state is not “everything runs through the generic DAG routine.”

It is:

1. one graph-oriented internal story,
2. one set of modeling verbs,
3. multiple evaluator paths chosen from preserved structure.

## Supported Model Class Right Now

The current prototype is intentionally narrower than the word "DAG" suggests.

What is supported now:

1. finite discrete latent state spaces,
2. explicit acyclic structure supplied by the model author,
3. chain models with one latent state variable per step and local observations,
4. rooted branching models where every non-root node has exactly one parent,
5. local conditional factors of the form parent-state to child-state, plus local observation likelihoods,
6. arbitrary single-node queries on the current rooted-tree family via two-pass message passing,
7. log-space internal inference for both sum-product and max-product on the currently supported families,
8. compiled evaluation contexts for the currently supported chain and rooted-tree families so static adjacency and weight tables are built once and reused across repeated evaluations,
9. an explicit `BeliefPropagation` engine name for the currently supported families, where it is exact and aliases the same sum-product recurrences used on chains and rooted trees.

What is not supported yet:

1. arbitrary multi-parent Bayesian-network nodes,
2. general factor graphs with factors over several variables,
3. shared-child DAG structure with true reconvergence,
4. arbitrary marginals over subsets of nodes,
5. continuous latent state,
6. unrestricted higher-order Hansei control flow compiled automatically.

So the honest description is: Phase 1 currently supports finite discrete chains and rooted single-parent branching graphs with arbitrary single-node queries, not a fully general DAG language.

## Compared To A Graph DSL

This is already a small embedded graph DSL, but it is deliberately biased toward Hansei-like model code rather than toward a full generic graphical-model toolkit.

Relative to a conventional graph DSL, the current approach is:

1. better aligned with ordinary Hansei modeling style,
2. easier to stage as an opt-in extension rather than a separate system,
3. narrower in semantics and currently less expressive,
4. missing generic factor representation, engine selection, and graph rewrites.

If the goal were to support arbitrary exact and approximate graphical-model algorithms, a more explicit graph DSL would likely become the cleaner long-term core.

If the goal is to keep a tight bridge to Hansei and only optimize a restricted but useful subset, the current embedded structured subset is the better experimental starting point.

## Relation To Exact Likelihood Style Uses

Yes, restricted graphs of this form can play a role similar to exact likelihood or collapsed exact sub-inference.

For supported model classes, the graph backend can provide:

1. exact normalized posteriors,
2. exact unnormalized evidence contributions,
3. exact marginal likelihoods for observed submodels,
4. exact conditional marginals needed by a larger host model.

The current prototype normalizes to posteriors for reporting, but the same evaluator family could also expose the raw partition function or evidence term directly.

That makes this a plausible backend for “solve this finite submodel exactly, then hand the result back” style use cases.

## Multiple Inference Engines On The Same Graph

Yes, that is one of the stronger reasons to converge on an explicit graph representation.

Once the structure is explicit, we can attach different engines to the same compiled graph family, for example:

1. forward dynamic programming for chains,
2. sum-product or belief propagation on trees or polytrees,
3. max-product or Viterbi style decoding,
4. loopy belief propagation as an approximate mode once multi-parent factors exist,
5. importance or ancestral sampling over the same explicit graph.

The current Phase 1 IR is not yet rich enough for all of those engines, but it is pointed in the right architectural direction: one structural representation, several evaluators.

Current code status:

1. the prototype now has an explicit engine layer over the compiled graph family,
2. `SumProduct` is implemented for both finite chains and rooted single-parent branching graphs,
3. `MaxProduct` is also implemented over those same structures,
4. `BeliefPropagation` is now also exposed explicitly for the currently supported families; on chains and rooted trees it is exact and currently aliases `SumProduct`,
5. the inference recurrences now run in log-space internally for numerical stability on longer sequences,
6. rooted-tree queries are no longer restricted to the root; the current prototype now uses a two-pass message scheme for arbitrary single-node queries,
7. rooted-tree structural sharing now uses interned structural subtree keys rather than canonical string signatures,
8. the rooted-tree evaluator now also caches repeated parent-pattern child-contribution and prefix/suffix arrays by subtree key, not only upward messages,
9. both supported families now have compiled evaluation contexts: chains precompute order and flattened log-transition tables, while rooted trees precompute node lookup, adjacency, local fragments, and subtree-key interning,
10. the prototype now also exposes an internal prepared-DAG path for repeated evaluation without rebuilding the compiled context each call,
11. benchmark output now includes lightweight compiled-context diagnostics, including prepared-context use counts for both supported families and rooted-tree cache-hit counts for upward-message and parent-pattern reuse,
12. the old posterior-only helpers are now thin wrappers over that engine dispatch rather than separate semantic cores,
13. a first trie-oriented segmentation prototype now also exists as a separate specialized exact DP over lexicon matches and string positions; it is intentionally not yet folded into the generic `FiniteDag` family because that segmentation surface naturally wants reconvergent position-sharing the current generic IR does not yet model,
14. the trie prototype now reports both top-k posterior segmentations and top-k MAP paths, exposes an internal Hansei-facing exact adapter that returns a `ProbabilitySpace<string>` from lexicon entries plus an observed string, and includes a beam-style approximate inference mode for larger lexicon-first segmentation problems,
15. the larger trie benchmark now also exercises explicit word-transition / boundary costs inside the specialized trie engine.
14. the trie prototype now prints explicit top-k segmentation output and includes both a small toy example and a larger ambiguous benchmark with a bigger lexicon and longer observed string.

So Bayesian posterior inference is already happening in both surface forms today:

1. `chain { ... }` compiles to a finite chain and runs exact sum-product,
2. `dag { ... }` compiles to the current rooted-tree graph family and runs exact sum-product,
3. both can now also run exact belief propagation under the explicit `BeliefPropagation` engine name, which is equivalent to `SumProduct` on the currently supported structures,
4. both can now also run max-product / MAP style decoding on the same compiled structure.

Important boundary:

1. current `BeliefPropagation` is exact only because the supported graph families are chains and rooted trees,
2. loopy belief propagation is not implemented yet,
3. implementing loopy BP would require a richer graph family with multi-parent factors or true shared-child reconvergence plus an iterative message schedule,
4. for the trie segmentation engine specifically, a beam-style approximation is a better fit than loopy BP because the underlying segmentation graph is acyclic and already admits exact dynamic programming.

Beam approximate inference in the trie prototype:

1. beam search is a standard search and decoding heuristic, especially common in NLP, speech recognition, and other sequence-model settings,
2. it does not change the local scoring model; it changes how much of the search space we keep at once,
3. at each left-to-right expansion step, we keep only the top `beamWidth` partial segmentations by score and drop the rest,
4. this makes it approximate rather than exact, because discarded partial paths can still contain posterior mass,
5. for this trie engine it is a pragmatic approximation because the underlying segmentation graph is acyclic and naturally enumerated left-to-right, so beam pruning is simple to add and easy to interpret.

Near-term planned extensions beyond the currently implemented scope:

1. extend structural sharing beyond the current rooted-tree subtree-key and parent-pattern caches if additional graph families justify it,
2. a specialized finite-state segmentation / decoding IR, with the concrete representation choice between tries and weighted finite automata still to be decided based on which abstraction gives the cleaner modeling and inference story.

## Trie Versus WFA Decision Process

The segmentation / decoding IR should not start from abstract elegance. It should start from the kind of model we want to write and the kind of sharing we want the backend to exploit.

Use a trie-oriented IR when:

1. the main object is a lexicon or prefix dictionary,
2. states are naturally described as string position plus prefix node,
3. weights are mostly local to token completion, token boundaries, or small context features,
4. interpretability and straightforward Hansei-adjacent modeling matter more than classical automata closure properties.

Use a weighted finite automaton when:

1. the model is already naturally an automaton or transducer,
2. we need composition, minimization, epsilon-style structure, or other automata-theoretic operations,
3. states are better viewed as machine states than lexicon-prefix states,
4. we expect to interoperate with existing weighted-automata style data or algorithms.

Practical decision rule:

1. if the first serious target is word segmentation from a lexicon with boundary costs or small language-model features, start with a trie,
2. if the first serious target already looks like weighted-path decoding over an automaton, start with a WFA,
3. if both look plausible, prefer the representation with the simpler explicit state summary and the clearer message-passing story inside the current finite-DAG backend.

Current recommendation:

1. begin with the lexicon-first framing and therefore prototype the segmentation IR as trie-oriented unless an immediate automata use case forces WFA first,
2. preserve the design so trie states could later be lowered into a more automata-like backend if that becomes useful.

How tries fit Hansei usage:

1. tries are useful when a Hansei model contains a finite lexicon-first subproblem such as exact segmentation or decoding over an observed string,
2. the trie segmenter can then be called as an exact submodel adapter rather than forcing the full Hansei continuation tree to rediscover lexicon structure implicitly,
3. boundary costs in this setting mean extra local weights at token boundaries, for example preferring fewer or more plausible word breaks or adding a simple language-model-style penalty or reward when one token ends and the next begins,
4. word-transition costs are one concrete boundary-cost form: the score for choosing the next token can depend on the previous token, which turns the trie DP state into position plus the previous chosen word context,
5. the current prototype now exposes composable boundary-cost templates, including a flat per-break factor and a simple bigram word model, so examples can express both a generic break penalty and specific token-to-token preferences without baking one monolithic scorer into the DP,
6. out-of-vocabulary handling is now available as an opt-in fallback inside the same trie DP: when no lexicon token starts at the current position, the engine can emit a bounded unknown chunk instead of failing immediately,
7. the current balanced default for that fallback is a bounded unknown chunk scored by a unigram character model plus a token-level penalty; this is more robust than a character bigram default when we do not have much character-level training signal,
8. a character bigram unknown scorer is also available, but it is deliberately optional because it introduces more parameters and more sparsity pressure than the unigram version,
9. an end-of-unknown score is now also supported and is useful when we want the model to care about how an unknown chunk terminates, for example preferring unknown spans that end in a letter over spans that end in a digit or punctuation mark,
10. character-class defaults are now also supported for the unknown scorer, which lets us assign different fallback preferences to letters, digits, punctuation, whitespace, and other symbols without having to specify a separate weight for every possible character.

How tries fit Hansei-adjacent modeling:

1. the trie acts as an explicit finite state summary for lexicon-driven segmentation, which is exactly the kind of structure the generic continuation tree does not expose directly,
2. the outer Hansei story can treat the trie segmenter as a specialized exact submodel over a finite observed string and a finite lexicon,
3. this keeps the semantics explicit: we are not claiming arbitrary Hansei programs become tries automatically, only that certain lexicon-first subproblems can be compiled into a trie-oriented DP cleanly.

## Adapter Back Into Hansei Proper

Yes, but we need to separate the exact and approximate cases carefully.

Plausible adapter modes:

1. exact collapsed submodel adapter:
    compile a supported finite submodel, run the graph backend exactly, then feed the resulting exact marginal or evidence back into a larger Hansei computation,
2. proposal adapter:
    sample from a graph-based proposal distribution and reweight in Hansei using importance correction,
3. heuristic approximation adapter:
    sample from the graph backend without full correction, but treat that explicitly as an approximate inference mode rather than ordinary exact Hansei semantics.

The first two are semantically defensible.

The third can still be useful, especially as a cheap approximate nested primitive inside beam or other approximate drivers, but it must stay explicitly opt-in because it changes the inference contract.

So the right long-term shape is probably:

1. a graph backend that can return exact messages, marginals, evidence, or samples,
2. a small adapter layer that exposes those results to Hansei,
3. explicit separation between exact collapsed use and approximate proposal-style use.

## Current Recommendation

Proceed with the experiment.

Do not start by changing Hansei core APIs.

Instead:

1. prove the value of a DAG evaluator on one or two known graph families,
2. measure the speedup relative to beam and importance,
3. add structural sharing only where semantic keys are explicit and stable,
4. prototype a segmentation / decoding IR as a concrete non-toy stand-alone use case,
5. only then decide which broader structural hooks are worth exposing.

That keeps the risk low and prevents us from adding generic hooks before we know exactly which information the optimizer truly needs.