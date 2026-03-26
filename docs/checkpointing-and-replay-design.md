# Checkpointing And Replay Design For Hansei

Status: Proposed near-term direction.
Date: 2026-03-26

## Goal

Define a practical checkpointing direction for Hansei that preserves the current modeling surface and current closure-based execution model.

The design should:

1. preserve the current host-language-centric modeling style,
2. avoid a full IR rewrite in the near term,
3. support same-process checkpointing first,
4. aim to support arbitrary samplers over time,
5. leave room for replay-based resume later,
6. treat explicit IR as an optional at-rest mechanism rather than the primary live execution form.

## Current Decision

Hansei should not move to an IR-native execution model now.

That would give up too much of what is currently valuable:

1. direct use of the host language for modeling,
2. closure-based suspension,
3. easy expression of recursive and causal models,
4. the current ergonomics for experimentation and tooling,
5. the ability to borrow F# control flow and abstraction directly.

The near-term direction should instead be:

1. keep the current live execution model,
2. add same-process checkpointing for sampler state,
3. treat replay as the later bridge toward more durable resume,
4. treat IR as an optional at-rest artifact rather than the main runtime representation.

## Terminology

### Samplers Are Execution Engines

In this design discussion, a sampler is an execution engine.

That includes examples such as:

1. path sampling,
2. importance sampling,
3. prepared importance sampling,
4. stochastic beam search,
5. any later SMC-like or hybrid inference driver.

The model produces a `ProbabilitySpace<'T>` representation. The sampler or inference engine decides how to traverse, expand, prune, weight, and accumulate that representation.

So the right abstraction boundary for checkpointing is usually the execution engine state rather than the raw `ProbabilitySpace<'T>` itself.

### Same-Process Checkpointing

Same-process checkpointing means:

1. an engine can expose its current state explicitly,
2. that state can be stored by host code and resumed later,
3. the process remains alive,
4. the stored state may still contain ordinary Hansei frontiers, memo thunks, and host closures,
5. no claim is made that the checkpoint can survive serialization or process restart.

This is the right first step because it works with the current representation.

### Replay

Replay means:

1. rebuild the model,
2. restore engine configuration and RNG state,
3. optionally restore accumulated answers and progress,
4. rerun the engine deterministically until the desired checkpoint boundary,
5. continue from there.

Replay is the practical intermediate step between same-process checkpointing and durable serialization.

### At-Rest IR

At-rest IR means an explicit graph, trace, or machine-state artifact that is produced for persistence, analysis, tooling, or future resume support.

It does not mean that all live execution must happen through that IR.

This distinction matters. Hansei can keep its current closure-based live representation while still allowing some explicit artifact to be produced alongside or after execution.

## Why Not A Live IR Now

The current core representation uses suspended thunks and memoized continuations. That is what makes the current system flexible and expressive.

A live IR-first architecture would require explicit machine states, explicit environments, explicit continuation forms, and likely a more restricted serializable subset of model state.

That would be a major effort, and it would likely reduce the ease with which Hansei currently reuses ordinary F# structure for:

1. recursion,
2. closures,
3. higher-order combinators,
4. direct host-language control flow,
5. quick modeling iteration.

That cost is not justified for the current stage of the project.

## Why Same-Process Checkpointing Fits The Current Core

Same-process checkpointing works because the engine state can keep references to ordinary Hansei frontiers and suspended subtrees.

That means an engine can pause and resume as long as:

1. its state remains external to the model,
2. the process remains alive,
3. the checkpoint is treated as an in-memory snapshot rather than a serializable artifact,
4. mutable driver internals do not leak into model closures.

The stochastic beam prototype already demonstrates this pattern. Its resumable state is valuable even though it is not yet safely serializable across process boundaries.

## What "Rebuild Model" Means

"Rebuild model" does not mean reconstructing arbitrary closures from serialized data.

It means reconstructing the root probabilistic program by calling the same model-building code again from explicit inputs.

Concretely, that means:

1. the host program stores a model descriptor,
2. that descriptor contains the model identity and its input parameters,
3. resume logic calls the same F# function again with the same inputs,
4. that function creates a fresh `ProbabilitySpace<'T>` from scratch,
5. replay then reruns the execution engine against that rebuilt model.

A simple example is:

```fsharp
type HiddenMarkovArgs =
    {
        Observations: bool[]
        TransitionP: float
        EmissionPWhenHot: float
        EmissionPWhenCold: float
    }

let buildHiddenMarkovModel args : ProbabilitySpace<string> =
    // ordinary Hansei model-building code
    ...
```

In that case, "rebuild model" means:

1. persist `HiddenMarkovArgs`,
2. call `buildHiddenMarkovModel args` again,
3. resume the engine by replaying from that root model.

This is different from serializing suspended continuations directly.

## General Direction For Arbitrary Samplers

If the goal is to support arbitrary samplers, the right common abstraction is not a serialized tree. It is an explicit execution-engine state machine.

Conceptually, every sampler should be able to expose:

1. initialization from a model,
2. advancement by one logical step or one batch,
3. a current estimate or posterior snapshot,
4. a same-process checkpoint object,
5. optional later replay metadata.

Conceptually:

```fsharp
type SamplerStepResult<'State, 'Estimate> =
    {
        State: 'State
        Estimate: 'Estimate
        IsComplete: bool
    }

type ISamplerEngine<'Model, 'State, 'Estimate> =
    abstract member Initialize: 'Model -> 'State
    abstract member Advance: 'State -> SamplerStepResult<'State, 'Estimate>
    abstract member Snapshot: 'State -> 'Estimate
```

The exact public API need not take this shape, but the semantic shape should be close to it.

The important point is that a checkpointable engine is an explicit state machine.

## Requirements For Same-Process Checkpointable Engines

Any engine that wants same-process checkpointing should satisfy these requirements.

### 1. Engine State Must Be External

Engine state must not be captured into model closures or stored inside `ProbabilitySpace<'T>` nodes.

It must live outside the probabilistic program and only point into it.

### 2. Snapshots Must Be Persistent-Safe In Process

A saved engine state should behave like an immutable snapshot even if the implementation uses ephemeral mutable buffers internally during each advance step.

That means:

1. old snapshots remain valid after advancing a newer snapshot,
2. mutable driver internals are not shared across snapshots in a way that mutates past states,
3. RNG state is explicit rather than hidden in mutable ambient objects.

### 3. Advancement Boundaries Must Be Explicit

Each engine should define what one resumable step means.

Examples:

1. one beam round,
2. one completed sample,
3. one batch of completed samples,
4. one particle stage,
5. one exact exploration layer.

The checkpoint should land on those boundaries rather than inside opaque internal loops.

### 4. Snapshot Must Not Promise Serialization

The initial checkpoint object may still contain live Hansei frontiers and memoized subtrees.

That is acceptable for same-process checkpointing.

The only promise is resumability while the process stays alive.

## Implications For Infinite Computation

Same-process checkpointing is compatible with infinite or open-ended computation as long as the engine is resumable and productive.

That means:

1. each advance step does finite work,
2. the engine can return a new saved state after that finite work,
3. the caller controls whether to continue,
4. the engine can expose a current approximation even if it will never fully terminate.

This is a good match for Hansei's current live representation because suspension already exists. The checkpointing layer only needs to externalize engine progress.

## At-Rest IR As A Future Optional Artifact

IR should be treated as a future optional artifact, not as the primary live representation.

The most useful role for such an artifact would be:

1. a partial explored graph,
2. a replay transcript,
3. a tooling/debugging view,
4. a persistent cache of explored structure,
5. a future bridge toward more durable resume.

That suggests a future direction where engines can optionally emit metadata such as:

1. model identity,
2. model parameters,
3. code version or fingerprint,
4. explicit RNG state,
5. frontier descriptors,
6. branch-choice transcripts,
7. local explored-graph fragments.

That metadata would support replay or offline tooling without forcing the live runtime itself to become IR-native.

## Replay As The Intermediate Mechanism

Replay is the practical bridge between same-process checkpoints and durable resume.

The idea is:

1. keep the current live model representation,
2. store explicit engine-level progress and inputs,
3. rebuild the root model from explicit parameters,
4. rerun deterministically to a checkpoint boundary,
5. continue from there.

Replay is easier than full serialization because it does not attempt to serialize arbitrary suspended closures.

Replay is harder than same-process checkpointing because it depends on determinism.

The required replay contract is:

1. deterministic model reconstruction,
2. explicit RNG state,
3. stable engine semantics,
4. stable frontier enumeration order,
5. versioned model descriptors,
6. clearly defined checkpoint boundaries.

## What This Means For Importance Sampling

Replay should be approached conservatively for importance sampling.

The easiest checkpoint boundary is between completed samples rather than inside a partially explored sample.

That suggests a staged approach:

1. first support same-process checkpoints for any live importance engine state we make explicit,
2. later add replay support at completed-sample boundaries,
3. only later consider replay inside a partially completed sample if profiling proves it is necessary.

This avoids the most fragile part of replay, which is reproducing the exact internal forcing and frontier-preparation path inside one in-flight sample.

## Recommended Near-Term Architecture

The near-term architecture should be:

1. models remain ordinary Hansei code,
2. samplers are explicit resumable execution engines,
3. checkpoints are same-process only,
4. replay metadata is optional and added later,
5. IR remains deferred and optional.

This is the best trade between preserving current strengths and adding useful checkpoint support.

## Concrete Next Steps

### Step 1: Define A Common Engine Vocabulary

Add a small internal design vocabulary for:

1. initialize,
2. advance,
3. snapshot current estimate,
4. detect completion,
5. same-process checkpoint state.

This does not require a public interface immediately, but the implementation style should converge on this shape.

### Step 2: Treat Stochastic Beam As The First Reference Engine

Use the existing stochastic beam state machine as the reference example for:

1. explicit external state,
2. immutable-in-practice snapshots,
3. explicit RNG state,
4. resumable rounds,
5. snapshotable posterior approximation.

### Step 3: Add The Same Shape To Importance Sampling

Refactor the importance sampler so that it can expose:

1. engine configuration,
2. explicit progress counters,
3. explicit RNG state where applicable,
4. accumulated answers,
5. a same-process resumable state between completed samples or batches.

The first version does not need fine-grained in-sample checkpoints.

### Step 4: Standardize Checkpoint Safety Rules

Document and enforce these rules for any checkpointable engine:

1. no mutable driver state captured by model closures,
2. no hidden shared mutable RNG object inside saved state,
3. no promise of serialization for same-process checkpoints,
4. clear checkpoint boundaries,
5. old snapshots remain valid after advancing new ones.

### Step 5: Add Optional Replay Metadata Later

Once same-process checkpointing works for at least beam and importance, add optional replay metadata such as:

1. model descriptor,
2. input parameters,
3. code version or manual version tag,
4. RNG state,
5. completed-sample or completed-round counts,
6. later, optional decision transcripts.

### Step 6: Revisit At-Rest IR Only After Replay Needs Are Clear

Only revisit explicit IR work if replay proves too fragile or too expensive for the engines we care about.

Until then, IR should remain a deferred persistence and tooling mechanism rather than a required runtime rewrite.

## Recommendation

The project should commit to same-process checkpointing first.

That means:

1. engines become explicit resumable state machines,
2. checkpoints are in-memory only,
3. arbitrary Hansei modeling remains unchanged,
4. replay is the next persistence step,
5. IR remains optional and deferred.

This keeps the current expressive runtime while still opening a practical path toward later persistence and tooling.