module Hansei.IncrementalSampling
open Hansei.Probability
open Hansei.Exploration
open Hansei.Sampling
open Prelude.Common 
open System
open Hansei.Utils 

type internal SamplerProgress =
    {
        TotalSamples: int
        CompletedSamples: int
    }

type internal ISamplerCheckpointState<'State, 'T> =
    abstract member Progress: SamplerProgress
    abstract member HasSnapshotState: bool
    abstract member SnapshotState: unit -> ProbabilitySpace<'T>
    abstract member AdvanceState: int -> 'State
    abstract member CompleteState: unit -> 'State

module internal SamplerCheckpointState =
    let remainingSamples progress = max 0 (progress.TotalSamples - progress.CompletedSamples)

    let validateTotalSamples totalSamples =
        if totalSamples <= 0 then
            invalidArg (nameof totalSamples) "totalSamples must be positive."

    let validateAdvanceCount sampleCount =
        if sampleCount <= 0 then
            invalidArg (nameof sampleCount) "sampleCount must be positive."

    let completedCountAsFloat progress = float progress.CompletedSamples

    let cloneAnswerDictionary (source: System.Collections.Generic.Dictionary<'T, float>) =
        let clone = System.Collections.Generic.Dictionary<'T, float>(source.Count)

        for KeyValue(value, weight) in source do
            clone.[value] <- weight

        clone

    let snapshotCompletedAnswers progress (completedAnswers: System.Collections.Generic.Dictionary<'T, float>) : ProbabilitySpace<'T> =
        if progress.CompletedSamples <= 0 then
            []
        else
            let completedSampleCount = completedCountAsFloat progress
            [ for KeyValue(value, weight) in completedAnswers -> Value value, weight / completedSampleCount ]

type internal SamplerRngState =
    internal
    | SamplerRngState of uint64

module internal SamplerRng =
    open Prelude.Math
    let createInitialState () =
        let seed = uint64 (max 1 (random.Next()))
        SamplerRngState seed

    let nextUniform (SamplerRngState state) =
        let mutable z = state + 0x9E3779B97F4A7C15UL
        z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
        z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
        let nextState = z ^^^ (z >>> 31)
        let mantissa = nextState >>> 11
        let unitFloat = float mantissa / 9007199254740992.0
        SamplerRngState nextState, unitFloat

let internal random_selector_state dosort rngState choices =
    let rec selection r ptotal pcum =
        function
        | [] -> None
        | (th, p) :: rest ->
            let nextCumulative = pcum + p

            if r < nextCumulative then
                Some(th, ptotal)
            else
                selection r ptotal nextCumulative rest

    let ptotal = List.sumBy snd choices

    if ptotal <= 0.0 then
        None, rngState
    else
        let nextRngState, unitFloat = SamplerRng.nextUniform rngState
        let r = unitFloat * ptotal
        let orderedChoices = if dosort then List.sortBy snd choices else choices
        selection r ptotal 0.0 orderedChoices, nextRngState

let internal random_prepared_frontier_selector_state rngState (frontier: PreparedFrontier<_>) =
    if frontier.Count = 0 then
        None, rngState
    elif frontier.Count = 1 then
        Some(frontier.Choices.[0], frontier.Corrections.[0]), rngState
    else
        let nextRngState, _ = SamplerRng.nextUniform rngState
        let mutable totalCorrection = 0.0
        let mutable selectedIndex = -1
        let mutable workingState = nextRngState

        for i in 0 .. frontier.Count - 1 do
            let correction = frontier.Corrections.[i]

            if correction > 0.0 then
                totalCorrection <- totalCorrection + correction

                let candidateState, unitFloat = SamplerRng.nextUniform workingState
                workingState <- candidateState

                if unitFloat * totalCorrection < correction then
                    selectedIndex <- i

        if selectedIndex < 0 then
            None, workingState
        else
            Some(frontier.Choices.[selectedIndex], totalCorrection), workingState

/// Same-process checkpoint for path sampling.
/// The saved state may still depend on live Hansei frontiers and selector closures.
type PathSamplingState<'T when 'T: equality> internal
    (
        progress: SamplerProgress,
        rngState: SamplerRngState,
        completedAnswers: System.Collections.Generic.Dictionary<'T, float>,
        sampleStep: SamplerRngState -> System.Collections.Generic.Dictionary<'T, float> -> SamplerRngState
    ) =

    member _.TotalSamples = progress.TotalSamples
    member _.CompletedSamples = progress.CompletedSamples
    member _.RemainingSamples = SamplerCheckpointState.remainingSamples progress
    member _.IsComplete = progress.CompletedSamples >= progress.TotalSamples
    member _.HasSnapshot = progress.CompletedSamples > 0

    member _.Snapshot() : ProbabilitySpace<'T> =
        SamplerCheckpointState.snapshotCompletedAnswers progress completedAnswers

    member this.Advance(sampleCount: int) =
        SamplerCheckpointState.validateAdvanceCount sampleCount

        if this.IsComplete then
            this
        else
            let stepCount = min sampleCount this.RemainingSamples
            let nextAnswers = SamplerCheckpointState.cloneAnswerDictionary completedAnswers
            let mutable nextRngState = rngState

            for _ in 1 .. stepCount do
                nextRngState <- sampleStep nextRngState nextAnswers

            PathSamplingState({ progress with CompletedSamples = progress.CompletedSamples + stepCount }, nextRngState, nextAnswers, sampleStep)

    member this.RunToCompletion() =
        if this.IsComplete then this else this.Advance(this.RemainingSamples)

    member this.ToStateSeq(sampleCount: int) : seq<PathSamplingState<'T>> =
        SamplerCheckpointState.validateAdvanceCount sampleCount

        seq {
            let mutable current = this
            yield current

            while not current.IsComplete do
                current <- current.Advance(sampleCount)
                yield current
        }

    member this.ToSnapshotSeq(sampleCount: int) : seq<ProbabilitySpace<'T>> =
        this.ToStateSeq(sampleCount)
        |> Seq.choose (fun state -> if state.HasSnapshot then Some(state.Snapshot()) else None)

    interface ISamplerCheckpointState<PathSamplingState<'T>, 'T> with
        member _.Progress = progress
        member _.HasSnapshotState = progress.CompletedSamples > 0
        member _.SnapshotState() = SamplerCheckpointState.snapshotCompletedAnswers progress completedAnswers
        member this.AdvanceState(sampleCount) = this.Advance(sampleCount)
        member this.CompleteState() = this.RunToCompletion()

let internal createRandomPathSamplingState dosort subsample totalSamples (ch: ProbabilitySpace<'T>) =
    SamplerCheckpointState.validateTotalSamples totalSamples

    let rec runOnePath rngState pcontrib ans choices =
        match collapseForcedPathBounded defaultSingletonUnravelLimit 0 1.0 false choices with
        | Choice1Of2 (v, p) -> addWeightedAnswer pcontrib ans v p, rngState
        | Choice2Of2 ([], _, _) -> ans, rngState
        | Choice2Of2 (sampleChoices, forcedMass, _) ->
            match random_selector_state dosort rngState (subsample sampleChoices) with
            | None, nextRngState -> ans, nextRngState
            | Some (th, ptotal), nextRngState -> runOnePath nextRngState (pcontrib * forcedMass * ptotal) ans [th, 1.0]

    let sampleStep rngState (answers: System.Collections.Generic.Dictionary<'T, float>) =
        let mutable working = Dict<'T, float>()

        for KeyValue(value, weight) in answers do
            working.[value] <- weight

        let updated, nextRngState = runOnePath rngState 1.0 working ch

        answers.Clear()

        for KeyValue(value, weight) in updated do
            answers.[value] <- weight

        nextRngState

    PathSamplingState({ TotalSamples = totalSamples; CompletedSamples = 0 }, SamplerRng.createInitialState (), System.Collections.Generic.Dictionary<'T, float>(), sampleStep)

let internal createGreedyPathSamplingState subsample totalSamples (ch: ProbabilitySpace<'T>) =
    SamplerCheckpointState.validateTotalSamples totalSamples

    let rec runOnePath pcontrib ans choices =
        match collapseForcedPathBounded defaultSingletonUnravelLimit 0 1.0 false choices with
        | Choice1Of2 (v, p) -> addWeightedAnswer pcontrib ans v p
        | Choice2Of2 ([], _, _) -> ans
        | Choice2Of2 (sampleChoices, forcedMass, _) ->
            match max_selector () (subsample sampleChoices) with
            | None -> ans
            | Some (th, ptotal) -> runOnePath (pcontrib * forcedMass * ptotal) ans [th, 1.0]

    let sampleStep rngState (answers: System.Collections.Generic.Dictionary<'T, float>) =
        let mutable working = Dict<'T, float>()

        for KeyValue(value, weight) in answers do
            working.[value] <- weight

        let updated = runOnePath 1.0 working ch
        answers.Clear()

        for KeyValue(value, weight) in updated do
            answers.[value] <- weight

        rngState

    PathSamplingState({ TotalSamples = totalSamples; CompletedSamples = 0 }, SamplerRng.createInitialState (), System.Collections.Generic.Dictionary<'T, float>(), sampleStep)
 
let internal addAnswerWeightToDictionary value weight (answers: System.Collections.Generic.Dictionary<'T, float>) =
    match answers.TryGetValue value with
    | true, existing -> answers.[value] <- existing + weight
    | false, _ -> answers.[value] <- weight

let internal cloneAnswerDictionary (source: System.Collections.Generic.Dictionary<'T, float>) =
    SamplerCheckpointState.cloneAnswerDictionary source

/// Same-process checkpoint for importance sampling.
/// The saved state may still contain live Hansei frontiers and closures.
type ImportanceSamplingState<'T when 'T: equality> internal
    (
        progress: SamplerProgress,
        rngState: SamplerRngState,
        deterministicValues: list<'T * float>,
        completedAnswers: System.Collections.Generic.Dictionary<'T, float>,
        sampleStep: option<SamplerRngState -> System.Collections.Generic.Dictionary<'T, float> -> SamplerRngState>
    ) =

    member _.TotalSamples = progress.TotalSamples
    member _.CompletedSamples = progress.CompletedSamples
    member _.RemainingSamples = SamplerCheckpointState.remainingSamples progress
    member _.IsComplete = sampleStep.IsNone || progress.CompletedSamples >= progress.TotalSamples
    member _.HasSnapshot = sampleStep.IsNone || progress.CompletedSamples > 0

    member _.Snapshot() : ProbabilitySpace<'T> =
        match sampleStep with
        | None -> [ for (value, weight) in deterministicValues -> Value value, weight ]
        | Some _ when progress.CompletedSamples <= 0 -> []
        | Some _ ->
            let combinedAnswers = cloneAnswerDictionary completedAnswers
            let completedSampleCount = SamplerCheckpointState.completedCountAsFloat progress

            for (value, weight) in deterministicValues do
                addAnswerWeightToDictionary value (completedSampleCount * weight) combinedAnswers

            [ for KeyValue(value, weight) in combinedAnswers -> Value value, weight / completedSampleCount ]

    member this.Advance(sampleCount: int) =
        SamplerCheckpointState.validateAdvanceCount sampleCount

        if this.IsComplete then
            this
        else
            let stepCount = min sampleCount this.RemainingSamples
            let nextAnswers = cloneAnswerDictionary completedAnswers
            let mutable nextRngState = rngState

            match sampleStep with
            | None -> ()
            | Some step ->
                for _ in 1 .. stepCount do
                    nextRngState <- step nextRngState nextAnswers

            ImportanceSamplingState({ progress with CompletedSamples = progress.CompletedSamples + stepCount }, nextRngState, deterministicValues, nextAnswers, sampleStep)

    member this.RunToCompletion() =
        if this.IsComplete then this else this.Advance(this.RemainingSamples)

    member this.ToStateSeq(sampleCount: int) : seq<ImportanceSamplingState<'T>> =
        SamplerCheckpointState.validateAdvanceCount sampleCount

        seq {
            let mutable current = this
            yield current

            while not current.IsComplete do
                current <- current.Advance(sampleCount)
                yield current
        }

    member this.ToSnapshotSeq(sampleCount: int) : seq<ProbabilitySpace<'T>> =
        this.ToStateSeq(sampleCount)
        |> Seq.choose (fun state -> if state.HasSnapshot then Some(state.Snapshot()) else None)

    interface ISamplerCheckpointState<ImportanceSamplingState<'T>, 'T> with
        member _.Progress = progress
        member _.HasSnapshotState = sampleStep.IsNone || progress.CompletedSamples > 0
        member this.SnapshotState() = this.Snapshot()
        member this.AdvanceState(sampleCount) = this.Advance(sampleCount)
        member this.CompleteState() = this.RunToCompletion()

let inline internal create_importance_sampling_state_core
    (createFrontier: int -> 'Frontier)
    (addPending: ProbabilitySpace<'T> -> float -> 'Frontier -> unit)
    (frontierCount: 'Frontier -> int)
    (getSingleton: 'Frontier -> ProbabilitySpace<'T> * float)
    (selectFrontier: 'Frontier -> option<ProbabilitySpace<'T> * float>)
    (selectFrontierWithState: option<SamplerRngState -> 'Frontier -> option<ProbabilitySpace<'T> * float> * SamplerRngState>)
    (subsample: ProbabilitySpace<'T> -> ProbabilitySpace<'T>)
    totalSamples
    max_pre_explore_depth
    pre_explore_min_mass
    maxdepth
    (ch: ProbabilitySpace<'T>)
    =
    SamplerCheckpointState.validateTotalSamples totalSamples

    if Double.IsNaN pre_explore_min_mass || pre_explore_min_mass < 0.0 then
        invalidArg (nameof pre_explore_min_mass) "pre_explore_min_mass must be finite and non-negative."

    let sumChoiceMass choices =
        let rec loop total =
            function
            | [] -> total
            | (_, p) :: rest -> loop (total + p) rest

        loop 0.0 choices

    let normalizeChoices ptotal choices =
        let invPtotal = 1.0 / ptotal

        let rec loop acc =
            function
            | [] -> List.rev acc
            | (x, p) :: rest -> loop ((x, p * invPtotal) :: acc) rest

        loop [] choices

    let prepareFrontier pcontrib ans choices =
        let frontier = createFrontier 4

        let rec collect remaining =
            match remaining with
            | [] -> ans, frontier
            | (Value v, p) :: rest ->
                addWeightedAnswer pcontrib ans v p |> ignore
                collect rest
            | (ContinuedSubTree t, p) :: rest ->
                match collapseForcedPathBounded defaultSingletonUnravelLimit 0 p false (force t) with
                | Choice1Of2 (v, p1) -> addWeightedAnswer pcontrib ans v p1 |> ignore
                | Choice2Of2 (_ch, branchCorrection, collapsedPrefix) ->
                    let sampled = subsample _ch

                    match sampled with
                    | [] -> ()
                    | _ ->
                        let retainedMass = sumChoiceMass sampled

                        if retainedMass > 0.0 then
                            match collapseForcedPathBounded defaultSingletonUnravelLimit 0 branchCorrection collapsedPrefix sampled with
                            | Choice1Of2 (v, p1) -> addWeightedAnswer pcontrib ans v p1 |> ignore
                            | Choice2Of2 (pendingChoices, correction, pendingCollapsedPrefix) ->
                                if retainedMass < nearly_one && not pendingCollapsedPrefix then
                                    addPending (normalizeChoices retainedMass pendingChoices) (correction * retainedMass) frontier
                                else if retainedMass < nearly_one && pendingCollapsedPrefix then
                                    addPending pendingChoices correction frontier
                                else
                                    addPending pendingChoices correction frontier

                collect rest

        collect choices

    let selectFrontierStep frontier rngState =
        match selectFrontierWithState with
        | Some select -> select rngState frontier
        | None -> selectFrontier frontier, rngState

    let rec loop depth pcontrib rngState (ans: Dict<_, _>) =
        function
        | [Value v, p] -> addWeightedAnswer pcontrib ans v p, rngState
        | [] -> ans, rngState
        | [ContinuedSubTree th, p] -> loop (depth + 1) (p * pcontrib) rngState ans (force th)
        | frontierChoices when depth < maxdepth ->
            match prepareFrontier pcontrib ans (subsample frontierChoices) with
            | ans, frontier when frontierCount frontier = 0 -> ans, rngState
            | ans, frontier ->
                match selectFrontierStep frontier rngState with
                | None, nextRngState -> ans, nextRngState
                | Some (th, correction), nextRngState -> loop (depth + 1) (pcontrib * correction) nextRngState ans th
        | _ -> ans, rngState

    let runOneSample pcontrib frontier rngState (answers: System.Collections.Generic.Dictionary<'T, float>) =
        let mutable working = Dict<'T, float>()

        for KeyValue(value, weight) in answers do
            working.[value] <- weight

        match selectFrontierStep frontier rngState with
        | None, nextRngState -> nextRngState
        | Some (th, correction), nextRngState ->
            let updated, finalRngState = loop 0 (pcontrib * correction) nextRngState working th
            answers.Clear()

            for KeyValue(value, weight) in updated do
                answers.[value] <- weight

            finalRngState

    let exactState ans =
        ImportanceSamplingState
            (
                { TotalSamples = totalSamples; CompletedSamples = 0 },
                SamplerRng.createInitialState (),
                [ for KeyValue(value, weight) in ans -> value, weight ],
                System.Collections.Generic.Dictionary<'T, float>(),
                None
            )

    let pendingState pcontrib ans frontier =
        ImportanceSamplingState
            (
                { TotalSamples = totalSamples; CompletedSamples = 0 },
                SamplerRng.createInitialState (),
                [ for KeyValue(value, weight) in ans -> value, weight ],
                System.Collections.Generic.Dictionary<'T, float>(),
                Some (runOneSample pcontrib frontier)
            )

    let rec pre_explore depth pcontrib ans current =
        match prepareFrontier pcontrib ans (subsample current) with
        | ans, frontier when frontierCount frontier = 0 -> exactState ans
        | ans, frontier when frontierCount frontier = 1 && depth < max_pre_explore_depth ->
            let nextChoices, correction = getSingleton frontier

            if (pcontrib * correction) >= pre_explore_min_mass then
                pre_explore (depth + 1) (pcontrib * correction) ans nextChoices
            else
                pendingState pcontrib ans frontier
        | ans, frontier -> pendingState pcontrib ans frontier

    pre_explore 0 1.0 (Dict()) ch

let internal create_importance_sampling_state_with_mass_budget subsample totalSamples max_pre_explore_depth pre_explore_min_mass maxdepth (selector: ImportanceSelector<_>) (ch: ProbabilitySpace<_>) =
    let createFrontier _ = ResizeArray<ProbabilitySpace<_> * float>()
    let addPending pendingChoices correction (frontier: ResizeArray<ProbabilitySpace<_> * float>) =
        frontier.Add(pendingChoices, correction)
    let frontierCount (frontier: ResizeArray<ProbabilitySpace<_> * float>) = frontier.Count
    let getSingleton (frontier: ResizeArray<ProbabilitySpace<_> * float>) = frontier.[0]
    let selectFrontier (frontier: ResizeArray<ProbabilitySpace<_> * float>) =
        if frontier.Count = 0 then
            None
        elif frontier.Count = 1 then
            Some frontier.[0]
        else
            selector frontier

    create_importance_sampling_state_core
        createFrontier
        addPending
        frontierCount
        getSingleton
        selectFrontier
        None
        subsample
        totalSamples
        max_pre_explore_depth
        pre_explore_min_mass
        maxdepth
        ch

let internal create_importance_sampling_state_prepared_with_mass_budget subsample totalSamples max_pre_explore_depth pre_explore_min_mass maxdepth (ch: ProbabilitySpace<_>) =
    let frontierCount (frontier: PreparedFrontier<_>) = frontier.Count
    let getSingleton (frontier: PreparedFrontier<_>) = frontier.Choices.[0], frontier.Corrections.[0]
    let selectFrontier (frontier: PreparedFrontier<_>) =
        if frontier.Count = 0 then
            None
        elif frontier.Count = 1 then
            Some(frontier.Choices.[0], frontier.Corrections.[0])
        else
            None

    create_importance_sampling_state_core
        createPreparedFrontier
        addPreparedFrontierChoice
        frontierCount
        getSingleton
        selectFrontier
        (Some (fun rngState frontier -> random_prepared_frontier_selector_state rngState frontier))
        subsample
        totalSamples
        max_pre_explore_depth
        pre_explore_min_mass
        maxdepth
        ch

let internal createImportanceSamplingStateOnDistribution (config: ImportanceSamplingConfig<'T>) totalSamples maxdepth sortBeforeSampling (selector: ImportanceSelector<'T> option) distr =
    match selector with
    | Some selector ->
        create_importance_sampling_state_with_mass_budget
            config.Subsample
            totalSamples
            config.PreExploreDepth
            config.PreExploreMinMass
            maxdepth
            selector
            distr
    | None ->
        create_importance_sampling_state_prepared_with_mass_budget
            config.Subsample
            totalSamples
            config.PreExploreDepth
            config.PreExploreMinMass
            maxdepth
            distr

let internal createImportanceSamplingState (config: ImportanceSamplingConfig<'T>) totalSamples maxdepth sortBeforeSampling (selector: ImportanceSelector<'T> option) distr =
    let effectiveConfig, inputDistribution =
        if config.UsePreExplore then
            config, shallowPreExplore config distr
        else
            { config with
                PreExploreDepth = 0
                PreExploreMinMass = 0.0
                ShallowExploreMaxNodes = None
                ShallowExploreMaxFrontier = None }, distr

    createImportanceSamplingStateOnDistribution effectiveConfig totalSamples maxdepth sortBeforeSampling selector inputDistribution
 
 
