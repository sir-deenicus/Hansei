module Hansei.StochasticBeam

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Hansei.Probability
open Hansei.Sampling
open Utils

type StochasticBeamConfig =
    {
        BeamWidth: int
        MaxRounds: int
        EliteCount: int
        DiversityBucketCount: int
        LookaheadDepth: int
        LookaheadStrength: float
        MinBeamWidth: int
        Seed: int
    }

type StochasticBeamStats =
    {
        Rounds: int
        MaxUniqueLiveWidth: int
        MaxLiveOccupancy: int
        MaxUniqueCandidateWidth: int
        MaxSelectedRepresentativeWidth: int
    }

type StochasticBeamSnapshot<'T> =
    {
        Dist: ProbabilitySpace<'T>
        PendingUniqueLive: int
        PendingLiveSlots: int
        PendingMass: float
    }

type internal FrontierItem<'T> =
    {
        Frontier: ProbabilitySpace<'T>
        Occupancy: int
        Weight: float
    }

type internal Advancement<'T> =
    | Dead
    | Done of 'T * float
    | Branching of ProbabilitySpace<'T> * float

type internal Candidate<'T> =
    | CandidateValue of 'T
    | CandidateFrontier of ProbabilitySpace<'T>

type internal CandidateRoundInfo<'T> =
    {
        Candidate: Candidate<'T>
        Weight: float
        CullWeight: float
        BucketKey: int
    }

type internal CandidateShare<'T> =
    {
        Candidate: Candidate<'T>
        Occupancy: int
        Weight: float
    }

type internal CandidateRoundSummary<'T> =
    {
        Candidates: CandidateRoundInfo<'T>[]
        CandidateCount: int
        RoundBeamWidth: int
        TotalCullWeight: float
    }

type internal BeamWorkspace<'T when 'T: equality> =
    {
        CandidatePool: ResizeArray<Candidate<'T> * float>
        NextLive: ResizeArray<FrontierItem<'T>>
        AnswerBuffer: Dictionary<'T, float>
        CullRemainder: ResizeArray<CandidateRoundInfo<'T>>
        SelectedShares: ResizeArray<CandidateShare<'T>>
        BucketTotals: Dictionary<int, float>
        SelectedBuckets: HashSet<int>
        ConsumedBuckets: HashSet<int>
        BucketWeights: ResizeArray<struct (int * float)>
        LookaheadCache: Dictionary<struct (int * int), float>
    }

type internal BeamStateData<'T when 'T: equality> =
    {
        Config: StochasticBeamConfig
        RngState: uint64
        Live: FrontierItem<'T>[]
        Answers: Dictionary<'T, float>
        Stats: StochasticBeamStats
    }

let createStochasticBeamConfig beamWidth maxRounds eliteCount diversityBucketCount lookaheadDepth lookaheadStrength minBeamWidth seed =
    {
        BeamWidth = beamWidth
        MaxRounds = maxRounds
        EliteCount = eliteCount
        DiversityBucketCount = diversityBucketCount
        LookaheadDepth = lookaheadDepth
        LookaheadStrength = lookaheadStrength
        MinBeamWidth = minBeamWidth
        Seed = seed
    }

let internal validateStochasticBeamConfig config =
    if config.BeamWidth <= 0 then
        invalidArg (nameof config.BeamWidth) "BeamWidth must be positive."
    elif config.MaxRounds <= 0 then
        invalidArg (nameof config.MaxRounds) "MaxRounds must be positive."
    elif config.EliteCount < 0 then
        invalidArg (nameof config.EliteCount) "EliteCount cannot be negative."
    elif config.DiversityBucketCount < 0 then
        invalidArg (nameof config.DiversityBucketCount) "DiversityBucketCount cannot be negative."
    elif config.LookaheadDepth < 0 then
        invalidArg (nameof config.LookaheadDepth) "LookaheadDepth cannot be negative."
    elif Double.IsNaN config.LookaheadStrength || config.LookaheadStrength < 0.0 || config.LookaheadStrength > 1.0 then
        invalidArg (nameof config.LookaheadStrength) "LookaheadStrength must be between 0.0 and 1.0."
    elif config.MinBeamWidth < 0 then
        invalidArg (nameof config.MinBeamWidth) "MinBeamWidth cannot be negative."
    elif config.MinBeamWidth > config.BeamWidth then
        invalidArg (nameof config.MinBeamWidth) "MinBeamWidth cannot exceed BeamWidth."

let internal validateRoundCount roundCount =
    if roundCount <= 0 then
        invalidArg (nameof roundCount) "roundCount must be positive."

let internal addBeamAnswerWeightInPlace value weight (answers: Dictionary<'T, float>) =
    match answers.TryGetValue value with
    | true, existing -> answers.[value] <- existing + weight
    | false, _ -> answers.[value] <- weight

let internal loadBeamAnswerBuffer (source: Dictionary<'T, float>) (buffer: Dictionary<'T, float>) =
    buffer.Clear()

    for KeyValue(value, weight) in source do
        buffer.[value] <- weight

let internal cloneBeamAnswerDictionary (source: Dictionary<'T, float>) =
    let clone = Dictionary<'T, float>(source.Count)

    for KeyValue(value, weight) in source do
        clone.[value] <- weight

    clone

let internal answerBufferToDistribution (buffer: Dictionary<'T, float>) : ProbabilitySpace<'T> =
    let total = buffer |> Seq.sumBy (fun (KeyValue(_, weight)) -> weight)

    if total <= 0.0 then
        []
    else
        let invTotal = 1.0 / total
        [ for KeyValue(value, weight) in buffer -> Value value, weight * invTotal ]

let internal createBeamWorkspace<'T when 'T: equality> beamWidth =
    {
        CandidatePool = ResizeArray<Candidate<'T> * float>(max 16 (beamWidth * 2))
        NextLive = ResizeArray<FrontierItem<'T>>(max 16 beamWidth)
        AnswerBuffer = Dictionary<'T, float>()
        CullRemainder = ResizeArray<CandidateRoundInfo<'T>>(max 16 beamWidth)
        SelectedShares = ResizeArray<CandidateShare<'T>>(max 16 beamWidth)
        BucketTotals = Dictionary<int, float>()
        SelectedBuckets = HashSet<int>()
        ConsumedBuckets = HashSet<int>()
        BucketWeights = ResizeArray<struct (int * float)>()
        LookaheadCache = Dictionary<struct (int * int), float>()
    }

let internal emptyBeamStats beamWidth =
    {
        Rounds = 0
        MaxUniqueLiveWidth = 1
        MaxLiveOccupancy = beamWidth
        MaxUniqueCandidateWidth = 0
        MaxSelectedRepresentativeWidth = 0
    }

let internal mix64 value =
    let mutable z = value + 0x9E3779B97F4A7C15UL
    z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
    z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
    z ^^^ (z >>> 31)

let internal nextUniform rngState =
    let nextState = mix64 rngState
    let mantissa = nextState >>> 11
    let unitFloat = float mantissa / 9007199254740992.0
    nextState, unitFloat

let internal collapseBeamForced initialWeight initialFrontier =
    let live = initialFrontier |> List.filter (fun (_, weight) -> weight > 0.0)

    match collapseForcedPathBounded defaultSingletonUnravelLimit 0 initialWeight false live with
    | Choice1Of2 (value, weight) -> Done(value, weight)
    | Choice2Of2 ([], _, _) -> Dead
    | Choice2Of2 (frontier, weight, _) -> Branching(frontier, weight)

let internal appendBeamCandidates parentWeight frontier (candidatePool: ResizeArray<Candidate<'T> * float>) =
    for node, branchWeight in frontier do
        if branchWeight > 0.0 then
            match node with
            | Value value -> candidatePool.Add(CandidateValue value, parentWeight * branchWeight)
            | ContinuedSubTree next ->
                match collapseBeamForced branchWeight (force next) with
                | Dead -> ()
                | Done (value, weight) -> candidatePool.Add(CandidateValue value, parentWeight * weight)
                | Branching (preparedFrontier, weight) -> candidatePool.Add(CandidateFrontier preparedFrontier, parentWeight * weight)

let internal combineHash seed value =
    ((seed <<< 5) + seed) ^^^ value

let internal nodeBucketHash node =
    match node with
    | Value value -> combineHash 17 (Unchecked.hash value)
    | ContinuedSubTree next -> combineHash 31 (RuntimeHelpers.GetHashCode(box next))

let internal frontierBucketHash frontier =
    let mutable first = Unchecked.defaultof<struct (float * int * int)>
    let mutable second = Unchecked.defaultof<struct (float * int * int)>
    let mutable third = Unchecked.defaultof<struct (float * int * int)>
    let mutable hasFirst = false
    let mutable hasSecond = false
    let mutable hasThird = false

    let inline insert weight nodeHash quantizedWeight =
        let firstWeight =
            if hasFirst then
                let struct (storedWeight, _, _) = first
                storedWeight
            else
                Double.NegativeInfinity

        let secondWeight =
            if hasSecond then
                let struct (storedWeight, _, _) = second
                storedWeight
            else
                Double.NegativeInfinity

        let thirdWeight =
            if hasThird then
                let struct (storedWeight, _, _) = third
                storedWeight
            else
                Double.NegativeInfinity

        if weight > firstWeight then
            if hasSecond then
                third <- second
                hasThird <- true

            if hasFirst then
                second <- first
                hasSecond <- true

            first <- struct (weight, nodeHash, quantizedWeight)
            hasFirst <- true
        elif weight > secondWeight then
            if hasSecond then
                third <- second
                hasThird <- true

            second <- struct (weight, nodeHash, quantizedWeight)
            hasSecond <- true
        elif weight > thirdWeight then
            third <- struct (weight, nodeHash, quantizedWeight)
            hasThird <- true

    for node, weight in frontier do
        if weight > 0.0 then
            insert weight (nodeBucketHash node) (int (Math.Round(weight * 1024.0)))

    let mutable hash = 53

    if hasFirst then
        let struct (_, nodeHash, quantizedWeight) = first
        hash <- combineHash (combineHash hash nodeHash) quantizedWeight

    if hasSecond then
        let struct (_, nodeHash, quantizedWeight) = second
        hash <- combineHash (combineHash hash nodeHash) quantizedWeight

    if hasThird then
        let struct (_, nodeHash, quantizedWeight) = third
        hash <- combineHash (combineHash hash nodeHash) quantizedWeight

    hash

let internal lookaheadCacheKey depth next =
    struct (depth, RuntimeHelpers.GetHashCode(box next))

let rec internal subtreeLookaheadMass depth next (cache: Dictionary<struct (int * int), float>) =
    let key = lookaheadCacheKey depth next

    match cache.TryGetValue key with
    | true, cached -> cached
    | false, _ ->
        let mass =
            match collapseBeamForced 1.0 (force next) with
            | Dead -> 0.0
            | Done (_, weight) -> weight
            | Branching (nextFrontier, weight) ->
                if depth <= 1 then
                    weight
                else
                    weight * relativeLookaheadMass (depth - 1) nextFrontier cache

        cache.[key] <- mass
        mass

and internal relativeLookaheadMass depth frontier (cache: Dictionary<struct (int * int), float>) =
    let live = frontier |> List.filter (fun (_, weight) -> weight > 0.0)
    let totalWeight = live |> List.sumBy snd

    if totalWeight <= 0.0 then
        0.0
    elif depth <= 0 then
        1.0
    else
        let reachableMass =
            live
            |> List.sumBy (fun (node, branchWeight) ->
                match node with
                | Value _ -> branchWeight
                | ContinuedSubTree next -> branchWeight * subtreeLookaheadMass depth next cache)

        reachableMass / totalWeight

let internal candidateLookaheadScore depth candidate (cache: Dictionary<struct (int * int), float>) =
    if depth <= 0 then
        1.0
    else
        match candidate with
        | CandidateValue _ -> 1.0
        | CandidateFrontier frontier -> relativeLookaheadMass depth frontier cache

let internal clamp01 value =
    max 0.0 (min 1.0 value)

let internal candidateBucketKey bucketCount candidate =
    if bucketCount <= 1 then
        0
    else
        let rawHash =
            match candidate with
            | CandidateValue value -> combineHash 97 (Unchecked.hash value)
            | CandidateFrontier frontier -> frontierBucketHash frontier

        abs rawHash % bucketCount

let internal effectiveBeamWidth config candidateCount =
    if config.MinBeamWidth <= 0 || config.MinBeamWidth >= config.BeamWidth then
        config.BeamWidth
    else
        let clampedCandidateCount = max 0 candidateCount
        let minWidth = min config.MinBeamWidth config.BeamWidth
        let span = config.BeamWidth - minWidth
        let diversityRatio = min 1.0 (float clampedCandidateCount / float config.BeamWidth)
        minWidth + int (Math.Round(diversityRatio * float span))

let internal buildCandidateRoundSummary config (workspace: BeamWorkspace<'T>) (items: (Candidate<'T> * float) array) =
    let roundBeamWidth = effectiveBeamWidth config items.Length
    let candidates = Array.zeroCreate items.Length

    if items.Length <= roundBeamWidth then
        for index in 0 .. items.Length - 1 do
            let candidate, weight = items.[index]
            candidates.[index] <-
                {
                    Candidate = candidate
                    Weight = weight
                    CullWeight = weight
                    BucketKey = 0
                }

        {
            Candidates = candidates
            CandidateCount = items.Length
            RoundBeamWidth = roundBeamWidth
            TotalCullWeight = items |> Array.sumBy snd
        }
    else
        workspace.LookaheadCache.Clear()
        let strength = clamp01 config.LookaheadStrength
        let mutable totalCullWeight = 0.0

        for index in 0 .. items.Length - 1 do
            let candidate, weight = items.[index]
            let score = candidateLookaheadScore config.LookaheadDepth candidate workspace.LookaheadCache
            let multiplier =
                if config.LookaheadDepth <= 0 || strength <= 0.0 then
                    1.0
                else
                    max 1e-12 ((1.0 - strength) + strength * score)

            let cullWeight = weight * multiplier
            totalCullWeight <- totalCullWeight + cullWeight
            candidates.[index] <-
                {
                    Candidate = candidate
                    Weight = weight
                    CullWeight = cullWeight
                    BucketKey = candidateBucketKey config.DiversityBucketCount candidate
                }

        {
            Candidates = candidates
            CandidateCount = items.Length
            RoundBeamWidth = roundBeamWidth
            TotalCullWeight = totalCullWeight
        }

let internal systematicResample rngState beamWidth totalCullWeight (items: ResizeArray<CandidateRoundInfo<'T>>) =
    if beamWidth <= 0 || totalCullWeight <= 0.0 || items.Count = 0 then
        [], rngState
    else
        let step = totalCullWeight / float beamWidth
        let nextRngState, offset = nextUniform rngState
        let mutable cursor = offset * step
        let resampled = ResizeArray<CandidateShare<'T>>()
        let mutable cumulative = items.[0].CullWeight
        let mutable index = 0
        let mutable currentIndex = -1
        let mutable currentOccupancy = 0

        let flushCurrent () =
            if currentIndex >= 0 && currentOccupancy > 0 then
                let selectedItem = items.[currentIndex]
                let correctedUnitWeight = step * selectedItem.Weight / selectedItem.CullWeight
                resampled.Add
                    {
                        Candidate = selectedItem.Candidate
                        Occupancy = currentOccupancy
                        Weight = correctedUnitWeight * float currentOccupancy
                    }

        for _ in 1 .. beamWidth do
            while cursor > cumulative && index + 1 < items.Count do
                index <- index + 1
                cumulative <- cumulative + items.[index].CullWeight

            if index = currentIndex then
                currentOccupancy <- currentOccupancy + 1
            else
                flushCurrent ()
                currentIndex <- index
                currentOccupancy <- 1

            cursor <- cursor + step

        flushCurrent ()
        List.ofSeq resampled, nextRngState

let internal copyCullRemainder (workspace: BeamWorkspace<'T>) startIndex (items: CandidateRoundInfo<'T> array) =
    workspace.CullRemainder.Clear()
    let mutable remainderCullWeight = 0.0

    for index in startIndex .. items.Length - 1 do
        let item = items.[index]
        workspace.CullRemainder.Add item
        remainderCullWeight <- remainderCullWeight + item.CullWeight

    remainderCullWeight

let internal reserveBucketRepresentatives (workspace: BeamWorkspace<'T>) slotCount startIndex (items: CandidateRoundInfo<'T> array) =
    if slotCount <= 0 || startIndex >= items.Length then
        0.0
    else
        workspace.BucketTotals.Clear()
        workspace.BucketWeights.Clear()
        workspace.SelectedBuckets.Clear()
        workspace.ConsumedBuckets.Clear()
        workspace.CullRemainder.Clear()

        for index in startIndex .. items.Length - 1 do
            let item = items.[index]

            match workspace.BucketTotals.TryGetValue item.BucketKey with
            | true, total -> workspace.BucketTotals.[item.BucketKey] <- total + item.CullWeight
            | false, _ -> workspace.BucketTotals.[item.BucketKey] <- item.CullWeight

        for KeyValue(bucketKey, totalWeight) in workspace.BucketTotals do
            workspace.BucketWeights.Add(struct (bucketKey, totalWeight))

        workspace.BucketWeights.Sort(Comparison(fun (struct (_, leftWeight)) (struct (_, rightWeight)) -> compare rightWeight leftWeight))

        for index in 0 .. min (slotCount - 1) (workspace.BucketWeights.Count - 1) do
            let struct (bucketKey, _) = workspace.BucketWeights.[index]
            workspace.SelectedBuckets.Add bucketKey |> ignore

        let mutable remainderCullWeight = 0.0

        for index in startIndex .. items.Length - 1 do
            let item = items.[index]

            if workspace.SelectedBuckets.Contains item.BucketKey && workspace.ConsumedBuckets.Add item.BucketKey then
                workspace.SelectedShares.Add
                    {
                        Candidate = item.Candidate
                        Occupancy = 1
                        Weight = item.Weight
                    }
            else
                workspace.CullRemainder.Add item
                remainderCullWeight <- remainderCullWeight + item.CullWeight

        remainderCullWeight

let internal systematicCull (workspace: BeamWorkspace<'T>) rngState eliteCount diversityBucketCount (summary: CandidateRoundSummary<'T>) =
    if summary.RoundBeamWidth <= 0 || summary.CandidateCount = 0 then
        [||], rngState
    elif summary.CandidateCount <= summary.RoundBeamWidth then
        summary.Candidates
        |> Array.map (fun item ->
            {
                Candidate = item.Candidate
                Occupancy = 1
                Weight = item.Weight
            }), rngState
    else
        let maxEliteCount =
            if summary.CandidateCount <= 1 then
                0
            else
                min (summary.RoundBeamWidth - 1) (summary.CandidateCount - 1)

        let retainedEliteCount = min (max 0 eliteCount) maxEliteCount
        let sortedCandidates = Array.copy summary.Candidates
        Array.sortInPlaceWith (fun left right -> compare right.CullWeight left.CullWeight) sortedCandidates
        workspace.SelectedShares.Clear()

        for index in 0 .. retainedEliteCount - 1 do
            let item = sortedCandidates.[index]
            workspace.SelectedShares.Add
                {
                    Candidate = item.Candidate
                    Occupancy = 1
                    Weight = item.Weight
                }

        let bucketSlots = summary.RoundBeamWidth - retainedEliteCount
        let remainderCullWeight =
            if diversityBucketCount > 1 && bucketSlots > 0 then
                reserveBucketRepresentatives workspace bucketSlots retainedEliteCount sortedCandidates
            else
                copyCullRemainder workspace retainedEliteCount sortedCandidates

        let resampleSlots = max 0 (summary.RoundBeamWidth - workspace.SelectedShares.Count)
        let resampledShares, nextRngState = systematicResample rngState resampleSlots remainderCullWeight workspace.CullRemainder

        for share in resampledShares do
            workspace.SelectedShares.Add share

        workspace.SelectedShares.ToArray(), nextRngState

let internal beamStateCompleteData state =
    Array.isEmpty state.Live || state.Stats.Rounds >= state.Config.MaxRounds

let internal initBeamStateData config distribution =
    validateStochasticBeamConfig config

    {
        Config = config
        RngState = uint64 (max 1 config.Seed)
        Live = [| { Frontier = distribution; Occupancy = config.BeamWidth; Weight = 1.0 } |]
        Answers = Dictionary<'T, float>()
        Stats = emptyBeamStats config.BeamWidth
    }

let internal advanceBeamRoundCore (workspace: BeamWorkspace<'T>) state =
    if beamStateCompleteData state then
        state
    else
        workspace.CandidatePool.Clear()
        workspace.NextLive.Clear()
        loadBeamAnswerBuffer state.Answers workspace.AnswerBuffer
        let roundNumber = state.Stats.Rounds + 1
        let maxUniqueLiveWidth = max state.Stats.MaxUniqueLiveWidth state.Live.Length
        let maxLiveOccupancy = max state.Stats.MaxLiveOccupancy (state.Live |> Array.sumBy (fun item -> item.Occupancy))

        for item in state.Live do
            match collapseBeamForced item.Weight item.Frontier with
            | Dead -> ()
            | Done (value, weight) -> addBeamAnswerWeightInPlace value weight workspace.AnswerBuffer
            | Branching (frontier, weight) -> appendBeamCandidates weight frontier workspace.CandidatePool

        let candidateItems = workspace.CandidatePool.ToArray()
        let roundSummary = buildCandidateRoundSummary state.Config workspace candidateItems
        let maxUniqueCandidateWidth = max state.Stats.MaxUniqueCandidateWidth roundSummary.CandidateCount
        let selected, nextRngState = systematicCull workspace state.RngState state.Config.EliteCount state.Config.DiversityBucketCount roundSummary
        let maxSelectedRepresentativeWidth = max state.Stats.MaxSelectedRepresentativeWidth selected.Length

        for selectedCandidate in selected do
            match selectedCandidate.Candidate with
            | CandidateValue value -> addBeamAnswerWeightInPlace value selectedCandidate.Weight workspace.AnswerBuffer
            | CandidateFrontier frontier ->
                workspace.NextLive.Add
                    {
                        Frontier = frontier
                        Occupancy = selectedCandidate.Occupancy
                        Weight = selectedCandidate.Weight
                    }

        {
            state with
                RngState = nextRngState
                Live = workspace.NextLive.ToArray()
                Answers = cloneBeamAnswerDictionary workspace.AnswerBuffer
                Stats =
                    {
                        Rounds = roundNumber
                        MaxUniqueLiveWidth = maxUniqueLiveWidth
                        MaxLiveOccupancy = maxLiveOccupancy
                        MaxUniqueCandidateWidth = maxUniqueCandidateWidth
                        MaxSelectedRepresentativeWidth = maxSelectedRepresentativeWidth
                    }
        }

let internal snapshotBeamStateCore (workspace: BeamWorkspace<'T>) state =
    loadBeamAnswerBuffer state.Answers workspace.AnswerBuffer

    let pendingUniqueLive, pendingLiveSlots, pendingMass =
        ((0, 0, 0.0), state.Live)
        ||> Array.fold (fun (uniqueLive, liveSlots, totalPendingMass) item ->
            match collapseBeamForced item.Weight item.Frontier with
            | Done (value, weight) ->
                addBeamAnswerWeightInPlace value weight workspace.AnswerBuffer
                uniqueLive, liveSlots, totalPendingMass
            | Dead ->
                uniqueLive, liveSlots, totalPendingMass
            | Branching _ ->
                uniqueLive + 1, liveSlots + item.Occupancy, totalPendingMass + item.Weight)

    {
        Dist = answerBufferToDistribution workspace.AnswerBuffer
        PendingUniqueLive = pendingUniqueLive
        PendingLiveSlots = pendingLiveSlots
        PendingMass = pendingMass
    }

type StochasticBeamState<'T when 'T: equality> internal (stateData: BeamStateData<'T>) =
    member _.Config = stateData.Config
    member _.Stats = stateData.Stats
    member _.CompletedRounds = stateData.Stats.Rounds
    member this.RemainingRounds =
        if this.IsComplete then
            0
        else
            max 0 (stateData.Config.MaxRounds - stateData.Stats.Rounds)

    member _.IsComplete = beamStateCompleteData stateData
    member _.HasSnapshot = true

    member _.SnapshotInfo() : StochasticBeamSnapshot<'T> =
        let workspace = createBeamWorkspace<'T> stateData.Config.BeamWidth
        snapshotBeamStateCore workspace stateData

    member this.Snapshot() : ProbabilitySpace<'T> =
        this.SnapshotInfo().Dist

    member this.Advance(roundCount: int) =
        validateRoundCount roundCount

        if this.IsComplete then
            this
        else
            let workspace = createBeamWorkspace<'T> stateData.Config.BeamWidth
            let stepCount = min roundCount this.RemainingRounds
            let mutable nextState = stateData

            for _ in 1 .. stepCount do
                if not (beamStateCompleteData nextState) then
                    nextState <- advanceBeamRoundCore workspace nextState

            StochasticBeamState(nextState)

    member this.RunToCompletion() =
        if this.IsComplete then
            this
        else
            let workspace = createBeamWorkspace<'T> stateData.Config.BeamWidth
            let mutable nextState = stateData

            while not (beamStateCompleteData nextState) do
                nextState <- advanceBeamRoundCore workspace nextState

            StochasticBeamState(nextState)

    member this.ToStateSeq(roundCount: int) : seq<StochasticBeamState<'T>> =
        validateRoundCount roundCount

        seq {
            let mutable current = this
            yield current

            while not current.IsComplete do
                current <- current.Advance(roundCount)
                yield current
        }

    member this.ToSnapshotSeq(roundCount: int) : seq<ProbabilitySpace<'T>> =
        this.ToStateSeq(roundCount)
        |> Seq.map (fun state -> state.Snapshot())

    member this.ToSnapshotInfoSeq(roundCount: int) : seq<StochasticBeamSnapshot<'T>> =
        this.ToStateSeq(roundCount)
        |> Seq.map (fun state -> state.SnapshotInfo())

let createStochasticBeamState config distribution =
    StochasticBeamState(initBeamStateData config distribution)

let stochasticBeam config distribution =
    (createStochasticBeamState config distribution).RunToCompletion().Snapshot()