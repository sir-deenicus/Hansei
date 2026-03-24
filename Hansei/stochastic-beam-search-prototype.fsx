#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open Hansei.Core.List
open Hansei.Core.List.Distributions
open Hansei.Utils

type SamplerResult =
    { Name: string; Dist: Map<string, float> }

type BeamConfig =
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

type TestCase =
    {
        Name: string
        Why: string
        Distribution: ProbabilitySpace<string>
        Samples: int
        MaxDepth: int
        BeamConfig: BeamConfig
        RawTopCount: int option
        Aggregations: (string * (string -> string)) list
    }

type private FrontierItem<'T> =
    {
        Frontier: ProbabilitySpace<'T>
        Occupancy: int
        Weight: float
    }

type private Advancement<'T> =
    | Dead
    | Done of 'T * float
    | Branching of ProbabilitySpace<'T> * float

type private Candidate<'T> =
    | CandidateValue of 'T
    | CandidateFrontier of ProbabilitySpace<'T>

type private WeightedCandidate<'T> =
    {
        Candidate: Candidate<'T>
        Weight: float
        CullWeight: float
    }

type private CandidateRoundInfo<'T> =
    {
        Candidate: Candidate<'T>
        Weight: float
        CullWeight: float
        BucketKey: int
    }

type private CandidateShare<'T> =
    {
        Candidate: Candidate<'T>
        Occupancy: int
        Weight: float
    }

type private BeamStats =
    {
        Rounds: int
        MaxUniqueLiveWidth: int
        MaxLiveOccupancy: int
        MaxUniqueCandidateWidth: int
        MaxSelectedRepresentativeWidth: int
        AdvanceMs: float
        CullMs: float
        TotalMs: float
    }

type private BeamState =
    {
        Config: BeamConfig
        RngState: uint64
        Live: FrontierItem<string>[]
        Answers: Map<string, float>
        Stats: BeamStats
    }

type private BeamWorkspace =
    {
        CandidatePool: ResizeArray<Candidate<string> * float>
        NextLive: ResizeArray<FrontierItem<string>>
        AnswerBuffer: Dictionary<string, float>
        CullRemainder: ResizeArray<CandidateRoundInfo<string>>
        SelectedShares: ResizeArray<CandidateShare<string>>
        BucketTotals: Dictionary<int, float>
        SelectedBuckets: HashSet<int>
        ConsumedBuckets: HashSet<int>
        BucketWeights: ResizeArray<struct (int * float)>
        LookaheadCache: Dictionary<struct (int * int), float>
    }

type private CandidateRoundSummary<'T> =
    {
        Candidates: CandidateRoundInfo<'T>[]
        CandidateCount: int
        RoundBeamWidth: int
        TotalCullWeight: float
    }

type private BeamSnapshot =
    {
        Dist: Map<string, float>
        PendingUniqueLive: int
        PendingLiveSlots: int
        PendingMass: float
    }

let private quiet f =
    let original = Console.Out
    use sink = new StringWriter()
    Console.SetOut sink
    try f () finally Console.SetOut original

let private aggregateValues (dist: ProbabilitySpace<'T>) =
    dist
    |> List.fold (fun acc (node, p) ->
        match node with
        | Value value -> Map.change value (fun existing -> Some (p + defaultArg existing 0.0)) acc
        | ContinuedSubTree _ -> acc) Map.empty

let private normalizeMap (dist: Map<'T, float>) =
    let total = dist |> Seq.sumBy (fun kv -> kv.Value)
    if total <= 0.0 then dist else dist |> Map.map (fun _ p -> p / total)

let private normalizedValues dist = dist |> aggregateValues |> normalizeMap

let private unionKeys (a: Map<'T, float>) (b: Map<'T, float>) =
    Set.union (a |> Map.keys |> Set.ofSeq) (b |> Map.keys |> Set.ofSeq)

let private l1Distance (expected: Map<'T, float>) (actual: Map<'T, float>) =
    unionKeys expected actual
    |> Seq.sumBy (fun key ->
        let expectedP = Map.tryFind key expected |> Option.defaultValue 0.0
        let actualP = Map.tryFind key actual |> Option.defaultValue 0.0
        abs (expectedP - actualP))

let private printTop title count (dist: Map<string, float>) =
    printfn "%s" title
    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.truncate count
    |> List.iter (fun (label, p) -> printfn "  %-26s %.6f" label p)

let private aggregateLabels project (dist: Map<string, float>) =
    dist
    |> Map.toList
    |> List.fold (fun acc (label, p) -> Map.change (project label) (fun existing -> Some (p + defaultArg existing 0.0)) acc) Map.empty

let private printAggregates title aggregations dist =
    for aggregationName, project in aggregations do
        printTop (sprintf "%s | %s" title aggregationName) 10 (aggregateLabels project dist)

let private exact distribution = Model.ExactInfer distribution |> normalizedValues
let private importance samples maxDepth distribution = quiet (fun () -> Model.ImportanceSamples(distribution, samples, maxDepth)) |> normalizedValues
let private importanceNoPre samples maxDepth distribution = quiet (fun () -> Model.ImportanceSamples(distribution, samples, maxDepth, preExplore = false)) |> normalizedValues
let private path samples distribution = quiet (fun () -> Model.PathSample(distribution, samples)) |> normalizedValues

let private benchmark repeats action =
    action () |> ignore
    let timer = Stopwatch.StartNew()
    for _ in 1 .. repeats do
        action () |> ignore
    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private averageBy projection items =
    items |> List.averageBy projection

let private addAnswerWeight value weight answers =
    Map.change value (fun existing -> Some (weight + defaultArg existing 0.0)) answers

let private addAnswerWeightInPlace value weight (answers: Dictionary<string, float>) =
    match answers.TryGetValue value with
    | true, existing -> answers.[value] <- existing + weight
    | false, _ -> answers.[value] <- weight

let private loadAnswerBuffer (source: Map<string, float>) (buffer: Dictionary<string, float>) =
    buffer.Clear()

    for KeyValue(label, weight) in source do
        buffer.[label] <- weight

let private answerBufferToMap (buffer: Dictionary<string, float>) =
    buffer
    |> Seq.fold (fun acc (KeyValue(label, weight)) -> Map.add label weight acc) Map.empty

let private createBeamWorkspace beamWidth =
    {
        CandidatePool = ResizeArray<Candidate<string> * float>(max 16 (beamWidth * 2))
        NextLive = ResizeArray<FrontierItem<string>>(max 16 beamWidth)
        AnswerBuffer = Dictionary<string, float>()
        CullRemainder = ResizeArray<CandidateRoundInfo<string>>(max 16 beamWidth)
        SelectedShares = ResizeArray<CandidateShare<string>>(max 16 beamWidth)
        BucketTotals = Dictionary<int, float>()
        SelectedBuckets = HashSet<int>()
        ConsumedBuckets = HashSet<int>()
        BucketWeights = ResizeArray<struct (int * float)>()
        LookaheadCache = Dictionary<struct (int * int), float>()
    }

let private elapsedMilliseconds startTicks =
    float (Stopwatch.GetTimestamp() - startTicks) * 1000.0 / float Stopwatch.Frequency

let private emptyBeamStats beamWidth =
    {
        Rounds = 0
        MaxUniqueLiveWidth = 1
        MaxLiveOccupancy = beamWidth
        MaxUniqueCandidateWidth = 0
        MaxSelectedRepresentativeWidth = 0
        AdvanceMs = 0.0
        CullMs = 0.0
        TotalMs = 0.0
    }

let private mix64 (value: uint64) =
    let mutable z = value + 0x9E3779B97F4A7C15UL
    z <- (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
    z <- (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
    z ^^^ (z >>> 31)

let private nextUniform rngState =
    let nextState = mix64 rngState
    let mantissa = nextState >>> 11
    let unitFloat = float mantissa / 9007199254740992.0
    nextState, unitFloat

let private collapseForced initialWeight initialFrontier =
    let rec loop weight frontier =
        let live = frontier |> List.filter (fun (_, p) -> p > 0.0)

        match live with
        | [] -> Dead
        | [Value value, p] -> Done (value, weight * p)
        | [ContinuedSubTree next, p] -> loop (weight * p) (force next)
        | branches -> Branching (branches, weight)

    loop initialWeight initialFrontier

let private appendCandidates parentWeight frontier (candidatePool: ResizeArray<Candidate<string> * float>) =
    for node, branchWeight in frontier do
        if branchWeight > 0.0 then
            match node with
            | Value value -> candidatePool.Add (CandidateValue value, parentWeight * branchWeight)
            | ContinuedSubTree next ->
                match collapseForced branchWeight (force next) with
                | Dead -> ()
                | Done (value, weight) -> candidatePool.Add (CandidateValue value, parentWeight * weight)
                | Branching (preparedFrontier, weight) -> candidatePool.Add (CandidateFrontier preparedFrontier, parentWeight * weight)

let private combineHash seed value =
    ((seed <<< 5) + seed) ^^^ value

let private nodeBucketHash node =
    match node with
    | Value value -> combineHash 17 (Unchecked.hash value)
    | ContinuedSubTree next -> combineHash 31 (RuntimeHelpers.GetHashCode(box next))

let private frontierBucketHash frontier =
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

let private lookaheadCacheKey depth next =
    struct (depth, RuntimeHelpers.GetHashCode(box next))

let rec private subtreeLookaheadMass depth next (cache: Dictionary<struct (int * int), float>) =
    let key = lookaheadCacheKey depth next

    match cache.TryGetValue key with
    | true, cached -> cached
    | false, _ ->
        let mass =
            match collapseForced 1.0 (force next) with
            | Dead -> 0.0
            | Done (_, weight) -> weight
            | Branching (nextFrontier, weight) ->
                if depth <= 1 then weight else weight * relativeLookaheadMass (depth - 1) nextFrontier cache

        cache.[key] <- mass
        mass

and private relativeLookaheadMass depth frontier (cache: Dictionary<struct (int * int), float>) =
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

let private candidateLookaheadScore depth candidate (cache: Dictionary<struct (int * int), float>) =
    if depth <= 0 then
        1.0
    else
        match candidate with
        | CandidateValue _ -> 1.0
        | CandidateFrontier frontier -> relativeLookaheadMass depth frontier cache

let private clamp01 value = max 0.0 (min 1.0 value)

let private candidateBucketKey bucketCount candidate =
    if bucketCount <= 1 then
        0
    else
        let rawHash =
            match candidate with
            | CandidateValue value -> combineHash 97 (Unchecked.hash value)
            | CandidateFrontier frontier -> frontierBucketHash frontier

        abs rawHash % bucketCount

let private effectiveBeamWidth config candidateCount =
    if config.MinBeamWidth <= 0 || config.MinBeamWidth >= config.BeamWidth then
        config.BeamWidth
    else
        let clampedCandidateCount = max 0 candidateCount
        let minWidth = min config.MinBeamWidth config.BeamWidth
        let span = config.BeamWidth - minWidth
        let diversityRatio = min 1.0 (float clampedCandidateCount / float config.BeamWidth)
        minWidth + int (Math.Round(diversityRatio * float span))

let private buildCandidateRoundSummary config (workspace: BeamWorkspace) (items: (Candidate<string> * float) array) =
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
                if config.LookaheadDepth <= 0 || strength <= 0.0 then 1.0
                else max 1e-12 ((1.0 - strength) + strength * score)

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

let private systematicResample rngState beamWidth totalCullWeight (items: ResizeArray<CandidateRoundInfo<'T>>) =
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

let private copyCullRemainder (workspace: BeamWorkspace) startIndex (items: CandidateRoundInfo<string> array) =
    workspace.CullRemainder.Clear()
    let mutable remainderCullWeight = 0.0

    for index in startIndex .. items.Length - 1 do
        let item = items.[index]
        workspace.CullRemainder.Add item
        remainderCullWeight <- remainderCullWeight + item.CullWeight

    remainderCullWeight

let private reserveBucketRepresentatives (workspace: BeamWorkspace) slotCount startIndex (items: CandidateRoundInfo<string> array) =
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

let private systematicCull (workspace: BeamWorkspace) rngState eliteCount diversityBucketCount (summary: CandidateRoundSummary<string>) =
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
            if summary.CandidateCount <= 1 then 0
            else min (summary.RoundBeamWidth - 1) (summary.CandidateCount - 1)

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

let private beamStateComplete state =
    Array.isEmpty state.Live || state.Stats.Rounds >= state.Config.MaxRounds

let private initBeamState config distribution =
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
    elif config.LookaheadStrength < 0.0 || config.LookaheadStrength > 1.0 then
        invalidArg (nameof config.LookaheadStrength) "LookaheadStrength must be between 0.0 and 1.0."
    elif config.MinBeamWidth < 0 then
        invalidArg (nameof config.MinBeamWidth) "MinBeamWidth cannot be negative."
    elif config.MinBeamWidth > config.BeamWidth then
        invalidArg (nameof config.MinBeamWidth) "MinBeamWidth cannot exceed BeamWidth."

    {
        Config = config
        RngState = uint64 (max 1 config.Seed)
        Live = [| { Frontier = distribution; Occupancy = config.BeamWidth; Weight = 1.0 } |]
        Answers = Map.empty
        Stats = emptyBeamStats config.BeamWidth
    }

let private advanceBeamRoundCore collectStats (workspace: BeamWorkspace) state =
    if beamStateComplete state then
        state
    else
        let roundStart = if collectStats then Stopwatch.GetTimestamp() else 0L
        let advanceStart = if collectStats then Stopwatch.GetTimestamp() else 0L
        workspace.CandidatePool.Clear()
        workspace.NextLive.Clear()
        loadAnswerBuffer state.Answers workspace.AnswerBuffer
        let roundNumber = state.Stats.Rounds + 1
        let maxUniqueLiveWidth = max state.Stats.MaxUniqueLiveWidth state.Live.Length
        let maxLiveOccupancy = max state.Stats.MaxLiveOccupancy (state.Live |> Array.sumBy (fun item -> item.Occupancy))

        for item in state.Live do
            match collapseForced item.Weight item.Frontier with
            | Dead -> ()
            | Done (value, weight) -> addAnswerWeightInPlace value weight workspace.AnswerBuffer
            | Branching (frontier, weight) -> appendCandidates weight frontier workspace.CandidatePool

        let candidateItems = workspace.CandidatePool.ToArray()
        let roundSummary = buildCandidateRoundSummary state.Config workspace candidateItems
        let maxUniqueCandidateWidth = max state.Stats.MaxUniqueCandidateWidth roundSummary.CandidateCount

        let advanceMs = if collectStats then elapsedMilliseconds advanceStart else 0.0
        let cullStart = if collectStats then Stopwatch.GetTimestamp() else 0L
        let selected, nextRngState = systematicCull workspace state.RngState state.Config.EliteCount state.Config.DiversityBucketCount roundSummary
        let cullMs = if collectStats then elapsedMilliseconds cullStart else 0.0
        let maxSelectedRepresentativeWidth = max state.Stats.MaxSelectedRepresentativeWidth selected.Length

        for selectedCandidate in selected do
            match selectedCandidate.Candidate with
            | CandidateValue value -> addAnswerWeightInPlace value selectedCandidate.Weight workspace.AnswerBuffer
            | CandidateFrontier frontier ->
                workspace.NextLive.Add
                    {
                        Frontier = frontier
                        Occupancy = selectedCandidate.Occupancy
                        Weight = selectedCandidate.Weight
                    }

        let nextAnswers = answerBufferToMap workspace.AnswerBuffer
        let nextLive = workspace.NextLive.ToArray()
        let totalMs = if collectStats then elapsedMilliseconds roundStart else 0.0

        {
            state with
                RngState = nextRngState
                Live = nextLive
                Answers = nextAnswers
                Stats =
                    if collectStats then
                        {
                            Rounds = roundNumber
                            MaxUniqueLiveWidth = maxUniqueLiveWidth
                            MaxLiveOccupancy = maxLiveOccupancy
                            MaxUniqueCandidateWidth = maxUniqueCandidateWidth
                            MaxSelectedRepresentativeWidth = maxSelectedRepresentativeWidth
                            AdvanceMs = state.Stats.AdvanceMs + advanceMs
                            CullMs = state.Stats.CullMs + cullMs
                            TotalMs = state.Stats.TotalMs + totalMs
                        }
                    else
                        { state.Stats with Rounds = roundNumber }
        }

let private advanceBeamRound state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    advanceBeamRoundCore true workspace state

let private advanceBeamRoundLean state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    advanceBeamRoundCore false workspace state

let private advanceBeamRounds roundCount state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    let rec loop remaining currentState =
        if remaining <= 0 || beamStateComplete currentState then
            currentState
        else
            loop (remaining - 1) (advanceBeamRoundCore true workspace currentState)

    loop roundCount state

let private advanceBeamRoundsLean roundCount state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    let rec loop remaining currentState =
        if remaining <= 0 || beamStateComplete currentState then
            currentState
        else
            loop (remaining - 1) (advanceBeamRoundCore false workspace currentState)

    loop roundCount state

let private runBeamToCompletion state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    let rec loop currentState =
        if beamStateComplete currentState then
            currentState
        else
            loop (advanceBeamRoundCore true workspace currentState)

    loop state

let private runBeamToCompletionLean state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    let rec loop currentState =
        if beamStateComplete currentState then
            currentState
        else
            loop (advanceBeamRoundCore false workspace currentState)

    loop state

let private snapshotBeamStateCore (workspace: BeamWorkspace) state =
    loadAnswerBuffer state.Answers workspace.AnswerBuffer

    let pendingUniqueLive, pendingLiveSlots, pendingMass =
        ((0, 0, 0.0), state.Live)
        ||> Array.fold (fun (uniqueLive, liveSlots, pendingMass) item ->
            match collapseForced item.Weight item.Frontier with
            | Done (value, weight) ->
                addAnswerWeightInPlace value weight workspace.AnswerBuffer
                uniqueLive, liveSlots, pendingMass
            | Dead -> uniqueLive, liveSlots, pendingMass
            | Branching _ -> uniqueLive + 1, liveSlots + item.Occupancy, pendingMass + item.Weight)

    let dist = workspace.AnswerBuffer |> answerBufferToMap |> normalizeMap

    {
        Dist = dist
        PendingUniqueLive = pendingUniqueLive
        PendingLiveSlots = pendingLiveSlots
        PendingMass = pendingMass
    }

let private snapshotBeamState state =
    let workspace = createBeamWorkspace state.Config.BeamWidth
    snapshotBeamStateCore workspace state

let private stochasticBeam (config: BeamConfig) distribution =
    let finalState =
        distribution
        |> initBeamState config
        |> runBeamToCompletion

    let snapshot = snapshotBeamState finalState
    snapshot.Dist, finalState.Stats

let private stochasticBeamLean (config: BeamConfig) distribution =
    let finalState =
        distribution
        |> initBeamState config
        |> runBeamToCompletionLean

    let workspace = createBeamWorkspace config.BeamWidth
    let snapshot = snapshotBeamStateCore workspace finalState
    snapshot.Dist

let private formatBeamConfig config =
    sprintf
        "beam=%d, min-beam=%d, rounds=%d, elites=%d, buckets=%d, lookahead-depth=%d, lookahead-strength=%.2f"
        config.BeamWidth
        config.MinBeamWidth
        config.MaxRounds
        config.EliteCount
        config.DiversityBucketCount
        config.LookaheadDepth
        config.LookaheadStrength

let private formatBeamStats stats =
    sprintf
            "rounds=%d, unique-live=%d, live-slots=%d, unique-candidates=%d, selected-reps=%d"
            stats.Rounds
            stats.MaxUniqueLiveWidth
            stats.MaxLiveOccupancy
            stats.MaxUniqueCandidateWidth
            stats.MaxSelectedRepresentativeWidth

let private printBeamSpaceDiagnostics stats =
    printfn "Beam space profile: unique-live=%d, live-slots=%d, unique-candidates=%d, selected-reps=%d" stats.MaxUniqueLiveWidth stats.MaxLiveOccupancy stats.MaxUniqueCandidateWidth stats.MaxSelectedRepresentativeWidth

let private printBeamTimingDiagnostics label advanceMs cullMs totalMs =
    printfn "%s advance=%.3f, cull=%.3f, total=%.3f" label advanceMs cullMs totalMs

let private baselineBeamConfig beamWidth maxRounds seed =
    {
        BeamWidth = beamWidth
        MaxRounds = maxRounds
        EliteCount = 0
        DiversityBucketCount = 0
        LookaheadDepth = 0
        LookaheadStrength = 0.0
        MinBeamWidth = beamWidth
        Seed = seed
    }

let private enhancedBeamConfig beamWidth maxRounds seed =
    {
        BeamWidth = beamWidth
        MaxRounds = maxRounds
        EliteCount = 64
        DiversityBucketCount = 32
        LookaheadDepth = 1
        LookaheadStrength = 0.5
        MinBeamWidth = max 256 (beamWidth / 4)
        Seed = seed
    }

let private withEliteRetention eliteCount config =
    { config with EliteCount = eliteCount }

let private withBucketedCulling bucketCount config =
    { config with DiversityBucketCount = bucketCount }

let private withLookahead depth strength config =
    { config with LookaheadDepth = depth; LookaheadStrength = strength }

let private withAdaptiveWidth minBeamWidth config =
    { config with MinBeamWidth = minBeamWidth }

let private printBeamStateSnapshot title state =
    let snapshot = snapshotBeamState state
    printfn "%s" title
    printfn "  rounds=%d pending-unique=%d pending-slots=%d pending-mass=%.6f" state.Stats.Rounds snapshot.PendingUniqueLive snapshot.PendingLiveSlots snapshot.PendingMass
    printTop "  posterior snapshot" 10 snapshot.Dist

let private coinPrior = [ 0.05; 0.15; 0.25; 0.35; 0.45; 0.55; 0.65; 0.75; 0.85; 0.95 ]

let private formatBias p = sprintf "p=%.2f" p
let private labelPosterior posterior = ProbabilitySpace.mapDistribution formatBias posterior

let private observeAll p observations =
    let rec loop = function
        | [] -> always p
        | observed :: rest ->
            dist {
                let! flip = bernoulli p
                do! observe (flip = observed)
                return! loop rest
            }

    loop observations

let private hardEvidencePosterior observations =
    dist {
        let! p = uniform coinPrior
        return! observeAll p observations
    }
    |> labelPosterior

let private geometricBounded maxCount p =
    let rec loop count =
        dist {
            if count >= maxCount then
                return count
            else
                let! stop = bernoulli p
                if stop then return count else return! loop (count + 1)
        }

    loop 0

let private randomPos () =
    dist {
        let! x = uniform [ 0 .. 9 ]
        let! y = uniform [ 0 .. 9 ]
        return x, y
    }

let private olegGate () =
    dist {
        let! gate = bernoulli 0.98

        if not gate then
            return 0
        else
            let! pos = randomPos ()
            do! observe (pos = (3, 5))
            return 1
    }

let private olegGateFactored () =
    dist {
        let! gate = bernoulli 0.98

        if not gate then
            return 0
        else
            do! Hansei.Core.List.exact_local_observe ((=) (3, 5)) (randomPos ())
            return 1
    }

let private olegInspiredPosterior =
    dist {
        let! warmup = geometricBounded 3 0.98
        let! a = olegGate ()
        let! b = olegGate ()
        let! c = olegGate ()
        do! observe (a + b + c = 3)
        return sprintf "warmup=%d" warmup
    }

let private olegInspiredPosteriorFactored =
    dist {
        let! warmup = geometricBounded 3 0.98
        let! a = olegGateFactored ()
        let! b = olegGateFactored ()
        let! c = olegGateFactored ()
        do! observe (a + b + c = 3)
        return sprintf "warmup=%d" warmup
    }

let private hiddenMarkovPosterior (observations: bool[]) =
    let rec loop step hiddenState =
        dist {
            if step >= observations.Length then
                return sprintf "final=%s" (if hiddenState then "Hot" else "Cold")
            else
                let emissionP = if hiddenState then 0.85 else 0.15
                let! emitted = bernoulli emissionP
                do! observe (emitted = observations.[step])

                let transitionP = if hiddenState then 0.85 else 0.15
                let! nextState = bernoulli transitionP
                return! loop (step + 1) nextState
        }

    dist {
        let! initial = bernoulli 0.5
        return! loop 0 initial
    }

let private hiddenMarkovFinalPosteriorExact (observations: bool[]) =
    let step (hotMass, coldMass) observed =
        let emittedHot = hotMass * (if observed then 0.85 else 0.15)
        let emittedCold = coldMass * (if observed then 0.15 else 0.85)
        let nextHot = emittedHot * 0.85 + emittedCold * 0.15
        let nextCold = emittedHot * 0.15 + emittedCold * 0.85
        nextHot, nextCold

    let finalHot, finalCold = Array.fold step (0.5, 0.5) observations
    normalizeMap (Map.ofList [ "final=Hot", finalHot; "final=Cold", finalCold ])

let private tests =
    [ { Name = "Hard evidence posterior"
        Why = "Bounded stochastic frontier search on repeated hard evidence."
        Distribution = hardEvidencePosterior [ true; true; true; true; true; true; false; true ]
        Samples = 2500
        MaxDepth = 40
        BeamConfig = enhancedBeamConfig 2500 128 17
        RawTopCount = Some 10
        Aggregations = [ "bias posterior", id ] }

      { Name = "Oleg-inspired rare evidence"
        Why = "Bounded stochastic frontier search on the buried-evidence Oleg-style case."
        Distribution = olegInspiredPosterior
        Samples = 2500
        MaxDepth = 40
        BeamConfig = enhancedBeamConfig 2500 128 17
        RawTopCount = Some 10
        Aggregations = [ "warmup posterior", id ] }

      { Name = "Oleg-inspired with exact local likelihood"
        Why = "The same frontier search after rewriting the local rejection step with exact_local_observe."
        Distribution = olegInspiredPosteriorFactored
        Samples = 2500
        MaxDepth = 40
        BeamConfig = enhancedBeamConfig 2500 128 17
        RawTopCount = Some 10
        Aggregations = [ "warmup posterior", id ] }

      { Name = "Sequential HMM evidence"
        Why = "A sequential model to test future streaming-compatible frontier search behavior."
        Distribution = hiddenMarkovPosterior [| true; true; true; false; true; true; false; true |]
        Samples = 2500
        MaxDepth = 64
        BeamConfig = enhancedBeamConfig 2500 256 17
        RawTopCount = Some 10
        Aggregations = [ "final state", id ] } ]

let private printRunSummary testCase =
    let exactDist = exact testCase.Distribution
    let beamDist, beamStats = quiet (fun () -> stochasticBeam testCase.BeamConfig testCase.Distribution)
    let beamTimingStats =
        [ for runIndex in 0 .. 4 ->
            let _, stats = quiet (fun () -> stochasticBeam { testCase.BeamConfig with Seed = testCase.BeamConfig.Seed + runIndex } testCase.Distribution)
            stats ]

    let estimates =
        [ { Name = "Importance"; Dist = importance testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Importance (no pre-explore)"; Dist = importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Path"; Dist = path testCase.Samples testCase.Distribution }
          { Name = sprintf "Stochastic beam (%s)" (formatBeamStats beamStats); Dist = beamDist } ]

    printfn "\n=== %s ===" testCase.Name
    printfn "%s" testCase.Why
    printfn "Samples / beam width: %d" testCase.Samples
    printfn "Beam config: %s" (formatBeamConfig testCase.BeamConfig)
    printBeamSpaceDiagnostics beamStats
    printBeamTimingDiagnostics
        "Beam timing profile (ms, avg over 5 runs):"
        (averageBy (fun stats -> stats.AdvanceMs) beamTimingStats)
        (averageBy (fun stats -> stats.CullMs) beamTimingStats)
        (averageBy (fun stats -> stats.TotalMs) beamTimingStats)

    match testCase.RawTopCount with
    | Some count -> printTop (sprintf "Exact (top %d)" count) count exactDist
    | None -> ()

    printAggregates "Exact" testCase.Aggregations exactDist

    for estimate in estimates do
        match testCase.RawTopCount with
        | Some count -> printTop (sprintf "%s (top %d)" estimate.Name count) count estimate.Dist
        | None -> ()

        printAggregates estimate.Name testCase.Aggregations estimate.Dist
        printfn "L1(exact, %s) = %.6f" estimate.Name (l1Distance exactDist estimate.Dist)

let private printStability testCase =
    let exactDist = exact testCase.Distribution
    printfn "\n--- Stability: %s ---" testCase.Name

    let importanceErrors =
        [ for _ in 1 .. 12 ->
            let estimate = importance testCase.Samples testCase.MaxDepth testCase.Distribution
            l1Distance exactDist estimate ]

    let pathErrors =
        [ for _ in 1 .. 12 ->
            let estimate = path testCase.Samples testCase.Distribution
            l1Distance exactDist estimate ]

    let beamErrors =
        [ for runIndex in 0 .. 11 ->
            let estimate = quiet (fun () -> stochasticBeamLean { testCase.BeamConfig with Seed = testCase.BeamConfig.Seed + runIndex } testCase.Distribution)
            l1Distance exactDist estimate ]

    printfn "Mean L1 importance over 12 runs = %.6f" (List.average importanceErrors)
    printfn "Mean L1 path over 12 runs       = %.6f" (List.average pathErrors)
    printfn "Mean L1 stochastic beam over 12 runs = %.6f" (List.average beamErrors)

let private printSpeed testCase =
    printfn "\n--- Speed: %s ---" testCase.Name
    printfn "Average ms/run over 5 runs"
    let beamTimingStats =
        [ for runIndex in 0 .. 4 ->
            let _, stats = quiet (fun () -> stochasticBeam { testCase.BeamConfig with Seed = testCase.BeamConfig.Seed + runIndex } testCase.Distribution)
            stats ]

    printfn "importance                 %.3f" (benchmark 5 (fun () -> importance testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "importance-no-pre          %.3f" (benchmark 5 (fun () -> importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "path                       %.3f" (benchmark 5 (fun () -> path testCase.Samples testCase.Distribution))
    printfn "stochastic-beam            %.3f" (benchmark 5 (fun () -> quiet (fun () -> stochasticBeam testCase.BeamConfig testCase.Distribution) |> ignore))
    printfn "beam-internal advance      %.3f" (averageBy (fun stats -> stats.AdvanceMs) beamTimingStats)
    printfn "beam-internal cull         %.3f" (averageBy (fun stats -> stats.CullMs) beamTimingStats)
    printfn "beam-internal total        %.3f" (averageBy (fun stats -> stats.TotalMs) beamTimingStats)

let private printSequentialHmmPrefixStudy () =
    let observations = [| true; true; true; false; true; true; false; true |]
    printfn "\n=== Sequential HMM Prefix Study ==="
    printfn "Each row uses the observation prefix of the given length and compares exact, importance, path, and stochastic beam."
    printfn "Columns: prefix-length | L1 importance | L1 path | L1 beam | ms importance | ms path | ms beam"

    for prefixLength in 1 .. observations.Length do
        let prefix = observations.[0 .. prefixLength - 1]
        let distribution = hiddenMarkovPosterior prefix
        let config = enhancedBeamConfig 2500 256 (17 + prefixLength)
        let exactDist = exact distribution
        let importanceDist = importance 2500 64 distribution
        let pathDist = path 2500 distribution
        let beamDist = quiet (fun () -> stochasticBeamLean config distribution)

        let importanceMs = benchmark 10 (fun () -> importance 2500 64 distribution)
        let pathMs = benchmark 10 (fun () -> path 2500 distribution)
        let beamMs = benchmark 10 (fun () -> quiet (fun () -> stochasticBeamLean config distribution) |> ignore)

        printfn "%13d | %13.6f | %7.6f | %7.6f | %13.3f | %7.3f | %7.3f" prefixLength (l1Distance exactDist importanceDist) (l1Distance exactDist pathDist) (l1Distance exactDist beamDist) importanceMs pathMs beamMs

let private printSequentialHmmIncrementalStudy () =
    let observations = [| true; true; true; false; true; true; false; true |]
    let distribution = hiddenMarkovPosterior observations
    let config = enhancedBeamConfig 2500 256 17
    let checkpoints = [ 1; 2; 4; 8; 12; 17 ]
    printfn "\n=== Incremental Beam State Study ==="
    printfn "This resumes the same stochastic beam state across multiple checkpoints rather than restarting from scratch."

    let mutable state = initBeamState config distribution
    let mutable completedRounds = 0

    for checkpoint in checkpoints do
        state <- advanceBeamRounds (checkpoint - completedRounds) state
        completedRounds <- checkpoint
        printBeamStateSnapshot (sprintf "Beam state after %d rounds" checkpoint) state

    let resumedMs =
        benchmark 5 (fun () ->
            let mutable resumedState = initBeamState config distribution
            let mutable priorCheckpoint = 0

            for checkpoint in checkpoints do
                resumedState <- advanceBeamRoundsLean (checkpoint - priorCheckpoint) resumedState
                priorCheckpoint <- checkpoint

            snapshotBeamState resumedState |> ignore)

    let oneShotMs =
        benchmark 5 (fun () ->
            quiet (fun () -> stochasticBeamLean config distribution) |> ignore)

    printfn "Incremental resume ms/run over 5 runs = %.3f" resumedMs
    printfn "One-shot beam ms/run over 5 runs      = %.3f" oneShotMs

let private printSequentialHmmAblationStudy () =
    let observations = [| true; true; true; false; true; true; false; true |]
    let distribution = hiddenMarkovPosterior observations
    let exactDist = exact distribution
    let baseline = baselineBeamConfig 2500 256 17
    let variants =
        [
            ("baseline", baseline)
            ("elite-only", baseline |> withEliteRetention 64)
            ("bucket-only", baseline |> withBucketedCulling 32)
            ("lookahead-only", baseline |> withLookahead 1 0.5)
            ("adaptive-only", baseline |> withAdaptiveWidth 625)
            ("all-enabled", enhancedBeamConfig 2500 256 17)
        ]

    printfn "\n=== Sequential HMM Ablation Study ==="
    printfn "One focused case: each row toggles exactly one implemented strategy on top of the same baseline, plus the fully enabled configuration."
    printfn "Columns: variant | mean L1 beam | ms beam | unique-live | selected-reps | config"

    for label, config in variants do
        let meanL1 =
            [ for runIndex in 0 .. 4 ->
                let estimate = quiet (fun () -> stochasticBeamLean { config with Seed = config.Seed + runIndex } distribution)
                l1Distance exactDist estimate ]
            |> List.average

        let beamMs = benchmark 5 (fun () -> quiet (fun () -> stochasticBeamLean config distribution) |> ignore)
        let _, stats = quiet (fun () -> stochasticBeam config distribution)
        printfn "%13s | %13.6f | %7.3f | %11d | %13d | %s" label meanL1 beamMs stats.MaxUniqueLiveWidth stats.MaxSelectedRepresentativeWidth (formatBeamConfig config)

let private printLargerSequentialHmmBenchmark () =
    let observations =
        [|
            true; true; true; false; true; true; false; true
            false; false; true; false; true; false; false; true
            true; false; true; true; false; true; false; false
            true; true; true; false; true; false; true; true
        |]

    let distribution = hiddenMarkovPosterior observations
    let exactDist = hiddenMarkovFinalPosteriorExact observations
    let config = enhancedBeamConfig 2500 512 101
    let importanceDist = importance 2500 256 distribution
    let pathDist = path 2500 distribution
    let beamDist, beamStats = quiet (fun () -> stochasticBeam config distribution)

    printfn "\n=== Larger Sequential HMM Benchmark ==="
    printfn "A longer HMM sequence to stress sequential scaling without using exponential exact-tree enumeration as the reference."
    printfn "Observation length: %d" observations.Length
    printfn "Samples / beam width: %d" 2500
    printfn "Beam config: %s" (formatBeamConfig config)
    printBeamSpaceDiagnostics beamStats
    printBeamTimingDiagnostics "Beam timing profile (ms):" beamStats.AdvanceMs beamStats.CullMs beamStats.TotalMs
    printTop "Exact DP reference (top 2)" 2 exactDist
    printTop "Importance (top 2)" 2 importanceDist
    printTop "Path (top 2)" 2 pathDist
    printTop (sprintf "Stochastic beam (%s) (top 2)" (formatBeamStats beamStats)) 2 beamDist
    printfn "L1(exact-DP, Importance) = %.6f" (l1Distance exactDist importanceDist)
    printfn "L1(exact-DP, Path) = %.6f" (l1Distance exactDist pathDist)
    printfn "L1(exact-DP, Stochastic beam) = %.6f" (l1Distance exactDist beamDist)

    let importanceMs = benchmark 3 (fun () -> importance 2500 256 distribution |> ignore)
    let pathMs = benchmark 3 (fun () -> path 2500 distribution |> ignore)
    let beamMs = benchmark 3 (fun () -> quiet (fun () -> stochasticBeamLean config distribution) |> ignore)

    printfn "importance ms/run            %.3f" importanceMs
    printfn "path ms/run                  %.3f" pathMs
    printfn "stochastic-beam ms/run       %.3f" beamMs

for testCase in tests do
    printRunSummary testCase
    printStability testCase
    printSpeed testCase

printSequentialHmmPrefixStudy ()
printSequentialHmmIncrementalStudy ()
printSequentialHmmAblationStudy ()
printLargerSequentialHmmBenchmark ()