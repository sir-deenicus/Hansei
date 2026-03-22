#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
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
        Live: FrontierItem<string> list
        Answers: Map<string, float>
        Stats: BeamStats
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
    let timer = Stopwatch.StartNew()
    for _ in 1 .. repeats do
        action () |> ignore
    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private averageBy projection items =
    items |> List.averageBy projection

let private addAnswerWeight value weight answers =
    Map.change value (fun existing -> Some (weight + defaultArg existing 0.0)) answers

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

let private candidateBucketKey bucketCount candidate =
    if bucketCount <= 1 then
        0
    else
        let rawHash =
            match candidate with
            | CandidateValue value -> combineHash 97 (Unchecked.hash value)
            | CandidateFrontier frontier ->
                frontier
                |> List.filter (fun (_, weight) -> weight > 0.0)
                |> List.sortByDescending snd
                |> List.truncate 3
                |> List.fold (fun hash (node, weight) ->
                    let quantizedWeight = int (Math.Round(weight * 1024.0))
                    combineHash (combineHash hash (nodeBucketHash node)) quantizedWeight) 53

        abs rawHash % bucketCount

let private systematicResample rngState beamWidth (items: (Candidate<'T> * float) list) =
    let totalWeight = items |> List.sumBy snd

    if beamWidth <= 0 || totalWeight <= 0.0 then
        [], rngState
    else
        let step = totalWeight / float beamWidth
        let nextRngState, offset = nextUniform rngState
        let mutable cursor = offset * step
        let offspringWeight = totalWeight / float beamWidth
        let sorted = items |> List.toArray
        let resampled = ResizeArray<CandidateShare<'T>>()
        let mutable cumulative = snd sorted.[0]
        let mutable index = 0
        let mutable currentIndex = -1
        let mutable currentOccupancy = 0

        let flushCurrent () =
            if currentIndex >= 0 && currentOccupancy > 0 then
                resampled.Add
                    {
                        Candidate = fst sorted.[currentIndex]
                        Occupancy = currentOccupancy
                        Weight = offspringWeight * float currentOccupancy
                    }

        for _ in 1 .. beamWidth do
            while cursor > cumulative && index + 1 < sorted.Length do
                index <- index + 1
                cumulative <- cumulative + snd sorted.[index]

            if index = currentIndex then
                currentOccupancy <- currentOccupancy + 1
            else
                flushCurrent ()
                currentIndex <- index
                currentOccupancy <- 1

            cursor <- cursor + step

        flushCurrent ()
        List.ofSeq resampled, nextRngState

let private reserveBucketRepresentatives bucketCount slotCount (items: (Candidate<string> * float) list) =
    if bucketCount <= 1 || slotCount <= 0 || List.isEmpty items then
        [], items
    else
        let grouped =
            items
            |> List.groupBy (fun (candidate, _) -> candidateBucketKey bucketCount candidate)
            |> List.map (fun (bucketKey, bucketItems) ->
                let sortedItems = bucketItems |> List.sortByDescending snd
                let totalWeight = sortedItems |> List.sumBy snd
                bucketKey, totalWeight, sortedItems)
            |> List.sortByDescending (fun (_, totalWeight, _) -> totalWeight)

        let selectedBuckets = grouped |> List.truncate slotCount
        let selectedKeys = selectedBuckets |> List.map (fun (bucketKey, _, _) -> bucketKey) |> Set.ofList

        let reservedShares =
            selectedBuckets
            |> List.choose (fun (_, _, bucketItems) ->
                match bucketItems with
                | [] -> None
                | (candidate, weight) :: _ ->
                    Some
                        {
                            Candidate = candidate
                            Occupancy = 1
                            Weight = weight
                        })

        let remainingItems =
            grouped
            |> List.collect (fun (bucketKey, _, bucketItems) ->
                if Set.contains bucketKey selectedKeys then
                    match bucketItems with
                    | [] -> []
                    | _ :: rest -> rest
                else
                    bucketItems)

        reservedShares, remainingItems

let private systematicCull rngState beamWidth eliteCount diversityBucketCount (items: (Candidate<string> * float) list) =
    if beamWidth <= 0 || List.isEmpty items then
        [], rngState
    else
        let maxEliteCount =
            if items.Length <= 1 then 0
            else min (beamWidth - 1) (items.Length - 1)

        let retainedEliteCount = min (max 0 eliteCount) maxEliteCount

        if retainedEliteCount <= 0 then
            let reservedBucketShares, bucketRemainder = reserveBucketRepresentatives diversityBucketCount beamWidth items
            let resampledShares, nextRngState = systematicResample rngState (beamWidth - reservedBucketShares.Length) bucketRemainder
            reservedBucketShares @ resampledShares, nextRngState
        else
            let sortedByWeight = items |> List.sortByDescending snd
            let eliteItems = sortedByWeight |> List.truncate retainedEliteCount
            let remainderItems = sortedByWeight |> List.skip retainedEliteCount
            let eliteShares =
                eliteItems
                |> List.map (fun (candidate, weight) ->
                    {
                        Candidate = candidate
                        Occupancy = 1
                        Weight = weight
                    })

            let reservedBucketShares, bucketRemainder = reserveBucketRepresentatives diversityBucketCount (beamWidth - retainedEliteCount) remainderItems
            let resampledShares, nextRngState = systematicResample rngState (beamWidth - retainedEliteCount - reservedBucketShares.Length) bucketRemainder
            eliteShares @ reservedBucketShares @ resampledShares, nextRngState

let private beamStateComplete state =
    state.Live.IsEmpty || state.Stats.Rounds >= state.Config.MaxRounds

let private initBeamState config distribution =
    if config.BeamWidth <= 0 then
        invalidArg (nameof config.BeamWidth) "BeamWidth must be positive."
    elif config.MaxRounds <= 0 then
        invalidArg (nameof config.MaxRounds) "MaxRounds must be positive."
    elif config.EliteCount < 0 then
        invalidArg (nameof config.EliteCount) "EliteCount cannot be negative."
    elif config.DiversityBucketCount < 0 then
        invalidArg (nameof config.DiversityBucketCount) "DiversityBucketCount cannot be negative."

    {
        Config = config
        RngState = uint64 (max 1 config.Seed)
        Live = [ { Frontier = distribution; Occupancy = config.BeamWidth; Weight = 1.0 } ]
        Answers = Map.empty
        Stats = emptyBeamStats config.BeamWidth
    }

let private advanceBeamRound state =
    if beamStateComplete state then
        state
    else
        let roundTimer = Stopwatch.StartNew()
        let advanceTimer = Stopwatch.StartNew()
        let candidatePool = ResizeArray<Candidate<string> * float>()
        let mutable answers = state.Answers
        let roundNumber = state.Stats.Rounds + 1
        let maxUniqueLiveWidth = max state.Stats.MaxUniqueLiveWidth state.Live.Length
        let maxLiveOccupancy = max state.Stats.MaxLiveOccupancy (state.Live |> List.sumBy (fun item -> item.Occupancy))

        for item in state.Live do
            match collapseForced item.Weight item.Frontier with
            | Dead -> ()
            | Done (value, weight) -> answers <- addAnswerWeight value weight answers
            | Branching (frontier, weight) -> appendCandidates weight frontier candidatePool

        advanceTimer.Stop()
        let candidateItems = List.ofSeq candidatePool
        let maxUniqueCandidateWidth = max state.Stats.MaxUniqueCandidateWidth candidateItems.Length

        let cullTimer = Stopwatch.StartNew()
        let selected, nextRngState = systematicCull state.RngState state.Config.BeamWidth state.Config.EliteCount state.Config.DiversityBucketCount candidateItems
        cullTimer.Stop()
        let maxSelectedRepresentativeWidth = max state.Stats.MaxSelectedRepresentativeWidth selected.Length
        let nextLive = ResizeArray<FrontierItem<string>>(selected.Length)

        for selectedCandidate in selected do
            match selectedCandidate.Candidate with
            | CandidateValue value -> answers <- addAnswerWeight value selectedCandidate.Weight answers
            | CandidateFrontier frontier ->
                nextLive.Add
                    {
                        Frontier = frontier
                        Occupancy = selectedCandidate.Occupancy
                        Weight = selectedCandidate.Weight
                    }

        roundTimer.Stop()

        {
            state with
                RngState = nextRngState
                Live = List.ofSeq nextLive
                Answers = answers
                Stats =
                    {
                        Rounds = roundNumber
                        MaxUniqueLiveWidth = maxUniqueLiveWidth
                        MaxLiveOccupancy = maxLiveOccupancy
                        MaxUniqueCandidateWidth = maxUniqueCandidateWidth
                        MaxSelectedRepresentativeWidth = maxSelectedRepresentativeWidth
                        AdvanceMs = state.Stats.AdvanceMs + advanceTimer.Elapsed.TotalMilliseconds
                        CullMs = state.Stats.CullMs + cullTimer.Elapsed.TotalMilliseconds
                        TotalMs = state.Stats.TotalMs + roundTimer.Elapsed.TotalMilliseconds
                    }
        }

let private advanceBeamRounds roundCount state =
    let rec loop remaining currentState =
        if remaining <= 0 || beamStateComplete currentState then
            currentState
        else
            loop (remaining - 1) (advanceBeamRound currentState)

    loop roundCount state

let private runBeamToCompletion state =
    let rec loop currentState =
        if beamStateComplete currentState then
            currentState
        else
            loop (advanceBeamRound currentState)

    loop state

let private snapshotBeamState state =
    let answersWithImmediateDone, pendingUniqueLive, pendingLiveSlots, pendingMass =
        ((state.Answers, 0, 0, 0.0), state.Live)
        ||> List.fold (fun (answers, uniqueLive, liveSlots, pendingMass) item ->
            match collapseForced item.Weight item.Frontier with
            | Done (value, weight) -> addAnswerWeight value weight answers, uniqueLive, liveSlots, pendingMass
            | Dead -> answers, uniqueLive, liveSlots, pendingMass
            | Branching _ -> answers, uniqueLive + 1, liveSlots + item.Occupancy, pendingMass + item.Weight)

    let dist = normalizeMap answersWithImmediateDone

    {
        Dist = dist
        PendingUniqueLive = pendingUniqueLive
        PendingLiveSlots = pendingLiveSlots
        PendingMass = pendingMass
    }

let private stochasticBeam (config: BeamConfig) distribution =
    let finalState =
        distribution
        |> initBeamState config
        |> runBeamToCompletion

    let snapshot = snapshotBeamState finalState
    snapshot.Dist, finalState.Stats

let private formatBeamStats stats =
    sprintf
            "rounds=%d, unique-live=%d, live-slots=%d, unique-candidates=%d, selected-reps=%d"
            stats.Rounds
            stats.MaxUniqueLiveWidth
            stats.MaxLiveOccupancy
            stats.MaxUniqueCandidateWidth
            stats.MaxSelectedRepresentativeWidth

let private printBeamDiagnostics stats =
        printfn "Beam space profile: unique-live=%d, live-slots=%d, unique-candidates=%d, selected-reps=%d" stats.MaxUniqueLiveWidth stats.MaxLiveOccupancy stats.MaxUniqueCandidateWidth stats.MaxSelectedRepresentativeWidth
        printfn "Beam timing profile (ms): advance=%.3f, cull=%.3f, total=%.3f" stats.AdvanceMs stats.CullMs stats.TotalMs

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

let private tests =
    [ { Name = "Hard evidence posterior"
        Why = "Bounded stochastic frontier search on repeated hard evidence."
        Distribution = hardEvidencePosterior [ true; true; true; true; true; true; false; true ]
        Samples = 2500
        MaxDepth = 40
        BeamConfig = { BeamWidth = 2500; MaxRounds = 128; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "bias posterior", id ] }

      { Name = "Oleg-inspired rare evidence"
        Why = "Bounded stochastic frontier search on the buried-evidence Oleg-style case."
        Distribution = olegInspiredPosterior
        Samples = 2500
        MaxDepth = 40
        BeamConfig = { BeamWidth = 2500; MaxRounds = 128; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "warmup posterior", id ] }

      { Name = "Oleg-inspired with exact local likelihood"
        Why = "The same frontier search after rewriting the local rejection step with exact_local_observe."
        Distribution = olegInspiredPosteriorFactored
        Samples = 2500
        MaxDepth = 40
        BeamConfig = { BeamWidth = 2500; MaxRounds = 128; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "warmup posterior", id ] }

      { Name = "Sequential HMM evidence"
        Why = "A sequential model to test future streaming-compatible frontier search behavior."
        Distribution = hiddenMarkovPosterior [| true; true; true; false; true; true; false; true |]
        Samples = 2500
        MaxDepth = 64
        BeamConfig = { BeamWidth = 2500; MaxRounds = 256; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "final state", id ] } ]

let private printRunSummary testCase =
    let exactDist = exact testCase.Distribution
    let beamDist, beamStats = quiet (fun () -> stochasticBeam testCase.BeamConfig testCase.Distribution)

    let estimates =
        [ { Name = "Importance"; Dist = importance testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Importance (no pre-explore)"; Dist = importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Path"; Dist = path testCase.Samples testCase.Distribution }
          { Name = sprintf "Stochastic beam (%s)" (formatBeamStats beamStats); Dist = beamDist } ]

    printfn "\n=== %s ===" testCase.Name
    printfn "%s" testCase.Why
    printfn "Samples / beam width: %d" testCase.Samples
    printBeamDiagnostics beamStats

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
            let estimate, _ = quiet (fun () -> stochasticBeam { testCase.BeamConfig with Seed = testCase.BeamConfig.Seed + runIndex } testCase.Distribution)
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
        let exactDist = exact distribution
        let importanceDist = importance 2500 64 distribution
        let pathDist = path 2500 distribution
        let beamDist, _ = quiet (fun () -> stochasticBeam { BeamWidth = 2500; MaxRounds = 256; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 + prefixLength } distribution)

        let importanceMs = benchmark 3 (fun () -> importance 2500 64 distribution)
        let pathMs = benchmark 3 (fun () -> path 2500 distribution)
        let beamMs = benchmark 3 (fun () -> quiet (fun () -> stochasticBeam { BeamWidth = 2500; MaxRounds = 256; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 + prefixLength } distribution) |> ignore)

        printfn "%13d | %13.6f | %7.6f | %7.6f | %13.3f | %7.3f | %7.3f" prefixLength (l1Distance exactDist importanceDist) (l1Distance exactDist pathDist) (l1Distance exactDist beamDist) importanceMs pathMs beamMs

let private printSequentialHmmIncrementalStudy () =
    let observations = [| true; true; true; false; true; true; false; true |]
    let distribution = hiddenMarkovPosterior observations
    let config = { BeamWidth = 2500; MaxRounds = 256; EliteCount = 64; DiversityBucketCount = 32; Seed = 17 }
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
                resumedState <- advanceBeamRounds (checkpoint - priorCheckpoint) resumedState
                priorCheckpoint <- checkpoint

            snapshotBeamState resumedState |> ignore)

    let oneShotMs =
        benchmark 5 (fun () ->
            quiet (fun () -> stochasticBeam config distribution) |> ignore)

    printfn "Incremental resume ms/run over 5 runs = %.3f" resumedMs
    printfn "One-shot beam ms/run over 5 runs      = %.3f" oneShotMs

for testCase in tests do
    printRunSummary testCase
    printStability testCase
    printSpeed testCase

printSequentialHmmPrefixStudy ()
printSequentialHmmIncrementalStudy ()