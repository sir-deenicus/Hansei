#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open System.IO
open Hansei.Core.List
open Hansei.Core.List.Distributions
open Hansei.Utils

type SamplerResult =
    { Name: string; Dist: Map<string, float> }

type SMCConfig =
    {
        NumParticles: int
        MaxStages: int
        ResampleEssRatio: float
        Seed: int
    }

type TestCase =
    {
        Name: string
        Why: string
        Distribution: ProbabilitySpace<string>
        Samples: int
        MaxDepth: int
        SmcConfig: SMCConfig
        RawTopCount: int option
        Aggregations: (string * (string -> string)) list
    }

type private LiveParticle<'T> =
    {
        Frontier: ProbabilitySpace<'T>
        Weight: float
    }

type private Advancement<'T> =
    | Dead
    | Done of 'T * float
    | Staged of ProbabilitySpace<'T> * float

type private LocalCandidate<'T> =
    | CandidateValue of 'T
    | CandidateFrontier of ProbabilitySpace<'T>

type private SMCStats =
    {
        Stages: int
        Resamples: int
        MaxLiveParticles: int
        MaxStagedWidth: int
    }

type StageMarkedModel<'State, 'T> =
    {
        Initial: ProbabilitySpace<'State>
        Steps: seq<'State -> ProbabilitySpace<'State>>
        Finalize: 'State -> 'T
    }

type private StateParticle<'State> =
    {
        State: 'State
        Weight: float
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

let private collapseForcedStage initialWeight initialFrontier =
    let rec loop weight frontier =
        let live = frontier |> List.filter (fun (_, p) -> p > 0.0)

        match live with
        | [] -> Dead
        | [Value value, p] -> Done (value, weight * p)
        | [ContinuedSubTree next, p] -> loop (weight * p) (force next)
        | branches -> Staged (branches, weight)

    loop initialWeight initialFrontier

let private prepareCandidates frontier =
    frontier
    |> List.choose (fun (node, branchWeight) ->
        if branchWeight <= 0.0 then
            None
        else
            match node with
            | Value value -> Some (CandidateValue value, branchWeight)
            | ContinuedSubTree next ->
                match collapseForcedStage branchWeight (force next) with
                | Dead -> None
                | Done (value, weight) -> Some (CandidateValue value, weight)
                | Staged (preparedFrontier, weight) -> Some (CandidateFrontier preparedFrontier, weight))

let private sampleCandidate (rng: Random) candidates =
    let totalMass = candidates |> List.sumBy snd

    if totalMass <= 0.0 || Double.IsNaN totalMass || Double.IsInfinity totalMass then
        None
    else
        let threshold = rng.NextDouble() * totalMass

        let rec loop cumulative = function
            | [] -> None
            | (candidate, weight) :: rest ->
                let cumulative' = cumulative + weight
                if threshold <= cumulative' then Some (candidate, totalMass) else loop cumulative' rest

        loop 0.0 candidates

let private effectiveSampleSize (particles: ResizeArray<LiveParticle<'T>>) =
    let totalWeight = particles |> Seq.sumBy (fun particle -> particle.Weight)

    if totalWeight <= 0.0 then
        0.0
    else
        let sumSquares =
            particles
            |> Seq.sumBy (fun particle ->
                let normalized = particle.Weight / totalWeight
                normalized * normalized)

        if sumSquares <= 0.0 then 0.0 else 1.0 / sumSquares

let private systematicResample (rng: Random) (particles: ResizeArray<LiveParticle<'T>>) =
    let count = particles.Count
    let totalWeight = particles |> Seq.sumBy (fun particle -> particle.Weight)

    if count = 0 || totalWeight <= 0.0 then
        ResizeArray()
    else
        let sorted = particles |> Seq.toArray
        let offspringWeight = totalWeight / float count
        let step = totalWeight / float count
        let mutable cursor = rng.NextDouble() * step
        let resampled = ResizeArray<LiveParticle<'T>>(count)
        let mutable cumulative = sorted.[0].Weight
        let mutable index = 0

        for _ in 1 .. count do
            while cursor > cumulative && index + 1 < sorted.Length do
                index <- index + 1
                cumulative <- cumulative + sorted.[index].Weight

            resampled.Add { Frontier = sorted.[index].Frontier; Weight = offspringWeight }
            cursor <- cursor + step

        resampled

let private systematicResampleStates (rng: Random) (particles: ResizeArray<StateParticle<'State>>) =
    let count = particles.Count
    let totalWeight = particles |> Seq.sumBy (fun particle -> particle.Weight)

    if count = 0 || totalWeight <= 0.0 then
        ResizeArray()
    else
        let sorted = particles |> Seq.toArray
        let offspringWeight = totalWeight / float count
        let step = totalWeight / float count
        let mutable cursor = rng.NextDouble() * step
        let resampled = ResizeArray<StateParticle<'State>>(count)
        let mutable cumulative = sorted.[0].Weight
        let mutable index = 0

        for _ in 1 .. count do
            while cursor > cumulative && index + 1 < sorted.Length do
                index <- index + 1
                cumulative <- cumulative + sorted.[index].Weight

            resampled.Add { State = sorted.[index].State; Weight = offspringWeight }
            cursor <- cursor + step

        resampled

let private exactStateMasses (dist: ProbabilitySpace<'State>) =
    explore None dist
    |> List.fold (fun acc (node, weight) ->
        match node with
        | Value value -> Map.change value (fun existing -> Some (weight + defaultArg existing 0.0)) acc
        | ContinuedSubTree _ -> acc) Map.empty

let private sampleWeightedMap (rng: Random) (dist: Map<'State, float>) =
    let totalMass = dist |> Seq.sumBy (fun kv -> kv.Value)

    if totalMass <= 0.0 || Double.IsNaN totalMass || Double.IsInfinity totalMass then
        None
    else
        let threshold = rng.NextDouble() * totalMass

        let rec loop cumulative = function
            | [] -> None
            | (value, weight) :: rest ->
                let cumulative' = cumulative + weight
                if threshold <= cumulative' then Some (value, totalMass, dist.Count) else loop cumulative' rest

        loop 0.0 (dist |> Map.toList)

let private effectiveSampleSizeStates (particles: ResizeArray<StateParticle<'State>>) =
    let totalWeight = particles |> Seq.sumBy (fun particle -> particle.Weight)

    if totalWeight <= 0.0 then
        0.0
    else
        let sumSquares =
            particles
            |> Seq.sumBy (fun particle ->
                let normalized = particle.Weight / totalWeight
                normalized * normalized)

        if sumSquares <= 0.0 then 0.0 else 1.0 / sumSquares

let private stagedSmc (config: SMCConfig) (model: StageMarkedModel<'State, 'T>) =
    let rng = Random(config.Seed)
    let answers = System.Collections.Generic.Dictionary<'T, float>()
    let mutable stages = 0
    let mutable resamples = 0
    let mutable maxLiveParticles = config.NumParticles
    let mutable maxStagedWidth = 0
    let mutable particles = ResizeArray<StateParticle<'State>>(config.NumParticles)

    let addAnswer value weight =
        let existing =
            match answers.TryGetValue value with
            | true, current -> current
            | false, _ -> 0.0

        answers.[value] <- existing + weight

    match sampleWeightedMap rng (exactStateMasses model.Initial) with
    | None ->
        Map.empty,
        { Stages = 0; Resamples = 0; MaxLiveParticles = 0; MaxStagedWidth = 0 }
    | Some (initialState, initialMass, initialWidth) ->
        maxStagedWidth <- initialWidth

        for _ in 1 .. config.NumParticles do
            particles.Add { State = initialState; Weight = initialMass }

        use enumerator = model.Steps.GetEnumerator()

        while particles.Count > 0 && stages < config.MaxStages && enumerator.MoveNext() do
            stages <- stages + 1
            let stepFn = enumerator.Current
            let nextParticles = ResizeArray<StateParticle<'State>>(particles.Count)

            for particle in particles do
                let nextDist = exactStateMasses (stepFn particle.State)

                match sampleWeightedMap rng nextDist with
                | None -> ()
                | Some (nextState, stageMass, stageWidth) ->
                    maxStagedWidth <- max maxStagedWidth stageWidth
                    nextParticles.Add { State = nextState; Weight = particle.Weight * stageMass }

            maxLiveParticles <- max maxLiveParticles nextParticles.Count

            if nextParticles.Count = 0 then
                particles <- nextParticles
            else
                let ess = effectiveSampleSizeStates nextParticles

                if ess < config.ResampleEssRatio * float nextParticles.Count then
                    resamples <- resamples + 1
                    particles <- systematicResampleStates rng nextParticles
                else
                    particles <- nextParticles

        for particle in particles do
            addAnswer (model.Finalize particle.State) particle.Weight

        let totalWeight = answers.Values |> Seq.sum

        let dist =
            if totalWeight <= 0.0 then
                Map.empty
            else
                answers
                |> Seq.map (fun kv -> kv.Key, kv.Value / totalWeight)
                |> Map.ofSeq

        dist,
        { Stages = stages
          Resamples = resamples
          MaxLiveParticles = maxLiveParticles
          MaxStagedWidth = maxStagedWidth }

let private smc (config: SMCConfig) distribution =
    if config.NumParticles <= 0 then
        invalidArg (nameof config.NumParticles) "NumParticles must be positive."
    elif config.MaxStages <= 0 then
        invalidArg (nameof config.MaxStages) "MaxStages must be positive."
    elif config.ResampleEssRatio <= 0.0 || config.ResampleEssRatio > 1.0 then
        invalidArg (nameof config.ResampleEssRatio) "ResampleEssRatio must be in the interval (0, 1]."

    let rng = Random(config.Seed)
    let answers = System.Collections.Generic.Dictionary<string, float>()
    let mutable stages = 0
    let mutable resamples = 0
    let mutable maxLiveParticles = config.NumParticles
    let mutable maxStagedWidth = 0
    let mutable live = ResizeArray<LiveParticle<string>>(config.NumParticles)

    for _ in 1 .. config.NumParticles do
        live.Add { Frontier = distribution; Weight = 1.0 }

    let addAnswer value weight =
        let existing =
            match answers.TryGetValue value with
            | true, current -> current
            | false, _ -> 0.0

        answers.[value] <- existing + weight

    while live.Count > 0 && stages < config.MaxStages do
        stages <- stages + 1

        let nextLive = ResizeArray<LiveParticle<string>>(live.Count)

        for particle in live do
            match collapseForcedStage particle.Weight particle.Frontier with
            | Dead -> ()
            | Done (value, weight) -> addAnswer value weight
            | Staged (frontier, stagedWeight) ->
                let candidates = prepareCandidates frontier
                maxStagedWidth <- max maxStagedWidth candidates.Length

                match sampleCandidate rng candidates with
                | None -> ()
                | Some (CandidateValue value, totalMass) -> addAnswer value (stagedWeight * totalMass)
                | Some (CandidateFrontier preparedFrontier, totalMass) ->
                    nextLive.Add { Frontier = preparedFrontier; Weight = stagedWeight * totalMass }

        maxLiveParticles <- max maxLiveParticles nextLive.Count

        if nextLive.Count = 0 then
            live <- nextLive
        else
            let ess = effectiveSampleSize nextLive

            if ess < config.ResampleEssRatio * float nextLive.Count then
                resamples <- resamples + 1
                live <- systematicResample rng nextLive
            else
                live <- nextLive

    if live.Count > 0 then
        for particle in live do
            match collapseForcedStage particle.Weight particle.Frontier with
            | Done (value, weight) -> addAnswer value weight
            | _ -> ()

    let totalWeight = answers.Values |> Seq.sum

    let dist =
        if totalWeight <= 0.0 then
            Map.empty
        else
            answers
            |> Seq.map (fun kv -> kv.Key, kv.Value / totalWeight)
            |> Map.ofSeq

    dist,
    { Stages = stages
      Resamples = resamples
      MaxLiveParticles = maxLiveParticles
      MaxStagedWidth = maxStagedWidth }

let private formatSmcStats stats =
    sprintf "stages=%d, resamples=%d, max-live=%d, max-stage-width=%d" stats.Stages stats.Resamples stats.MaxLiveParticles stats.MaxStagedWidth

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

let private hiddenMarkovStageMarked (observations: bool[]) =
    {
        Initial =
            dist {
                let! initial = bernoulli 0.5
                return initial
            }
        Steps =
            observations
            |> Seq.map (fun observed hiddenState ->
                dist {
                    let emissionP = if hiddenState then 0.85 else 0.15
                    let! emitted = bernoulli emissionP
                    do! observe (emitted = observed)

                    let transitionP = if hiddenState then 0.85 else 0.15
                    let! nextState = bernoulli transitionP
                    return nextState
                })
        Finalize = fun hiddenState -> sprintf "final=%s" (if hiddenState then "Hot" else "Cold")
    }

let private tests =
    [ { Name = "Hard evidence posterior"
        Why = "Phase 2 baseline: compare SMC against the existing samplers on repeated hard evidence."
        Distribution = hardEvidencePosterior [ true; true; true; true; true; true; false; true ]
        Samples = 2500
        MaxDepth = 40
        SmcConfig = { NumParticles = 2500; MaxStages = 128; ResampleEssRatio = 0.5; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "bias posterior", id ] }

      { Name = "Oleg-inspired rare evidence"
        Why = "Phase 2 buried-evidence benchmark: local rejection is late and severe."
        Distribution = olegInspiredPosterior
        Samples = 2500
        MaxDepth = 40
        SmcConfig = { NumParticles = 2500; MaxStages = 128; ResampleEssRatio = 0.5; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "warmup posterior", id ] }

      { Name = "Oleg-inspired with exact local likelihood"
        Why = "Phase 2 factored benchmark: local rejection is rewritten into exact local likelihood factors."
        Distribution = olegInspiredPosteriorFactored
        Samples = 2500
        MaxDepth = 40
        SmcConfig = { NumParticles = 2500; MaxStages = 128; ResampleEssRatio = 0.5; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "warmup posterior", id ] }

      { Name = "Sequential HMM evidence"
        Why = "Phase 2 sequential model: evidence arrives incrementally across many stages rather than in one late hard test."
        Distribution = hiddenMarkovPosterior [| true; true; true; false; true; true; false; true |]
        Samples = 2500
        MaxDepth = 64
        SmcConfig = { NumParticles = 2500; MaxStages = 256; ResampleEssRatio = 0.5; Seed = 17 }
        RawTopCount = Some 10
        Aggregations = [ "final state", id ] } ]

let private printRunSummary testCase =
    let exactDist = exact testCase.Distribution
    let smcDist, smcStats = quiet (fun () -> smc testCase.SmcConfig testCase.Distribution)

    let estimates =
        [ { Name = "Importance"; Dist = importance testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Importance (no pre-explore)"; Dist = importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Path"; Dist = path testCase.Samples testCase.Distribution }
          { Name = sprintf "SMC (%s)" (formatSmcStats smcStats); Dist = smcDist } ]

    printfn "\n=== %s ===" testCase.Name
    printfn "%s" testCase.Why
    printfn "Samples / particles: %d" testCase.Samples

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

    let smcErrors =
        [ for runIndex in 0 .. 11 ->
            let estimate, _ = quiet (fun () -> smc { testCase.SmcConfig with Seed = testCase.SmcConfig.Seed + runIndex } testCase.Distribution)
            l1Distance exactDist estimate ]

    printfn "Mean L1 importance over 12 runs = %.6f" (List.average importanceErrors)
    printfn "Mean L1 path over 12 runs       = %.6f" (List.average pathErrors)
    printfn "Mean L1 smc over 12 runs        = %.6f" (List.average smcErrors)

let private printSpeed testCase =
    printfn "\n--- Speed: %s ---" testCase.Name
    printfn "Average ms/run over 5 runs"
    printfn "importance                 %.3f" (benchmark 5 (fun () -> importance testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "importance-no-pre          %.3f" (benchmark 5 (fun () -> importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "path                       %.3f" (benchmark 5 (fun () -> path testCase.Samples testCase.Distribution))
    printfn "smc                        %.3f" (benchmark 5 (fun () -> quiet (fun () -> smc testCase.SmcConfig testCase.Distribution) |> ignore))

let private printStageMarkedComparison () =
    let observations = [| true; true; true; false; true; true; false; true |]
    let exactDist = exact (hiddenMarkovPosterior observations)
    let genericSmcDist, genericStats = quiet (fun () -> smc { NumParticles = 2500; MaxStages = 256; ResampleEssRatio = 0.5; Seed = 17 } (hiddenMarkovPosterior observations))
    let stagedSmcDist, stagedStats = quiet (fun () -> stagedSmc { NumParticles = 2500; MaxStages = 256; ResampleEssRatio = 0.5; Seed = 17 } (hiddenMarkovStageMarked observations))

    printfn "\n=== Stage-Marked SMC Comparison ==="
    printfn "This compares the generic tree-driven SMC prototype with an explicit user-stage variant on the same HMM model."
    printTop "Exact (top 10)" 10 exactDist
    printTop (sprintf "Generic SMC (%s)" (formatSmcStats genericStats)) 10 genericSmcDist
    printTop (sprintf "Stage-marked SMC (%s)" (formatSmcStats stagedStats)) 10 stagedSmcDist
    printfn "L1(exact, Generic SMC) = %.6f" (l1Distance exactDist genericSmcDist)
    printfn "L1(exact, Stage-marked SMC) = %.6f" (l1Distance exactDist stagedSmcDist)
    printfn "Average ms/run over 5 runs"
    printfn "generic smc               %.3f" (benchmark 5 (fun () -> quiet (fun () -> smc { NumParticles = 2500; MaxStages = 256; ResampleEssRatio = 0.5; Seed = 17 } (hiddenMarkovPosterior observations)) |> ignore))
    printfn "stage-marked smc          %.3f" (benchmark 5 (fun () -> quiet (fun () -> stagedSmc { NumParticles = 2500; MaxStages = 256; ResampleEssRatio = 0.5; Seed = 17 } (hiddenMarkovStageMarked observations)) |> ignore))

for testCase in tests do
    printRunSummary testCase
    printStability testCase
    printSpeed testCase

printStageMarkedComparison ()