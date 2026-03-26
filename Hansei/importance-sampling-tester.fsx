#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open System.IO
open Hansei
open Hansei.Probability
open Hansei.Distributions

type SamplerResult =
    { Name: string; Dist: Map<string, float> }

type TestCase =
    {
        Name: string
        Why: string
        Distribution: ProbabilitySpace<string>
        Samples: int
        MaxDepth: int
        MassMetric: (string * (string -> bool)) option
        RawTopCount: int option
        Aggregations: (string * (string -> string)) list
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

let private focusedMass predicate (dist: Map<string, float>) =
    dist |> Seq.sumBy (fun kv -> if predicate kv.Key then kv.Value else 0.0)

let private massMetricValue (_, predicate) dist =
    focusedMass predicate dist

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
    for _ in 1 .. repeats do action () |> ignore
    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private printRunSummary (testCase: TestCase) =
    let exactDist = exact testCase.Distribution

    let estimates =
        [ yield { Name = "Importance"; Dist = importance testCase.Samples testCase.MaxDepth testCase.Distribution }
          yield { Name = "Importance (no pre-explore)"; Dist = importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution }
          yield { Name = "Path"; Dist = path testCase.Samples testCase.Distribution } ]

    printfn "\n=== %s ===" testCase.Name
    printfn "%s" testCase.Why
    printfn "Samples per sampler run: %d" testCase.Samples

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

        match testCase.MassMetric with
        | Some ((metricName, _) as metric) ->
            printfn "%s %s = %.6f" metricName estimate.Name (massMetricValue metric estimate.Dist)
        | None -> ()

    match testCase.MassMetric with
    | Some ((metricName, _) as metric) -> printfn "%s exact = %.6f" metricName (massMetricValue metric exactDist)
    | None -> ()

let private printStability (testCase: TestCase) =
    let exactDist = exact testCase.Distribution
    printfn "\n--- Stability: %s ---" testCase.Name

    let importanceErrors =
        [ for _ in 1 .. 18 ->
            let estimate = importance testCase.Samples testCase.MaxDepth testCase.Distribution
            l1Distance exactDist estimate ]

    let pathErrors =
        [ for _ in 1 .. 18 ->
            let estimate = path testCase.Samples testCase.Distribution
            l1Distance exactDist estimate ]

    printfn "Mean L1 importance over 18 runs = %.6f" (List.average importanceErrors)
    printfn "Mean L1 path over 18 runs       = %.6f" (List.average pathErrors)

    match testCase.MassMetric with
    | Some ((metricName, _) as metric) ->
        let exactFocus = massMetricValue metric exactDist

        let importanceFocusError =
            [ for _ in 1 .. 36 ->
                let estimate = importance testCase.Samples testCase.MaxDepth testCase.Distribution
                abs (massMetricValue metric estimate - exactFocus) ]
            |> List.average

        let pathFocusError =
            [ for _ in 1 .. 36 ->
                let estimate = path testCase.Samples testCase.Distribution
                abs (massMetricValue metric estimate - exactFocus) ]
            |> List.average

        printfn "Mean %s error importance  = %.6f" metricName importanceFocusError
        printfn "Mean %s error path        = %.6f" metricName pathFocusError
    | _ -> ()

let private printSpeed (testCase: TestCase) =
    printfn "\n--- Speed: %s ---" testCase.Name
    printfn "Average ms/run over 6 runs"
    printfn "importance                 %.3f" (benchmark 6 (fun () -> importance testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "importance-no-pre          %.3f" (benchmark 6 (fun () -> importanceNoPre testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "path                       %.3f" (benchmark 6 (fun () -> path testCase.Samples testCase.Distribution))

let private coinPrior = [ 0.05; 0.15; 0.25; 0.35; 0.45; 0.55; 0.65; 0.75; 0.85; 0.95 ]

let private formatBias p = sprintf "p=%.2f" p
let private labelPosterior posterior = ProbabilitySpace.mapDistribution formatBias posterior
let private labelBiasComponent (label: string) =
    let index = label.LastIndexOf("-p=")
    if index >= 0 then label.Substring(index + 1) else label

let private labelRootBucket (label: string) =
    if label.StartsWith("rare-") then "rare" else "heavy"

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
            do! exact_local_observe ((=) (3, 5)) (randomPos ())
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

let private branchPosterior prefix branchId =
    let observations = [ true; true; true; true; false; true; true ]

    dist {
        let! p = uniform coinPrior
        let! biasScale = bernoulli (if prefix = "rare" then 0.8 else 0.35)
        let effectiveP = if biasScale then min 0.98 (p + 0.05) else max 0.02 (p - 0.05)
        return! observeAll effectiveP observations
    }
    |> ProbabilitySpace.mapDistribution (fun p -> sprintf "%s-%03d-%s" prefix branchId (formatBias p))

let private rootWeightedBranches =
    [ yield! [ for i in 1 .. 8 -> ("heavy", i), 0.12 ]
      yield! [ for i in 1 .. 200 -> ("rare", i), 0.0002 ] ]

let private rareRootPosterior =
    dist {
        let! branch = distribution rootWeightedBranches
        let prefix, branchId = branch
        return! branchPosterior prefix branchId
    }

let private tests = [
        {   Name = "Path vs importance under hard evidence"
            Why = "Repeated hard evidence creates many dead or tiny-mass paths. Importance keeps weighted evidence contributions, while path sampling tends to spend more of its budget on low-yield trajectories."
            Distribution = hardEvidencePosterior [ true; true; true; true; true; true; false; true ]
            Samples = 2500
            MaxDepth = 40
            MassMetric = None
            RawTopCount = Some 10
            Aggregations = [ "bias posterior", id ] }

        {   Name = "Rare root mixture limitation case"
            Why = "This keeps the rare-root scenario because it is still informative: even when there is a rare early bucket, the harder problem is the posterior work inside each branch. Prepared importance helps, but root-level coverage alone is not the main story."
            Distribution = rareRootPosterior
            Samples = 2500
            MaxDepth = 40
            MassMetric = Some ("Rare bucket mass", fun label -> label.StartsWith("rare-"))
            RawTopCount = None
            Aggregations = [ "root bucket mass", labelRootBucket; "bias posterior", labelBiasComponent ] }

        {   Name = "Oleg-inspired rare evidence"
            Why = "This adapts Oleg's repeated rare-evidence pattern. The hard part is deep evidence, not root-mode coverage, so prepared importance should strongly beat path while no-pre-explore importance exposes how much the shallow preparation is buying."
            Distribution = olegInspiredPosterior
            Samples = 2500
            MaxDepth = 40
            MassMetric = None
            RawTopCount = Some 10
            Aggregations = [ "warmup posterior", id ] }

        {   Name = "Oleg-inspired with exact local likelihood"
            Why = "This rewrites each rare gate using exact_local_observe over the local position model. It removes one buried rejection step per gate and shows what model-side likelihood factoring buys without changing the outer model structure."
            Distribution = olegInspiredPosteriorFactored
            Samples = 2500
            MaxDepth = 40
            MassMetric = None
            RawTopCount = Some 10
            Aggregations = [ "warmup posterior", id ] } ]

for testCase in tests do
    printRunSummary testCase
    printStability testCase
    printSpeed testCase

let private printIncrementalState label topCount snapshot completed remaining =
    printfn "%s completed=%d remaining=%d" label completed remaining
    printTop (sprintf "%s snapshot" label) topCount (normalizedValues snapshot)

let private printStateStep label topCount hasSnapshot completed remaining snapshot =
    printfn "%s completed=%d remaining=%d hasSnapshot=%b" label completed remaining hasSnapshot

    if hasSnapshot then
        printTop (sprintf "%s snapshot" label) topCount (normalizedValues snapshot)

let private printIncrementalApiDemo () =
    let demo = List.head tests
    let chunkSize = 500

    printfn "\n=== Incremental API Demo ==="
    printfn "Test case: %s" demo.Name
    printfn "Chunk size: %d" chunkSize

    let importanceState = Model.ImportanceSamplesState(demo.Distribution, demo.Samples, demo.MaxDepth, preExplore = false)
    printfn "\nRaw importance state-machine API"
    printfn "  let state0 = Model.ImportanceSamplesState(distr, samples, maxDepth, preExplore = false)"
    printfn "  let state1 = state0.Advance(chunkSize)"
    printfn "  let snapshot = state1.Snapshot()"

    let importanceState1 = importanceState.Advance(chunkSize)
    let importanceState2 = importanceState1.Advance(chunkSize)
    printIncrementalState "importance state1" 5 (importanceState1.Snapshot()) importanceState1.CompletedSamples importanceState1.RemainingSamples
    printIncrementalState "importance state2" 5 (importanceState2.Snapshot()) importanceState2.CompletedSamples importanceState2.RemainingSamples

    printfn "\nYielded importance snapshot API"
    printfn "  Model.ImportanceSamplesState(distr, samples, maxDepth, preExplore = false).ToSnapshotSeq(chunkSize)"

    (Model.ImportanceSamplesState(demo.Distribution, demo.Samples, demo.MaxDepth, preExplore = false)).ToSnapshotSeq(chunkSize)
    |> Seq.truncate 3
    |> Seq.iteri (fun index snapshot ->
        printTop (sprintf "importance seq snapshot %d" (index + 1)) 5 (normalizedValues snapshot))

    printfn "\nYielded importance state API"
    printfn "  Model.ImportanceSamplesState(distr, samples, maxDepth, preExplore = false).ToStateSeq(chunkSize)"

    (Model.ImportanceSamplesState(demo.Distribution, demo.Samples, demo.MaxDepth, preExplore = false)).ToStateSeq(chunkSize)
    |> Seq.truncate 3
    |> Seq.iteri (fun index state ->
        printStateStep
            (sprintf "importance seq state %d" (index + 1))
            5
            state.HasSnapshot
            state.CompletedSamples
            state.RemainingSamples
            (state.Snapshot()))

    let pathState = Model.PathSampleState(demo.Distribution, demo.Samples)
    printfn "\nRaw path state-machine API"
    printfn "  let state0 = Model.PathSampleState(distr, samples)"
    printfn "  let state1 = state0.Advance(chunkSize)"
    printfn "  let snapshot = state1.Snapshot()"

    let pathState1 = pathState.Advance(chunkSize)
    let pathState2 = pathState1.Advance(chunkSize)
    printIncrementalState "path state1" 5 (pathState1.Snapshot()) pathState1.CompletedSamples pathState1.RemainingSamples
    printIncrementalState "path state2" 5 (pathState2.Snapshot()) pathState2.CompletedSamples pathState2.RemainingSamples

    printfn "\nYielded path snapshot API"
    printfn "  Model.PathSampleState(distr, samples).ToSnapshotSeq(chunkSize)"

    (Model.PathSampleState(demo.Distribution, demo.Samples)).ToSnapshotSeq(chunkSize)
    |> Seq.truncate 3
    |> Seq.iteri (fun index snapshot ->
        printTop (sprintf "path seq snapshot %d" (index + 1)) 5 (normalizedValues snapshot))

    printfn "\nYielded path state API"
    printfn "  Model.PathSampleState(distr, samples).ToStateSeq(chunkSize)"

    (Model.PathSampleState(demo.Distribution, demo.Samples)).ToStateSeq(chunkSize)
    |> Seq.truncate 3
    |> Seq.iteri (fun index state ->
        printStateStep
            (sprintf "path seq state %d" (index + 1))
            5
            state.HasSnapshot
            state.CompletedSamples
            state.RemainingSamples
            (state.Snapshot()))

printIncrementalApiDemo ()
