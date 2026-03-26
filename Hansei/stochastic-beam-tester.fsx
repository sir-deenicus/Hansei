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
open Hansei.StochasticBeam

type SamplerResult =
    { Name: string; Dist: Map<string, float> }

type BeamTestCase =
    {
        Name: string
        Why: string
        Distribution: ProbabilitySpace<string>
        Samples: int
        MaxDepth: int
        BeamWidth: int
        MaxRounds: int
        RawTopCount: int option
        Aggregations: (string * (string -> string)) list
    }

let private quiet f =
    let original = Console.Out
    use sink = new StringWriter()
    Console.SetOut sink
    try
        f ()
    finally
        Console.SetOut original

let private aggregateValues (dist: ProbabilitySpace<'T>) =
    dist
    |> List.fold (fun acc (node, p) ->
        match node with
        | Value value -> Map.change value (fun existing -> Some (p + defaultArg existing 0.0)) acc
        | ContinuedSubTree _ -> acc) Map.empty

let private normalizeMap (dist: Map<'T, float>) =
    let total = dist |> Seq.sumBy (fun kv -> kv.Value)

    if total <= 0.0 then
        dist
    else
        dist |> Map.map (fun _ p -> p / total)

let private normalizedValues dist =
    dist |> aggregateValues |> normalizeMap

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

let private benchmark repeats action =
    let timer = Stopwatch.StartNew()

    for _ in 1 .. repeats do
        action () |> ignore

    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private exact distribution =
    Model.ExactInfer distribution |> normalizedValues

let private importance samples maxDepth distribution =
    quiet (fun () -> Model.ImportanceSamples(distribution, samples, maxDepth)) |> normalizedValues

let private path samples distribution =
    quiet (fun () -> Model.PathSample(distribution, samples)) |> normalizedValues

let private beam beamWidth maxRounds distribution =
    quiet (fun () -> Model.StochasticBeam(distribution, beamWidth, maxRounds = maxRounds, seed = 17)) |> normalizedValues

let private formatBias p = sprintf "p=%.2f" p

let private labelPosterior posterior =
    ProbabilitySpace.mapDistribution formatBias posterior

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
    let coinPrior = [ 0.05; 0.15; 0.25; 0.35; 0.45; 0.55; 0.65; 0.75; 0.85; 0.95 ]

    dist {
        let! p = uniform coinPrior
        return! observeAll p observations
    }
    |> labelPosterior

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
        Why = "Beam should keep more useful posterior mass alive than raw path sampling under repeated hard evidence."
        Distribution = hardEvidencePosterior [ true; true; true; true; true; true; false; true ]
        Samples = 2500
        MaxDepth = 40
        BeamWidth = 2500
        MaxRounds = 128
        RawTopCount = Some 10
        Aggregations = [ "bias posterior", id ] }
      { Name = "Sequential HMM evidence"
        Why = "Beam state snapshots are particularly meaningful on sequential evidence because the live frontier evolves round by round."
        Distribution = hiddenMarkovPosterior [| true; true; true; false; true; true; false; true |]
        Samples = 2500
        MaxDepth = 64
        BeamWidth = 2500
        MaxRounds = 256
        RawTopCount = Some 10
        Aggregations = [ "final state", id ] } ]

let private printRunSummary testCase =
    let exactDist = exact testCase.Distribution

    let estimates =
        [ { Name = "Importance"; Dist = importance testCase.Samples testCase.MaxDepth testCase.Distribution }
          { Name = "Path"; Dist = path testCase.Samples testCase.Distribution }
          { Name = sprintf "Stochastic beam (beam=%d, rounds=%d)" testCase.BeamWidth testCase.MaxRounds
            Dist = beam testCase.BeamWidth testCase.MaxRounds testCase.Distribution } ]

    printfn "\n=== %s ===" testCase.Name
    printfn "%s" testCase.Why
    printfn "Samples / beam width: %d" testCase.Samples

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

let private printSpeed testCase =
    printfn "\n--- Speed: %s ---" testCase.Name
    printfn "Average ms/run over 5 runs"
    printfn "importance                 %.3f" (benchmark 5 (fun () -> importance testCase.Samples testCase.MaxDepth testCase.Distribution))
    printfn "path                       %.3f" (benchmark 5 (fun () -> path testCase.Samples testCase.Distribution))
    printfn "stochastic-beam            %.3f" (benchmark 5 (fun () -> beam testCase.BeamWidth testCase.MaxRounds testCase.Distribution))

let private printBeamStateStep label topCount (state: StochasticBeamState<string>) =
    let snapshotInfo = state.SnapshotInfo()
    printfn "%s rounds=%d remaining=%d pending-unique=%d pending-slots=%d pending-mass=%.6f" label state.CompletedRounds state.RemainingRounds snapshotInfo.PendingUniqueLive snapshotInfo.PendingLiveSlots snapshotInfo.PendingMass
    printTop (sprintf "%s snapshot" label) topCount (normalizedValues snapshotInfo.Dist)

let private printIncrementalApiDemo () =
    let demo = List.last tests
    let roundStep = 2

    printfn "\n=== Stochastic Beam API Demo ==="
    printfn "Test case: %s" demo.Name
    printfn "Round step: %d" roundStep

    let state0 = Model.StochasticBeamState(demo.Distribution, demo.BeamWidth, maxRounds = demo.MaxRounds, seed = 17)
    printfn "\nRaw beam state-machine API"
    printfn "  let state0 = Model.StochasticBeamState(distr, beamWidth, maxRounds = maxRounds, seed = 17)"
    printfn "  let state1 = state0.Advance(roundStep)"
    printfn "  let snapshot = state1.SnapshotInfo()"

    let state1 = state0.Advance(roundStep)
    let state2 = state1.Advance(roundStep)
    printBeamStateStep "beam state1" 5 state1
    printBeamStateStep "beam state2" 5 state2

    printfn "\nYielded beam state API"
    printfn "  Model.StochasticBeamState(distr, beamWidth, maxRounds = maxRounds, seed = 17).ToStateSeq(roundStep)"

    (Model.StochasticBeamState(demo.Distribution, demo.BeamWidth, maxRounds = demo.MaxRounds, seed = 17)).ToStateSeq(roundStep)
    |> Seq.take 3
    |> Seq.iteri (fun index state -> printBeamStateStep (sprintf "beam seq state %d" (index + 1)) 5 state)

    printfn "\nYielded beam snapshot API"
    printfn "  Model.StochasticBeamState(distr, beamWidth, maxRounds = maxRounds, seed = 17).ToSnapshotInfoSeq(roundStep)"

    (Model.StochasticBeamState(demo.Distribution, demo.BeamWidth, maxRounds = demo.MaxRounds, seed = 17)).ToSnapshotInfoSeq(roundStep)
    |> Seq.take 3
    |> Seq.iteri (fun index (snapshotInfo: StochasticBeamSnapshot<string>) ->
        printfn "beam seq snapshot %d pending-unique=%d pending-slots=%d pending-mass=%.6f" (index + 1) snapshotInfo.PendingUniqueLive snapshotInfo.PendingLiveSlots snapshotInfo.PendingMass
        printTop (sprintf "beam seq snapshot %d dist" (index + 1)) 5 (normalizedValues snapshotInfo.Dist))

for testCase in tests do
    printRunSummary testCase
    printSpeed testCase

printIncrementalApiDemo ()