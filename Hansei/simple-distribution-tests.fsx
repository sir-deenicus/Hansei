#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open Hansei.Core.List
open Hansei.Core.List.Distributions

type Example =
    {
        Name: string
        Distribution: ProbabilitySpace<string>
        Samples: int
        MaxDepth: int
    }

let private example name samples maxDepth distribution =
    {
        Name = name
        Distribution = distribution
        Samples = samples
        MaxDepth = maxDepth
    }

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

let private printDistribution title (dist: Map<'T, float>) =
    printfn "%s" title

    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.iter (fun (value, p) -> printfn "  %-18A %.6f" value p)

let private runExample (example: Example) =
    let exact = example.Distribution |> Model.ExactInfer |> normalizedValues
    let importance = Model.ImportanceSamples(example.Distribution, example.Samples, example.MaxDepth) |> normalizedValues
    let importanceNoPreExplore = Model.ImportanceSamples(example.Distribution, example.Samples, example.MaxDepth, preExplore = false) |> normalizedValues
    let path = Model.PathSample(example.Distribution, example.Samples) |> normalizedValues

    printfn "\n=== %s ===" example.Name
    printDistribution "Exact" exact
    printDistribution "Importance" importance
    printDistribution "Importance (no pre-explore)" importanceNoPreExplore
    printDistribution "Path" path
    printfn "L1(exact, importance) = %.6f" (l1Distance exact importance)
    printfn "L1(exact, importance-no-pre) = %.6f" (l1Distance exact importanceNoPreExplore)
    printfn "L1(exact, path)       = %.6f" (l1Distance exact path)

let private basicExamples =
    [ example "Fair coin" 2000 8 <|
          dist {
              let! coin = bernoulli 0.5
              return if coin then "Heads" else "Tails"
          }

      example "Two fair coins | at least one head" 4000 10 <|
          dist {
              let! a = bernoulli 0.5
              let! b = bernoulli 0.5
              do! observe (a || b)
              return sprintf "(%b, %b)" a b
          }

      example "Three biased coins | at least two heads" 6000 12 <|
          dist {
              let! a = bernoulli 0.2
              let! b = bernoulli 0.5
              let! c = bernoulli 0.8
              let heads = [ a; b; c ] |> List.filter id |> List.length
              do! observe (heads >= 2)
              return sprintf "%d heads" heads
          }

      example "Two 3-sided dice sum" 6000 12 <|
          dist {
              let! a = uniform [ 1; 2; 3 ]
              let! b = uniform [ 1; 2; 3 ]
              return sprintf "sum=%d" (a + b)
          } ]

let private coinPrior =
    [ 0.05; 0.15; 0.25; 0.35; 0.45; 0.55; 0.65; 0.75; 0.85; 0.95 ]

let private coinObservations = [ true; true; true; false; true ]

let private formatBias p = sprintf "p=%.2f" p
let private coinLikelihood p observed = if observed then p else 1.0 - p

let private labelPosterior posterior =
    ProbabilitySpace.mapDistribution formatBias posterior

let private exactLocalObserveViaExplore test (localModel: ProbabilitySpace<'T>) =
    let totalWeight =
        explore None localModel
        |> List.sumBy (fun (node, branchWeight) ->
            match node with
            | Value value -> if test value then branchWeight else 0.0
            | ContinuedSubTree _ -> 0.0)

    factor totalWeight

let private observeAll p observations =
    let rec loop =
        function
        | [] -> always p
        | observed :: rest ->
            dist {
                let! flip = bernoulli p
                do! observe (flip = observed)
                return! loop rest
            }

    loop observations

let private coinPosteriorObservation posterior observed =
    dist {
        let! p = posterior
        let! flip = bernoulli p
        do! observe (flip = observed)
        return p
    }

let private coinPosteriorWithModel observations =
    dist {
        let! p = uniform coinPrior
        return! observeAll p observations
    }
    |> labelPosterior

let private coinPosteriorWithFold observations =
    observations
    |> List.fold coinPosteriorObservation (uniform coinPrior)
    |> labelPosterior

let private coinPosteriorWithForLoop observations =
    dist {
        let! p = uniform coinPrior

        do!
            dist {
                for observed in observations do
                    let! flip = bernoulli p
                    do! observe (flip = observed)
            }

        return formatBias p
    }

let private coinPosteriorWithSoftObserve observations =
    dist {
        let! p = uniform coinPrior

        do!
            dist {
                for observed in observations do
                    do! soft_observe (coinLikelihood p observed)
            }

        return formatBias p
    }

let private localPositionObserveModel =
    dist {
        let! bucket = uniform [ "left"; "right" ]
        let target = if bucket = "left" then (2, 1) else (7, 8)
        let! pos =
            dist {
                let! x = uniform [ 0 .. 9 ]
                let! y = uniform [ 0 .. 9 ]
                return x, y
            }

        do! observe (pos = target)
        return bucket
    }

let private localPositionExactLocalObserveModel =
    dist {
        let! bucket = uniform [ "left"; "right" ]
        let target = if bucket = "left" then (2, 1) else (7, 8)

        do!
            exact_local_observe
                ((=) target)
                (dist {
                    let! x = uniform [ 0 .. 9 ]
                    let! y = uniform [ 0 .. 9 ]
                    return x, y
                })

        return bucket
    }

let private localPositionExploreBasedExactLocalObserveModel =
    dist {
        let! bucket = uniform [ "left"; "right" ]
        let target = if bucket = "left" then (2, 1) else (7, 8)

        do!
            exactLocalObserveViaExplore
                ((=) target)
                (dist {
                    let! x = uniform [ 0 .. 9 ]
                    let! y = uniform [ 0 .. 9 ]
                    return x, y
                })

        return bucket
    }

let private posteriorExamples =
        [   example "Biased coin posterior | recursive model" 8000 24 (coinPosteriorWithModel coinObservations)
            example "Biased coin posterior | fold updates" 8000 24 (coinPosteriorWithFold coinObservations)
            example "Biased coin posterior | CE for-loop" 8000 24 (coinPosteriorWithForLoop coinObservations)
            example "Biased coin posterior | soft_observe with CE for-loop" 8000 24 (coinPosteriorWithSoftObserve coinObservations)
            example "Local evidence | explicit observe" 8000 16 localPositionObserveModel
            example "Local evidence | exact_local_observe" 8000 16 localPositionExactLocalObserveModel ]

let private benchmark repeats action =
    let timer = Stopwatch.StartNew()
    for _ in 1 .. repeats do
        action () |> ignore
    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private printPosteriorConsistencyCheck () =
    let modelExact = coinPosteriorWithModel coinObservations |> Model.ExactInfer |> normalizedValues
    let foldExact = coinPosteriorWithFold coinObservations |> Model.ExactInfer |> normalizedValues
    let forLoopExact = coinPosteriorWithForLoop coinObservations |> Model.ExactInfer |> normalizedValues
    let localObserveExact = localPositionObserveModel |> Model.ExactInfer |> normalizedValues
    let localExactObserveExact = localPositionExactLocalObserveModel |> Model.ExactInfer |> normalizedValues

    printfn "\n=== Posterior Consistency Check ==="
    printfn "L1(model exact, fold exact) = %.6f" (l1Distance modelExact foldExact)
    printfn "L1(model exact, for-loop exact) = %.6f" (l1Distance modelExact forLoopExact)
    printfn "L1(local observe exact, exact_local_observe exact) = %.6f" (l1Distance localObserveExact localExactObserveExact)
    printfn "The CE for-loop form is supported here because ProbabilitySpaceBuilder.For sequences iterations and discards each iteration result."

let private printExactLocalObserveBenchmark () =
    let directExact = localPositionExactLocalObserveModel |> Model.ExactInfer |> normalizedValues
    let exploreExact = localPositionExploreBasedExactLocalObserveModel |> Model.ExactInfer |> normalizedValues

    printfn "\n=== exact_local_observe Helper Benchmark ==="
    printfn "L1(direct helper exact, explore-based helper exact) = %.6f" (l1Distance directExact exploreExact)
    printfn "Average ms/run over 50 exact-inference runs"
    printfn "direct exact_local_observe       %.3f" (benchmark 50 (fun () -> Model.ExactInfer localPositionExactLocalObserveModel))
    printfn "explore-based exact_local_observe %.3f" (benchmark 50 (fun () -> Model.ExactInfer localPositionExploreBasedExactLocalObserveModel))

List.append basicExamples posteriorExamples
|> List.iter runExample

printPosteriorConsistencyCheck ()
printExactLocalObserveBenchmark ()
