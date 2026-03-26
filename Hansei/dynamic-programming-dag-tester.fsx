#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open Hansei
open Hansei.DynamicProgrammingDag

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

let private buildHmmWeights () =
    let states = [| false; true |]

    let initialWeight _ = 0.5

    let observationWeight state observed =
        match state, observed with
        | true, true
        | false, false -> 0.85
        | _ -> 0.15

    let transitionWeight source target =
        match source, target with
        | true, true
        | false, false -> 0.85
        | _ -> 0.15

    let label state = if state then "final=Hot" else "final=Cold"
    states, initialWeight, transitionWeight, observationWeight, label

let private buildDirectHmm (observations: bool[]) =
    let states, initialWeight, transitionWeight, observationWeight, label = buildHmmWeights ()
    Chain.compileFinite observations states initialWeight transitionWeight observationWeight label

let private buildStructuredHmm (observations: bool[]) =
    let states, initialWeight, transitionWeight, observationWeight, label = buildHmmWeights ()

    if observations.Length = 0 then
        Chain.compile
            (chain {
                let! initialState = Chain.finite "HiddenState" states initialWeight
                return! Chain.query initialState label
            })
    else
        let rec loop index currentState =
            chain {
                do! Chain.observe currentState observationWeight observations.[index]

                let! nextState = Chain.transition currentState transitionWeight

                if index = observations.Length - 1 then
                    return nextState
                else
                    return! loop (index + 1) nextState
            }

        Chain.compile
            (chain {
                let! initialState = Chain.finite "HiddenState" states initialWeight
                let! finalState = loop 0 initialState
                return! Chain.query finalState label
            })

let private buildWeatherDag () =
    let states = [| "sunny"; "cloudy"; "rainy" |]

    let initialWeight state =
        match state with
        | "sunny" -> 0.5
        | "cloudy" -> 0.3
        | "rainy" -> 0.2
        | _ -> 0.0

    let sensorTransition source target =
        match source, target with
        | "sunny", "sunny" -> 0.8
        | "sunny", "cloudy" -> 0.15
        | "sunny", "rainy" -> 0.05
        | "cloudy", "sunny" -> 0.2
        | "cloudy", "cloudy" -> 0.6
        | "cloudy", "rainy" -> 0.2
        | "rainy", "sunny" -> 0.05
        | "rainy", "cloudy" -> 0.25
        | "rainy", "rainy" -> 0.7
        | _ -> 0.0

    let umbrellaWeight state observation =
        match state, observation with
        | "sunny", true -> 0.1
        | "sunny", false -> 0.9
        | "cloudy", true -> 0.5
        | "cloudy", false -> 0.5
        | "rainy", true -> 0.9
        | "rainy", false -> 0.1
        | _ -> 0.0

    let trafficWeight state observation =
        match state, observation with
        | "sunny", true -> 0.2
        | "sunny", false -> 0.8
        | "cloudy", true -> 0.6
        | "cloudy", false -> 0.4
        | "rainy", true -> 0.85
        | "rainy", false -> 0.15
        | _ -> 0.0

    Dag.compile
        (dag {
            let! weather = Dag.root "Weather" states initialWeight
            let! umbrellaSensor = Dag.child weather "UmbrellaSensor" sensorTransition
            let! trafficSensor = Dag.child weather "TrafficSensor" sensorTransition
            do! Dag.observe umbrellaSensor umbrellaWeight true
            do! Dag.observe trafficSensor trafficWeight true
            return! Dag.query weather (fun state -> sprintf "weather=%s" state)
        })

let private printHmmCase label (observations: bool[]) =
    let directDag = buildDirectHmm observations
    let structuredDag = buildStructuredHmm observations
    let directPrepared = Model.PrepareFiniteDag directDag
    let structuredPrepared = Model.PrepareFiniteDag structuredDag
    let directPosterior = Model.EvaluateFiniteDag directDag
    let structuredPosterior = Model.EvaluateFiniteDag structuredDag
    let directEvidence = directPrepared.ExactPosterior()
    let structuredMap = structuredPrepared.MaxPosterior()

    printfn "\n=== %s ===" label
    printfn "Observation length: %d" observations.Length
    printTop "DP DAG posterior (direct IR)" 2 directPosterior
    printTop "DP DAG posterior (structured chain)" 2 structuredPosterior
    printfn "Evidence (sum-product) = %.6f" directEvidence.Evidence
    printfn "Log evidence (sum-product) = %.6f" directEvidence.LogEvidence
    printfn "MAP label (max-product) = %s (score %.6f)" structuredMap.BestLabel structuredMap.Score
    printfn "MAP log-score (max-product) = %.6f" structuredMap.LogScore
    printfn "L1(direct IR, structured chain) = %.6f" (l1Distance directPosterior structuredPosterior)

let private printWeatherCase () =
    let dag = buildWeatherDag ()
    let prepared = Model.PrepareFiniteDag dag
    let posterior = prepared.Evaluate()
    let beliefPosterior = prepared.BeliefPropagation().Posterior
    let mapResult = prepared.MaxPosterior()

    printfn "\n=== Branching Weather DAG ==="
    printTop "Posterior at query node" 3 posterior
    printTop "Belief propagation posterior" 3 beliefPosterior
    printfn "L1(sum-product, belief propagation) = %.6f" (l1Distance posterior beliefPosterior)
    printfn "MAP label = %s (score %.6f)" mapResult.BestLabel mapResult.Score
    printfn "MAP log-score = %.6f" mapResult.LogScore

let shortObservations = [| true; true; true; false; true; true; false; true |]
let longObservations =
    [|
        true; true; true; false; true; true; false; true
        false; false; true; false; true; false; false; true
        true; false; true; true; false; true; false; false
        true; true; true; false; true; false; true; true
    |]

printfn "=== Dynamic Programming DAG Phase 1 Tester ==="
printfn "Integrated Phase 1 covers finite linear chains and rooted trees."
printHmmCase "Short HMM chain" shortObservations
printHmmCase "Long HMM chain" longObservations
printWeatherCase ()