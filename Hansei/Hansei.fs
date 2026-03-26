module Hansei.Sampling

open Hansei
open Hansei.Probability
open Hansei.Exploration
open Utils
open Prelude.Common
open System

(* ------------------------------------------------------------------------ *)
(* Approximate inference strategies:				                        *)
(* Trace a few paths from the root to a leaf of the search tree             *)
(* The following procedures are non-deterministic; they use a given selector*)
(* procedure, of the type 'selector', to chose among the alternatives.      *)
(* For top-level inference, the selector uses system random generator.      *)

(* Naive, rejection sampling: the baseline *)
(* Random selection from a list of choices, using system randomness *) 

let internal defaultSingletonUnravelLimit = 64 

let inline internal addWeightedAnswer pcontrib ans v p =
    insertWithx (+) v (p * pcontrib) ans

let rec internal collapseForcedPathBounded singletonUnravelLimit depth ps collapsedPrefix choices =
    match choices with
    | _ when depth >= singletonUnravelLimit -> Choice2Of2(choices, ps, collapsedPrefix)
    | [ContinuedSubTree next, p] -> collapseForcedPathBounded singletonUnravelLimit (depth + 1) (p * ps) true (force next)
    | [Value v, p] -> Choice1Of2(v, p * ps)
    | _ -> Choice2Of2(choices, ps, collapsedPrefix)


let sample_path selector subsample nsamples ch =
    if nsamples <= 0 then
        invalidArg (nameof nsamples) "nsamples must be positive."

    let t0 = System.DateTime.Now

    let rec loop pcontrib ans choices =
        match collapseForcedPathBounded defaultSingletonUnravelLimit 0 1.0 false choices with
        | Choice1Of2 (v, p) -> addWeightedAnswer pcontrib ans v p
        | Choice2Of2 ([], _, _) -> ans
        | Choice2Of2 (sampleChoices, forcedMass, _) ->
            match selector (subsample sampleChoices) with
            | None -> ans
            | Some (th, ptotal) -> loop (pcontrib * forcedMass * ptotal) ans [th, 1.0]

    let sample_runner samples count =
        let mutable current = samples

        for _ in 1 .. count do
            current <- loop 1.0 current ch

        current

    let ans = sample_runner (Dict()) nsamples
    let ns = float nsamples
    let t1 = System.DateTime.Now

    printfn
        "sample_path: done %d worlds\nTime taken: %A seconds"
        nsamples
        (round 3 ((t1 - t0).TotalSeconds))

    [ for (KeyValue (v, p)) in ans -> Value v, p / ns ]

let beam_search beamwidth maxDepth maxExpandedNodes ch =
    let maxDepth = defaultArg maxDepth 256
    let maxExpandedNodes = defaultArg maxExpandedNodes 100000

    if beamwidth <= 0 then
        invalidArg (nameof beamwidth) "beamwidth must be positive."
    elif maxDepth <= 0 then
        invalidArg (nameof maxDepth) "maxDepth must be positive."
    elif maxExpandedNodes <= 0 then
        invalidArg (nameof maxExpandedNodes) "maxExpandedNodes must be positive."

    let inline scoreOf (_, score) = score

    let inline insertTopCandidate (buffer: ResizeArray<_>) candidate =
        let candidateScore = scoreOf candidate

        if buffer.Count < beamwidth then
            buffer.Add candidate

            let mutable index = buffer.Count - 1
            while index > 0 && scoreOf buffer.[index] < scoreOf buffer.[index - 1] do
                let tmp = buffer.[index - 1]
                buffer.[index - 1] <- buffer.[index]
                buffer.[index] <- tmp
                index <- index - 1
        elif candidateScore > scoreOf buffer.[0] then
            buffer.[0] <- candidate

            let mutable index = 0
            while index + 1 < buffer.Count && scoreOf buffer.[index] > scoreOf buffer.[index + 1] do
                let tmp = buffer.[index + 1]
                buffer.[index + 1] <- buffer.[index]
                buffer.[index] <- tmp
                index <- index + 1

    let expandNode (frontier: ResizeArray<_>) =
        function
        | (Value v, p) -> insertTopCandidate frontier (Value v, p)
        | (ContinuedSubTree t, p) ->
            match collapseForcedPathBounded defaultSingletonUnravelLimit 0 p false (force t) with
            | Choice1Of2 (v, p1) -> insertTopCandidate frontier (Value v, p1)
            | Choice2Of2 ([], _, _) -> ()
            | Choice2Of2 (choices, forcedMass, _) ->
                for (node, p1) in choices do
                    insertTopCandidate frontier (node, forcedMass * p1)

    let toDescendingList (buffer: ResizeArray<_>) =
        [ for i in buffer.Count - 1 .. -1 .. 0 -> buffer.[i] ]

    let rec loop depth expandedNodes current =
        match current with
        | [] -> []
        | _ when depth >= maxDepth -> current
        | _ when expandedNodes >= maxExpandedNodes -> current
        | _ ->
            let nextFrontier = ResizeArray<_>(beamwidth)
            let mutable hasSubtrees = false
            let mutable nodesExpanded = expandedNodes

            for candidate in current do
                if nodesExpanded < maxExpandedNodes then
                    expandNode nextFrontier candidate
                    nodesExpanded <- nodesExpanded + 1

            for i in 0 .. nextFrontier.Count - 1 do
                match fst nextFrontier.[i] with
                | ContinuedSubTree _ -> hasSubtrees <- true
                | Value _ -> ()

            let next = toDescendingList nextFrontier

            if hasSubtrees then
                loop (depth + 1) nodesExpanded next
            else
                next

    loop 0 0 ch

let internal sample_dist_with_mass_budget subsample nsamples max_pre_explore_depth pre_explore_min_mass maxdepth selector (ch: ProbabilitySpace<_>) =
    if nsamples <= 0 then
        invalidArg (nameof nsamples) "nsamples must be positive."
    elif Double.IsNaN pre_explore_min_mass || pre_explore_min_mass < 0.0 then
        invalidArg (nameof pre_explore_min_mass) "pre_explore_min_mass must be finite and non-negative."

    let normalizeChoices ptotal choices =
        let invPtotal = 1.0 / ptotal
        choices
        |> List.map (fun (x, p) -> x, p * invPtotal)

    let prepareFrontier pcontrib ans choices =
        let rec collect acc =
            function (* explore the branch a bit *)
            | [] -> ans, acc
            | (Value v, p) :: rest ->
                addWeightedAnswer pcontrib ans v p |> ignore
                collect acc rest
            | (ContinuedSubTree t, p) :: rest ->
                let acc =
                    match collapseForcedPathBounded defaultSingletonUnravelLimit 0 p false (force t) with
                    | Choice1Of2 (v, p1) ->
                        addWeightedAnswer pcontrib ans v p1 |> ignore
                        acc
                    | Choice2Of2 (_ch, branchMass, _) ->
                        let sampled = subsample _ch

                        match sampled with
                        | [] -> acc
                        | _ ->
                            let ptotal = List.sumBy snd sampled

                            if ptotal <= 0.0 then
                                acc
                            elif ptotal < nearly_one then
                                let normalized = normalizeChoices ptotal sampled

                                match collapseForcedPathBounded defaultSingletonUnravelLimit 0 (branchMass * ptotal) false normalized with
                                | Choice1Of2 (v, p1) ->
                                    addWeightedAnswer pcontrib ans v p1 |> ignore
                                    acc
                                | Choice2Of2 (normalizedChoices, retainedMass, _) ->
                                    (normalizedChoices, retainedMass) :: acc
                            else
                                match collapseForcedPathBounded defaultSingletonUnravelLimit 0 branchMass false sampled with
                                | Choice1Of2 (v, p1) ->
                                    addWeightedAnswer pcontrib ans v p1 |> ignore
                                    acc
                                | Choice2Of2 (sampledChoices, retainedMass, _) ->
                                    (sampledChoices, retainedMass) :: acc

                collect acc rest

        collect [] choices

    let rec loop depth pcontrib (ans: Dict<_, _>) =
        function
        | [Value v, p] -> addWeightedAnswer pcontrib ans v p
        | [] -> ans
        | [ContinuedSubTree th, p] -> loop (depth + 1) (p * pcontrib) ans (force th)
        | ch when depth < maxdepth -> (* choosing one thread randomly *)
            match prepareFrontier pcontrib ans (subsample ch) with
            | (ans, []) -> ans
            | (ans, cch) ->
                match selector cch with 
                | None -> ans
                | Some (th, ptotal) -> loop (depth + 1) (pcontrib * ptotal) ans th 
        | _ -> ans

    let toploop pcontrib cch ans =
        (* cch are already pre-explored *)
        match selector cch with
        | None -> ans
        | Some (th, ptotal) -> loop 0 (pcontrib * ptotal) ans th 

    let sample_runner samples th count =
        let mutable current = samples

        for _ in 1 .. count do
            current <- th current

        current

    let driver pcontrib vals cch =
        let ans = sample_runner (Dict()) (toploop pcontrib cch) nsamples
        let ns = float nsamples
        //let ans = Map.fold (fun ans v p  -> insertWith (+) v (ns * p) ans) ans vals
        for (KeyValue (v, p)) in vals do
            insertWithx (+) v (ns * p) ans |> ignore

        printfn "sample_importance: done %d worlds\n" nsamples
        //Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans
        [ for (KeyValue (v, p)) in ans -> Value v, p / ns ]

    let rec pre_explore depth pcontrib ans ch =
        (* pre-explore initial threads *)
        match prepareFrontier pcontrib ans (subsample ch) with
        | (ans, []) -> (* pre-exploration solved the problem *) [ for (KeyValue (v, p)) in ans -> Value v, p ]
        | (ans, [ ch, p ]) when depth < max_pre_explore_depth && (pcontrib * p) >= pre_explore_min_mass -> (* only one choice, make more *)
            pre_explore (depth + 1) (pcontrib * p) ans ch
        | (ans, cch) -> driver pcontrib ans cch

    pre_explore 0 1.0 (Dict()) ch 

let sample_dist subsample nsamples max_pre_explore_depth maxdepth selector (ch: ProbabilitySpace<_>) =
    sample_dist_with_mass_budget subsample nsamples max_pre_explore_depth 0.0 maxdepth selector ch

let inline internal sample_dist_importance_core
    (createFrontier: int -> 'Frontier)
    (addPending: ProbabilitySpace<'T> -> float -> 'Frontier -> unit)
    (frontierCount: 'Frontier -> int)
    (getSingleton: 'Frontier -> ProbabilitySpace<'T> * float)
    (selectFrontier: 'Frontier -> option<ProbabilitySpace<'T> * float>)
    (subsample: ProbabilitySpace<'T> -> ProbabilitySpace<'T>)
    nsamples
    max_pre_explore_depth
    pre_explore_min_mass
    maxdepth
    (ch: ProbabilitySpace<'T>)
    =
    if nsamples <= 0 then
        invalidArg (nameof nsamples) "nsamples must be positive."
    elif Double.IsNaN pre_explore_min_mass || pre_explore_min_mass < 0.0 then
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

    let rec loop depth pcontrib (ans: Dict<_, _>) =
        function
        | [Value v, p] -> addWeightedAnswer pcontrib ans v p
        | [] -> ans
        | [ContinuedSubTree th, p] -> loop (depth + 1) (p * pcontrib) ans (force th)
        | ch when depth < maxdepth ->
            match prepareFrontier pcontrib ans (subsample ch) with
            | ans, frontier when frontierCount frontier = 0 -> ans
            | ans, frontier ->
                match selectFrontier frontier with
                | None -> ans
                | Some (th, correction) -> loop (depth + 1) (pcontrib * correction) ans th
        | _ -> ans

    let toploop pcontrib frontier ans =
        match selectFrontier frontier with
        | None -> ans
        | Some (th, correction) -> loop 0 (pcontrib * correction) ans th

    let sample_runner samples th count =
        let mutable current = samples

        for _ in 1 .. count do
            current <- th current

        current

    let driver pcontrib vals frontier =
        let ans = sample_runner (Dict()) (toploop pcontrib frontier) nsamples
        let ns = float nsamples

        for (KeyValue (v, p)) in vals do
            insertWithx (+) v (ns * p) ans |> ignore

        printfn "sample_importance: done %d worlds\n" nsamples
        [ for (KeyValue (v, p)) in ans -> Value v, p / ns ]

    let rec pre_explore depth pcontrib ans current =
        match prepareFrontier pcontrib ans (subsample current) with
        | ans, frontier when frontierCount frontier = 0 ->
            [ for (KeyValue (v, p)) in ans -> Value v, p ]
        | ans, frontier when frontierCount frontier = 1 && depth < max_pre_explore_depth ->
            let nextChoices, correction = getSingleton frontier
            if (pcontrib * correction) >= pre_explore_min_mass then
                pre_explore (depth + 1) (pcontrib * correction) ans nextChoices
            else
                driver pcontrib ans frontier
        | ans, frontier -> driver pcontrib ans frontier

    pre_explore 0 1.0 (Dict()) ch

let internal sample_dist_importance_with_mass_budget subsample nsamples max_pre_explore_depth pre_explore_min_mass maxdepth (selector: ImportanceSelector<_>) (ch: ProbabilitySpace<_>) =
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

    sample_dist_importance_core
        createFrontier
        addPending
        frontierCount
        getSingleton
        selectFrontier
        subsample
        nsamples
        max_pre_explore_depth
        pre_explore_min_mass
        maxdepth
        ch

let internal sample_dist_importance_prepared_with_mass_budget subsample nsamples max_pre_explore_depth pre_explore_min_mass maxdepth (selector: PreparedFrontierSelector<_>) (ch: ProbabilitySpace<_>) =
    let frontierCount (frontier: PreparedFrontier<_>) = frontier.Count
    let getSingleton (frontier: PreparedFrontier<_>) = frontier.Choices.[0], frontier.Corrections.[0]

    sample_dist_importance_core
        createPreparedFrontier
        addPreparedFrontierChoice
        frontierCount
        getSingleton
        selector
        subsample
        nsamples
        max_pre_explore_depth
        pre_explore_min_mass
        maxdepth
        ch

let sample_dist_importance subsample nsamples max_pre_explore_depth maxdepth (selector: ImportanceSelector<_>) (ch: ProbabilitySpace<_>) =
    sample_dist_importance_with_mass_budget subsample nsamples max_pre_explore_depth 0.0 maxdepth selector ch


//=================

type internal ImportanceSamplingConfig<'T> =
    {
        Subsample: ProbabilitySpace<'T> -> ProbabilitySpace<'T>
        UsePreExplore: bool
        PreExploreDepth: int
        PreExploreMinMass: float
        ShallowExploreMaxNodes: int option
        ShallowExploreMaxFrontier: int option
    }

let internal createImportanceSamplingConfig usePreExplore subsample preExploreDepth preExploreMinMass shallowExploreMaxNodes shallowExploreMaxFrontier =
    {
        Subsample = subsample
        UsePreExplore = usePreExplore
        PreExploreDepth = preExploreDepth
        PreExploreMinMass = preExploreMinMass
        ShallowExploreMaxNodes = shallowExploreMaxNodes
        ShallowExploreMaxFrontier = shallowExploreMaxFrontier
    }

let internal shallowPreExplore (config: ImportanceSamplingConfig<'T>) distr =
    shallow_explore_with_budget config.Subsample config.ShallowExploreMaxNodes config.ShallowExploreMaxFrontier distr

let internal runImportanceSamplesOnDistribution (config: ImportanceSamplingConfig<'T>) nsamples maxdepth sortBeforeSampling (selector: ImportanceSelector<'T> option) distr =
    match selector with
    | Some selector ->
        sample_dist_importance_with_mass_budget
            config.Subsample
            nsamples
            config.PreExploreDepth
            config.PreExploreMinMass
            maxdepth
            selector
            distr
    | None ->
        sample_dist_importance_prepared_with_mass_budget
            config.Subsample
            nsamples
            config.PreExploreDepth
            config.PreExploreMinMass
            maxdepth
            (random_prepared_frontier_selector sortBeforeSampling)
            distr


let internal runImportanceSamples (config: ImportanceSamplingConfig<'T>) nsamples maxdepth sortBeforeSampling (selector: ImportanceSelector<'T> option) distr =
    let effectiveConfig, inputDistribution =
        if config.UsePreExplore then
            config, shallowPreExplore config distr
        else
            { config with
                PreExploreDepth = 0
                PreExploreMinMass = 0.0
                ShallowExploreMaxNodes = None
                ShallowExploreMaxFrontier = None }, distr

    runImportanceSamplesOnDistribution effectiveConfig nsamples maxdepth sortBeforeSampling selector inputDistribution

