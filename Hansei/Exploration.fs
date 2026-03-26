module Hansei.Exploration

open System
open Hansei.Probability
open Prelude.Common
open Prelude.Math
open Utils

let explore (maxdepth: int option) (choices: ProbabilitySpace<'T>) =
    let inline canDescend depth =
        match maxdepth with
        | None -> true
        | Some limit -> depth <= limit

    let answers = Dict()
    let mutable susp = []
    let stack = ResizeArray<_>()
    stack.Add(struct (choices, 1.0, 0))

    while stack.Count > 0 do
        let topIndex = stack.Count - 1
        let struct (pending, pathWeight, depth) = stack.[topIndex]
        stack.RemoveAt(topIndex)

        match pending with
        | [] -> ()
        | (Value v, branchWeight) :: rest ->
            insertWithx (+) v (branchWeight * pathWeight) answers |> ignore

            match rest with
            | [] -> ()
            | _ -> stack.Add(struct (rest, pathWeight, depth))
        | (ContinuedSubTree t, branchWeight) :: rest when canDescend depth ->
            match rest with
            | [] -> ()
            | _ -> stack.Add(struct (rest, pathWeight, depth))

            stack.Add(struct (force t, branchWeight * pathWeight, depth + 1))
        | (c, branchWeight) :: rest ->
            susp <- (c, branchWeight * pathWeight) :: susp

            match rest with
            | [] -> ()
            | _ -> stack.Add(struct (rest, pathWeight, depth))

    //Map.fold (fun a v p -> (p, Value v)::a) susp ans : ProbabilitySpace<'T>
    [ yield! susp
      for (KeyValue (v, p)) in answers -> Value v, p ]

let nearly_one = 1.0 - 1e-7

(* Explore but do not flatten the tree:
   perform exact inference to the given depth
   We still pick out all the produced answers and note the failures. *)

let internal shallow_explore_with_budget
    (subsample: ProbabilitySpace<_> -> ProbabilitySpace<_>)
    (maxExpandedNodes: int option)
    (maxFrontierWidth: int option)
    (choices: ProbabilitySpace<_>)
    =
    let maxExpandedNodes = defaultArg maxExpandedNodes Int32.MaxValue
    let maxFrontierWidth = defaultArg maxFrontierWidth Int32.MaxValue
    let singletonUnravelLimit = 64

    let rec collapse_singleton depth ps choices =
        match choices with
        | _ when depth >= singletonUnravelLimit -> Choice2Of2(choices, ps, true)
        | [ContinuedSubTree next, p] -> collapse_singleton (depth + 1) (p * ps) (force next)
        | [Value v, p] -> Choice1Of2(v, p * ps)
        | _ -> Choice2Of2(choices, ps, false)

    if maxExpandedNodes <= 0 then
        invalidArg (nameof maxExpandedNodes) "maxExpandedNodes must be positive when provided."

    if maxFrontierWidth <= 0 then
        invalidArg (nameof maxFrontierWidth) "maxFrontierWidth must be positive when provided."

    let add_answer pcontrib v mp = insertWithx (+) v pcontrib mp
    let normalize_choices ptotal choices =
        let invPtotal = 1.0 / ptotal
        choices |> List.map (fun (x, p) -> x, p * invPtotal)

    let rec retain_remaining acc retainedMass retainedCount =
        function
        | [] -> acc, retainedMass, retainedCount
        | ((_, p) as choice) :: rest -> retain_remaining (choice :: acc) (retainedMass + p) (retainedCount + 1) rest

    let retain_choice acc retainedMass retainedCount choice p =
        (choice, p) :: acc, retainedMass + p, retainedCount + 1

    let rec loop pc ans acc retainedMass retainedCount expandedNodes =
        function
        | [] -> (ans, acc, retainedMass, retainedCount, expandedNodes)
        | remaining when retainedCount >= maxFrontierWidth ->
            let acc, retainedMass, retainedCount = retain_remaining acc retainedMass retainedCount remaining
            ans, acc, retainedMass, retainedCount, expandedNodes
        | ((Value v, p) :: rest) -> loop pc (add_answer (p * pc) v ans) acc retainedMass retainedCount expandedNodes rest
        | ((ContinuedSubTree t, p) :: rest) ->
            match collapse_singleton 0 p (force t) with
            | Choice1Of2 (v, p1) -> loop pc (add_answer (p1 * pc) v ans) acc retainedMass retainedCount expandedNodes rest
            | Choice2Of2 ([], _, _) -> loop pc ans acc retainedMass retainedCount expandedNodes rest
            | Choice2Of2 (collapsedChoices, branchMass, _) when expandedNodes >= maxExpandedNodes ->
                let acc, retainedMass, retainedCount = retain_choice acc retainedMass retainedCount (continuedSubTree Memoize (fun () -> collapsedChoices)) branchMass
                loop pc ans acc retainedMass retainedCount expandedNodes rest
            | Choice2Of2 (collapsedChoices, branchMass, _) ->
                let (ans, ch, childMass, _, expandedNodes) =
                    loop (pc * branchMass) ans [] 0.0 0 (expandedNodes + 1) (subsample collapsedChoices)

                let acc, retainedMass, retainedCount =
                    if childMass = 0.0 then
                        acc, retainedMass, retainedCount
                    else if childMass < nearly_one then
                        let ch' = normalize_choices childMass ch
                        retain_choice acc retainedMass retainedCount (continuedSubTree Memoize (fun () -> ch')) (branchMass * childMass)
                    else
                        retain_choice acc retainedMass retainedCount (continuedSubTree Memoize (fun () -> ch)) branchMass

                loop pc ans acc retainedMass retainedCount expandedNodes rest

    let (ans, susp, _, _, _) = loop 1.0 (Dict()) [] 0.0 0 0 (subsample choices)

    [ yield! susp
      for (KeyValue (v, p)) in ans -> Value v, p ]

let shallow_explore
    (subsample: ProbabilitySpace<_> -> ProbabilitySpace<_>)
    (maxExpandedNodes: int)
    (choices: ProbabilitySpace<_>)
    =
    shallow_explore_with_budget subsample (Some maxExpandedNodes) None choices


//====================

let max_selector _ choices = Seq.maxBy snd choices |> Some

type ImportanceSelector<'T> = ResizeArray<ProbabilitySpace<'T> * float> -> option<ProbabilitySpace<'T> * float>

type internal PreparedFrontier<'T> =
    {
        mutable Choices: ProbabilitySpace<'T> array
        mutable Corrections: float array
        mutable Count: int
    }

type internal PreparedFrontierSelector<'T> = PreparedFrontier<'T> -> option<ProbabilitySpace<'T> * float>

let internal createPreparedFrontier capacity =
    let initialCapacity = max 4 capacity

    {
        Choices = Array.zeroCreate initialCapacity
        Corrections = Array.zeroCreate initialCapacity
        Count = 0
    }

let internal addPreparedFrontierChoice choice correction (frontier: PreparedFrontier<_>) =
    if frontier.Count = frontier.Choices.Length then
        let newCapacity = frontier.Count * 2
        let newChoices = Array.zeroCreate newCapacity
        let newCorrections = Array.zeroCreate newCapacity
        Array.Copy(frontier.Choices, newChoices, frontier.Count)
        Array.Copy(frontier.Corrections, newCorrections, frontier.Count)
        frontier.Choices <- newChoices
        frontier.Corrections <- newCorrections

    frontier.Choices.[frontier.Count] <- choice
    frontier.Corrections.[frontier.Count] <- correction
    frontier.Count <- frontier.Count + 1

let max_importance_selector () : ImportanceSelector<_> =
    fun choices ->
        if choices.Count = 0 then
            None
        else
            let mutable bestTree, bestCorrection = choices.[0]

            for i in 1 .. choices.Count - 1 do
                let tree, correction = choices.[i]

                if correction > bestCorrection then
                    bestTree <- tree
                    bestCorrection <- correction

            Some(bestTree, bestCorrection)

let random_importance_selector _ : ImportanceSelector<_> =
    fun choices ->
        if choices.Count = 0 then
            None
        else
            let mutable totalCorrection = 0.0
            let mutable selectedIndex = -1

            for i in 0 .. choices.Count - 1 do
                let _, correction = choices.[i]

                if correction > 0.0 then
                    totalCorrection <- totalCorrection + correction

                    if random.NextDouble(0.0, totalCorrection) < correction then
                        selectedIndex <- i

            if selectedIndex < 0 then
                None
            else
                let tree, _ = choices.[selectedIndex]
                Some(tree, totalCorrection)

let internal random_prepared_frontier_selector _ : PreparedFrontierSelector<_> =
    fun frontier ->
        if frontier.Count = 0 then
            None
        elif frontier.Count = 1 then
            Some(frontier.Choices.[0], frontier.Corrections.[0])
        else
            let mutable totalCorrection = 0.0
            let mutable selectedIndex = -1

            for i in 0 .. frontier.Count - 1 do
                let correction = frontier.Corrections.[i]

                if correction > 0.0 then
                    totalCorrection <- totalCorrection + correction

                    if random.NextDouble(0.0, totalCorrection) < correction then
                        selectedIndex <- i

            if selectedIndex < 0 then
                None
            else
                Some(frontier.Choices.[selectedIndex], totalCorrection)
  
let random_selector dosort =
    let rec selection r ptotal pcum =
        function
        | [] -> None //failwith "Choice selection: can't happen"
        | (th, p) :: rest ->
            let pcum = pcum + p

            if r < pcum then
                Some(th, ptotal)
            else
                selection r ptotal pcum rest

    fun choices ->
        let ptotal = List.sumBy snd choices
        let r = random.NextDouble(0., ptotal) (* 0<=r<ptotal *)

        if dosort then
            List.sortBy snd choices
        else
            choices
        |> selection r ptotal 0.0