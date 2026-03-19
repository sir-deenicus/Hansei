module Hansei.Core.List

open Hansei.Continuation
open Hansei.Utils
open Prelude.Common
open Prelude.Math
open System
open Prelude.Math
open Hansei.FSharpx.Collections

//open Hansei.FSharpx.Collections.LazyList.ComputationExpressions

//open FSharp.Collections.ParallelSeq

//Originally inspired by but long diverged from
//https://gist.github.com/einblicker/3245547#file-hansei
//Based on
//http://okmij.org/ftp/kakuritu/Hansei.html
//Ocaml style comments in code below are Oleg's

//This framework is much more flexible than the system described in Expert F#.
//A continuation monad is used to describe distributions as nested lazy list trees.
//Better base for http://dippl.org

type ProbabilitySpace<'T> = list<WeightedTree<'T> * float>
and WeightedTree<'T> =
    | Value of 'T
    | ContinuedSubTree of Lazy<ProbabilitySpace<'T>>
 
//if use value instead of Continued, infinite computations will fail to return/terminate
let distribution weightedlist : ProbabilitySpace<_> =
    List.map (fun (v, p) -> 
        ContinuedSubTree(lazy [Value v, 1.]), p) weightedlist 

let inline distributionOfSeq weightedlist : ProbabilitySpace<_> =
    List.map (fun (v, p) -> 
        ContinuedSubTree(lazy [Value v, 1.]), float p) (List.ofSeq weightedlist)

let always x : ProbabilitySpace<_> =
    distribution [x, 1.]

let exactly x = distribution [ x, 1. ]

let fail () : ProbabilitySpace<_> = []

let reflect tree k =
    let rec make_choices pv =
        List.map
            (function
            | (Value x, p) -> ContinuedSubTree(lazy (k x)), p
            | (ContinuedSubTree (Lazy t), p) -> ContinuedSubTree(lazy (make_choices t)), p)
            pv

    make_choices tree: ProbabilitySpace<_>
  
type ProbabilitySpaceBuilder() =
    member inline d.Bind(space, k) = reflect space k
    member d.Return v = always v
    member d.ReturnFrom vs : ProbabilitySpace<_> = vs
    member inline _.YieldFrom vs: ProbabilitySpace<_>  = vs 
    member d.BindReturn(p: ProbabilitySpace<'a>, f: 'a -> 'b) : ProbabilitySpace<_> = 
        reflect p (f >> always)
    member d.Zero() = []
    member d.Combine(x, y) = List.append x y

    member d.Yield x = d.Return x
     
    member _.Delay(f: unit -> ProbabilitySpace<_>) =
        [ ContinuedSubTree(lazy (f ())), 1. ] 

    member _.TryFinally(body: unit -> ProbabilitySpace<'a>, compensation: unit -> unit) : ProbabilitySpace<'a> =
        try
            body ()
        finally
            compensation ()

    member this.Using(resource: 'a, body: 'a -> ProbabilitySpace<'b>) : ProbabilitySpace<'b> when 'a :> IDisposable =
        this.TryFinally(
            (fun () -> body resource),
            (fun () ->
                if not (obj.ReferenceEquals(resource, null)) then
                    resource.Dispose()))

    member this.While(guard: unit -> bool, body: unit -> ProbabilitySpace<unit>) : ProbabilitySpace<unit> =
        if guard () then
            this.Bind(body (), fun () -> this.While(guard, body))
        else
            this.Return ()

    member this.For(sequence: seq<'a>, body: 'a -> ProbabilitySpace<unit>) : ProbabilitySpace<unit> =
        this.Using(
            sequence.GetEnumerator(),
            fun enumerator ->
                this.While(
                    enumerator.MoveNext,
                    fun () -> this.Delay(fun () -> body enumerator.Current)))

let dist = ProbabilitySpaceBuilder()

let observe test : ProbabilitySpace<_> =
    dist {
        if not test then
            return! fail ()
        else return ()
    }

let soft_observe weight : ProbabilitySpace<unit> =
    if Double.IsNaN weight || weight < 0.0 then
        invalidArg (nameof weight) "soft_observe weight must be finite and non-negative."
    elif weight = 0.0 then
        fail ()
    else [ Value (), weight ]

let constrain test = observe test

module ProbabilitySpace =
    let filterDistribution f p =
        dist {
            let! x = p
            do! observe (f x)
            return x
        }

    let mapDistribution f p =
        dist {
            let! x = p
            return f x
        } : ProbabilitySpace<_>

    let hasSubtrees nodes =
        nodes
        |> List.exists (function
            | (ContinuedSubTree _, _) -> true
            | (Value _, _) -> false)

    let inline getTopProbs maxp data =
        let rec innerloop curritems cumulativeprob =
            function
            | [] -> curritems
            | _ when cumulativeprob > maxp -> curritems
            | ((_, p) as item :: ps) -> innerloop (item :: curritems) (p + cumulativeprob) ps

        innerloop [] 0. data

    let getLargeProbItems maxp data =
        getTopProbs maxp (List.sortByDescending snd data)

    let nucleusSamples k p (probs: list<_>) =
        let choices = getLargeProbItems p probs

        if k > 0 then
            choices |> List.rev |> List.takeOrMax k
            else choices

    let typicalSamples k p (probs: list<_>) =
        let ent = -1. * (List.sumBy (fun (_, p) -> p * log (p+1e-40)) probs)

        let sorted =
            probs
            |> List.sortBy (fun (_, p) -> abs (-log p - ent)) 

        if k = 0 then
            getTopProbs p sorted
        else
            sorted |> List.takeOrMax k |> getTopProbs p

    let extractValue =
        function
        | Value x -> x
        | _ -> failwith "Not a value"

    let tryExtractValue =
        function
        | Value x -> Some x
        | _ -> None

    let printWith fp f distr =
        List.map
            (function
            | (Value x, p) -> f x, fp p
            | (ContinuedSubTree _, p) -> "...", fp p)
            distr

    let inline top l =
        l |> List.sortByDescending snd |> List.head

    let best l = l |> List.maxBy snd |> fst

    let mapInValues f l =
        [ for (v, p) in l do
              match v with
              | Value x -> yield (Value(f x), p)
              | _ -> yield (v, p) ]

    let mapProbs pf f l =
        [ for (v, p) in l do
              match v with
              | Value x -> yield (f x, pf p)
              | _ -> () ]

    let map f l =
        [ for (v, p) in l do
              match v with
              | Value x -> yield (f x, p)
              | _ -> () ]


let explore (maxdepth: int option) (choices: ProbabilitySpace<'T>) =
    let rec loop p depth down susp answers =
        match (down, susp, answers) with
        | _, [], answers -> answers
        | _, ((Value v, pt) :: rest), (ans, susp) ->
            loop p depth down rest (insertWithx (+) v (pt * p) ans, susp)
        | true, ((ContinuedSubTree (Lazy t), pt) :: rest), answers ->
            let down' =
                Option.map (fun x -> depth < x) maxdepth
                |> Option.defaultValue true

            loop (pt * p) (depth + 1) down' t answers
            |> loop p depth true rest

        | (down, ((c, pt) :: rest), (ans, susp)) -> loop p depth down rest (ans, (c, pt * p) :: susp)

    let (ans, susp) = loop 1.0 0 true choices (Dict(), [])

    //Map.fold (fun a v p -> (p, Value v)::a) susp ans : ProbabilitySpace<'T>
    [ yield! susp
      for (KeyValue (v, p)) in ans -> Value v, p ]

let nearly_one = 1.0 - 1e-7

(* Explore but do not flatten the tree:
   perform exact inference to the given depth
   We still pick out all the produced answers and note the failures. *)

let shallow_explore
    (subsample: ProbabilitySpace<_> -> ProbabilitySpace<_>)
    (maxdepth: int)
    (choices: ProbabilitySpace<_>)
    =
    let add_answer pcontrib v mp = insertWithx (+) v pcontrib mp

    let rec loop pc depth ans acc =
        function
        | [] -> (ans, acc)
        | _ when maxdepth = -1 -> (ans, acc)
        | ((Value v, p) :: rest) -> loop pc depth (add_answer (p * pc) v ans) acc rest
        | (c :: rest) when depth >= maxdepth -> loop pc depth ans (c :: acc) rest
        | ((ContinuedSubTree (Lazy t), p) :: rest) ->
            let (ans, ch) = loop (pc * p) (depth + 1) ans [] (subsample t)
            let ptotal = List.fold (fun pa (_, p) -> pa + p) 0.0 ch

            let acc =
                if ptotal = 0.0 then
                    acc
                else if ptotal < nearly_one then
                    (let ch' =
                        ch
                        |> List.map (fun (x, p) -> x, p / ptotal) 

                     ContinuedSubTree(lazy ch'), p * ptotal)
                    :: acc
                else
                    (ContinuedSubTree(lazy ch), p) :: acc

            loop pc depth ans acc rest

    let (ans, susp) = loop 1.0 0 (Dict()) [] (subsample choices)

    [ yield! susp
      for (KeyValue (v, p)) in ans -> Value v, p ]


///(* Explore the tree till we find the first success -- the first leaf
///   (V v) -- and return the resulting tree. If the tree turns out to
///   have no leaves, return the empty tree. *)
let rec first_success maxdepth =
    function
    | [] -> Seq.empty
    | _ when maxdepth = 0 -> Seq.empty
    | ((Value _, _) :: _) as l ->
        l
        |> Seq.groupBy fst
        |> Seq.map (fun (v, ps) -> v, Seq.sumBy snd ps)

    | ((ContinuedSubTree (Lazy t), pt) :: rest) -> (* Unclear: expand and do BFS *)
        first_success (maxdepth - 1) (List.append rest (List.map (fun (v, p) -> v, pt * p) t))

let inline first_success_rnd maxdepth ch =
    let rec loop maxdepth =
        function
        | [] -> None
        | _ when maxdepth = 0 -> None
        | ((Value _, _) :: _) as l ->
            let choices =
                [| for (v, p) in l do
                       match v with
                       | Value x -> yield (x, p)
                       | _ -> () |]

            if choices.Length = 0 then
                None
            else
                Some(Array.sampleOne choices)
        | ((ContinuedSubTree (Lazy t), pt) :: rest) -> (* Unclear: expand and do BFS *)
            loop (maxdepth - 1) (List.append rest (List.map (fun (v, p) -> (v, pt * p)) t))

    loop maxdepth ch

(* ------------------------------------------------------------------------ *)
(* Approximate inference strategies:				                        *)
(* Trace a few paths from the root to a leaf of the search tree             *)
(* The following procedures are non-deterministic; they use a given selector*)
(* procedure, of the type 'selector', to chose among the alternatives.      *)
(* For top-level inference, the selector uses system random generator.      *)

(* Naive, rejection sampling: the baseline *)
(* Random selection from a list of choices, using system randomness *)

let max_selector _ choices = Seq.maxBy snd choices |> Some

type ImportanceSelector<'T> = ResizeArray<ProbabilitySpace<'T> * float> -> option<ProbabilitySpace<'T> * float>

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

let private partitionRootStrata epsilon (choices: ProbabilitySpace<_>) =
    let rec loop heavyAcc heavyMass lightAcc lightMass =
        function
        | [] -> List.rev heavyAcc, heavyMass, List.rev lightAcc, lightMass
        | ((_, p) as branch) :: rest when p >= epsilon ->
            loop (branch :: heavyAcc) (heavyMass + p) lightAcc lightMass rest
        | ((_, p) as branch) :: rest ->
            loop heavyAcc heavyMass (branch :: lightAcc) (lightMass + p) rest

    loop [] 0.0 [] 0.0 choices

let private allocateStratifiedSamples nsamples totalA totalB =
    let hasA = totalA > 0.0
    let hasB = totalB > 0.0
    let nonEmptyStrata = (if hasA then 1 else 0) + (if hasB then 1 else 0)

    if nonEmptyStrata = 0 then
        0, 0
    elif nonEmptyStrata = 1 then
        if hasA then nsamples, 0 else 0, nsamples
    elif nsamples = 1 then
        if totalA >= totalB then 1, 0 else 0, 1
    else
        let reservedA = 1
        let reservedB = 1
        let remaining = nsamples - reservedA - reservedB
        let remainingFloat = float remaining
        let massTotal = totalA + totalB
        let rawA = remainingFloat * totalA / massTotal
        let rawB = remainingFloat - rawA
        let baseA = int (floor rawA)
        let baseB = int (floor rawB)
        let leftover = remaining - baseA - baseB
        let fracA = rawA - float baseA
        let fracB = rawB - float baseB

        if leftover = 0 then
            reservedA + baseA, reservedB + baseB
        elif fracA >= fracB then
            reservedA + baseA + leftover, reservedB + baseB
        else
            reservedA + baseA, reservedB + baseB + leftover

let private mergeScaledResults scale results (combinedDist: Dict<_, _>) =
    for (v, p) in results do
        match v with
        | Value value -> insertWithx (+) value (p * scale) combinedDist |> ignore
        | ContinuedSubTree _ -> ()

let private defaultSingletonUnravelLimit = 64 

let inline private addWeightedAnswer pcontrib ans v p =
    insertWithx (+) v (p * pcontrib) ans

let rec private collapseForcedPathBounded singletonUnravelLimit depth ps collapsedPrefix choices =
    match choices with
    | _ when depth >= singletonUnravelLimit -> Choice2Of2(choices, ps, collapsedPrefix)
    | [ContinuedSubTree (Lazy next), p] -> collapseForcedPathBounded singletonUnravelLimit (depth + 1) (p * ps) true next
    | [Value v, p] -> Choice1Of2(v, p * ps)
    | _ -> Choice2Of2(choices, ps, collapsedPrefix)

let private sample_dist_stratified_core runSampler nsamples epsilon ch =
    if nsamples <= 0 then
        invalidArg (nameof nsamples) "nsamples must be positive."
    elif epsilon < 0.0 || epsilon > 1.0 || Double.IsNaN epsilon then
        invalidArg (nameof epsilon) "epsilon must be in the inclusive range [0.0, 1.0]."
    elif List.isEmpty ch then
        []
    else
        let heavyBranches, totalHeavyP, lightBranches, totalLightP = partitionRootStrata epsilon ch
        let overallTotalP = totalHeavyP + totalLightP

        if overallTotalP <= 0.0 then
            []
        else
            let heavySamples, lightSamples = allocateStratifiedSamples nsamples totalHeavyP totalLightP

            let combinedDist = Dict<_, _>()

            if heavySamples > 0 && not (List.isEmpty heavyBranches) then
                runSampler heavySamples heavyBranches
                |> fun results -> mergeScaledResults totalHeavyP results combinedDist

            if lightSamples > 0 && not (List.isEmpty lightBranches) then
                runSampler lightSamples lightBranches
                |> fun results -> mergeScaledResults totalLightP results combinedDist

            let finalTotalP = combinedDist.Values |> Seq.sum

            if finalTotalP <= 0.0 then
                []
            else
                [ for (KeyValue(v, p)) in combinedDist -> Value v, p / finalTotalP ]


let path_sample selector subsample nsamples ch =
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
        "path_sample: done %d worlds\nTime taken: %A seconds"
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
        | (ContinuedSubTree (Lazy t), p) ->
            match collapseForcedPathBounded defaultSingletonUnravelLimit 0 p false t with
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

let sample_dist subsample nsamples max_pre_explore_depth maxdepth selector (ch: ProbabilitySpace<_>) =
    if nsamples <= 0 then
        invalidArg (nameof nsamples) "nsamples must be positive."

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
            | (ContinuedSubTree (Lazy t), p) :: rest ->
                let acc =
                    match collapseForcedPathBounded defaultSingletonUnravelLimit 0 p false t with
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
        | [ContinuedSubTree (Lazy th), p] -> loop (depth + 1) (p * pcontrib) ans th
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
        | (ans, [ ch, p ]) when depth < max_pre_explore_depth -> (* only one choice, make more *)
            pre_explore (depth + 1) (pcontrib * p) ans ch
        | (ans, cch) -> driver pcontrib ans cch

    pre_explore 0 1.0 (Dict()) ch 

let sample_dist_importance subsample nsamples max_pre_explore_depth maxdepth (selector: ImportanceSelector<_>) (ch: ProbabilitySpace<_>) =
    if nsamples <= 0 then
        invalidArg (nameof nsamples) "nsamples must be positive."

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
        let frontier = ResizeArray<ProbabilitySpace<_> * float>()

        let rec collect remaining =
            match remaining with
            | [] -> ans, frontier
            | (Value v, p) :: rest ->
                addWeightedAnswer pcontrib ans v p |> ignore
                collect rest
            | (ContinuedSubTree (Lazy t), p) :: rest ->
                let addPending pendingChoices correction =
                    frontier.Add(pendingChoices, correction)

                match collapseForcedPathBounded defaultSingletonUnravelLimit 0 p false t with
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
                                    addPending (normalizeChoices retainedMass pendingChoices) (correction * retainedMass)
                                else if retainedMass < nearly_one && pendingCollapsedPrefix then
                                    addPending pendingChoices correction
                                else
                                    addPending pendingChoices correction

                collect rest

        collect choices

    let rec loop depth pcontrib (ans: Dict<_, _>) =
        function
        | [Value v, p] -> addWeightedAnswer pcontrib ans v p
        | [] -> ans
        | [ContinuedSubTree (Lazy th), p] -> loop (depth + 1) (p * pcontrib) ans th
        | ch when depth < maxdepth ->
            match prepareFrontier pcontrib ans (subsample ch) with
            | ans, frontier when frontier.Count = 0 -> ans
            | ans, frontier ->
                match selector frontier with
                | None -> ans
                | Some (th, correction) -> loop (depth + 1) (pcontrib * correction) ans th
        | _ -> ans

    let toploop pcontrib frontier ans =
        match selector frontier with
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
        | ans, frontier when frontier.Count = 0 ->
            [ for (KeyValue (v, p)) in ans -> Value v, p ]
        | ans, frontier when frontier.Count = 1 && depth < max_pre_explore_depth ->
            let nextChoices, correction = frontier.[0]
            pre_explore (depth + 1) (pcontrib * correction) ans nextChoices
        | ans, frontier -> driver pcontrib ans frontier

    pre_explore 0 1.0 (Dict()) ch 
    

/// <summary>
/// A top-level stratified wrapper around the importance sampler.
/// It partitions the initial frontier into two strata and allocates samples across them.
/// </summary>
/// <param name="subsample">Function to select a subset of branches at each step.</param>
/// <param name="nsamples">The TOTAL number of samples to run across all strata.</param>
/// <param name="maxdepth">The maximum recursion depth for exploration.</param>
/// <param name="selector">Function to select one branch to follow from a list of continuations.</param>
/// <param name="epsilon">The top-level probability threshold used to separate "heavy" from "light" branches.</param>
/// <param name="ch">The initial probability space to sample from.</param>
/// <returns>A normalized probability distribution as a list of (Value, probability) pairs.</returns>
let sample_dist_stratified subsample nsamples max_pre_explore_depth maxdepth selector (epsilon: float) (ch: ProbabilitySpace<_>) =
    sample_dist_stratified_core
        (fun sampleCount branches ->
            sample_dist subsample sampleCount max_pre_explore_depth maxdepth selector branches)
        nsamples
        epsilon
        ch

/// <summary>
/// A top-level stratified wrapper around the fast importance sampler.
/// It partitions the initial frontier into two strata and allocates samples across them.
/// </summary>
/// <param name="subsample">Function to select a subset of branches at each step.</param>
/// <param name="nsamples">The TOTAL number of samples to run across all strata.</param>
/// <param name="maxdepth">The maximum recursion depth for exploration.</param>
/// <param name="selector">Selector over prepared continuation frontiers with explicit correction factors.</param>
/// <param name="epsilon">The top-level probability threshold used to separate "heavy" from "light" branches.</param>
/// <param name="ch">The initial probability space to sample from.</param>
/// <returns>A normalized probability distribution as a list of (Value, probability) pairs.</returns>
let sample_dist_importance_stratified subsample nsamples max_pre_explore_depth maxdepth (selector: ImportanceSelector<_>) (epsilon: float) (ch: ProbabilitySpace<_>) =
    sample_dist_stratified_core
        (fun sampleCount branches ->
            sample_dist_importance subsample sampleCount max_pre_explore_depth maxdepth selector branches)
        nsamples
        epsilon
        ch

//=================
///////////////////
let sample_importanceAux subsample selector pre_explore_maxdepth shallow_explore_maxdepth maxdpeth nsamples distr =
    sample_dist subsample nsamples pre_explore_maxdepth maxdpeth selector (shallow_explore subsample shallow_explore_maxdepth distr)

let sample_importanceFastAux subsample selector pre_explore_maxdepth shallow_explore_maxdepth maxdpeth nsamples distr =
    sample_dist_importance subsample nsamples pre_explore_maxdepth maxdpeth selector (shallow_explore subsample shallow_explore_maxdepth distr)
 

type Model() =
    static member ImportanceSamples
        (
            distr,
            nsamples,
            maxdepth,
            ?subsample,
            ?sortBeforeSampling,
            ?pre_exploreDepth,
            ?shallowExploreDepth,
            ?selector
        ) =
        match selector with
        | Some selector ->
            sample_importanceAux
                (defaultArg subsample id)
                selector
                (defaultArg pre_exploreDepth 3)
                (defaultArg shallowExploreDepth 2)
                maxdepth
                nsamples
                distr
        | None ->
            sample_importanceFastAux
                (defaultArg subsample id)
                (random_importance_selector (defaultArg sortBeforeSampling false))
                (defaultArg pre_exploreDepth 3)
                (defaultArg shallowExploreDepth 2)
                maxdepth
                nsamples
                distr 

    /// <summary>
    /// Fast stratified importance sampling over the root frontier using the
    /// optimized sampler path.  Epsilon separates "heavy" and "light"
    /// branches; at least one sample is allocated to each nonempty stratum.
    /// </summary>
    static member ImportanceSamplesStratified
        (
            distr,
            nsamples,
            maxdepth,
            epsilon,
            ?subsample,
            ?sortBeforeSampling,
            ?pre_exploreDepth,
            ?shallowExploreDepth,
            ?selector
        ) =
        let sel =
            defaultArg selector (random_importance_selector (defaultArg sortBeforeSampling false))

        sample_dist_importance_stratified
            (defaultArg subsample id)
            nsamples
            (defaultArg pre_exploreDepth 3)
            (defaultArg shallowExploreDepth 2)
            sel
            epsilon
            distr

    static member ExactInfer(distr, ?limit) = explore limit distr

    static member PathSample(distr, nsamples, ?sortBeforeSampling, ?subsample) =
        path_sample (random_selector (defaultArg sortBeforeSampling true)) (defaultArg subsample id) nsamples distr
 
    static member GreedySample(distr, nsamples, ?subsample) =
        path_sample (max_selector ()) (defaultArg subsample id) nsamples distr

    static member BeamSearch(distr, ?beamwidth, ?maxDepth, ?maxExpandedNodes) =
        beam_search
            (defaultArg beamwidth 1)
            maxDepth
            maxExpandedNodes
            distr


type ModelFrom<'a, 'b when 'b: comparison>(distr: ProbabilitySpace<'b>, ?subsampler) =
    let subsample = defaultArg subsampler id

    member __.model = distr

    member __.ImportanceSample(nsamples, maxdepth, ?preExploreDepth, ?shallowExploreDepth, ?subsampler, ?selector, ?sortBeforeSampling) =
        match selector with
        | Some selector ->
            sample_importanceAux
                (defaultArg subsampler subsample)
                selector
                (defaultArg preExploreDepth 3)
                (defaultArg shallowExploreDepth 2)
                maxdepth
                nsamples
                distr
        | None ->
            sample_importanceFastAux
                (defaultArg subsampler subsample)
                (random_importance_selector (defaultArg sortBeforeSampling false))
                (defaultArg preExploreDepth 3)
                (defaultArg shallowExploreDepth 2)
                maxdepth
                nsamples
                distr 

    /// Fast stratified version using the optimized importance sampler.
    member __.ImportanceSampleStratified(nsamples, maxdepth, epsilon, ?preExploreDepth, ?shallowExploreDepth, ?subsampler, ?selector, ?sortBeforeSampling) =
        let sel =
            defaultArg selector (random_importance_selector (defaultArg sortBeforeSampling false))

        sample_dist_importance_stratified
            (defaultArg subsampler subsample)
            nsamples
            (defaultArg preExploreDepth 3)
            (defaultArg shallowExploreDepth 2)
            sel
            epsilon
            distr

    member __.ExactInfer(?limit) = explore limit distr

    member __.PathSample(nsamples, ?sortBeforeSampling, ?subsampler) =
        path_sample
            (random_selector (defaultArg sortBeforeSampling true))
            (defaultArg subsampler subsample)
            nsamples
            distr

    member __.GreedySample(nsamples, ?subsampler) =
        path_sample (max_selector ()) (defaultArg subsampler subsample) nsamples distr

    member __.BeamSearch(?beamwidth, ?maxDepth, ?maxExpandedNodes) =
        beam_search
            (defaultArg beamwidth 1)
            maxDepth
            maxExpandedNodes
            distr


//=-=-=-=-=-=-=-=-=-
module Distributions =

    let bernoulli p =
        distribution [ (true, p)
                       (false, 1.0 - p) ]

    let bernoulliChoice p (a, b) = distribution [ (a, p); (b, 1.0 - p) ]

    let uniform (items: 'a list) =
        let num = float items.Length
        distribution (List.map (fun item -> item, 1. / num) items)

    let categorical distr =
        distribution (List.normalizeWeights distr)

    //=-=-=-=-=-=-=-=-=-=

    let rec geometric n p =
        dist {
            let! a = bernoulli p

            if a then
                return n
            else
                return! (geometric (n + 1) p)
        }

    ///polya's urn
    let rec beta roundto draws a b =
        dist {
            if draws <= 0 then
                return (round roundto (a / (a + b)))
            else
                let! ball =
                    categorical [ 1, a / (a + b)
                                  2, b / (a + b) ]

                if ball = 1 then
                    return! beta roundto (draws - 1) (a + 1.) b
                else
                    return! beta roundto (draws - 1) a (b + 1.)
        }

    let rec dirichlet3 roundto draws a b c : ProbabilitySpace<_> =
        dist {
            if draws <= 0 then
                return
                    (Array.map
                        (round roundto)
                        [| a / (a + b + c)
                           b / (a + b + c)
                           c / (a + b + c) |])
            else
                let! ball =
                    categorical [ 1, a / (a + b + c)
                                  2, b / (a + b + c)
                                  3, c / (a + b + c) ]

                if ball = 1 then
                    return! dirichlet3 roundto (draws - 1) (a + 1.) b c
                elif ball = 2 then
                    return! dirichlet3 roundto (draws - 1) a (b + 1.) c
                else
                    return! dirichlet3 roundto (draws - 1) a b (c + 1.)
        }

    let rec dirichlet roundto draws d : ProbabilitySpace<_> =
        dist {
            let t = List.sum d

            if draws <= 0 then
                return (List.map (fun a -> round roundto (a / t)) d)
            else
                let ps = List.mapi (fun i a -> i, (a / t)) d
                let! ball = categorical ps

                let d' = List.mapi (fun i a -> if i = ball then a + 1. else a) d

                return! dirichlet roundto (draws - 1) d'
        }

    let discretizedSampler coarsener sampler (n: int) =
        dist {
            return!
                categorical (
                    [ for _ in 1..n -> sampler () ]
                    |> coarsenWith coarsener
                )
        }
