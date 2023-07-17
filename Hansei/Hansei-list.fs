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
    | Continued of Lazy<ProbabilitySpace<'T>>

//if use value instead of Continued, infinite computations will fail to return/terminate
let distribution weightedlist : ProbabilitySpace<_> =
    List.map (fun (v, p) -> 
        Continued(lazy [Value v, 1.]), p) weightedlist 

let inline distributionOfSeq weightedlist : ProbabilitySpace<_> =
    List.map (fun (v, p) -> 
        Continued(lazy [Value v, 1.]), float p) (List.ofSeq weightedlist)

let always x : ProbabilitySpace<_> =
    distribution [x, 1.]

let exactly x = distribution [ x, 1. ]

let fail () : ProbabilitySpace<_> = []

let reflect tree k =
    let rec make_choices pv =
        List.map
            (function
            | (Value x, p) -> Continued(lazy (k x)), p
            | (Continued (Lazy t), p) -> Continued(lazy (make_choices t)), p)
            pv

    make_choices tree: ProbabilitySpace<_>

type ProbabilitySpaceBuilder() =
    member inline d.Bind(space, k) = reflect space k
    member d.Return v = always v
    member d.ReturnFrom vs : ProbabilitySpace<_> = vs
    member d.BindReturn(p: ProbabilitySpace<'a>, f: 'a -> 'b) : ProbabilitySpace<_> = 
        reflect p (f >> always)
    member d.Zero() = []
    member d.Combine(x, y) = List.append x y
    member d.Delay(f: unit -> list<_>) = f () 
    member d.Yield x = d.Return x
    
let dist = ProbabilitySpaceBuilder()

let observe test : ProbabilitySpace<_> =
    dist {
        if not test then
            return! fail ()
        else return ()
    }

let constrain test = observe test

module ProbabilitySpace =
    let filterDistribution f p =
        dist {
            let! x = p
            do! observe (f x)
            return x
        }

    let hasSubtrees nodes =
        nodes
        |> List.exists (function
            | (Continued _, _) -> true
            | (Value _, _) -> false)

    let inline getTopProbs maxp data =
        let rec innerloop curritems cumulativeprob =
            function
            | []-> curritems
            | _ when cumulativeprob > maxp -> curritems
            | ((_, p) as item :: ps) -> innerloop (item :: curritems) (p + cumulativeprob) ps

        innerloop [] 0. data

    let getLargeProbItems maxp data =
        getTopProbs maxp (List.sortByDescending snd data)

    let nucleusSamples k p (probs: list<_>) =
        let choices = getLargeProbItems p probs

        if k > 0 then
            choices |> List.rev |> List.takeOrMax k
        else
            choices

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
            | (Continued _, p) -> "...", fp p)
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
        | true, ((Continued (Lazy t), pt) :: rest), answers ->
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
        | ((Continued (Lazy t), p) :: rest) ->
            let (ans, ch) = loop (pc * p) (depth + 1) ans [] (subsample t)
            let ptotal = List.fold (fun pa (_, p) -> pa + p) 0.0 ch

            let acc =
                if ptotal = 0.0 then
                    acc
                else if ptotal < nearly_one then
                    (let ch' =
                        ch
                        |> List.map (fun (x, p) -> x, p / ptotal) 

                     Continued(lazy ch'), p * ptotal)
                    :: acc
                else
                    (Continued(lazy ch), p) :: acc

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

    | ((Continued (Lazy t), pt) :: rest) -> (* Unclear: expand and do BFS *)
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
        | ((Continued (Lazy t), pt) :: rest) -> (* Unclear: expand and do BFS *)
            loop (maxdepth - 1) (List.append rest (List.map (fun (v, p) -> (v, pt * p)) t))

    loop maxdepth ch

(* ------------------------------------------------------------------------ *)
(*	Approximate inference strategies:				                        *)
(*  Trace a few paths from the root to a leaf of the search tree            *)
(* The following procedures are non-deterministic; they use a given selector*)
(* procedure, of the type 'selector', to chose among the alternatives.      *)
(* For top-level inference, the selector uses system random generator.      *)

(* Naive, rejection sampling: the baseline *)
(* Random selection from a list of choices, using system randomness *)

let max_selector _ choices = Seq.maxBy snd choices |> Some
  
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


let rejection_sample selector subsample nsamples ch =
    let t0 = System.DateTime.Now

    let rec loop depth pcontrib ans =
        function
        | [Value v, p] -> insertWithx (+) v (p * pcontrib) ans
        | [] -> ans
        | [Continued (Lazy th), p] -> loop (depth + 1) (p * pcontrib) ans th
        | ch -> 
            match selector (subsample ch) with
            | None -> ans
            | Some(th, ptotal) -> loop (depth + 1) (pcontrib * ptotal) ans ([th, 1.0])

    let rec driver (ch: ProbabilitySpace<_>) ans =
        function
        | 0 ->
            let ns = float nsamples
            let t1 = System.DateTime.Now

            printfn
                "rejection_sample: done %d worlds\nTime taken: %A seconds"
                nsamples
                (round 3 ((t1 - t0).TotalSeconds))
            //Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans : ProbabilitySpace<_>
            [ for (KeyValue (v, p)) in ans -> Value v, p / ns ]

        | n -> driver ch (loop 0 1.0 ans ch) (n - 1)

    driver ch (Dict()) nsamples
      
let beam_search beamwidth ch =
    let rec pop pcontrib =
        function
        | (Value v, p) -> [Value v, p * pcontrib]
        | (Continued (Lazy t), p) ->
            match t with
            | [] -> []
            | [Value v, p1] -> [Value v, p * p1 * pcontrib]
            | ch ->
                List.sortByDescending (fun (_, p1) -> p * p1 * pcontrib) ch
                |> List.takeOrMax beamwidth
                |> List.map (fun (n, p1) -> n, p * p1 * pcontrib)

    let rec loop depth pcontrib =
        function 
        | [n] ->
            let node = pop pcontrib n

            if node |> ProbabilitySpace.hasSubtrees then
                loop (depth + 1) pcontrib node
            else
                node
        | [] -> [] 
        | ch ->
            let candidates =
                List.map (pop pcontrib) ch
                |> List.concat
                |> List.sortByDescending snd
                |> List.takeOrMax beamwidth

            if candidates |> ProbabilitySpace.hasSubtrees then
                loop (depth + 1) pcontrib candidates
            else
                candidates

    loop 0 1. ch
     
let sample_dist subsample nsamples maxdepth selector (ch: ProbabilitySpace<_>) =
    let rec unravelSingleton d ps =
        function
        | [Continued (Lazy v), p] -> unravelSingleton (d + 1) (p * ps) v
        | [Value v, p] -> Some(v, p * ps)
        | _ -> None

    let look_ahead pcontrib (ans, acc) =
        function (* explore the branch a bit *)
        | (Value v, p) -> insertWithx (+) v (p * pcontrib) ans, acc
        | (Continued (Lazy t), p) ->
            match t with
            | [] -> (ans, acc)
            | [Value v, p1] -> insertWithx (+) v (p * p1 * pcontrib) ans, acc
            | _ch ->
                match unravelSingleton 0 1. _ch with
                | Some (v, p1) -> insertWithx (+) v (p * p1 * pcontrib) ans, acc
                | _ ->
                    let ch = subsample _ch
                    let ptotal = List.fold (fun pa (_, p) -> pa + p) 0.0 ch

                    (ans,
                     if ptotal < nearly_one then
                         (List.map (fun (x, p) -> (x, p / ptotal)) ch, p * ptotal)
                         :: acc
                     else
                         (ch, p) :: acc)

    let rec loop depth pcontrib (ans: Dict<_, _>) =
        function
        | [Value v, p] -> insertWithx (+) v (p * pcontrib) ans
        | [] -> ans
        | [Continued (Lazy th), p] -> loop (depth + 1) (p * pcontrib) ans th
        | ch when depth < maxdepth -> (* choosing one thread randomly *)
            match List.fold (look_ahead pcontrib) (ans, []) (subsample ch) with
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

    let rec sample_runner samples th =
        function
        | 0 -> samples
        | n -> sample_runner (th samples) th (n - 1)

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
        match List.fold (look_ahead pcontrib) (ans, []) (subsample ch) with
        | (ans, []) -> (* pre-exploration solved the problem *) [ for (KeyValue (v, p)) in ans -> Value v, p ]
        | (ans, [ ch, p ]) when depth < maxdepth -> (* only one choice, make more *)
            pre_explore (depth + 1) (pcontrib * p) ans ch
        | (ans, cch) -> driver pcontrib ans cch

    pre_explore 0 1.0 (Dict()) ch 
    
//=================
///////////////////
let sample_importanceAux subsample selector pre_explore_maxdepth maxdpeth nsamples distr =
    sample_dist subsample nsamples maxdpeth selector (shallow_explore subsample pre_explore_maxdepth distr)

let iterative_deepening minsamples subsample selector pre_explore_maxdepth maxdepth nsamples distr =
    let rec iterate n =
        let res =
            sample_importanceAux subsample selector pre_explore_maxdepth n nsamples distr

        if res.Length < minsamples && n <= maxdepth then
            iterate (n + 1)
        else
            res

    iterate 1

type Model() =
    static member ImportanceSamples
        (
            distr,
            nsamples,
            maxdepth,
            ?subsample,
            ?sortBeforeSampling,
            ?shallowExploreDepth,
            ?selector
        ) =
        sample_importanceAux
            (defaultArg subsample id)
            (defaultArg selector (random_selector (defaultArg sortBeforeSampling true)))
            (defaultArg shallowExploreDepth 3)
            maxdepth
            nsamples
            distr

    static member IterativeDeepening
        (
            distr,
            nsamples,
            maxdepth,
            minsamples,
            ?subsample,
            ?sortBeforeSampling,
            ?shallowExploreDepth,
            ?selector
        ) =
        iterative_deepening
            minsamples
            (defaultArg subsample id)
            (defaultArg selector (random_selector (defaultArg sortBeforeSampling true)))
            (defaultArg shallowExploreDepth 3)
            maxdepth
            nsamples
            distr

    static member ExactInfer(distr, ?limit) = explore limit distr

    static member RejectionSample(distr, nsamples, ?sortBeforeSampling, ?subsample) =
        rejection_sample (random_selector (defaultArg sortBeforeSampling true)) (defaultArg subsample id) nsamples distr


    static member GreedySample(distr, nsamples, ?subsample) =
        rejection_sample (max_selector ()) (defaultArg subsample id) nsamples distr

    static member BeamSearch(distr, ?beamwidth) =
        beam_search (defaultArg beamwidth 1) distr


type ModelFrom<'a, 'b when 'b: comparison>(distr: ProbabilitySpace<'b>, ?subsampler) =
    let subsample = defaultArg subsampler id

    member __.model = distr

    member __.ImportanceSample(nsamples, maxdepth, ?shallowExploreDepth, ?subsampler, ?selector, ?sortBeforeSampling) =
        sample_importanceAux
            (defaultArg subsampler subsample)
            (defaultArg selector (random_selector (defaultArg sortBeforeSampling true)))
            (defaultArg shallowExploreDepth 3)
            maxdepth
            nsamples
            distr

    member __.IterativeDeepening
        (
            nsamples,
            maxdepth,
            minsamples,
            ?shallowExploreDepth,
            ?selector,
            ?subsampler,
            ?sortBeforeSampling
        ) =
        iterative_deepening
            minsamples
            (defaultArg subsampler subsample)
            (defaultArg selector (random_selector (defaultArg sortBeforeSampling true)))
            (defaultArg shallowExploreDepth 3)
            maxdepth
            nsamples
            distr

    member __.ExactInfer(?limit) = explore limit distr

    member __.RejectionSample(nsamples, ?sortBeforeSampling, ?subsampler) =
        rejection_sample
            (random_selector (defaultArg sortBeforeSampling true))
            (defaultArg subsampler subsample)
            nsamples
            distr

    member __.GreedySample(nsamples, ?subsampler) =
        rejection_sample (max_selector ()) (defaultArg subsampler subsample) nsamples distr

    member __.BeamSearch(?beamwidth) =
        beam_search (defaultArg beamwidth 1) distr


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
