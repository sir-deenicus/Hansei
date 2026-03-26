module Hansei.Probability 

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
    | ContinuedSubTree of Memo<ProbabilitySpace<'T>>

type SubtreeMemoization =
    | Memoize
    | NoMemoize

let inline subtreeMemoizationEnabled subtreeMemoization =
    match subtreeMemoization with
    | Memoize -> true
    | NoMemoize -> false

let internal continuedSubTree subtreeMemoization f =
    ContinuedSubTree (memoWith (subtreeMemoizationEnabled subtreeMemoization) f)
 
//if use value instead of Continued, infinite computations will fail to return/terminate
let distributionWith subtreeMemoization weightedlist : ProbabilitySpace<_> =
    List.map (fun (v, p) -> 
        continuedSubTree subtreeMemoization (fun () -> [Value v, 1.]), p) weightedlist 

let distribution weightedlist : ProbabilitySpace<_> =
    distributionWith Memoize weightedlist

let distributionOfSeqWith subtreeMemoization weightedlist : ProbabilitySpace<_> =
    List.map (fun (v, p) -> 
        continuedSubTree subtreeMemoization (fun () -> [Value v, 1.]), float p) (List.ofSeq weightedlist)

let distributionOfSeq weightedlist : ProbabilitySpace<_> =
    distributionOfSeqWith Memoize weightedlist

let alwaysWith subtreeMemoization x : ProbabilitySpace<_> =
    distributionWith subtreeMemoization [x, 1.]

let always x : ProbabilitySpace<_> =
    alwaysWith Memoize x

let exactlyWith subtreeMemoization x = distributionWith subtreeMemoization [ x, 1. ]

let exactly x = exactlyWith Memoize x

let fail () : ProbabilitySpace<_> = []

let reflectWith subtreeMemoization tree k =
    let rec make_choices pv =
        List.map
            (function
            | (Value x, p) -> continuedSubTree subtreeMemoization (fun () -> k x), p
            | (ContinuedSubTree t, p) -> continuedSubTree subtreeMemoization (fun () -> make_choices (force t)), p)
            pv

    make_choices tree: ProbabilitySpace<_>

let reflect tree k = reflectWith Memoize tree k
  
type ProbabilitySpaceBuilder(?subtreeMemoization) =
    let subtreeMemoization = defaultArg subtreeMemoization Memoize

    member d.Bind(space, k) = reflectWith subtreeMemoization space k
    member d.Return v = alwaysWith subtreeMemoization v
    member d.ReturnFrom vs : ProbabilitySpace<_> = vs
    member inline _.YieldFrom vs: ProbabilitySpace<_>  = vs 
    member d.BindReturn(p: ProbabilitySpace<'a>, f: 'a -> 'b) : ProbabilitySpace<_> = 
        reflectWith subtreeMemoization p (f >> alwaysWith subtreeMemoization)
    member d.Zero() = [] 
    member d.Yield x = d.Return x
     
    member _.Delay(f: unit -> ProbabilitySpace<_>) =
        [ continuedSubTree subtreeMemoization f, 1. ] 

    member this.While(guard: unit -> bool, body: unit -> ProbabilitySpace<unit>) : ProbabilitySpace<unit> =
        if guard () then
            this.Bind(body (), fun () -> this.While(guard, body))
        else
            this.Return ()

    member this.For(sequence: seq<'a>, body: 'a -> ProbabilitySpace<'b>) : ProbabilitySpace<unit> =
        let items = Seq.toList sequence

        let rec loop remaining =
            match remaining with
            | [] -> this.Return ()
            | x :: rest -> this.Bind(body x, fun _ -> loop rest)

        this.Delay(fun () -> loop items)

let dist = ProbabilitySpaceBuilder()
let distNoMemo = ProbabilitySpaceBuilder(NoMemoize)

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

let factor weight : ProbabilitySpace<unit> =
    soft_observe weight

/// Exactly integrate a finite local model into a single likelihood factor.
/// This is useful when a small latent submodel can be summed out analytically
/// instead of sampled and rejected later.
let exact_local_likelihood likelihood (localModel: ProbabilitySpace<'T>) : ProbabilitySpace<unit> =
    let rec totalLocalWeight pathWeight choices =
        choices
        |> List.sumBy (fun (node, branchWeight) ->
            let combinedWeight = pathWeight * branchWeight

            match node with
            | Value value ->
                let likelihoodWeight = likelihood value

                if Double.IsNaN likelihoodWeight || Double.IsInfinity likelihoodWeight || likelihoodWeight < 0.0 then
                    invalidArg (nameof likelihood) "exact_local_likelihood requires finite non-negative local likelihoods."

                combinedWeight * likelihoodWeight
            | ContinuedSubTree next -> totalLocalWeight combinedWeight (force next))

    let totalWeight =
        totalLocalWeight 1.0 localModel

    factor totalWeight

let exact_local_observe test (localModel: ProbabilitySpace<'T>) : ProbabilitySpace<unit> =
    exact_local_likelihood (fun value -> if test value then 1.0 else 0.0) localModel

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

