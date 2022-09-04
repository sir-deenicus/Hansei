#r @"C:\Users\cybernetic\source\repos\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
// #I @"C:\Users\cybernetic\.nuget\packages\"
// #r @"mathnet.numerics\4.9.0\lib\net40\MathNet.Numerics.dll"

#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx"
//open Parseutils
open Prelude.Common
open Prelude.Math
open System
open MathNet.Symbolics.Core.Vars
open MathNet.Symbolics.Utils
open Hansei.FSharpx.Collections.LazyList.ComputationExpression
open MathNet.Symbolics.NumberProperties 
open Hansei.FSharpx.Collections
open Hansei.Backtracking

(**A simple wrapper around the list monad can do some book-keeping of probabilities for us by silently tracking/propagating joint probabilities*)
module SimplestWrapper =
    let inline bind (dist: list<'item * 'number>) (k: 'item -> list<'changeditem * 'number>) =
        [ for (x, p) in dist do
              for (y, p2) in k x do
                  yield (y, p * p2) ]

    let fail () = []
    let bernoulli p = [ true, p; false, 1. - p ]

    let uniform l =
        l |> List.map (fun x -> x, 1. / float l.Length)

    let inline categorical l = List.normalizeWeights l
    let bernoulliChoice a b p = [ a, p; b, 1. - p ]
    let always x = [ x, 1. ]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero() = always ()

        member d.MergeSources(xs, ys) =
            [ for (x, p) in xs do
                  for (y, p2) in ys do
                      yield ((x, y), p * p2) ]

    let dist = DistributionBuilder()

    let observe test =
        dist { if not test then return! fail () }

open SimplestWrapper
 
dist {
    let! b = bernoulli 0.5
    and! b2 = bernoulli 0.75
    and! b3 = bernoulli 0.75
    let! c = uniform [ b2; b3 ]
    and! b4 = bernoulli 0.25
    return (b, b2, b3, c, b4)
}     

dist {
    let! b = bernoulliChoice "H" "T" 0.5
    let! b2 = bernoulliChoice "H" "T" 0.5

    let! b3 =
        if b2 = "H" then
            bernoulliChoice "H" "T" 0.25
        else
            bernoulliChoice "H" "T" 0.5

    return (b, b2, b3)
}

(**There is a close relationship between control structures for non-determinism and inference. Monads show up a lot when you want to manage complex control flow using higher order functions and a simple pipelining design. Nondeterminism is about computations which have multiple possibilities and branching paths. Hence the close relation. *)
(** But what if we want to sample from a very large space? The below computation will run for a very long time. Laziness will be helpful. *)
dist {
    let! a = uniform [1..10000]
    let! b = uniform [1..10000]
    return (a + b)
} 
 
module LazyListWrapper =
    open Hansei.FSharpx.Collections.LazyList.ComputationExpression
    let inline bind (dist : LazyList<'item * 'number>)
               (k : 'item -> LazyList<'changeditem * 'number>) =

        lzlist {
            let! (x, p) = dist
            let! (y, p2) = k x
            yield (y, p * p2)
        }

    let fail() = LazyList.empty
    let bernoulli p = lzlist {yield true, p; yield false, 1.- p} 
    let always x = lzlist {yield x,1.}

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = LazyList.choice a b
        member __.Delay(f: unit -> LazyList<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

module LazySeqWrapper =
    let inline bind (dist : seq<'item * 'number>)
               (k : 'item -> seq<'changeditem * 'number>) =
        seq { for (x, p) in dist do
                for (y, p2) in k x do
                    yield (y, p * p2) }

    let fail() = Seq.empty
    let bernoulli p = seq [true, p; false, 1.- p]
    let uniform l = l |> Seq.map (fun x -> x, 1./float (Seq.length l))
    let bernoulliChoice a b p = seq [a,p;b,1.- p]
    let always x = seq [x,1.]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = Seq.concat [a;b]
        member __.Delay(f: unit -> seq<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open LazySeqWrapper
 
let qn =
    dist {
        let! a = uniform (seq [1..10000])
        let! b = uniform (seq [1..10000])
        return (a + b) % 20
    } |> Seq.take 100 |> Seq.toArray |> Array.groupBy fst |> Array.map (fun (x,ps) -> x, Array.sumBy snd ps)

let rec geom c p =
    dist {
        let! b = bernoulli p
        if b then return c
        else return! geom (c+1) p
    }

dist {
    let! i = geom 0 0.5
    do! observe (i> 3)
    let! j = geom 1 0.6
    return (i+j)
} |> Seq.take 1

geom 0 0.6 |> Seq.take 10  |> Seq.toArray

let rec infiniteFlips p = LazyListWrapper.dist {
    let! b = LazyListWrapper.bernoulli p
    return b
    return! infiniteFlips p
    }

infiniteFlips 0.001 |> LazyList.take 10 |> LazyList.toArray//|> Seq.take 10

dist {
    let! b = infiniteFlips 0.5
    let! j = geom 1 0.6
    do! observe (not b)
    return (b, j)
} 

seq {
    for i in 1 .. 10 do
        for j in Seq.initInfinite id do
            if i > 5 then yield (i, j)
}
|> Seq.take 1

seq {
    for i in 1 .. 10 do
        for j in Seq.initInfinite id do
            yield (i, j)
}
|> Seq.take 12
|> Seq.toArray

let ll1 = LazyList.ofSeq (Seq.initInfinite id)

ll1 |> LazyList.take 3 |> LazyList.toArray

seq { for i in 1..10 do
        for j in Seq.initInfinite id do
            if i > 5 then yield (i,j) }
|> Seq.take 1

lzlist {   
    let! i = LazyList.ofList [1..10]
    let! j = ll1 
    yield (i,j) }
|> LazyList.take 12
|> Seq.toArray

module BtWrapper =
    let inline bind (dist : LazyStream<'item * 'number>)
               (k : 'item -> LazyStream<'changeditem * 'number>) =
        bt { let! (x, p) = dist
             let! (y, p2) = k x
             yield (y, p * p2) }

    let fail() = Nil
    let bernoulli p = bt {yield true, p; yield false, 1.- p}
    let uniform l = l |> Seq.map (fun x -> x, 1./float (Seq.length l))
    let bernoulliChoice a b p = bt {yield a,p; yield b,1.- p}
    let always x = bt {yield x,1.}

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = choice id a b
        member __.Delay(f: unit -> LazyStream<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

    //import Control.Applicative
    //import Control.Monad.Logic
    
    //parents :: [ (String, String) ]
    //parents = [ ("Sarah",  "John")
    //          , ("Arnold", "John")
    //          , ("John",   "Anne")
    //          ]
    
    //grandparent :: String -> Logic String







    //grandparent grandchild = do (p, c) <- choose parents
    //                            (c', g) <- choose parents
    //                            guard (c == c')
    //                            guard (g == grandchild)
    //                            pure p
    
    //choose = foldr ((<|>) . pure) empty
    
    //main = do let grandparents = observeAll (grandparent "Anne")
    //          putStrLn $ "Anne's grandparents are: " <> show grandparents



//
open BtWrapper

let rec infiniteFlips p = dist {
        let! b = bernoulli p
        return b
        return! infiniteFlips p
    }

infiniteFlips 0.001 |> run 100 |> Seq.take 50 |> Seq.toArray

