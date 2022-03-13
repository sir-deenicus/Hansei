module Hansei.DiscreteProb

open Prelude.Common

module ListProb =  
    let inline aggregate items =
        items
        |> List.groupBy fst
        |> List.map (keepLeft (List.sumBy snd))

    let inline bind (dist: list<'item * 'number>) (k: 'item -> list<'changeditem * 'number>) =
        [ for (x, p) in dist do
            for (y, p2) in k x do
                yield (y, p * p2) ]

    let fail () = []

    let inline always one x = [ x, one ]

    type DistributionBuilder<'T>(one : 'T) =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always one v
        member d.ReturnFrom vs = vs
        member d.Zero() = always one ()

        member inline d.MergeSources(xs: list<'item * 'number>, ys: list<'item * 'number>) =
            [ for (x, p) in xs do
                for (y, p2) in ys do
                    yield ((x,y), p * p2) ] 

    let dist one = DistributionBuilder(one)

    let inline bernoulli one p = [ true, p; false, one - p ]

    let inline uniform ofInt one l =
        l |> List.map (fun x -> x, one / ofInt l.Length)

    let inline categorical l = List.normalizeWeights l

    let inline bernoulliChoice one a b p = [ a, p; b, one - p ]
     
    let observe one test =
        dist one { if not test then return! fail () }
         

module Float =
    let dist = ListProb.dist 1.
    let uniform l = ListProb.uniform float 1. l
    let observe = ListProb.observe 1.
    let bernoulli p = ListProb.bernoulli 1. p
    let bernoulliChoice a b p = ListProb.bernoulliChoice 1. a b p
