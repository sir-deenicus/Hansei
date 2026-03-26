module Hansei.Distributions
open Probability
open Prelude.Common
open Utils

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
