module Hansei.SymbolicProb

open System
open Hansei.Utils
open Hansei.Continuation    
open MathNet.Symbolics
open Prelude.Common
open MathNet.Numerics
open MathNet.Symbolics.Core
open Hansei.GenericProb

type SymbolicProbabilitySpace<'T> = GenericProbabilitySpace<Expression,'T>
 
let inline reify0 m = m (fun x -> [(1Q, Value x)])

let exactly x = distribution [1Q, x] 

let explore (maxdepth : int option) (choices : SymbolicProbabilitySpace<'T>) = 
    GenericProb.explore (Algebraic.simplify true) 1Q maxdepth choices
     : SymbolicProbabilitySpace<'T>

let inline exact_reify model   =  explore None     (reify0 model)  

let inline limit_reify n model =  explore (Some n) (reify0 model)   

let normalize (choices:list<Expression * 'a>) = normalize (List.sumBy fst) choices

let random_selector choices = random_selector Expression.toFloat 0Q choices

let rejection_sampler nsamples ch =
    rejection_sample_dist 1Q Expression.FromInt32 random_selector nsamples ch 

type Model() =  
    static member ReifySymbolic(thunk, ?limit) = 
        match limit with
        | None -> exact_reify thunk
        | Some n -> limit_reify n thunk

module Distributions =
    open GenericProb.Distributions

    let bernoulli p = bernoulli 1Q p

    let bernoulliChoice p (a,b) = bernoulliChoice 1Q p (a,b)
                                                          
    let uniform (items:'a list) = uniform 1Q Expression.FromInt32 items

    let categorical distr = distribution distr 

    let geometric n p = geometric bernoulli n p 

    let beta draws a b = beta 1Q draws a b