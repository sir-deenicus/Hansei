﻿module Hansei.Generic.SymbolicProb

open System
open Hansei.Utils
open Hansei.Continuation    
open MathNet.Symbolics
open Prelude.Common
open MathNet.Numerics
open MathNet.Symbolics.Core
open Hansei.Generic.GenericProb
open MathNet.Symbolics.NumberProperties
open Hansei.Generic.GenericProb.Distributions
open Hansei.FSharpx.Collections

type SymbolicProbabilitySpace<'T> = GenericProbabilitySpace<'T, Expression>
 
let distribution_of_lazy ch = distributionOfLazy 1Q ch 
            
let distribution ch = distribution_of_lazy (LazyList.ofList ch)  
  
let always x = always 1Q x
    
let exactly x = exactly 1Q x
     
let dist = GenericProbabilitySpaceBuilder(1Q)

let observe test = observe 1Q test

let constrain test = constrain 1Q test
    
let filterDistribution f p = filterDistribution 1Q f p

let explore maxdepth choices = explore 1Q maxdepth choices
 
module Distributions =
    let bernoulli p = bernoulli 1Q p
    let uniform l = uniform 1Q Expression.FromInt32 l
    let categorical distr = categorical 1Q distr
    let geometric n p = geometric 1Q bernoulli n p
    let beta n a b = beta 1Q n a b

    let dirichlet n d = dirichlet 1Q n d
    let discretizedSampler coarsener sampler (n: int) =
        discretizedSampler 1Q Expression.FromInt32 coarsener sampler n

let log0 x = if x = 0Q then 0Q else log x

let toBits x = x / log 2Q  

module ProbabilitySpace =
    let inline expectedValue f ps =
        ProbabilitySpace.expectedValue (fun i -> Vars.r[i]) f ps

  