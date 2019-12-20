﻿module Hansei.SymbolicProb

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

//let explore (maxdepth : int option) (choices : SymbolicProbabilitySpace<'T>) =
//  let rec loop p depth down susp answers =
//    match (down, susp, answers) with
//    | (_, [], answers) -> answers 
 
//    | (_, (pt, Value v) :: rest, (ans, susp)) ->
//      loop p depth down rest (insertWithx (+) v (pt*p) ans, susp)
 
//    | (true, (pt,Continued (Lazy t))::rest, answers) ->
//      let down' = match maxdepth with Some x -> depth < x | None -> true
//      loop p depth true rest <| loop (pt*p) (depth+1) down' (t) answers
 
//    | (down, (pt,c)::rest, (ans,susp)) ->
//      loop p depth down rest (ans, (pt*p,c)::susp)
       
//  let (ans,susp) = loop 1Q 0 true choices (Dict(), [])   
//  [ yield! susp
//    for (KeyValue(v,p)) in ans -> Algebraic.simplify true p, Value v] : SymbolicProbabilitySpace<_>

//module ProbabilitySpace =
//    let map f l = 
//        [for (p,v) in l do
//            match v with 
//            | Value x -> yield (p, Value(f x))
//            | _ -> yield (p,v)]
//    let mapValues f l = 
//        [for (p,v) in l do
//            match v with 
//            | Value x -> yield (p , (f x))
//            | _ -> ()]
            
//    let mapValuesProb fp f l = 
//        [for (p,v) in l do
//            match v with 
//            | Value x -> yield (fp p, (f x))
//            | _ -> ()]

//module Distributions =    
//  let bernoulli p = distribution [(p, true); (1Q-p, false)]

//  let bernoulliChoice p (a,b) = distribution [(p, a); (1Q-p, b)]
                                                          
//  let uniform (items:'a list) = 
//      let num = BigRational.FromInt items.Length |> Expression.FromRational
//      distribution (List.map (fun item -> 1Q/num, item) items)

//  let categorical distr = distribution distr 



//  ///polya's urn
//  let rec beta draws a b = cont {
//      if draws <= 0 then return a/(a+b)
//      else let! ball = categorical [a/(a+b),1;b/(a+b),2]
//           if ball = 1 then return! beta (draws - 1) (a+1Q) b
//           else return! beta (draws-1) a (b+1Q)
//  }

//let observe test = cont { if not test then return! fail() }

//let inline exact_reify model   =  explore None     (reify0 model)  
//let inline limit_reify n model =  explore (Some n) (reify0 model)   

//type Model<'a,'b when 'b : comparison>(thunk:(('a -> SymbolicProbabilitySpace<'a>) -> SymbolicProbabilitySpace<'b>)) =   
//     member __.model = thunk
//     member __.Reify (?limit) = match limit with None -> exact_reify thunk | Some n -> limit_reify n thunk 

//let inline normalize (choices:list<Expression * 'a>) =
//  let sum = List.sumBy (fst) choices
//  List.map (fun (p, v) -> (p/sum, v)) choices


 