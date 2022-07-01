module Hansei.Rationals

open MathNet.Numerics
open Prelude.Common 
open MathNet.Symbolics.Core


//let nearly_one = 9007198354021067N/9007199254740992N 

//type RationalProbabilitySpace<'T> = GenericProbabilitySpace<BigRational,'T>
 
//let inline reify0 m = m (fun x -> [(1N, Value x)])

//let exactly x = distribution [1N, x] 

//let explore (maxdepth : int option) (choices : RationalProbabilitySpace<'T>) = 
//    GenericProb.explore id 1N maxdepth choices
//     : RationalProbabilitySpace<'T>

//let inline exact_reify model   =  explore None     (reify0 model)  

//let inline limit_reify n model =  explore (Some n) (reify0 model)   

//let normalize (choices:list<BigRational * 'a>) = normalize (List.sumBy fst) choices

//let random_selector choices = random_selector float 0N choices

//let rejection_sampler nsamples ch : RationalProbabilitySpace<_> =
//    rejection_sample_dist 1N BigRational.FromInt random_selector nsamples ch 
    
//module Distributions =
//    open GenericProb.Distributions

//    let bernoulli p = bernoulli 1N p

//    let bernoulliChoice p (a,b) = bernoulliChoice 1N p (a,b)
                                                          
//    let uniform (items:'a list) = uniform 1N BigRational.FromInt items

//    let categorical distr = distribution distr 

//    let geometric n p = geometric bernoulli n p 

//    let beta draws a b = beta 1N draws a b