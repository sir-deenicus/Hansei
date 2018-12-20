module Hansei.Continuation.Rational

open MathNet.Numerics
 
type ValueContinuation<'T> = 
    | Value of 'T 
    | Continued of Lazy<RationalProbabilitySpace<'T>>    
and RationalProbabilitySpace<'T> = list<BigRational * ValueContinuation<'T>>  

let reflect tree k =  
    let rec make_choices pv = 
        List.map (function 
          | (p, Value x) -> (p, Continued(lazy(k x)))
          | (p, Continued(Lazy x)) -> (p, Continued(lazy(make_choices x)))) pv
        
    make_choices tree 

(* Variable elimination optimization: transform a stochastic function
   a -> b to a generally faster function
*)
let variable_elim reify f arg = reflect (reify (f arg))    
