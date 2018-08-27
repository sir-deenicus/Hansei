module Hansei.Continuation.Quantum

open System.Numerics

type ListofContinuationTrees<'T> = list<Complex * ValueContinuation<'T>>
and ValueContinuation<'T> = 
    | Value of 'T 
    | Continued of Lazy<ListofContinuationTrees<'T>>    

type QuantumProbabilitySpace<'T> = ListofContinuationTrees<'T>
type MixedProbabilitySpace<'T> = list<float * ValueContinuation<'T>>

let valueExtract = function Value x -> x

let valueExtract2 = function Value x -> Some x | _ -> None

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
