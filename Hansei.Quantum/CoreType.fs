module Hansei.Types.Quantum

open MathNet.Symbolics

type QuantumProbabilitySpace<'T> = list<Complex * ComplexWeightedTree<'T>>
and ComplexWeightedTree<'T> = 
    | Value of 'T 
    | Continued of Lazy<QuantumProbabilitySpace<'T>>    
     
type MixedProbabilitySpace<'T> = list<float * ComplexWeightedTree<'T>>

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
