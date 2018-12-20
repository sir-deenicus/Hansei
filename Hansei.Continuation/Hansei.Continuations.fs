module Hansei.Continuation

//continuation monad
type Cont<'R, 'A> = (('A -> 'R) -> 'R)
type ContBuilder() =
  member __.ReturnFrom(x) = x
  member __.Return(x) = fun k -> k x 
  member __.Bind(m, f) = fun k -> m (fun a -> (f a) k)
  member this.Zero() = this.Return ()  
  
let cont = ContBuilder()

///////////////////////////////////

type ProbabilitySpace<'T> = list<float * WeightedTree<'T>>
and WeightedTree<'T> = 
    | Value of 'T 
    | Continued of Lazy<ProbabilitySpace<'T>>    
      
let valueExtract = function Value x -> x

let valueExtract2 = function Value x -> Some x | _ -> None


let reflect tree k =  
    let rec make_choices pv = 
        List.map (function 
          | (p, Value x) -> (p, Continued(lazy(k x)))
          | (p, Continued(Lazy x)) -> (p, Continued(lazy(make_choices x)))) pv
        
    make_choices tree  : ProbabilitySpace<_>
    
(* Variable elimination optimization: transform a stochastic function
   a -> b to a generally faster function
*)
let variable_elim reify f arg = reflect (reify (f arg))    
 
let mapContinuation f thunk = cont {
    let! x = thunk
    return f x
} 

let rec drawrandom draws n p = cont {
    if n = 0 then return draws
    else let! fresh = p
         return! drawrandom (fresh::draws) (n-1) p
}

module ProbabilitySpace =
    let printWith f x =  
        List.map (function (p, Value x) -> p, f x | (p, Continued _) -> p, "...") x
    let map f l = 
        [for (p, t) in l do match t with Value(x) -> yield p, Value(f x) | _ -> ()]