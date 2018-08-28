module Hansei.Continuation

//continuation monad
type Cont<'R, 'A> = (('A -> 'R) -> 'R)
type ContBuilder() =
  member __.ReturnFrom(x) = x
  member __.Return(x) = fun k -> k x 
  member __.Bind(m, f) = fun k -> m (fun a -> (f a) k)
  member this.Zero() = this.Return ()  


type ListofContinuationTrees<'T> = list<float * ValueContinuation<'T>>
and ValueContinuation<'T> = 
    | Value of 'T 
    | Continued of Lazy<ListofContinuationTrees<'T>>    

type ProbabilitySpace<'T> = ListofContinuationTrees<'T>

let valueExtract = function Value x -> x

let valueExtract2 = function Value x -> Some x | _ -> None

let cont = ContBuilder()


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

let filterContinuation f p = cont {
    let! x = p
    do! (f x)
    return x
}   

let mapContinuation f thunk = cont {
    let! x = thunk
    return f x
}