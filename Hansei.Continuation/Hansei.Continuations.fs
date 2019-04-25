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


module Continuation = 
    let map f thunk = cont {
        let! x = thunk
        return f x
    } 

module ProbabilitySpace = 
    (* Variable elimination optimization: transform a stochastic function
       a -> b to a generally faster function
    *) 

    let rec drawrandom draws n p = cont {
        if n = 0 then return draws
        else let! fresh = p
             return! drawrandom (fresh::draws) (n-1) p
    }
