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

module Continuation = 
    let map f thunk = cont {
        let! x = thunk
        return f x
    }  

    let rec drawrandom draws n p = cont {
        if n = 0 then return draws
        else let! fresh = p
             return! drawrandom (fresh::draws) (n-1) p
    }
