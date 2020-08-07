module Hansei.Continuation

type Cont<'T, 'S, 'R> = ('T -> 'S) -> 'R

//continuation monad
type ContBuilder() =
    member __.ReturnFrom(x) = x
    member __.Return(x) : Cont<_, _, _> = (fun k -> k x)
    member __.Bind(c : Cont<_, _, _>, f : _ -> Cont<_, _, _>) : Cont<_, _, _> =
        fun k -> c (fun a -> (f a) k)
    member this.Zero() = this.Return() 

let cont = ContBuilder()

/////////////////////////////////// 

module Continuation = 
    let map f thunk : Cont<_, _, _> = cont {
        let! x = thunk
        return f x 
    }

    let shift f = fun k -> f k

    let reset f = fun k -> k (f id)

    let shift2 (f : (_ -> Cont<_, _, _>) -> Cont<_, _, _>)  =
        fun k -> f (fun v -> cont.Return (k v)) id

    let run f = f id

    let iterate n thunk = 
        let rec loop draws n = cont {
            if n = 0 then return draws
            else let! fresh = thunk
                 return! loop (fresh::draws) (n-1)
            }
        loop [] n
