module Hansei.Continuation

type Cont<'T, 'S, 'R> = ('T -> 'S) -> 'R

//continuation monad
type ContBuilder() =
    member __.ReturnFrom(x) = x
    member __.Return(x) : Cont<_, _, _> = fun k -> k x
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

    let run f = f id

    let iterate n thunk =
        let rec loop draws n = cont {
            if n = 0 then
                return draws
            else
                let! fresh = thunk
                return! loop (fresh :: draws) (n - 1) } 
        loop [] n

module DelimitedContinuation =
    type DCont<'T, 'R> = Cont<'T, 'R, 'R>

    let run (thunk: DCont<'T, 'T>) =
        thunk id

    let reset (thunk: DCont<'T, 'T>) : DCont<'T, 'R> =
        fun k -> k (run thunk)

    let shift (f: ('T -> DCont<'R, 'R>) -> DCont<'R, 'R>) : DCont<'T, 'R> =
        fun k ->
            let reify value : DCont<'R, 'R> =
                fun k' -> k' (k value)

            run (f reify)

    let map f thunk : DCont<_, _> = cont {
        let! x = thunk
        return f x
    }

    let iterate n thunk =
        let rec loop draws n = cont {
            if n = 0 then
                return draws
            else
                let! fresh = thunk
                return! loop (fresh :: draws) (n - 1) }
        loop [] n
