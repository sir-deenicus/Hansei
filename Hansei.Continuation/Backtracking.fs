module Hansei.Backtracking

//The backtracking monad or fairstream concept is also Oleg's, copied from here: http://fortysix-and-two.blogspot.com/2009/09/simple-and-fair-backtracking.html
//and like the probability monad of this library, leverages the power of lazy non-determinism. The backtracking monad also implements
//the logic monad interface and so also captures the core of logic programming.

type LazyStream<'a> =
    | Nil //empty
    | One of 'a //one element
    | Choice of 'a * LazyStream<'a> //one element, and maybe more
    | Thunk of Lazy<LazyStream<'a>> //suspended stream
  
let rec choice k r r' = 
    match r with
    | Nil -> k r' //first empty-> try the second
    | One a -> k (Choice(a, r')) //Put in front of the new stream
    | Choice (a, rs) -> choice (fun xs -> k(Choice(a, xs))) r' rs //interleave r and r' here
    | Thunk (Lazy l) ->
        match r' with
        | Nil -> k l
        | One b -> k (Choice(b, l))
        | Choice (b, rs') -> choice (fun xs -> k(Choice(b, xs))) l rs'
        | Thunk (Lazy l2) -> k(Thunk(lazy(choice id l l2)))
    
let rec bind m f =
    match m with
    | Nil -> Nil
    | One a -> f a
    | Choice (a, r) -> choice id (f a) (Thunk(lazy (bind r f)))
    | Thunk (Lazy i) -> Thunk(lazy (bind i f))
  
module FairStream =
    let rec map f m =
        match m with
        | Nil -> Nil
        | One a -> One(f a)
        | Choice (a, r) -> Choice(f a, Thunk(lazy (map f r)))
        | Thunk (Lazy i) -> Thunk(lazy (map f i)) 
    
    let head = function 
        | One a -> Some a 
        | Choice(a, _) -> Some a
        | Thunk (Lazy (One a)) -> Some a
        | Thunk (Lazy (Choice(a, _))) -> Some a 
        | Nil -> None
        | Thunk (Lazy _) -> 
            failwith "Tried for head of Nested layers of lazy. It's not clear what the semantics of head should be here" //should not be encountered?

    let tail = function 
        | One _ -> Some Nil
        | Choice(_, r) -> Some r
        | Thunk (Lazy (One _)) -> Some Nil
        | Thunk (Lazy (Choice(_, r))) -> Some (Thunk (lazy r))
        | Nil -> None 
        | Thunk (Lazy l) ->
            failwith "Tried for tail of Nested layers of lazy. It's not clear what the semantics of tail should be here" //should not be encountered?
    
    let rec map2 f m m2 = 
        match m, m2 with
        | Nil, _ 
        | _ , Nil -> Nil
        | One a, r -> 
            match head r with 
            | None -> Nil
            | Some b -> One (f a b)
        | r, One b -> 
            match head r with 
            | None -> Nil
            | Some a -> One (f a b)
        | Choice (a, r), Choice(b,r2) -> Choice(f a b,Thunk(lazy (map2 f r r2)))
        | Thunk (Lazy i), l -> Thunk(lazy (map2 f i l))
        | l, Thunk (Lazy i) -> Thunk(lazy (map2 f l i))

    let zip m1 m2 = map2 (fun a b -> a,b) m1 m2 
    

type FairStream() =
    member __.Return a = One a
    member __.ReturnFrom(x) = x
    member __.Yield a = One a
    member __.Bind(m, f) = bind m f
    member __.Zero() = Nil
    member __.Combine(r, r') = choice id r r'
    member __.Delay(f: unit -> LazyStream<_>) = Thunk(Lazy.Create f)
    member __.MergeSources(xs, ys) = FairStream.zip xs ys

let bt = FairStream()

let rec run depth stream =
    match (depth, stream) with
    | _, Nil -> Seq.empty
    | _, One a -> seq { yield a }
    | _, Choice (a, r) -> seq { yield a; yield! run depth r }
    | 0, Thunk _ -> Seq.empty //exhausted depth
    | d, Thunk (Lazy r) -> run (d - 1) r

let guard assertion = bt { if assertion then return () }
 
let choices xs =
    let rec build xs = bt {
        match xs with 
        | [] -> () 
        | x::xs' -> yield x; return! build xs'
     } 
    build xs
