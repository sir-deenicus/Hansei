module Hansei.Backtracking

open Hansei.FSharpx.Collections

//The backtracking monad or fairstream concept is also Oleg's, based on the core given here: http://fortysix-and-two.blogspot.com/2009/09/simple-and-fair-backtracking.html
//and like the probability monad of this library, leverages the power of lazy non-determinism. The backtracking monad also implements
//the logic monad interface and so also captures the core of logic programming.
//One can imagine relaxing limitations--regular lists can also model non-determinism but are exhaustive.
//Moving to lazy lists the new issue is one of the bind operation where one or more are infinite lists. We never finish the first list,
//fairstreams fix this by interleaving. The core addition over lazy lists is the lazy suspended Thunk case.

type LazyStream<'a> =
    | Nil //empty
    | One of 'a //one element
    | Choice of 'a * LazyStream<'a> //one element, and maybe more
    | Thunk of Lazy<LazyStream<'a>> //suspended stream

let rec choice k r r' =
    match r with
    | Nil -> k r' //first empty-> try the second
    | One a -> k (Choice(a, r')) //Put in front of the new stream
    | Choice(a, rs) -> choice (fun xs -> k (Choice(a, xs))) r' rs //interleave r and r' here
    | Thunk(Lazy l) ->
        match r' with
        | Nil -> k r
        | One b -> k (Choice(b, l))
        | Choice(b, rs') -> choice (fun xs -> k (Choice(b, xs))) l rs'
        | Thunk(Lazy l2) -> k (Thunk(lazy (choice id l l2))) //interleave the two suspended streams

let rec bind m f =
    match m with
    | Nil -> Nil
    | One a -> f a
    | Choice(a, r) -> choice id (f a) (Thunk(lazy (bind r f)))
    | Thunk(Lazy i) -> Thunk(lazy (bind i f))

///rewrite bind to use continuation passing style
let rec bindc k m f =
    match m with
    | Nil -> k Nil
    | One a -> k (f a)
    | Choice(a, r) ->
        //bindc (fun xs -> k (Thunk(lazy (choice id (f a) xs)))) r f
        Thunk(lazy (bindc (fun rs -> k (choice id (f a) rs)) r f))
    | Thunk(Lazy i) -> k (Thunk(lazy (bindc id i f)))

module FairStream =
    let rec map f m =
        match m with
        | Nil -> Nil
        | One a -> One(f a)
        | Choice(a, r) -> Choice(f a, Thunk(lazy (map f r)))
        | Thunk(Lazy i) -> Thunk(lazy (map f i))

    let rec skipUntil giveupN f m =
        let rec tryfind n m =
            if n <= 0 then
                None
            else
                match m with
                | Nil -> None
                | One a as x -> if (f a) then Some x else None
                | Choice(a, rest) as r -> if f a then Some r else tryfind (n - 1) rest
                | Thunk(Lazy i) -> tryfind (n - 1) i

        tryfind giveupN m

    let rec first giveupN f m =
        let rec tryfind n m =
            if n <= 0 then
                None
            else
                match m with
                | Nil -> None
                | One a -> if (f a) then Some a else None
                | Choice(a, r) -> if f a then Some a else tryfind (n - 1) r
                | Thunk(Lazy i) -> tryfind (n - 1) i

        tryfind giveupN m

    let exists giveupN f m = first giveupN f m |> Option.isSome

    let rec headn maxn n =
        function
        | One a -> Some a
        | Choice(a, _) -> Some a
        | Thunk(Lazy(One a)) -> Some a
        | Thunk(Lazy(Choice(a, _))) -> Some a
        | Nil -> None
        | Thunk(Lazy l) ->
            if n > maxn then
                failwith "Nesting levels max hit"
            else
                headn maxn (n + 1) l

    let rec tailn maxn n =
        function
        | One _ -> Some Nil
        | Choice(_, r) -> Some r
        | Thunk(Lazy(One _)) -> Some Nil
        | Thunk(Lazy(Choice(_, r))) -> Some r
        | Nil -> None
        | Thunk(Lazy l) ->
            if n > maxn then
                failwith "Nesting levels max hit"
            else
                tailn maxn (n + 1) l

    let rec headAndTailN maxn n =
        function
        | One a
        | Thunk(Lazy(One a)) -> Some(a, Nil)
        | Choice(a, t)
        | Thunk(Lazy(Choice(a, t))) -> Some(a, t)
        | Nil -> None
        | Thunk(Lazy l) ->
            if n > maxn then
                failwith "Nesting levels max hit"
            else
                headAndTailN maxn (n + 1) l

    let head l = headn 4 0 l

    let tail l = tailn 4 0 l

    let (|Cons|Empty|) l =
        match (headAndTailN 4 0 l) with
        | None -> Empty
        | Some(a, t) -> Cons(a, t)

    let rec map2 f m m2 =
        match m, m2 with
        | Nil, _
        | _, Nil -> Nil
        | One a, r ->
            match head r with
            | None -> Nil
            | Some b -> One(f a b)
        | r, One b ->
            match head r with
            | None -> Nil
            | Some a -> One(f a b)
        | Choice(a, r), Choice(b, r2) -> Choice(f a b, Thunk(lazy (map2 f r r2)))
        | Thunk(Lazy i), l -> Thunk(lazy (map2 f i l))
        | l, Thunk(Lazy i) -> Thunk(lazy (map2 f l i))

    let zip m1 m2 = map2 (fun a b -> a, b) m1 m2

    let rec filter f m =
        match m with
        | Nil -> Nil
        | Choice(a, Nil)
        | One a -> if f a then m else Nil
        | Choice(a, r) ->
            if f a then
                Choice(a, Thunk(lazy (filter f r)))
            else
                Thunk(lazy (filter f r))
        | Thunk(Lazy i) -> Thunk(lazy (filter f i))

    let rec concat (ls: LazyStream<LazyStream<'a>>) =
        match ls with
        | Nil -> Nil
        | One a -> a
        | Choice(a, Nil) -> a
        | Choice(Nil, b) -> Thunk(lazy (concat b))
        | Choice(a, b) -> Thunk(lazy (choice id a (concat b)))
        | Thunk(Lazy r) -> Thunk(lazy (concat r))

    ///Warning!!! Must be Finite!!
    let fold f s ls =
        //Use CPS
        let rec loop k s l =
            match l with
            | Empty -> k s
            | Cons(x, xs) -> loop (fun s' -> k (f s' x)) s xs

        loop id s ls

    let reduce f ls =
        match ls with
        | Empty -> failwith "Empty stream"
        | Cons(x, xs) -> fold f x xs

    let combine l1 l2 = choice id l1 l2

    let ofList xs =
        let rec build xs =
            match xs with
            | [] -> Nil
            | [ x ] -> One x
            | x :: xs -> Choice(x, Thunk(lazy (build xs)))

        build xs

    let ofArray xs = ofList (Array.toList xs)

    let ofSeq (xs: _ seq) = 
        let e = xs.GetEnumerator()
        let rec loop() =
            if e.MoveNext() then
                Choice(e.Current, Thunk(lazy (loop())))
            else
                e.Dispose()
                Nil

        loop()

    let ofLazyList xs = 
        let rec build xs =
            match xs with
            | LazyList.Nil -> Nil
            | LazyList.Singleton x -> One x
            | LazyList.Cons(x, xs) -> Choice(x, Thunk(lazy (build xs)))

        build xs

    let cartesianProduct xs ys = //as either stream might be infinite, we are forced to use bind
        bindc id xs (fun x -> bindc id ys (fun y -> One(x, y)))


type FairStream() =
    member fs.YieldFrom x = fs.ReturnFrom x
    member __.Yield a = One a
    member __.Return a = One a
    member __.ReturnFrom(x) = x
    member __.Bind(m, f) = bindc id m f
    member __.Zero() = Nil
    member __.Combine(r, r') = choice id r r'
    member __.Delay(f: unit -> LazyStream<_>) = Thunk(Lazy.Create f)
    member __.BindReturn(stream: LazyStream<'a>, f: 'a -> 'b) = FairStream.map f stream

    member __.BindReturn2(stream: LazyStream<'a>, stream2: LazyStream<'b>, f: 'a -> 'b -> 'c) =
        FairStream.map2 f stream stream2

let bt = FairStream()

let rec run depth stream =
    match (depth, stream) with
    | _, Nil -> LazyList.empty
    | _, One a -> LazyList.singleton a
    | _, Choice(a, r) ->
        LazyList.lazyList {
            yield a
            yield! run depth r
        }
    //LazyList.cons a (run depth r)
    | Some 0, Thunk _ -> LazyList.empty //exhausted depth
    | d, Thunk(Lazy r) -> run (Option.map (fun n -> n - 1) d) r

let guard assertion =
    bt {
        if assertion then
            return ()
    }

///list to fairstream
let choices xs = FairStream.ofList xs
    

