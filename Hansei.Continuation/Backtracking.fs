module Hansei.Backtracking

open Hansei.FSharpx.Collections
open Prelude.Common

//The backtracking monad or fairstream concept is also Oleg's, based on the core given here: http://fortysix-and-two.blogspot.com/2009/09/simple-and-fair-backtracking.html
//and like the probability monad of this library, leverages the power of lazy non-determinism. The backtracking monad also implements
//the logic monad interface and so also captures the core of logic programming. This is because unlike lazy lists (which are depth first), it maintains a BFSish frontier and is complete.
//One can imagine relaxing limitations--regular lists can also model non-determinism but are exhaustive.
//Moving to lazy lists the new issue is one of the bind operation where one or more are infinite lists. We never finish the first list,
//fairstreams fix this by interleaving. The core addition over lazy lists is the lazy suspended Thunk case.

type LazyFairStream<'a> =
    | Nil //empty
    | One of 'a //one element
    | Choice of 'a * LazyFairStream<'a> //one element, and maybe more
    | Thunk of Lazy<LazyFairStream<'a>> //suspended stream

[<TailCall>]
let rec choice0 k r r' =
    match r with
    | Nil -> k r' //first empty-> try the second
    | One a -> k (Choice(a, r')) //Put in front of the new stream
    | Choice(a, rs) -> choice0 (fun xs -> k (Choice(a, xs))) r' rs //interleave r and r' here
    | Thunk(Lazy l) ->
        match r' with
        | Nil -> k r
        | One b -> k (Choice(b, l))
        | Choice(b, rs') -> choice0 (fun xs -> k (Choice(b, xs))) l rs'
        | Thunk(Lazy l2) -> k (Thunk(lazy (choice0 id l l2))) //interleave the two suspended streams

[<TailCall>]
let rec choice r r' =
    match r, r' with
    | Nil, s -> s 
    | s, Nil -> s
    | One a, s -> Choice(a, s)
    | s, One b -> Choice(b, s)
    | Choice(a, rs), s -> Choice(a, Thunk(lazy (choice s rs)))          // swap roles each step
    | Thunk(Lazy l), Thunk(Lazy l2) -> Thunk(lazy (choice l l2))
    | Thunk(Lazy l), s -> Thunk(lazy (choice s l))                       // swap to keep alternation 
    
//let choice k r r' = k (interleave r r')
  
let rec bind m f =
    match m with
    | Nil -> Nil
    | One a -> f a
    | Choice(a, r) -> choice (f a) (Thunk(lazy (bind r f)))
    | Thunk(Lazy i) -> Thunk(lazy (bind i f))
     
let rec bindc k m f =
    match m with
    | Nil -> k Nil
    | One a -> k (f a)
    | Choice(a, r) ->
        //bindc (fun xs -> k (Thunk(lazy (choice id (f a) xs)))) r f
        Thunk(lazy (bindc (fun rs -> k (choice (f a) rs)) r f))
    | Thunk(Lazy i) -> k (Thunk(lazy (bindc id i f)))

[<TailCall>]
let rec bindc2 k m f =
    match m with
    | Nil -> k Nil
    | One a -> k (f a)
    | Choice(a, r) ->
        bindc2 (fun xs -> k (Thunk(lazy (choice (f a) xs)))) r f
    | Thunk(Lazy i) -> k (Thunk(lazy (bindc2 id i f)))

[<TailCall>]
let rec bindc3 k m f =
    match m with
    | Nil -> k Nil
    | One a -> k (f a)
    | Choice(a, r) ->
        // Emit current branch immediately; defer recursion on r.
        // No extra continuation layering; only one Thunk per Choice (like bind).
        k (choice (f a) (Thunk(lazy (bindc3 id r f))))
    | Thunk(Lazy i) ->
        // Preserve suspension; recurse when forced.
        k (Thunk(lazy (bindc3 id i f)))

// Collapse chains of Thunk to avoid deep recursion on left-spines (prevents SO in bind)
[<TailCall>]
let rec private unwrap =
    function
    | Thunk (Lazy t) -> unwrap t
    | s -> s

// Stack-safe, non-CPS bind: never mixes CPS; only one Thunk added per Choice,
// and Thunk chains are collapsed before each step.
let bindSafe m f =
    let rec step s =
        match unwrap s with
        | Nil -> Nil
        | One a -> f a
        | Choice(a, rest) ->
            // Interleave current mapped head with deferred processing of the rest
            choice (f a) (Thunk(lazy (step rest)))
        | Thunk _ -> failwith "unreachable (unwrap removed Thunk)"
    step m

[<TailCall>]
let rec bindSafeRec m f =
    match unwrap m with
    | Nil -> Nil
    | One a -> f a
    | Choice(a, rest) ->
        // Emit the mapped head immediately and recurse on the tail.
        // The recursive call is suspended to preserve fair interleaving.
        choice (f a) (Thunk(lazy (bindSafeRec rest f)))
    | Thunk _ -> failwith "unreachable (unwrap removed Thunk)"

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

    let head l = headn 5 0 l

    let tail l = tailn 5 0 l

    let (|Cons|Empty|) l =
        match (headAndTailN 5 0 l) with
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

    let rec concat (ls: LazyFairStream<LazyFairStream<'a>>) =
        match ls with
        | Nil -> Nil
        | One a -> a
        | Choice(a, Nil) -> a
        | Choice(Nil, b) -> Thunk(lazy (concat b))
        | Choice(a, b) -> Thunk(lazy (choice a (concat b)))
        | Thunk(Lazy r) -> Thunk(lazy (concat r))

    let fold maxSteps f s ls =
        let rec loop steps k acc l =
            if steps <= 0 then None
            else
                match l with
                | Empty -> Some (k acc)
                | Cons(x, xs) -> loop (steps - 1) (fun acc' -> k (f acc' x)) acc xs
        loop maxSteps id s ls

    /// <summary>
    /// Produces a stream of accumulated values by applying a binary function to an accumulator and each element of an input stream,
    /// analogous to List.scan/Seq.scan.
    /// </summary>
    /// <param name="f">Function that combines the current accumulator with the next element to produce a new accumulator.</param>
    /// <param name="init">Initial accumulator value that becomes the first element of the resulting stream.</param>
    /// <param name="s">Input stream to scan. Expected to be one of the stream cases: Nil, One, Choice, or Thunk(Lazy ...).</param>
    /// <returns>
    /// A stream whose first element is the initial accumulator and whose subsequent elements are the successive accumulated values.
    /// </returns>
    /// <remarks>
    /// The implementation preserves laziness by wrapping recursive work in Thunk where appropriate, so the tail of the resulting stream
    /// is computed only as demanded. Use this when you need incremental accumulation over potentially infinite or lazily-produced streams.
    /// </remarks>
    let scan f init s =
        let rec step acc stream =
            match stream with
            | Nil -> One acc
            | One a -> Choice(acc, One (f acc a))
            | Choice(a, r) -> Choice(acc, Thunk(lazy (step (f acc a) r)))
            | Thunk(Lazy t) -> Thunk(lazy (step acc t))
        step init s  
        
    let reduce maxSteps f ls =
        match ls with
        | Empty -> failwith "Empty stream"
        | Cons(x, xs) -> fold maxSteps f x xs

    let combine l1 l2 = choice l1 l2

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

    let takeWith f n s =
        let rec loop k acc s =
            if k = 0 then List.rev acc else
            match s with
            | Nil -> List.rev acc
            | One a -> f a; List.rev (a::acc)
            | Choice(a,r) -> f a; loop (k-1) (a::acc) r
            | Thunk(Lazy t) -> loop k acc t
        loop n [] s

    let take n s = takeWith id n s

    /// Attach indices to a stream: (0,x0),(1,x1),...
    let indexed (s: LazyFairStream<'a>) : LazyFairStream<int * 'a> =
        let rec loop i s =
            match s with
            | Nil -> Nil
            | One a -> One(i, a)
            | Choice(a, rest) -> Choice((i, a), Thunk(lazy (loop (i+1) rest)))
            | Thunk(Lazy t) -> Thunk(lazy (loop i t))
        loop 0 s 

    /// Remove duplicates (first occurrence wins) – requires equality & hashing.
    let removeDuplicates (s: LazyFairStream<'a>) =
        let seen = Hashset() 
        let rec loop stream =
            match stream with
            | Nil -> Nil
            | One a ->
                if seen.Add a then One a else Nil
            | Choice(a, rest) ->
                if seen.Add a then Choice(a, Thunk(lazy (loop rest)))
                else Thunk(lazy (loop rest))
            | Thunk(Lazy t) -> Thunk(lazy (loop t))
        loop s

    let cartesianProduct xs ys = //as either stream might be infinite, we are forced to use bind
        bindc2 id xs (fun x -> bindc2 id ys (fun y -> One(x, y)))
     

type FairStream() =
    member fs.YieldFrom x = fs.ReturnFrom x
    member __.Yield a = One a
    member __.Return a = One a
    member __.ReturnFrom(x) = x
    member __.Bind(m, f) = bindc id m f
    member __.Zero() = Nil
    member __.Combine(r, r') = choice  r r'
    member __.Delay(f: unit -> LazyFairStream<_>) = Thunk(Lazy.Create f)
    member __.BindReturn(stream: LazyFairStream<'a>, f: 'a -> 'b) = FairStream.map f stream
 
type FairStream2() =
    member fs.YieldFrom x = fs.ReturnFrom x
    member __.Yield a = One a
    member __.Return a = One a
    member __.ReturnFrom(x) = x
    member __.Bind(m, f) = bindc2 id m f
    member __.Zero() = Nil
    member __.Combine(r, r') = choice r r'
    member __.Delay(f: unit -> LazyFairStream<_>) = Thunk(Lazy.Create f)
    member __.BindReturn(stream: LazyFairStream<'a>, f: 'a -> 'b) = FairStream.map f stream
 

type FairStream3() =
    member _.Yield a = One a
    member _.Return a = One a
    member _.ReturnFrom x = x
    member _.Bind(m, f) = bindSafeRec m f
    member _.Zero() = Nil
    member _.Combine(r, r') = choice r r'
    member _.Delay(f: unit -> LazyFairStream<_>) = Thunk(Lazy.Create f)
    member _.BindReturn(stream: LazyFairStream<'a>, f: 'a -> 'b) = FairStream.map f stream
 
    member t.MergeSources(stream1: LazyFairStream<'a>, stream2: LazyFairStream<'b>) =
        FairStream.cartesianProduct stream1 stream2

let bt = FairStream()

let bt2 = FairStream2()

let bt3 = FairStream3()

// let rec run depth stream =
//     match (depth, stream) with
//     | _, Nil -> LazyList.empty
//     | _, One a -> LazyList.singleton a
//     | _, Choice(a, r) ->
//         LazyList.lazyList {
//             yield a
//             yield! run depth r
//         }
//     //LazyList.cons a (run depth r)
//     | Some 0, Thunk _ -> LazyList.empty //exhausted depth
//     | d, Thunk(Lazy r) -> run (Option.map (fun n -> n - 1) d) r

let rec run budget stream =
    if Option.defaultValue 1 budget <= 0 then LazyList.empty else
    match stream with
    | Nil -> LazyList.empty
    | One a -> LazyList.singleton a
    | Choice(a, r) ->
        LazyList.lazyList {
            yield a
            yield! run (Option.map (fun n -> n - 1) budget) r
        } 
    | Thunk(Lazy r) -> run budget r
    
let guard assertion =
    bt {
        if assertion then
            return ()
    }

let fail() = bt.Zero()

///list to fairstream
let choices xs = FairStream.ofList xs
    

