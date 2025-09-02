module Hansei.TreeSearch

open Hansei

module Exhaustive =
    //the "search" monad is just the list monad
    let choices xs = List.ofSeq xs 
    let exactly x = choices [x]  

    type ListMonad() =
       member __.Bind(m, f) = List.collect f m
       member __.Return x = [x]
       member __.ReturnFrom l = l : _ list
       member __.Zero () = []
       member __.Combine(l1,l2) = List.append l1 l2
       member __.Delay f = f ()

    let search = ListMonad()
    let fail() = []
    let guard assertion = search { if assertion then return () }  

module SeqSearch =
    // a clear, explicit name for the seq-based search monad
    let choices xs : seq<'a> = xs
    let exactly x = choices [x]

    type SeqMonad() =
       member __.Bind(m, f) = Seq.collect f m
       member __.Return x = seq { yield x }
       member __.ReturnFrom s = s
       member __.Zero () = Seq.empty
       member __.Combine(s1,s2) = Seq.append s1 s2
       member __.Delay f = seq { yield! f () }

    let search = SeqMonad()
    let fail() = search.Zero()
    let guard b = search { if b then return () }
    let constrain = guard
    
module Backtracking =
    let choices xs = Backtracking.choices (List.ofSeq xs)
    let guard b = Backtracking.guard b
    let constrain = guard
    let search = Backtracking.FairStream()
    let bt = Backtracking.bt
    let fail() = bt.Zero()
    let exactly x = choices [x]

module Backtracking2 =
    let choices xs = Backtracking.choices (List.ofSeq xs)
    let guard b = Backtracking.guard b
    let constrain = guard
    let search = Backtracking.FairStream2()
    let bt = Backtracking.bt2
    let fail() = bt.Zero()
    let exactly x = choices [x]

module Backtracking3 =
    let choices xs = Backtracking.choices (List.ofSeq xs)
    let guard b = Backtracking.guard b
    let constrain = guard
    let search = Backtracking.FairStream2()
    let bt = Backtracking.bt2
    let fail() = bt.Zero()
    let exactly x = choices [x]


module LazyList =
    open Hansei.FSharpx.Collections
    let choices xs = LazyList.ofSeq xs
    let guard b = LazyList.guard b
    let constrain = guard
    let search = LazyList.LazyListMonad() 
    let fail() = search.Zero()
    let exactly x = choices [x]