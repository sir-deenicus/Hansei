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
    let guard assertion = search { if assertion then return () }  


module Backtracking =
    let choices xs = Backtracking.choices (List.ofSeq xs)
    let guard b = Backtracking.guard b
    let constrain = guard
    let search = Backtracking.FairStream()
    let bt = Backtracking.bt
    let exactly x = choices [x]

module LazyList =
    open Hansei.FSharpx.Collections
    let choices xs = LazyList.ofSeq xs
    let guard b = LazyList.guard b
    let constrain = guard
    let search = LazyList.LazyListMonad() 
    let exactly x = choices [x]