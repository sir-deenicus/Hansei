module Hansei.Utils

open Prelude.Common
open Prelude.Math
open System
open Hansei.Continuation
open DictionarySlim
open Prelude.Collections.FibonacciHeap
open Prelude.Collections 
open System.Collections.Generic

//////////////////////////////////////  

let insertWith fn key item m =    
    match Map.tryFind key m with
    | Some v' -> Map.add key (fn item v') m
    | None -> Map.add key item m  

let insertWithx fn key item (d:Dict<_,_>) = 
    match d.tryFind key with
    | Some v' -> d.[key] <- (fn item v') 
    | None -> d.Add(key, item)
    d


//////////////////////////////////////

let inline normalize (choices) =
    let sum = List.sumBy snd choices
    List.map (fun (v, p) -> (v, p/sum)) choices
     
let inline expectedValue f ps = 
    ps
    |> List.sumBy (fun (x,p) -> f x * p)
  
//////////////////////////////////////


let inline coarsenWithGeneric tonumber f samples =  
     List.groupBy f samples
     |> List.map (fun (x,xs) -> x, tonumber xs.Length)  
     
let coarsenWith f samples = coarsenWithGeneric float f samples
 

     