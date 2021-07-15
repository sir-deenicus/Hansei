module Hansei.Utils

open Prelude.Common
open Prelude.Math
open System
open Hansei.Continuation
open CollectionSlim
open Prelude.Collections.FibonacciHeap
open Prelude.Collections
open Microsoft.Collections.Extensions
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

let inline insertIntoSlim fn key item (m:MapSlim<_,_>) =   
    match m.GetOption key with
    | ValueSome v' -> m.Set (key, fn item v')  
    | ValueNone -> m.Set(key, item)
    m


//////////////////////////////////////

let inline normalize (choices) =
  let sum = List.sumBy fst choices
  List.map (fun (p, v) -> (p/sum, v)) choices
  
let inline normalizeWeights data =
    let sum = Array.sumBy snd data |> float
    [|for (x,p) in data -> x, float p / sum|]  

//////////////////////////////////////


let inline coarsenWithGeneric tonumber f samples =  
     List.groupBy f samples
     |> List.map (fun (x,xs) -> tonumber xs.Length, x)  

let coarsenWith f samples = coarsenWithGeneric float f samples
 

     