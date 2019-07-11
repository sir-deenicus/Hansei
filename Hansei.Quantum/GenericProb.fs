module Hansei.GenericProb

open System
open Hansei.Utils
open Hansei.Continuation    
open MathNet.Symbolics
open MathNet.Symbolics.Extras 
open Prelude.Common
open MathNet.Numerics

type GenericProbabilitySpace<'a, 'T> = list<'a * GenericWeightedTree<'a, 'T>>
and GenericWeightedTree<'a, 'T> = 
    | Value of 'T 
    | Continued of Lazy<GenericProbabilitySpace<'a, 'T>>    

let valueExtract = function Value x -> x

let valueExtract2 = function Value x -> Some x | _ -> None

let distribution ch k = List.map (fun (p:'a,v) -> (p, Continued(lazy(k v)))) ch

let fail () = distribution []

let inline reify0 one m = m (fun x -> [(one , Value x)])

let exactly one x = distribution [one , x] 

let inline explore one (maxdepth : int option) (choices : GenericProbabilitySpace<'a , 'T>) =
  let rec loop p depth down susp answers =
    match (down, susp, answers) with
    | (_, [], answers) -> answers 
 
    | (_, (pt, Value v) :: rest, (ans, susp)) ->
      loop p depth down rest (insertWithx (+) v (pt*p) ans, susp)
 
    | (true, (pt,Continued (Lazy t))::rest, answers) ->
      let down' = match maxdepth with Some x -> depth < x | None -> true
      loop p depth true rest <| loop (pt*p) (depth+1) down' (t) answers
 
    | (down, (pt,c)::rest, (ans,susp)) ->
      loop p depth down rest (ans, (pt*p,c)::susp)
       
  let (ans,susp) = loop one 0 true choices (Dict(), [])   
  [ yield! susp
    for (KeyValue(v,p)) in ans -> p, Value v] : GenericProbabilitySpace<_, _>

let inline exact_reify one model = explore one None (reify0 one model)
let inline limit_reify one n model = explore one (Some n) (reify0 one model)

type Model() =  
    static member inline Reify(one, thunk, ?limit) = 
        match limit with
        | None -> exact_reify one thunk
        | Some n -> limit_reify one n thunk
         