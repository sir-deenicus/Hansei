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

module Samplers =
    let rec sample_beta n a b = 
        if n <= 0 then  (round 2 (a/(a+b)))
        else let i = Stats.discreteSample [|a/(a+b);b/(a+b)|]
             if i = 0 then sample_beta (n - 1) (a+1.) b
             else sample_beta (n-1) a (b+1.)

    let sample_poisson lambda = 
        let p = exp -lambda
        let u = random.NextDouble()
        let rec loop s p x =
            if u <= s then x 
            else let x' = x + 1.
                 let p' = p * lambda/x'
                 loop (s+p') p' x'
        loop p p 0.

    let rec sample_std_normal () =
      let u = max Double.Epsilon (random.NextDouble())
      let v = max Double.Epsilon (random.NextDouble())

      sqrt (-2. * log u)  * cos(2. * pi * v) 

    let sample_std_lognormal = sample_std_normal >> exp

    let sample_normal mean stdev = mean + sample_std_normal() * stdev

    let sample_lognormal mu scale = exp(mu + sample_std_normal() * scale)
  
    let sample_gamma a = 
        let d = a - 1./3. 
        let c = 1./(sqrt 9. * d)
        let rec loop () = 
            let z = sample_std_normal() 
            let u = random.NextDouble()
            let v = (1. + c * z) ** 3.
            let dv = d * v
            if z > -1./c && log u < 0.5 * z**2. + d - dv + d * log v then round 1 dv 
            else loop()
        loop () 
    
    let sample_exponential roundto lambda = 
        let u = random.NextDouble()
        round roundto ((-log u)/lambda) 


let sampleN_No_Replacements sampler n items =
    let rec sampleN_without_replacement i ch =
        function
        | _ when i >= n -> ch
        | [] -> ch
        | choices ->
            let choiceIndex = sampler choices
            let mutable c = -1
            let mutable found = None

            let rem =
                [ for x in choices do
                    c <- c + 1
                    if c <> choiceIndex then yield x
                    else found <- Some x ]
            sampleN_without_replacement (i + 1) ((choiceIndex, found.Value)::ch) rem
    sampleN_without_replacement 0 [] items
////////////////////////////////////// 
//let insertWith fn key item m =
//    let v' = Map.tryPick (fun k' v' -> if key = k' then Some v' else None) m
//    match v' with
//    | Some v' -> Map.add key (fn item v') m
//    | None -> Map.add key item m 

let insertWith fn key item m =    
    match Map.tryFind key m with
    | Some v' -> Map.add key (fn item v') m
    | None -> Map.add key item m  

let insertWithx fn key item (d:Dict<_,_>) = 
    match d.tryFind key with
    | Some v' -> d.[key] <- (fn item v') 
    | None -> d.Add(key, item)
    d

let inline insertWith2 fn key item (m:MapSlim<_,_>) =   
    match m.GetOption key with
    | ValueSome v' -> m.Set (key, fn item v')  
    | ValueNone -> m.Set(key, item)
    m

//////////////////////////////////////

let inline testPath (paths : Dict<_,_>) x =
    match paths.tryFind x with
    | Some -1. -> true, -1.
    | Some r -> false, r
    | None -> false, 1.

let rec propagateUp maxWeight isreward (paths : Dict<_, _>) attenuateUp r =
    function
    | _ when r < 0.01 -> ()
    | [] -> ()
    | (_ :: path) ->
        paths.ExpandElseAdd path (fun v ->
            if v = -1. || v >= maxWeight then v
            else max 0. (if isreward then v + r
                         else v * r)) (if isreward then min maxWeight (1. + r)
                                       else r)
        propagateUp maxWeight isreward paths attenuateUp (r * attenuateUp) path

//////////////////////////////////////

let inline normalize (choices) =
  let sum = List.sumBy fst choices
  List.map (fun (p, v) -> (p/sum, v)) choices

//////////////////////////////////////

let filterWith f data = 
  let matches = data |> Array.filter f
  (Array.length matches |> float) / (float data.Length) 
     
let inline conditionalProbability conditional matchwith m = 
    let sub = Map.filter (fun k _ -> conditional k) m
    let matches = Map.filter (fun k _ -> matchwith k) sub
    (Map.sum matches) / (Map.sum sub)
    
let inline conditionalSubspace conditional m = 
    let sub = Map.filter (fun k _ -> conditional k) m    
    Map.normalize sub
    
let inline sum p = List.sumBy fst p

let inline filterToSubspace conditional m = Map.filter (fun k _ -> conditional k) m    

//////////////////////////////////////
//=-=-=-=-=-=-=-=-=-=
//Float specific
//=-=-=-=-=-=-=-=-=-=
let histogram len (d:_[]) =
    let stringr100 n x = string (round n (x * 100.))
    let maxp,_ = Array.maxBy fst d
    Array.map (fun (p:float,x)-> 
          [|sprintf "%A" x ;
            stringr100 2 p + "%"; 
            String.replicate (int(round 0 (p/maxp * len))) "█" |]) d
    |> makeTable "\n" [|"item";"p"; ""|] ""  

let histogram2 len d =
    d |> Seq.toArray
      |> Array.rev
      |> histogram len
//////////////////////////////////////

let inline coarsenWithGeneric tonumber f samples =  
    List.groupBy f samples
    |> List.map (fun (x,xs) -> tonumber xs.Length, x)  
     
let inline mapDistribution projectTo m = 
    Map.toArray m 
    |> Array.map (fun (x,p) -> projectTo x,p) 
    |> Array.groupBy fst 
    |> Array.map (fun (x,xs) -> x, Array.sumBy snd xs) 
    |> Map.ofArray 
      
let inline probabilityOf mapsum filter m = 
    mapsum (Map.filter (fun k _ -> filter k) m)

let inline entropy log0 mapsum dist = -(mapsum(Map.map (fun _ p -> p * log0 p) dist))

let inline mutualInformation log0 mapsum (joint:Map<_,_>) =
    joint |> Map.map (fun (x,y) pxy ->
        let px = probabilityOf mapsum (fst >> (=) x) joint
        let py = probabilityOf mapsum (snd >> (=) y) joint 
          
        pxy * log0(pxy/(px * py))) 

let inline kldivergence log0 mapsum (pA:Map<_,_>) (pB:Map<_,_>) =
    pA |> Map.map (fun x p_a ->        
        let p_b = probabilityOf mapsum ((=) x) pB
        p_a * log0(p_a/ p_b))
 
 //=========================

let coarsenWith f samples = coarsenWithGeneric float f samples

let toBits x = x / log 2. 