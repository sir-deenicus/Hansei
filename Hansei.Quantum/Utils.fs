module Hansei.Utils
 
open Prelude.Common
open Prelude.Math       
open MathNet.Symbolics 
open Hansei.GenericProb

module Map = 
    let inline sum zero m = Map.fold (fun sum _ x -> sum + x) (zero) m
    let sumComplex m = sum (Complex 0Q) m
    let sumf m = sum 0. m
    let squaredSum m = Map.fold (fun sum _ (x:Complex) -> sum + x.Magnitude ** 2) 0Q m  
    //let normalize (m : Map<'a,_>) = 
    //    let total = sqrt (squaredSum m)
    //    Map.map (fun _ v -> v / total) m
       
////////////////////////////  

let inline histogram tostring tofloat len (d:_ seq) =
    let maxp,_ = Seq.maxBy (fst >> tofloat) d
    Array.map (fun (p,x)-> 
          [|sprintf "%A" x ;
            tostring p; 
            String.replicate (int(round 0 (tofloat(p/maxp) * len))) "█" |]) (Seq.toArray d)
    |> makeTable "\n" [|"item";"p"; ""|] ""  
    
////////////////////////////  
let inline mkProbabilityMap normalize t =
    Map [for (p, v) in (normalize t) do
          match v with
           | Value x -> yield (x,p)
           | _ -> ()]    
////////////////////////////  
module Symbolic =
    open Hansei
    open Prelude.ProbabilityTools
    let log0 x = if x = 0Q then 0Q else log x

    let toBits x = x / log 2Q  

  