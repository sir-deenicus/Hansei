module Hansei.Utils
 
open Prelude.Common
open Prelude.Math
open Hansei.TextUtils        
open MathNet.Symbolics 
open Hansei.GenericProb

module Map =
  let sumComplex m = Map.fold (fun sum _ x -> sum + x) (Complex 0Q) m
  let sumf m = Map.fold (fun sum _ x -> sum + x) (0.) m
  let inline sum zero m = Map.fold (fun sum _ x -> sum + x) (zero) m
  let squaredSum m = Map.fold (fun sum _ (x:Complex) -> sum + x.Magnitude ** 2) 0Q m  
  let normalize (m : Map<'a,_>) = 
      let total = sqrt (squaredSum m)
      Map.map (fun _ v -> v / total) m
       
////////////////////////////  

let inline histogram tostring tofloat len (d:_ seq) =
    let maxp,_ = Seq.maxBy (fst >> tofloat) d
    Array.map (fun (p,x)-> 
          [|sprintf "%A" x ;
            tostring p; 
            String.replicate (int(round 0 (tofloat(p/maxp) * len))) "#" |]) (Seq.toArray d)
    |> makeTable "\n" [|"item";"p"; ""|] ""  
    
////////////////////////////  
let inline mkProbabilityMap normalize t =
    Map [for (p, v) in (normalize t) do
          match v with
           | Value x -> yield (x,p)
           | _ -> ()]    
////////////////////////////  
module Symbolic =
    let toBits x = x / log 2Q  

    let probabilityOf filter m = 
        Map.sum 0Q (Map.filter (fun k _ -> filter k) m) 

    let entropy dist = -(Map.map (fun _ p -> p * log p) dist |> Map.sum 0Q)

    let mutualInformation (joint:Map<_,_>) =
        joint |> Map.map (fun (x,y) pxy ->
            let px = probabilityOf (fst >> (=) x) joint
            let py = probabilityOf (snd >> (=) y) joint  

            pxy * log(pxy/(px * py))) 

    let kldivergence (pA:Map<_,_>) (pB:Map<_,_>) =
        pA |> Map.map (fun x p_a ->        
            let p_b = probabilityOf ((=) x) pB
            p_a * log(p_a/ p_b))
         