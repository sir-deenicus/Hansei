module Hansei.Utils.Rational

open Hansei.TextUtils
open System
open Hansei.Continuation
open MathNet.Numerics
open Prelude.Common
open Prelude.Math
open Hansei.Utils

module BigRational =     
  ///limited by range of decimal (which is used as a less noisy alternative to floats)
  let fromFloat (f:float) =
      let df = decimal f
      if df = floor df then BigRational.FromBigInt (Numerics.BigInteger df)
      else
        let s = string (df - floor df)
        let pow10 = Numerics.BigInteger 10 ** (s.Length - 2)
        BigRational.FromBigIntFraction(Numerics.BigInteger(df * decimal pow10), pow10)
  let fromFloatRepeating (f:float) =
      let df = decimal f
      let len = float((string (df - floor df)).Length - 2)
      let pow10 = decimal (10. ** len)
      if abs f < 1. then
        BigRational.FromIntFraction(int (df * pow10), int (floor (pow10 - df)))
      else BigRational.FromIntFraction(int (df * pow10 - floor df), int (pow10 - 1M))
 

////////////////////////////
let histogram len (d:_[]) =
    let maxp,_ = Array.maxBy fst d
    Array.map (fun (p:BigRational,x)-> 
          [|sprintf "%A" x ;
            string p; 
            String.replicate (int(round 0 (float(p/maxp) * len))) "#" |]) d
    |> makeTable "\n" [|"item";"p"; ""|] ""  

let histogram2 len d =
    d |> Seq.toArray
      |> Array.rev
      |> histogram len

/////////////////////

module Map =
  let sumf m = Map.fold (fun sum _ x -> sum + x) 0. m
  let sum m = Map.fold (fun sum _ x -> sum + x) 0N m
  let normalize (m : Map<'a,_>) = 
      let total = sum m
      Map.map (fun _ v -> v / total) m

///////////////

let inline mkProbabilityMap t =
    Map [for (p, v) in (normalize t) do
          match v with
           | Value x -> yield (x,p)
           | _ -> ()]      

let probabilityOf2 m item = 
    match Map.tryFind item m with
     | None -> 0N
     | Some p -> p

let inline probabilityOf filter m = 
    Map.sum (Map.filter (fun k _ -> filter k) m)

let inline conditionalProbability conditional matchwith m = 
    let sub = Map.filter (fun k _ -> conditional k) m
    let matches = Map.filter (fun k _ -> matchwith k) sub
    (Map.sum matches) / (Map.sum sub)
    
let inline conditionalSubspace conditional m = 
    let sub = Map.filter (fun k _ -> conditional k) m    
    Map.normalize sub

//=-=-=-=-=-=-=-=-=-=

let mutualInformation (joint:Map<_,_>) =
    joint |> Map.map (fun (x,y) pxy ->
        let px = probabilityOf (fst >> (=) x) joint
        let py = probabilityOf (snd >> (=) y) joint 
        
        let fpx,fpy,fpxy = float px, float py, float pxy

        fpxy * log0(fpxy/(fpx * fpy)))


let inline kldivergence (pA:Map<_,_>) (pB:Map<_,_>) =
    pA |> Map.map (fun x p_a ->        
        let p_b = probabilityOf ((=) x) pB
        float p_a * log0(float p_a/ float p_b))
