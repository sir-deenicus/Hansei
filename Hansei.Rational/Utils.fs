module Hansei.Utils.Rational

open Hansei.TextUtils
open System
open Hansei.Types.Rational
open MathNet.Numerics
open Prelude.Common
open Prelude.Math
open Hansei.Utils 

module BigRational =
    open Microsoft.FSharp.Core.Operators

    let almostZero x = (floor x) / x > 0.999999

    let fromFloatDouble (df : float) =
        let rec countDigits n x =
            let x' = x * 10.
            if almostZero x' then n + 1
            else countDigits (n + 1) x'
        if almostZero df then BigRational.FromBigInt(Numerics.BigInteger df)
        else
            let dpart = df - floor df
            let dpow = countDigits 0 dpart
            let pow10 = Numerics.BigInteger 10 ** int dpow
            BigRational.FromBigIntFraction
                (Numerics.BigInteger(df * double pow10), pow10)

    ///limited by range of decimal (which is used as a less noisy alternative to floats)
    let fromFloat (f : float) =
        let df = decimal f
        if df = floor df then BigRational.FromBigInt(Numerics.BigInteger df)
        else
            let decimalpart = string (df - floor df)
            let pow10 = Numerics.BigInteger 10 ** (decimalpart.Length - 2)
            BigRational.FromBigIntFraction
                (Numerics.BigInteger(df * decimal pow10), pow10)

    let fromFloatRepeating (f : float) =
        let df = decimal f
        let len = float ((string (df - floor df)).Length - 2)
        let pow10 = decimal (10. ** len)
        if abs f < 1. then
            BigRational.FromIntFraction
                (int (df * pow10), int (floor (pow10 - df)))
        else
            BigRational.FromIntFraction
                (int (df * pow10 - floor df), int (pow10 - 1M))
 

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

let inline mkProbabilityMap t =
    Map [for (p, v) in (normalize t) do
          match v with
           | Value x -> yield (x,p)
           | _ -> ()]      

let probabilityOf2 m item = 
    match Map.tryFind item m with
     | None -> 0N
     | Some p -> p 

//=-=-=-=-=-=-=-=-=-=
