module Hansei.QUtils
 
open Prelude.Common
open Prelude.Math
open Hansei.TextUtils
open Hansei.Continuation
open Hansei.Quantum     
open Hansei.Types.Quantum       
open MathNet.Symbolics
open MathNet.Symbolics.Extras  

module Map =
  let sum m = Map.fold (fun sum _ x -> sum + x) (Complex 0Q) m
  let sumf m = Map.fold (fun sum _ x -> sum + x) (0.) m
  let squaredSum m = Map.fold (fun sum _ (x:Complex) -> sum + x.Magnitude ** 2) 0Q m  
  let normalize (m : Map<'a,_>) = 
      let total = sqrt (squaredSum m)
      Map.map (fun _ v -> v / total) m

let inline measure0 (choices:list<Complex * 'a>) =  
  List.map (fun (p, v) -> ((Complex.magnitude p) ** 2, v)) choices

let inline measure (choices) =  
  List.map (fun (p, v) -> ((Complex.magnitude p) ** 2, valueExtract v)) (Q.exact_reify choices)

let inline measure2 (choices:list<Complex * _ >) =  
  List.map (fun (p, v) -> ((Complex.magnitude p) ** 2, valueExtract v)) (choices)
////////////////////////////
module Q =
    let histogram len (sq) =
        let d = Seq.toArray sq
        let maxp,_ = Array.maxBy (fst >> Expression.toFloat) d

        Array.map (fun (p:Expression,x)-> 
              [|sprintf "%A" x ;
                p.ToFormattedString() ; 
                String.replicate (int(round 0 (Expression.toFloat (p/maxp) * len))) "#" |]) d
        |> makeTable "\n" [|"item";"p"; ""|] ""  

    let histogram1 measureWith len d =
        measureWith d 
        |> List.rev  
        |> histogram len

    let histogram2 len d = histogram1 measure len d

    ///////////////

    //===========
    let inline normalizef (choices:list<float * 'a>) =
      let sum = List.sumBy (fst) choices
      List.map (fun (p, v) -> (p/sum, v)) choices

    let inline normalize (choices:list<Complex * 'a>) =
      let sum = List.sumBy (fst >> Complex.magnitude >> squared) choices |> sqrt 
      List.map (fun (p, v) -> (p /sum, v)) choices

    //////////

    let inline mkProbabilityMap t =
        Map [for (p, v) in (t) do
              match v with
               | Value x -> yield (x,p)
               | _ -> ()]      

    let measureWithThenToRational m qs = m qs |> List.map (fun (p,x) -> Expression.toRational p, x)
    let measureWithThenToFloat m qs = m qs |> List.map (fun (p,x) -> Expression.toFloat p, x)

    let measureAsRational qs = measureWithThenToRational measure qs
    let measureAsRational2 qs = measureWithThenToRational measure2 qs

let amplitude m item = 
    match Map.tryFind item m with
     | None -> (Complex 0Q)
     | Some p -> p

let inline amplitudeFor filter m = 
    Map.sum (Map.filter (fun k _ -> filter k) m)

let inline conditionQSpace conditional matchwith m = 
    let sub = Map.filter (fun k _ -> conditional k) m
    let matches = Map.filter (fun k _ -> matchwith k) sub
    (Map.sum matches) / (Map.squaredSum sub)
    
let inline conditionalQSubspace conditional m = 
    let sub = Map.filter (fun k _ -> conditional k) m    
    Map.normalize sub
    
//=-=-=-=-=-=-=-=-=-=
 ////////////

