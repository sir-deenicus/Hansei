module Hansei.QUtils
 
open Prelude.Common
open Prelude.Math
open Hansei.TextUtils
open Hansei.Continuation.Quantum
open System.Numerics
open MathNet.Numerics        
open Hansei.Quantum              

module Map  =
  let sum m = Map.fold (fun sum _ x -> sum + x) (Complex (0.,0.)) m
  let sumf m = Map.fold (fun sum _ x -> sum + x) (0.) m
  let squaredSum m = Map.fold (fun sum _ (x:Complex) -> sum + x.Magnitude ** 2.) 0. m |> complexf
  let normalize (m : Map<'a,_>) = 
      let total = sqrt (squaredSum m)
      Map.map (fun _ v -> v / total) m

let inline measure0 (choices:list<Complex * 'a>) =  
  List.map (fun (p, v) -> ((Complex.magnitude p) ** 2., v)) choices

let inline measure (choices) =  
  List.map (fun (p, v) -> ((Complex.magnitude p) ** 2., valueExtract v)) (qexact_reify choices)

let inline measure2 (choices:list<Complex * 'a>) =  
  List.map (fun (p, v) -> ((Complex.magnitude p) ** 2., valueExtract v)) (choices)
////////////////////////////

let qhistogram len (sq) =
    let d = Seq.toArray sq
    let maxp,_ = Array.maxBy (fst) d

    Array.map (fun (p,x)-> 
          [|sprintf "%A" x ;
            string p; 
            String.replicate (int(round 0 (p/maxp * len))) "#" |]) d
    |> makeTable "\n" [|"item";"p"; ""|] ""  

let qhistogram1 m len d =
    m d |> List.rev  
        |> qhistogram len

let qhistogram2 len d = qhistogram1 measure len d

///////////////

//===========
let inline normalizef (choices:list<float * 'a>) =
  let sum = List.sumBy (fst) choices
  List.map (fun (p, v) -> (p/sum, v)) choices

let inline qnormalize (choices:list<Complex * 'a>) =
  let sum = List.sumBy (fst >> Complex.magnitude>>squared) choices |> sqrt |> complexf
  List.map (fun (p, v) -> (p/sum, v)) choices

//////////

let inline mkProbabilityMap t =
    Map [for (p, v) in (t) do
          match v with
           | Value x -> yield (x,p)
           | _ -> ()]      

let amplitudeFor2 m item = 
    match Map.tryFind item m with
     | None -> (complexf 0.)
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
