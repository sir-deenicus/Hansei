module Hansei.Quantum

open Hansei.Utils
open Hansei.Continuation   
open Hansei.GenericProb
open MathNet.Symbolics.LinearAlgebra
open MathNet.Symbolics
open MathNet.Symbolics.Core
open Prelude.Math
open MathNet.Symbolics.NumberTheory
open Prelude.Common

//Core of Hansei modified from base:
//https://gist.github.com/einblicker/3245547#file-hansei
//==========
//It makes more sense I think, to use symbolic representations rather than complex numbers. This has more value as a 
//pedagogical tool to get a firmer mental representation of quantum computing without going to a proper representation

type QuantumProbabilitySpace<'T> = GenericProbabilitySpace<Complex,'T>

let qstate ch k = distribution ch k 

let fail () = qstate []

let inline reify0 m = m (fun x -> [(Complex 1Q, Value x)])

//===============

let (<+>) a (b:string) = (a + b).Replace(">|", "")

let observeState test = observe test

let filterByObserving f p = cont {
    let! x = p
    do! observeState (f x)
    return x }   
     
let exactly x = qstate [Complex 1Q, x]

let explore (maxdepth : int option) (choices : QuantumProbabilitySpace<'T>) = 
    GenericProb.explore id (Complex 1Q) maxdepth choices
     : QuantumProbabilitySpace<'T>

let inline exact_reify model   =  explore None     (reify0 model)  
let inline limit_reify n model =  explore (Some n) (reify0 model)                   

let normalize (choices:list<Complex * 'a>) =
    normalize (List.sumBy(fst >> Complex.magnitude >> squared) >> sqrt) choices

//=-=-=-=-=-=-=-=-=-=

let buildHadamardGate x = 
    if List.sum x <> 1Q then failwith "qubit must be either [1;0] or [0;1]"
    else  Vector x * (1Q/sqrt 2Q) * Matrix [[1Q; 1Q]; [1Q;-1Q]]
          |> Vector.toList
          |> List.mapi (fun i c -> Complex c,"|" + string i + ">")

let hadamardGate = function
     | "|0>" -> buildHadamardGate [1Q;0Q] 
     | "|1>" -> buildHadamardGate [0Q;1Q] 
     | _ -> failwith "Unexpected input. Must be either |0> or |1>"

let hadamard v = v |> hadamardGate |> qstate

let qstateOfVector v = 
       Vector.toList v
    |> List.mapi (fun i c -> c,"|" + string i + ">")  

let stringToVec q =
    if q = "|0>" then
        [ Complex 1Q; Complex 0Q ]
    else
        [ Complex 0Q; Complex 1Q ]

let phaseShiftGate angle q =
    let x = stringToVec q
    qstateOfVector
        (Matrix
            [ [ Complex 1Q; Complex 0Q ]
              [ Complex 0Q; Complex(cos (angle), sin (angle)) ] ]
         * Vector x)

let phaseShift angle q = phaseShiftGate angle q |> qstate

let rotateZGate angle q =
    let x = stringToVec q 
    qstateOfVector
        (Matrix
            [ [ Complex(cos (-angle), sin (-angle)); Complex 0Q ]
              [ Complex 0Q; Complex(cos (angle), sin (angle)) ] ]
         * Vector x)

let rotateZ angle q = rotateZGate angle q |> qstate

let rotateXGate angle q =
    let x = stringToVec q 
    qstateOfVector
        (Matrix
            [ [ Complex(cos (angle)); Complex(0Q, -sin (angle)) ]
              [ Complex(0Q, -sin (angle)); Complex(cos (angle)) ] ]
         * Vector x)

let rotateX angle q = rotateXGate angle q |> qstate

let rotateYGate angle q =
    let x = stringToVec q 
    qstateOfVector
        (Vector x * Matrix [ [ Complex(cos (angle)); Complex(-sin (angle)) ]
                             [ Complex(sin (angle)); Complex(cos (angle)) ] ])
    
let rotateY angle q = rotateYGate angle q |> qstate

let bflip = function
    | "|0>" -> "|1>" 
    | "|1>" -> "|0>" 
    | _ -> failwith "Unexpected input. Must be either |0> or |1>"

let cnot c b = if c = "|1>" then bflip b else b

let bell qb1 qb2 =
    cont {
      let! c = hadamard qb1
      let! b = exactly qb2
      return (c,cnot c b)} 

////////

type Model() =  
    static member ReifyQuantum(thunk, ?limit) = 
        match limit with
        | None -> exact_reify thunk
        | Some n -> limit_reify n thunk

let measureReified (choices:list<Complex * _ >) =  
    List.map (fun (p, v) -> ((Complex.magnitude p) ** 2, valueExtract v)) choices

let measure q = q |> exact_reify |> measureReified

let histogram fp f len d =
    d
    |> measureReified  
    |> ProbabilitySpace.mapItemsProb fp f
    |> List.rev  
    |> histogram Utils.fmt (Expression.toFloat >> Option.get) len

let histogram2 len d = histogram id id len d  

let amplitude m item = 
    match Map.tryFind item m with
     | None -> (Complex 0Q)
     | Some p -> p

let inline amplitudeFor filter m = 
    Map.sum (Map.filter (fun k _ -> filter k) m)

let inline conditionQSpace conditional matchwith m = 
    let sub = Map.filter (fun k _ -> conditional k) m
    let matches = Map.filter (fun k _ -> matchwith k) sub
    (Map.sumComplex matches) / (Map.squaredSum sub)
    
let inline conditionalQSubspace conditional m = 
    let sub = Map.filter (fun k _ -> conditional k) m    
    Map.normalize sub