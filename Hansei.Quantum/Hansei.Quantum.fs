module Hansei.Quantum

open Hansei.Utils
open Hansei.Continuation   
open Hansei.Continuation.Quantum
open MathNet.Symbolics.LinearAlgebra
open MathNet.Symbolics
open MathNet.Symbolics.Extras

//Core of Hansei modified from base:
//https://gist.github.com/einblicker/3245547#file-hansei
//==========
//It makes more sense I think, to use symbolic representations rather than complex numbers. This has more value as a 
//pedagogical tool to get a firmer mental representation of quantum computing without going to a proper representation

let qstate ch k = List.map (fun (p,v) -> (p, Continued(lazy(k v)))) ch

let fail () = qstate []

let inline qreify0 m = m (fun x -> [(Complex 1Q, Value x)])

let qexactly x = qstate [Complex 1Q, x]

let explore (maxdepth : int option) (choices : QuantumProbabilitySpace<'T>) =
  let rec loop (p:Complex) depth down susp answers =
    match (down, susp, answers) with
    | (_, [], answers) -> answers 
 
    | (_, (pt, Value v) :: rest, (ans, susp)) ->
      loop p depth down rest (insertWith (+) v (pt*p) ans, susp)
 
    | (true, (pt,Continued (Lazy t))::rest, answers) ->
      let down' = match maxdepth with Some x -> depth < x | None -> true
      loop p depth true rest <| loop (pt*p) (depth+1) down' (t) answers
 
    | (down, (pt,c)::rest, (ans,susp)) ->
      loop p depth down rest (ans, (pt*p,c)::susp)

  let (ans, susp) = loop (Complex 1Q) 0 true choices (Map.empty, [])
  Map.fold (fun a v p -> (p, Value v)::a) susp ans : QuantumProbabilitySpace<'T>

//===============

let observeState test = cont { if not test then return! fail() }

let filterByObserving f p = cont {
    let! x = p
    do! observeState (f x)
    return x }   

let inline qexact_reify model   =  explore None     (qreify0 model)  
let inline qlimit_reify n model =  explore (Some n) (qreify0 model)                   

//=-=-=-=-=-=-=-=-=-=
let hadamard0 x = 
    if List.sum x <> 1Q then failwith "qubit must be either [1;0] or [0;1]"
    else  Vector x * (1Q/sqrt 2Q) * Matrix [[1Q; 1Q]; [1Q;-1Q]]
          |> Vector.toList
          |> List.mapi (fun i c -> Complex c,i)  
          |> qstate
    
let bflip = function
    | 0 -> 1 
    | 1 -> 0 
    | _ -> failwith "Unexpected input. Must be either 0 or 1"

let cnot c b = if c = 1 then bflip b else b

let hadamard = function
     | 0 -> hadamard0 [1Q;0Q] 
     | 1 -> hadamard0 [0Q;1Q] 
     | _ -> failwith "Must be 0 or 1"

let vqstate v = 
       Vector.toList v
    |> List.mapi (fun i c -> c,i)  
    |> qstate

let phaseShift angle q = 
    let x = if q = 0 then [Complex 1Q;Complex 0Q] else [Complex 0Q;Complex 1Q]            
    
    vqstate (Matrix [[Complex 1Q; Complex 0Q]; 
                     [Complex 0Q; Complex(cos (angle), sin (angle))]] * Vector x)
      

let rotateZ angle q = 
    let x = if q = 0 then [Complex 1Q;Complex 0Q] else [Complex 0Q;Complex 1Q]            
    
    vqstate(Matrix [[Complex(cos (-angle), sin (-angle)); Complex 0Q]; 
                    [Complex 0Q; Complex(cos (angle), sin (angle))]] * Vector x)
    
let rotateX angle q = 
    let x = if q = 0 then [Complex 1Q;Complex 0Q] else [Complex 0Q;Complex 1Q]            
    
    vqstate (Matrix [[Complex (cos (angle)); Complex(0Q, -sin (angle))]; 
                 [Complex(0Q, -sin (angle)); Complex (cos (angle))]] * Vector x)
 
let rotateY angle q = 
    let x = if q = 0 then [Complex 1Q;Complex 0Q] else [Complex 0Q;Complex 1Q]            
    
    vqstate (Vector x * Matrix [[Complex (cos (angle)); Complex(-sin (angle))]; 
                                [Complex(sin (angle)); Complex (cos (angle))]])
 

let bell qb1 qb2 =
    cont {
      let! c = hadamard qb1
      let! b = qexactly qb2
      return (c,cnot c b)} 