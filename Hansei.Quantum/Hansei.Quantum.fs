module Hansei.Quantum

open System
open Hansei.Utils
open Hansei.Continuation   
open Hansei.Continuation.Quantum
open System.Numerics
open MathNet.Numerics
open MathNet.Numerics.ComplexExtensions
open MathNet.Numerics.LinearAlgebra

//Core of Hansei modified from base:
//https://gist.github.com/einblicker/3245547#file-hansei
//Remaining and majority of code (till line 250) ported/lightly modified from
//http://okmij.org/ftp/kakuritu/Hansei.html
//Ocaml style comments in code below are Oleg's

//This framework is much more flexible than the system described in Expert F#. 
//A continuation monad is used to describe distributions as lazy continuation trees.
//==========

let complexf f = Complex(f,0.)

let qstate ch k = List.map (fun (p,v) -> (p, Continued(lazy(k v)))) ch

let fail () = qstate []

let inline qreify0 m = m (fun x -> [(Complex (1.,0.), Value x)])

let qexactly x = qstate [Complex (1.,0.), x]


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

  let (ans, susp) = loop (complexf 1.) 0 true choices (Map.empty, [])
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
    if List.sum x <> 1. then failwith "qubit must be either [1;0] or [0;1]"
    else  vector x * (1./sqrt 2.) * matrix [[1.; 1.]; [1.;-1.]]
          |> Vector.toList
          |> List.mapi (fun i c -> complexf c,i)  
          |> qstate
    
let bflip = function
    | 0 -> 1 
    | 1 -> 0 
    | _ -> failwith "Unexpected input. Must be either 0 or 1"

let cnot c b = if c = 1 then bflip b else b

let hadamard = function
     | 0 -> hadamard0 [1.;0.] 
     | 1 -> hadamard0 [0.;1.] 
     | _ -> failwith "Must be 0 or 1"

let vqstate v = 
       Vector.toList v
    |> List.mapi (fun i c -> c,i)  
    |> qstate

let phaseShift angle q = 
    let x = if q = 0 then [complexf 1.;complexf 0.] else [complexf 0.;complexf 1.]            
    
    vqstate (matrix [[complexf 1.; complexf 0.]; 
                     [complexf 0.; Complex(cos (angle), sin (angle))]] * vector x)
    

let rotateZ angle q = 
    let x = if q = 0 then [complexf 1.;complexf 0.] else [complexf 0.;complexf 1.]            
    
    vqstate(matrix [[Complex(cos (-angle), sin (-angle)); complexf 0.]; 
                    [complexf 0.; Complex(cos (angle), sin (angle))]] * vector x)
    
let rotateX angle q = 
    let x = if q = 0 then [complexf 1.;complexf 0.] else [complexf 0.;complexf 1.]            
    
    vqstate (matrix [[complexf (cos (angle)); Complex(0., -sin (angle))]; 
                     [Complex(0., -sin (angle)); complexf (cos (angle))]] * vector x)
 
let rotateY angle q = 
    let x = if q = 0 then [complexf 1.;complexf 0.] else [complexf 0.;complexf 1.]            
    
    vqstate (vector x * matrix [[complexf (cos (angle)); complexf(-sin (angle))]; 
                                [complexf(sin (angle)); complexf (cos (angle))]])
 

let bell qb1 qb2 =
    cont {
      let! c = hadamard qb1
      let! b = qexactly qb2
      return (c,cnot c b)} 