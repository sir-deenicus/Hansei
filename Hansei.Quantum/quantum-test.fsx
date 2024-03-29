﻿#I @"..\"
#r @"bin\Debug\net47\Prelude.dll"
#r @"bin\Debug\net47\Hansei.Core.dll"
#r @"bin\Debug\net47\MathNet.Numerics.dll"
#r @"bin\Debug\net47\MathNet.Numerics.FSharp.dll" 
#r @"bin\Debug\net47\MathNet.Symbolic.Ext.dll"
#r @"bin\Debug\net47\Simple-Symbolics.dll" 
#r @"bin\Debug\net47\Hansei.SymbolicQuantum.dll"

open Prelude.Math
open Hansei.Continuation
open Hansei.Quantum
open Hansei.Utils 
open Hansei 
open MathNet.Symbolics.Core
open Hansei.GenericProb
open Hansei.SymbolicProb.Distributions
open Hansei.SymbolicProb 
open MathNet.Numerics
open MathNet.Symbolics
open MathNet.Symbolics.Core.Constants 
open System  

module QM = Quantum
 
cont {
    let! a = bernoulli (1/2Q) 
    let! b = bernoulli (4/5Q)
    return (a,b)
} |> rejection_sampler 1000
  |> ProbabilitySpace.mapValuesProb Expression.toFloat id

cont {
    let! a = bernoulli (1/2Q) 
    let! b = bernoulli (4/5Q)
    return (a,b)
} |> exact_reify
  |> ProbabilitySpace.mapValuesProb Expression.toFloat id

bell "|0>" "|1>" |> QM.exact_reify |> Quantum.histogram id (fun (a,b) -> a <+> b) 20.

rotateX (pi/3) "|1>" |> Quantum.exact_reify |> QM.histogram2 20. 

rotateX (pi/3) "|1>" |> Quantum.exact_reify

Model.ReifyQuantum(rotateX (pi/3) "|1>")

Model.ReifySymbolic( bernoulli (1/2Q) , 3)

//partial trace or arising of classicality
cont {
    let! (a,b) = bell "|0>" "|1>"  
    return (a) }
|> Quantum.exact_reify
|> measureReified 
|> List.map (fun (p,x) -> (Algebraic.simplify true p).ToFormattedString() , x) 

cont {
  let! c = hadamard "|1>" 
  let! b = hadamard c
  return b} 
  |> QM.exact_reify

cont {
  let! a,b = bell "|0>" "|1>"
  let! c,d = bell a "|0>"
  let! e,f = bell "|1>" d
  let! g,h = bell b c
  do! observeState (g = "|1>")
  return (c)}    
   |> QM.exact_reify
   //|> QM.histogram2 20.

cont {
  let! a,b = bell "|0>" "|1>"
  do! observeState (b = "|0>")
  return (a)}   
   |> QM.exact_reify
   |> QM.histogram2 20.
 
 
let sampler1 ch = best_first_sample_dist None None (Complex 0Q) (Complex 1Q) (MathNet.Symbolics.Complex.magnitude >> squared >> Expression.toFloat) 2. 18 2 10 0. 120 ch

let random_selectorQ choices = GenericProb.random_selector (MathNet.Symbolics.Complex.magnitude >> squared >> Expression.toFloat) (Complex 0Q) choices

//This should return nonsense...and it does
let rejection_sample_distQ nsamples ch =
    rejection_sample_dist (Complex 1Q) (Expression.FromInt32 >> Complex) random_selectorQ nsamples ch 
 
cont {
  let! a = hadamard "|0>"
  let! b = hadamard a
  let! c,d = bell a b 
  let! e = bell c d
  let! p = rotateX (pi/8) (snd e)
  do! observeState (p="|1>" && d = "|0>")
  return (fst e <+> c <+> p)
}  
  |> sampler1 //rejection_sample_distQ 1000 //QM.exact_reify 
  //|> QM.normalize
  //
   |> fun x -> x.Values
   |> List.map (fun (p,x) -> p.Simplify() , x)
   |> measureReified |> List.sumBy fst |> Expression.toFloat
  //|> QM.histogram2 20.                  

match Infix.parse "10562500/10812500*(sin(π/8))^2 + 10562500/10812500*(cos(π/8))^2" with
| ParseResult.ParsedExpression  e -> e |> Expression.toFloat

cont {
  let! a = hadamard "|0>"
  let! b = hadamard a
  let! c,d = bell a b 
  let! e = bell c d
  let! p = rotateX (pi/8) (snd e)
  do! observeState (p="|1>" && d = "|0>")
  return (fst e <+> c <+> p)
}  
  |> QM.exact_reify 
  |> List.map (fun (p,x) -> p.Simplify(), x)
  |> measureReified |> List.sumBy fst |> Expression.toFloat
  |> QM.histogram2 20.  
     
let hh() =
    cont {
          let! a = hadamard "|0>"
          let! b = hadamard a
          let! c,d = bell a b 
          let! e = bell c d
          let! p = rotateX (pi/8) (snd e)
          do! observeState (p="|1>" && d = "|0>")
          return (fst e <+> c <+> p)
        } 

hh () |> QM.reify0 |> first_success 90|> Option.map (fun (c,x) -> x, c.Magnitude |> squared |> MathNet.Symbolics.Utils.fmt) // //|> List.map (fun (p,x) -> p.Simplify() |> string, x)  
hh() |> QM.exact_reify |> List.map (fun (p,x) -> p.Simplify() |> string, x) 

Model.ReifyQuantum(
    cont {
      let! a = hadamard "|0>"
      let! b = hadamard a
      let! c,d = bell a b 
      let! e = bell c d
      let! p = rotateX (pi/8) (snd e)
      do! observeState (p="|1>" && d = "|0>")
      return (fst e <+> c <+> p)
    }, 6)
      |> List.map (fun (p,x) -> p.Simplify() |> string, x)  

let q2 = 
  cont {
    let! a = hadamard "|0>" 
    let! b = hadamard a
    return (a,b) //does this make sense?
  } |> QM.exact_reify 
       
q2 |> QM.histogram2 20.     


let bstate = 
    bell "|1>" "|1>"                                      
    |> Quantum.exact_reify 
    |> List.map (fun (p, x) -> p.Simplify(), x)

measureReified bstate |> List.map (fun (p,x) -> Expression.toFloat p, x)
 
cont {
  let! q = bernoulliChoice (1Q/5Q) (q2,bstate)
  let! s = categorical (measureReified q)
  return (s)
} |> exact_reify
  |> normalize
   
cont {
  let! q = bernoulliChoice (1Q/5Q) (q2,bstate) 
  return q
} |> exact_reify
   

//////////////// 
       
let T q = phaseShift (pi/4.) q

T "|0>" |> measure  

rotateZ (pi/8.) "|0>" |> measure |> ProbabilitySpace.mapItemsProb Expression.toRational id

phaseShift (pi/8.) "|1>" |> measure


let xor a b = (a + b) % 2

let xor2 (a:string) (b:string) = (int a.[1] + int b.[1]) % 2

let t = function false -> 0 | true -> 1     

//Classical approach a should = b
cont {
   let! b1 = bernoulli (1Q/2Q)
   let! b2 = bernoulli (1Q/2Q)
   let (a,b) = 1,1
   return (xor a b = t(b1 && b2))
} |> exact_reify 

let epr b1 b2 = 
    cont {
      let! (a,b) = bell "|1>" "|1>" //entangle
      let! a' = if b1 then rotateX (-pi/8) a else QM.exactly a 
      let! b' = if b2 then rotateX (pi/8) b else QM.exactly b
      return (a',b') } 
 
cont {
    let! b1 = bernoulli (1Q/2Q)
    let! b2 = bernoulli (1Q/2Q)
    let! (a,b) = categorical (measure (epr b1 b2))
    return t(b1 && b2) = xor2 a (bflip b)
} |> exact_reify
  //|> List.map (fun (p,x) -> Expression.toFloat p , x)
  |> List.map (fun (p,x) -> (Expression.FullerSimplify p).ToFormattedString(), x)
//|> histogram2 20.         
 
cont {
    let! b1 = bernoulli (1Q/2Q)
    let! b2 = bernoulli (1Q/2Q)
    let! (a,b) = categorical (measure (epr b1 b2))
    return t(b1 && b2) = xor2 a (bflip b)
} |> rejection_sampler 1000
  |> List.map (fun (p,x) -> Expression.toFloat p , x)
  //|> List.map (fun (p,x) -> (Expression.FullerSimplify p).ToFormattedString(), x)
   
bell "|1>" "|1>" |> filterByObserving (fun (a,_) -> a = "|1>") |> QM.exact_reify |> QM.normalize |> QM.histogram2 20.

bell "|0>" "|0>"|> QM.exact_reify |> QM.histogram2 20.
 
let person angle q b = measure (cont { return! (if b = 1 then rotateX angle q else QM.exactly q) })
let person2 angle q b = cont { return! (if b = 1 then rotateX angle q else QM.exactly q) }

let alice = person (-pi/8)  //|> toFloat
let bob = person (pi/8) 

let alice2 = person2 (-pi/8)  //|> toFloat
let bob2 = person2 (pi/8) 

let epr2 = bell "|1>" "|1>"   

let epr3 b1 b2 = cont {
    let! (a,b) = bell "|1>" "|1>" //entangle
    let! a' = alice2 a b1
    let! b' = bob2 b b2
    return (a',b') } 

cont {
   let! b1 = bernoulliChoice (1Q/2Q) (0,1)
   let! b2 = bernoulliChoice (1Q/2Q) (0,1)
  
   let! (a,b) = categorical (measure epr2)
   let! ca = categorical (alice a b1)
   let! cb = categorical (bob b b2) 
                                       
   return b1 &&& b2 = xor2 ca (bflip cb)
} |> exact_reify 
      |> List.map (fun (p,x) -> Expression.toFloat p , x)
  //    |> List.map (fun (p,x) -> (Expression.FullerSimplify p).ToFormattedString(), x)
  //|> normalize 
 // |> histogram2 20. 

cont {
   let! b1 = bernoulliChoice (1Q/2Q) (0,1)
   let! b2 = bernoulliChoice (1Q/2Q) (0,1)
  
   let! (ca,cb) = categorical (measure (epr3 b1 b2)) 
                                       
   return b1 &&& b2 = xor2 ca (bflip cb)
} |> exact_reify 
      |> List.map (fun (p,x) -> Expression.toFloat p , x)
     // |> List.map (fun (p,x) -> (Expression.FullerSimplify p).ToFormattedString(), x)


let ns = 1Q/4Q - cos(π/2Q)/4Q
 
let zs = 4/2*(sin(π/8))**2*(cos(π/8))**2 

Algebraic.simplify true (cos(pi/4Q))

Expression.FullerSimplify ns