#I @"..\"
#r @"bin\Debug\net47\Prelude.dll"
#r @"bin\Debug\net47\Hansei.Core.dll"
#r @"bin\Debug\net47\MathNet.Numerics.dll"
#r @"bin\Debug\net47\MathNet.Numerics.FSharp.dll" 
#r @"bin\Debug\net47\MathNet.Symbolic.Ext.dll"
#r @"bin\Debug\net47\Simple-Symbolics.dll" 
#r @"bin\Debug\net47\Hansei.SymbolicQuantum.dll"

open Hansei.Continuation
open Hansei.Quantum
open Hansei.QUtils 
open Hansei
open MathNet.Symbolics.Extras.Vars 
open MathNet.Symbolics.Extras
open Hansei.SymbolicProb.Distributions
open Hansei.SymbolicProb
open Hansei.Utils
open MathNet.Symbolics
open MathNet.Numerics
open System 

bell "|0>" "|1>" |> Q.histogram2 20.
 
rotateX (pi/3) "|1>" |> Q.histogram2 20. 

rotateX (pi/3) "|1>" |> Q.exact_reify

//partial trace or arising of classicality
cont {
    let! (a,b) = bell "|0>" "|1>"  
    return (a)
}
|> Q.exact_reify
|> measure2
|> List.map (fun (p,x) -> (Algebraic.simplify true p).ToFormattedString() , x) 

cont {
  let! c = hadamard "|1>" 
  let! b = hadamard c
  return b} 
  |> Q.exact_reify

cont {
  let! a,b = bell "|0>" "|1>"
  let! c,d = bell a "|0>"
  let! e,f = bell "|1>" d
  let! g,h = bell b c
  do! observeState (g = "|1>")
  return (c)}    
   |> Q.exact_reify
   //|> Q.histogram2 20.

cont {
  let! a,b = bell "|0>" "|1>"
  do! observeState (b = "|0>")
  return (a)}   
   |> Q.histogram2 20.

cont {
  let! a = hadamard "|0>"
  let! b = hadamard a
  let! c,d = bell a b 
  let! e = bell c d
  let! p = rotateX (pi/8) (snd e)
  do! observeState (p="|1>" && d = "|0>")
  return (fst e <+> c <+> p)
}  
  |> Q.exact_reify 
  |> List.map (fun (p,x) -> p.Simplify(), x)
  |> Q.histogram1 measure2 20.                    
    
let q2 = 
  cont {
    let! a = hadamard "|0>"
    let! b = hadamard a
    return (a,b)
  } |> Q.exact_reify 
                      

let bstate = 
    bell "|1>" "|1>"                                      
    |> Q.exact_reify 
    |> List.map (fun (p, x) -> p.Simplify(), x)
measure2 bstate |> List.map (fun (p,x) -> Expression.toFloat p, x)
 
open Hansei.Utils

cont {
  let! q = bernoulliChoice (1Q/5Q) (q2,bstate)
  let! s = categorical (measure2 q)
  return (s)
} |> exact_reify
  |> normalize
   
cont {
  let! q = bernoulliChoice (1Q/5Q) (q2,bstate) 
  return (Q.measureAsRational2 q)
} |> exact_reify
   

//////////////// 
      
let formatExpr qs = qs |> List.map (fun (p:MathNet.Symbolics.Expression,x) -> (MathNet.Symbolics.Trigonometric.simplify p).ToFormattedString(), x) 

let asRational qs = qs |> List.map (fun (p:MathNet.Symbolics.Expression,x) -> Expression.toRational p, x) 


let T q = phaseShift (pi/4.) q

T "|0>" |> measure |> asRational

rotateZ (pi/8.) "|0>" |> measure |> asRational

phaseShift (pi/8.) "|1>" 

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
      let! a' = if b1 then rotateX (-pi/8) a else Q.exactly a 
      let! b' = if b2 then rotateX (pi/8) b else Q.exactly b
      return (a',b') } 
 
cont {
    let! b1 = bernoulli (1Q/2Q)
    let! b2 = bernoulli (1Q/2Q)
    let! (a,b) = categorical (measure (epr b1 b2))
    return t(b1 && b2) = xor2 a (bflip b)
} |> exact_reify
  |> List.map (fun (p,x) -> Expression.toFloat p , x)
    //|> List.map (fun (p,x) -> ( p ).ToFormattedString(), x)
//|> histogram2 20.        
  

let ns = 1Q/4Q - cos(π/2Q)/4Q


let zs = 4/2*(sin(π/8))**2*(cos(π/8))**2 
Algebraic.simplify true (cos(pi/4Q))

Trigonometric.simplify ns |> Algebraic.simplify true  

bell "|1>" "|1>" |> filterByObserving (fun (a,_) -> a = "|0>") |> Q.exact_reify |> Q.normalize |> Q.histogram1 measure2 20.

bell "|0>" "|0>"|> Q.exact_reify |> Q.normalize |> Q.histogram1 measure2 20.
               

let person angle q b = measure (cont { return! (if b = 1 then rotateX angle q else Q.exactly q) })

let alice = person (pi/8)  //|> toFloat
let bob = person (-pi/8) 

let epr2 = bell "|1>" "|1>"                                  

cont {
   let! b1 = bernoulliChoice (1Q/2Q) (0,1)
   let! b2 = bernoulliChoice (1Q/2Q) (0,1)
  
   let! (a,b) = categorical (measure epr2)
   let! ca = categorical (alice a b1)
   let! cb = categorical (bob b b2) 
                                       
   return b1 &&& b2 = xor2 ca (bflip cb)
} |> exact_reify 
      //|> List.map (fun (p,x) -> Expression.toFloat p , x)
  |> List.map (fun (p,x) -> (  p ).ToFormattedString(), x)
  //|> normalize 
 // |> histogram2 20.
