#I @"..\"
#r @"bin\Release\net45\Prelude.dll"
#r @"bin\Release\netcoreapp2.0\Hansei.Core.dll"
#r @"bin\Release\net45\MathNet.Numerics.FSharp.dll" 
#r @"bin\Release\net45\mathnet.symbolics.dll"
#r @"bin\Release\net45\Simple-Symbolics.dll" 
#r @"bin\Release\netcoreapp2.0\Hansei.SymbolicQuantum.dll"

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

bell 0 1 |> qhistogram2 20.
 
rotateX (pi/3) 1 |> qhistogram2 20. 


//partial trace or arising of classicality
cont {
    let! (a,b) = bell 0 1 
    return (a)
}
|> qexact_reify
|> measure2
|> List.map (fun (p,x) -> (Algebraic.simplify true p).ToFormattedString() , x) 

cont {
  let! c = hadamard 1 
  let! b = hadamard c
  return (c)} 
  |> qhistogram2 20.

cont {
  let! a,b = bell 0 1
  let! c,d = bell a 0
  let! e,f = bell 1 d
  let! g,h = bell b c
  do! observeState (g = 1)
  return (c)}    
   |> qhistogram2 20.

cont {
  let! a,b = bell 0 1
  do! observeState (b = 0)
  return (a)}   
   |> qhistogram2 20.

cont {
  let! a = hadamard 0
  let! b = hadamard a
  let! c,d = bell a b 
  let! e = bell c d
  let! p = rotateX (pi/8.) (snd e)
  do! observeState (p=1 && d = 0)
  return (fst e,c,p)
}  
  |> qexact_reify 
  |> qhistogram1 measure2 20.                    
    
let q2 = 
  cont {
    let! a = hadamard 0
    let! b = hadamard a
    return (a,b)
  } |> qexact_reify 
                      

let bstate = 
    bell 1 1                                      
    |> qexact_reify
    |> qnormalize                        

qhistogram1 measure2 20. bstate


let measureWithThenToRational m qs = m qs |> List.map (fun (p,x) -> Expression.toRational p, x)
let measureWithThenToFloat m qs = m qs |> List.map (fun (p,x) -> Expression.toFloat p, x)

let measureAsRational qs = measureWithThenToRational measure qs
let measureAsRational2 qs = measureWithThenToRational measure2 qs

open Hansei.Utils

cont {
  let! q = bernoulliChoice (1Q/5Q) (q2,bstate)
  let! s = categorical (measure2 q)
  return (s)
} |> exact_reify
  |> normalize
  |> histogram2 20.

 
cont {
  let! q = bernoulliChoice (1Q/5Q) (q2,bstate) 
  return (measureAsRational2 q)
} |> exact_reify
  |> histogram2 20.
   

//////////////// 
      
let formatExpr qs = qs |> List.map (fun (p:MathNet.Symbolics.Expression,x) -> (MathNet.Symbolics.Trigonometric.simplify p).ToFormattedString(), x) 

let asRational qs = qs |> List.map (fun (p:MathNet.Symbolics.Expression,x) -> Expression.toRational p, x) 


let T q = phaseShift (pi/4.) q

T 0 |> measure |> asRational

rotateZ (pi/8.) 0 |> measure |> asRational

phaseShift (pi/8.) 1 

let xor a b = (a + b) % 2

let t = function false -> 0 | true -> 1     

//Classical approach a should = b
cont {
   let! b1 = bernoulli (1Q/2Q)
   let! b2 = bernoulli (1Q/2Q)
   let (a,b) = 1,1
   return (xor a b = t(b1 && b2))
} |> exact_reify
  |> histogram2 20.

let epr b1 b2 = 
    cont {
      let! (a,b) = bell 1 1 //entangle
      let! a' = if b1 then rotateX (-pi/8) a else qexactly a 
      let! b' = if b2 then rotateX (pi/8) b else qexactly b
      return (a',b') } 
let zz =
    cont {
      let! b1 = bernoulli (1Q/2Q)
      let! b2 = bernoulli (1Q/2Q)
      let! (a,b) = categorical (measure (epr b1 b2))
      return t(b1 && b2) = xor a (bflip b)
    } |> exact_reify
    //  |> List.map (fun (p,x) -> (Expression.toFloat p , x)
      |> List.map (fun (p,x) -> ( p ).ToFormattedString(), x)
  //|> histogram2 20.        
  

let ns = 1Q/4Q - cos(π/2Q)/4Q


let zs = 4/2*(sin(π/8))**2*(cos(π/8))**2 
Algebraic.simplify true (cos(pi/4Q))

Trigonometric.simplify ns |> Algebraic.simplify true  

bell 1 1 |> filterByObserving (fun (a,_) -> a = 0) |> qexact_reify |> qnormalize |> qhistogram1 measure2 20.

bell 0 0 |> qexact_reify |> qnormalize |> qhistogram1 measure2 20.
               

let person angle q b = measure (cont { return! (if b = 1 then rotateX angle q else qexactly q) })

let toFloat = List.map (fun (p,x) -> Expression.toFloat p, x)

let alice = person (pi/8)  //|> toFloat
let bob = person (-pi/8) 

let epr2 = bell 1 1                                  

cont {
   let! b1 = bernoulliChoice (1Q/2Q) (0,1)
   let! b2 = bernoulliChoice (1Q/2Q) (0,1)
  
   let! (a,b) = categorical (measure epr2)
   let! ca = categorical (alice a b1)
   let! cb = categorical (bob b b2) 
                                       
   return b1 &&& b2 = xor ca (bflip cb)
} |> exact_reify 
      //|> List.map (fun (p,x) -> Expression.toFloat p , x)
  |> List.map (fun (p,x) -> (  p ).ToFormattedString(), x)
  //|> normalize 
 // |> histogram2 20.
