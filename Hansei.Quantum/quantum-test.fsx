#r @"bin\Release\net45\Prelude.dll"
#r @"bin\Release\netcoreapp2.0\Hansei.Core.dll"
#r @"bin\Release\net45\mathnet.symbolics.dll"
#r @"bin\Release\net45\Simple-Symbolics.dll"
#r @"bin\Release\netcoreapp2.0\Hansei.SymbolicQuantum.dll"

open Hansei.Continuation
open Hansei.Quantum
open Hansei.QUtils 
open Hansei
open MathNet.Symbolics.Extras.Vars 

bell 0 1 |> qhistogram2 20.
 
rotateX (pi/3) 1 |> qhistogram2 20. 

cont {
  let! c = hadamard 1 
  let! b = hadamard c
  return (c)} 
  |> qhistogram2 20.

   