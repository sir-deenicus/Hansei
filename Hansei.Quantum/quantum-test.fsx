#I @"C:\Users\Admin\Documents\Visual Studio 2017\Libs\MathNet"

#r @"bin\Release\netcoreapp2.1\Hansei.Core.dll"
#r @"..\fparsec\net40-client\fparsecCs.dll"
#r @"..\fparsec\net40-client\fparsec.dll"
#r @".\lib\netstandard2.0\MathNet.Numerics.dll"
#r @".\lib\netstandard2.0\MathNet.Numerics.FSharp.dll"
#r @".\symbolics\net40\mathnet.symbolics.dll"
#r @"bin\Release\netcoreapp2.1\Simple-Symbolics.dll"
#r @"bin\Release\netcoreapp2.1\Hansei.Quantum.dll"

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