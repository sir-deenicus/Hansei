#r @"bin\Debug\net47\Prelude.dll"
#r @"bin\Debug\net47\Hansei.Core.dll"
#r @"bin\Debug\net47\Hansei.dll"
#r @"..\Hansei.Rational\bin\Release\net47\MathNet.Numerics.dll"

open Prelude.Common
open System
open Hansei.Core
open Hansei.Continuation
open Hansei.Core.Distributions
open Hansei 
open MathNet.Numerics.Distributions

let a1,b1 =
    cont {
        let! a = bernoulli 0.5
        let! b = bernoulli 0.8
        return (a,b)
    } |> Thunk |> best_first_sample_dist None 0. 10 1 1

b1    
a1
b1 |> Reified |>  best_first_sample_dist None 0. 10 2 1

#I @"C:\Users\cybernetic\source\repos\FSLMDB\FSLMDB\bin\Debug\net47"
#r "FsPickler.dll" 

open MBrace.FsPickler
let fs = FsPickler.CreateBinarySerializer()
let deserialize data = fs.UnPickle data : 'b  
let serialize item = fs.Pickle item

serialize b1 

let kk = Dict<float, float>()

kk.Add(-1.,-1.)

kk.[-1.] = -1.

open Prelude.Math

geometric 10 0.8 |> limit_reify 6

[|for _ in 0 .. 999 -> Prelude.Math.Stats.discreteSample (Array.normalize [| 0.; 0.1 ; 0.|])|] |> Seq.counts
let ax , yx, cl = geometric 10 0.8 |> Thunk|> best_first_sample_dist None None 0. 50. 10 2 3
ax
Seq.toArray cl
yx

yx |> Reified |>  best_first_sample_dist None (Some cl) 0. 50. 25 3 2 
  
cl |> keyValueSeqtoPairArray 

let zipf s n = ModelFrom(cont { let! n = discretizedSampler (round 2)
                                             (fun () -> Zipf.Sample(s, n) |> float)
                                             10000
                                return n })

let m =
    ModelFrom(cont {
              let! s = uniform [ 1.65..0.001..1.8 ]
              let m = zipf s 100
              let k = m.Reify() |> List.rev
              do! constrain (k.Length >= 10)
              let z = k.[..9] |> List.sumBy fst
              let z2 = k.[..4] |> List.sumBy fst
              do! constrain (abs (z - 0.95) <= 0.04 && abs (z2 - 0.81) <= 0.03)
              return (s)
          })

m.Reify()
|> List.sortBy fst
|> Utils.normalize
|> Utils.histogram2 20.

(zipf 1.794 100).Reify()
|> List.sortByDescending fst
|> List.map
       (fun (p, (Value x)) ->
       x, (p * 100e9)
           .ToString("c2",
                     System.Globalization.CultureInfo.CreateSpecificCulture
                         ("en-US"))) // |> List.sortByDescending snd

((zipf 1.794 100).Reify() |> List.rev).[..4] |> List.sumBy fst
