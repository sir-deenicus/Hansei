#r @"bin\Debug\net45\Prelude.dll"
#r @"bin\Debug\net45\Hansei.Core.dll"
#r @"bin\Debug\net45\Hansei.dll"
#r @"..\Hansei.Rational\bin\Release\net45\MathNet.Numerics.dll"

open Prelude.Common
open System
open Hansei.Core
open Hansei.Continuation
open Hansei.Core.Distributions
open Hansei
open MathNet.Numerics.Distributions

let zipf s n = Model(cont { let! n = discretizedSampler (round 2)
                                         (fun () -> Zipf.Sample(s, n) |> float)
                                         10000
                            return n })

let m =
    Model(cont {
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
       x,
       (p * 100e9)
           .ToString("c2",
                     System.Globalization.CultureInfo.CreateSpecificCulture
                         ("en-US"))) // |> List.sortByDescending snd
((zipf 1.794 100).Reify() |> List.rev).[..4] |> List.sumBy fst
