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
let rec sampleN_without_replacement sampler n i ch =
    function
    | _ when i >= n -> ch
    | [] -> ch
    | choices ->
        let choiceIndex = sampler choices
        let mutable c = -1
        let mutable found = None

        let rem =
            [ for x in choices do
                  c <- c + 1
                  if c <> choiceIndex then yield x
                  else found <- Some x ]
        sampleN_without_replacement sampler n (i + 1) ((choiceIndex, found.Value)::ch) rem

open Prelude.Math

let discreteSampleL ps =  
    Stats.discreteSample (Array.normalize [|for (p,w,_) in ps -> p * w|])

let discreteSampleLOpp ps =
    [| for (p, w,_) in ps ->
           if p = 0. then p * w
           else 1. - (p * w) |]
    |> Array.normalize
    |> Stats.discreteSample

let best_first_sample_distc (maxtime:_ option) prevtabu lowp maxwidth maxdepth width niters space =
    let t0 = System.DateTime.Now
    let paths = defaultArg prevtabu (Dict<int32 list, float>())
    let maxbeamdepth = int(log maxwidth / log (float width))
    let inline testPath k =
        match paths.tryFind k with
        | Some -1. -> true, -1.
        | Some x -> false, x
        | None -> false, 1.
    let rec propagateUp r =
        function
        | _ when r < 0.01 -> ()
        | [] -> ()
        | (_ :: path) ->
            paths.ExpandElseAdd path (fun v ->
                if v = -1. then v
                else max 0. (v + v * r)) (1. + r)
            propagateUp (r * 0.5) path

    let rec loop (curpath:int32 list) depth pcontrib ans =
        function
        | _ when depth > maxdepth
                  || (maxtime.IsSome
                       && (DateTime.Now - t0).TotalSeconds > maxtime.Value) ->
            ans
        | [] -> 
            propagateUp -0.1 curpath
            paths.ExpandElseAdd curpath (fun _ -> -1.) -1.; 
            ans
        | ([ (p, Value v) ] : ProbabilitySpace<_>) ->
            propagateUp 0.1 curpath
            paths.ExpandElseAdd curpath (fun _ -> -1.) -1.
            Hansei.Utils.insertWithx (fun _ t -> t) v (p * pcontrib,1.) ans
        | [ (p, Continued(Lazy th)) ] ->
            loop curpath (depth + 1) (p*pcontrib) ans (th)
        | ch ->
            let fch = ch |> List.mapi (fun i (p,t) -> 
                let pass, w = testPath (i::curpath) 
                if pass then 0., 0., t else p, w, t)
            let bwidth = if depth > maxbeamdepth then 1 else width
            let choices =
                if Prelude.Math.random.NextDouble() < lowp then
                    sampleN_without_replacement discreteSampleLOpp bwidth 0 [] fch
                else sampleN_without_replacement discreteSampleL bwidth 0 [] fch
        
            let selected =
                [for (b, (p,_,t)) in choices do
                  if p <> 0. then 
                    yield loop (b::curpath) (depth + 1) (pcontrib) ans [p, t ]]

            let aggr = Dict() 
            for samples in selected do aggr.MergeWith (fun _ t -> t) samples
            aggr

    let rec sampler (ch : ProbabilitySpace<_>) ans =
        function
        | 0 ->
            let t1 = System.DateTime.Now
            printfn "done %d worlds\nTime taken: %A seconds" niters
                (round 3 ((t1 - t0).TotalSeconds))
            [ for (KeyValue(v, (p,n))) in ans -> p/n, Value v ], ch,paths //: ProbabilitySpace<_>
        | n ->
            let ans = loop [] 0 1.0 ans ch
            sampler ch ans (n - 1) 
    let ch = 
        match space with
        | Thunk t -> reify0 t
        | Reified p -> p

    sampler (ch) (Dict()) niters : ('a ProbabilitySpace * 'a ProbabilitySpace * Dict<int32 list, float>)

let kk = Dict<float, float>()

kk.Add(-1.,-1.)

kk.[-1.] = -1.

open Prelude.Math

geometric 10 0.8 |> limit_reify 6

[|for _ in 0 .. 999 -> Prelude.Math.Stats.discreteSample (Array.normalize [| 0.; 0.1 ; 0.|])|] |> Seq.counts
let ax , yx, cl = geometric 10 0.8 |> Thunk|> best_first_sample_distc None None 0. 50. 10 2 3
ax
Seq.toArray cl
yx

yx |> Reified |>  best_first_sample_distc None (Some cl) 0. 50. 25 3 2 
  
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
