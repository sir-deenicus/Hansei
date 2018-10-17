#r @"bin\Debug\net45\Prelude.dll"
#r @"bin\Debug\net45\Hansei.Core.dll"
#r @"bin\Debug\net45\Hansei.dll"

open Prelude.Common
open System
open Hansei.Core
open Hansei.Continuation
open Hansei.Core.Distributions
open Hansei

module Array = 
  let mapColumn condition f (a:_[][]) =
      [|for r in 0..a.Length - 1 ->
          [| for c in 0..a.[0].Length - 1 -> 
               if condition c a.[r].[c] then f a.[r].[c] else a.[r].[c] |] |]

let header = "Title;Year;Critic RT;RT count; RT User Score;RT User Count;IMDB;Meta critic; meta user".Split(';')
let uheader = "RottenTomatoes Critic;RottenTomatoes User;IMDB;Metacritic critic;Metacritic user".Split(';')

let movies =
    "
    Iron Man;2008;  94;268;  91;1078187;  7.9;  79;  8.5

    The Incredible Hulk;  2008;  67;225;  71;736932;  6.8;  61;  7.4

    Iron Man 2;  2010;  73;279;  72;478115;  7;  57;  6.4

    Thor;  2011;  77;207;  76;244561;  7;  57;  7.1

    Captain America: The First Avenger; 2011; 80;252;  74;189570;  6.9;  66; 7.2

    Marvel's The Avengers;  2012;  92;328;  91; 1131538;  8.1;  69;  8

    Iron Man 3;  2013;  80;298;  78;482107;  7.2;  62;  6.5

    Thor: The Dark World;  2013;  66;253;  77;307835;  7;  54;  7.3

    Captain America: The Winter Soldier;  2014;  89;242;  92;278538;  7.8;  70;  8.4

    Guardians of the Galaxy;  2014;  91;295;  92;251536;  8.1;  76;  8.6

    Avengers: Age of Ultron;  2015;  75;325;  83;284902;  7.4;  66;  7

    Ant-Man;  2015;  82;287;  86;163656;  7.3;  64;  8

    Captain America: Civil War; 2016;  91;359;  89;175545;  7.8;  75;  8.2

    Doctor Strange;  2016;  90;305;  86;106088;  7.5;  72;  8.2 

    Guardians of the Galaxy Vol. 2;  2017;  83;334;  88;104587;  7.7;  67;  7.7

    Spider-Man: Homecoming;  2017;  92;312;  88;101991;  7.5;  73;  7.6

    Thor: Ragnarok;  2017;  92;295;  88;85736;  8;  74;  7.6

    Black Panther;  2018;  97;282;  76; 53537;  7.8;  88; 6.5
    "

let movienames, stats =
    movies.Split('\n') 
     |> Array.mapFilter trim ((<>) "") 
     |> Array.map (fun line -> 
         let cols = line.Split(';') 
         cols.[0],  cols.[1..] |> Array.map float)
     |> Array.unzip
      
type Pattern = 
   | Diff of int * int * float
   | Comparison of int * int  
   | And of Pattern * Pattern
   | Empty 

let ratings = Array.selectColumns (set [1;3;5;6;7]) stats |> Array.map (Array.map (fun x -> 10. ** -(floor (log10 x) + 1.) * x ))

let len = ratings.[0].Length - 1

let rec prettyPrint = function
      | Comparison(i1,i2) ->  
         sprintf " %s < %s" uheader.[i1] uheader.[i2]
      | Diff(i1,i2,diff) ->  
         sprintf "%s - %s < %f" uheader.[i1] uheader.[i2] diff
      | And (p1,p2) -> sprintf "%s And %s" (prettyPrint p1) (prettyPrint p2)
      | Empty -> ""

let rec toFunction = function 
     | Comparison(i1,i2) -> fun (r:_[]) -> r.[i1] < r.[i2]
     | Diff(i1,i2,diff) -> fun (r:_[]) -> abs(r.[i1] - r.[i2]) < diff
     | And (f,g) -> fun r -> toFunction f r && toFunction g r
     | Empty -> fun _ -> true

let test = function 
    | filter -> 
       let f = Array.filter (toFunction filter) ratings 
       float f.Length / float ratings.Length       

let test2 = 
    function 
    | filter -> 
        let fails = ResizeArray()
        let f = Array.filteri (fun i (r:_[]) -> 
                  let cond = (toFunction filter) r
                  if not cond then fails.Add(movienames.[i]) |> ignore; 
                  cond) ratings 
        (float f.Length , float ratings.Length), fails 

let m = 
    Model(cont { 
       let! compare = bernoulli 0.5
       if compare then
        let! i1 = uniform_int_range 0 len
        let! i2 = uniform_int_range 0 len  
        do! constrain (i1 <> i2) 
        let pattern = Comparison(i1,i2)
        let! p = uniform_float 20
        do! observe (p < test pattern)
        return pattern
       else let! i1 = uniform_int_range 0 len
            let! i2 = uniform_int_range 0 len
            let (i,j) = lessToLeft (i1, i2) 
            do! constrain ((i,j) = (0,1) || (i, j) = (3,4) ) 
            let! r = uniform [0.0..0.05..0.25] 
            let pattern = Diff(i,j,r)
            let! p = uniform_float 20
            do! observe (p < test pattern)
            return pattern })

let rec combo maxn n prog = cont {  
    if n > maxn then return prog
    elif n > 1 then 
      let! p2 = m.model 
      let pattern = And (prog, p2)
      let! p = uniform_float 20
      do! observe (p < test pattern)
      return! combo maxn (n+1) (pattern)
    else
      let! p1 = m.model
      let! p2 = m.model 
      do! constrain (p1 <> p2)
      let pattern = And (p1, p2)
      let! p = uniform_float 20
      do! observe (p < test pattern)
      return! combo maxn (n+1) pattern }

let m2 = Model(combo 3 1 Empty)

let resultsall = m.Reify() |> Utils.normalize 
resultsall |> List.sortByDescending fst |> printWith prettyPrint |> Utils.histogram2 20.

let results = m.ImportanceSample(1000, 10) 
results |> List.sortBy fst |> Utils.normalize |> printWith prettyPrint |> Utils.histogram2 20.

let resultsall2 = m2.Reify()|> Utils.normalize 
resultsall2 |> List.sortByDescending fst |> printWith prettyPrint |> Utils.histogram2 20.

let results2 = m2.ImportanceSample(5000,250) 

m2.ImportanceSampleExplore(5000,250,0.5) |> List.sortBy fst |> Utils.normalize |> printWith prettyPrint |> Utils.histogram2 20.

results2 |> List.sortBy fst |> Utils.normalize |> printWith prettyPrint |> Utils.histogram2 20.

List.map (fun (p,Value x) -> prettyPrint x, test2 x) results2 |> List.sortByDescending (snd >> fst >> uncurry2 (/)) //|> List.filter (snd >> snd >> fun l -> Seq.length l = 1 && Seq.contains "Black Panther" l)//
List.map (fun (p,Value x) -> test x |> round 1) results2 |> Seq.counts |> keyValueSeqtoPairArray //
Model(cont { let! a = uniform ["cat";"dog"] 
             let r = if a = "cat" then 0.9 else 0.2
             let! p = uniform_float 10
             do! observe (p < r)
             return a} ).Reify()|> Utils.normalize |> Utils.histogram2 20.