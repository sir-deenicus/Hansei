
#r @"C:\Users\cybernetic\source\repos\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
// #I @"C:\Users\cybernetic\.nuget\packages\"
// #r @"mathnet.numerics\4.9.0\lib\net40\MathNet.Numerics.dll"

#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx"
//open Parseutils
open Prelude.Common
open Prelude.Math
open System

open MathNet.Symbolics.Core.Vars
open MathNet.Symbolics.Utils
open MathNet.Symbolics
open Hansei.FSharpx.Collections.LazyList.ComputationExpression
open MathNet.Symbolics.NumberProperties 
open Hansei.FSharpx.Collections
open Hansei.Backtracking
open Prelude.SimpleGraphs

let g = Prelude.SimpleDirectedGraphs.DirectedMultiGraph<string, string>()


g.InsertVertex "A"
g.InsertVertex "B"
g.InsertVertex "C"

g.InsertEdge ("A","B","3")
g.InsertEdge ("B","C","")

g.Vertices

//g.Remove "B"

g.Edges

g.GetEdges("A")
g.GetEdges("B")

let g2 = Prelude.SimpleGraphs.UndirectedGraph<string>()
g2.InsertVertex "A"
g2.InsertVertex "B"
g2.InsertVertex "C"

g2.InsertEdge ("A","B")

g2.GetEdges "B"
 

////////////////////////////////////////////////////////////////


module VisualProb =
    let inline bind (dist : list<'item * 'number list>)
               (k : 'item -> list<'changeditem list * 'number list>) =
        [ for (x, p) in dist do
              for (y, p2) in k x do
                  yield (y, p @ p2 ) ]

    let fail() = []
    let bernoulli p = [true, [p]; false, [1.- p]]
    let uniform l = l |> List.map (fun x -> x, [1./float l.Length])
    let categorical l = l |> List.map (fun (x,p) -> x, [p])
    let bernoulliChoice a b p = [a,[p];b,[1.- p]]
    let always x = [x,[1.]]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = [v,[]]
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open VisualProb 

let inline aggregate prob =
    prob
    |> List.groupBy fst 
    |> List.map (fun (x, ps) -> x, List.collect snd ps |> List.reduce (*))
     

//POINT 1: Observing, conditioning filters information out, subsequently making graph hard to interpret
dist {
    let! b = bernoulliChoice "B" "T" 0.95
    let! concede = if b="B" then bernoulli 0.78 else always false
    do! observe (not concede = true)
    return [b; string concede]
}  |> aggregate


//POINT 2: Number of things returned must equal number of bindings done
let bnn = 
    dist {
        let! b = bernoulliChoice "H" "T" 0.5
        let! b2 = bernoulliChoice "H" "T" 0.5
        return [b;b2]
    } //|> aggregate

let bn =
    dist
        {   let! b = bernoulliChoice "H" "T" 0.5
            do! observe (b = "H")
            let! b2 = bernoulliChoice "H" "T" 0.5
            let! b3 = if b2 = "H" then bernoulliChoice "H" "T" 0.25 else bernoulliChoice "H" "T" 0.5
            let! b4 = bernoulliChoice "H" "T" 0.75
            return [b; "(b=H)"; b2;"_";b4] }
let bn =
    dist
        {   let! b = bernoulliChoice "H" "T" 0.5
            if b = "H" then
                let! b2 = bernoulliChoice "H" "T" 0.5
                let! b3 = if b2 = "H" then bernoulliChoice "H" "T" 0.25 else bernoulliChoice "H" "T" 0.5
                let! b4 = bernoulliChoice "H" "T" 0.75
                return [b; b2;"_";b4]
            else return []}

let bn =
    dist
        {   let! b = bernoulliChoice "H" "T" 0.5
            let! b2 = bernoulliChoice "H" "T" 0.5
            return [ b;b2;string( b=b2 && b = "H")] }
let bn =
    dist
        {   let! b = bernoulliChoice "H" "T" 0.5
            let! b2 = bernoulliChoice "H" "T" 0.5
            do! observe (not ( b=b2 && b = "H"))
            return [b;b2;"b=b2 && b = H"] }




let bn = dist {
    let! child1 = bernoulliChoice "boy" "girl" 0.5
    let! child2 = bernoulliChoice "boy" "girl" 0.5//ProbList.bernoulliChoice p ("boy2", "girl2")
    let! seefirst = uniform [child1; child2]
    do! observe (seefirst.Contains "boy")
    return [child1;child2; "see " + seefirst]
}


let hasHeads count = List.filter ((=) "H") >> List.length >> ((<=) count)

let rec atleast_n_in_m_flips count flips n = dist {
    if n = 0 then
     if hasHeads count flips then return List.rev(string true :: flips)
     else return List.rev(string false :: flips)
    else let! flip = bernoulliChoice "H" "T" 0.5
         return! atleast_n_in_m_flips count (flip::flips) (n-1) }

let bn = atleast_n_in_m_flips 2 [] 3

open Prelude.SimpleDirectedGraphs

let rec constructProbGraph trackhistory zero sep soleLast (prev: string) (g: DirectedMultiGraph<_, _>) =
    function
    | [ node ], [ w ] when soleLast ->
        if prev <> "" then
            g.InsertVertex prev |> ignore

            if fst w <> zero then
                g.InsertEdge(prev, node, w) |> ignore
    | node :: nodes, w :: ws ->
        let node' =
            if not trackhistory || prev = "" then node
            else (prev + sep + node)

        if prev <> "" then
            g.InsertVertex prev |> ignore

            if fst w <> zero then
                g.InsertEdge(prev, node', w) |> ignore

        constructProbGraph trackhistory zero sep soleLast node' g (nodes, ws)
    | [], [] -> ()
    | _ -> failwith "unexpected error"
     
      

let bn =
    dist {   
        let! b = bernoulliChoice "H" "T" 0.5
        let! b2 = bernoulliChoice "H" "T" 0.5
        let! b3 = bernoulliChoice "H" "T" 0.25
        return [b;b2;"_" ] }
         

open Prelude.SimpleGraphs
 

let bnn = 
    dist {
        let! p1 = uniform ([0.0;0.5;1.0] )
        let! p2 = uniform ([0.0;0.5;1.0] )
        //let! col = categorical ["red", p1; "green", p2]
        //do! observe (col = "green")
        return [sprintf "p1=%A" p1;sprintf "p2=%A" p2]//;sprintf "%A" [p1;p2]]
     }
open VisualProb     
let bnn = 
    dist {
        let! p = uniform [0.0..0.2..1.0] //prior
        let! b = bernoulliChoice "flip heads" "flip tails" p //joint of two possibilities and all of prior
        return [$"p={p}"; b]
    }


#r @"C:\Users\cybernetic\.nuget\packages\spark.net\1.0.2\lib\SparkLib.dll"

List.map (snd >> List.reduce (*)) bnn
|> List.toArray
|> SparkNet.Spark.Render

 
let bnn2 = 
    dist {
        let! p = uniform [0.0..0.2..1.0] //prior
        let! b = bernoulliChoice "flip heads" "flip tails" p //joint of two possibilities and prior
        do! observe (b = "flip tails") //conditional update, like a likelihood
        return [string p; $"observe {b}"; $"{p}\\'"] //path through possibilities
     }

List.map (snd >> List.reduce (*)) bnn3
|> List.toArray
|> SparkNet.Spark.Render
  
let mutable m_w = 521288629u
let mutable m_z = 362436069u

// This is the heart of the generator.
// It uses George Marsaglia's MWC algorithm to produce an unsigned integer.
// See http://www.bobwheeler.com/statistics/Password/MarsagliaPost.txt
let getUint() =
    m_z <- 36969u * (m_z &&& 65535u) + (m_z >>> 16)
    m_w <- 18000u * (m_w &&& 65535u) + (m_w >>> 16)
    (m_z <<< 16) + m_w  

// Produce a uniform random sample from the open interval (0, 1).
// The method will not return either end point.
let getUniform() =
    // 0 <= u < 2^32
    let u = getUint()
    // The magic number below is 1/(2^32 + 2).
    // The result is strictly between 0 and 1.
    (float u + 1.0) * 2.328306435454494e-10 
bnn2

let bnn2b = 
    dist {
        let! p = uniform [0.0..0.2..1.0]
        let! b = bernoulliChoice "flip heads" "flip tails" p 
        do! observe (b = "flip heads")
        return [string p; $"observe {b}"; $"{p}\\'"]
     }    

let bnn3 = 
    dist {
        let! p = bnn2
        printfn "%A" p
        let! b = bernoulli (p |> List.head |> Double.Parse)
        do! observe (b = true)
        return p @ [string b; "observe true"] 
     }
    

//let rec coinLikelihood prior l = dist {
//    let! p = prior
//    let! b = bernoulli p
//    do! observe (b = l)
//    return [p]}

//[false; false;true;true] |> List.fold coinLikelihood (uniform [0.0..0.05..1.]) 
//|> exact_reify
//|> histogram2 20. 

bnn
|> groupit2
|> List.map (fun (sl, p) -> String.concat "\n"  sl, p)
//|> List.filter (fun (s,_) -> s.Contains "BIDEN WINS PA" |> not && s.ToLower().Contains "biden wins ga" |> not && s.ToLower().Contains "biden wins az" |> not)
//|> List.normalizeWeights
|> List.filter (fun (s,_) -> s.Contains "TRUMP WIN")
|> List.sortByDescending snd
|> List.sumBy snd
|> round 3 
73. + 20. + 7.
bnn
|> groupit2
|> List.groupBy (fst >> List.tail)  
|> List.map (fun (x, ps) -> x, List.map snd ps |> List.sum)
|> List.sumBy snd 

let inline processProbMonad (str: 'a -> string) probm =
    [ for (nodes, ps) in probm do
          let (_, nodelist, jointprobs) =
              ((1, [], []), nodes)
              ||> List.fold (fun (i, nodelist, plist) node ->
                  let n = str node :: nodelist

                  let jointprob =
                      (ps
                       |> List.take (min i ps.Length)
                       |> List.reduce (*)),
                      ps.[(min i ps.Length) - 1]

                  i + 1, n, jointprob :: plist)

          if jointprobs <> [] then
              yield nodelist, List.rev jointprobs ]

let kz = processProbMonad string bnn3
let totw =
    kz 
    |> List.sumBy (snd >> List.last >> fst) 

let tot =
    kz
    |> List.map snd
    |> List.map List.head
    |> List.sumBy fst

let len = kz.Length
l

let kz2 =
    [ for (i, (xs, ps)) in List.zip [ 1 .. kz.Length ] kz ->
          let (p, cp), t = List.head ps, List.tail ps
          let len = List.length xs

          List.mapi (fun j x ->
              if j = len - 1 then
                  sprintf "%s(%d)" x i
              else x)

          xs, (p / tot, cp) :: t ]

let xs, ps = List.head kz

let inline buildGraph zero probmonad =
    let preprocessed = processProbMonad string probmonad
    
    let totw =
        preprocessed 
        |> List.sumBy (snd >> List.last >> fst) 

    let processed =
        [ for (nodes, ps) in preprocessed ->
              let len = List.length ps

              let ps' =
                  ps
                  |> List.mapi (fun j (jointprob, condprob) ->
                      if j = len - 1 then jointprob / totw, condprob
                      else condprob, jointprob)

              nodes, ps' ]
    
    let g = DirectedMultiGraph<_, _>()
    
    g.InsertVertex "root" |> ignore
    
    for (nodelist,problist) in processed do
        let n = List.head nodelist
        let w = List.head problist
        if not(g.ContainsEdge ("root", n) |> Option.defaultValue false) then
            g.InsertEdge("root", n, w) |> ignore 
        constructProbGraph true zero "," false  "" g (nodelist, problist)
    g

g
createDagreGraph2 (Pair.map (round 2) >> string) 100 50 g
|> disp false "nn1" 1800 6600
|> printf "%s"

|> Windows.Forms.Clipboard.SetText

bnn  |> groupit2 |> List.groupBy (fst >> List.tail)  
|> List.map (fun (x, ps) -> x, List.map snd ps |> List.sum)

|> writeStrRaw

bnn2 


mergeNodes g

g.Edges

kz  |> List.groupBy fst
    |> List.map (fun (x,ps) -> x, ps |> List.sumBy (fun (_,cc) -> fst (List.head cc) )  ) //
    |> List.normalizeWeights
    |> List.sumBy snd

bn.Categorical |> List.groupBy fst |> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.reduce addLists)|> List.map (keepLeft (List.reduce (*))) |> List.sumBy snd
bn.Categorical |> List.groupBy fst |> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.reduce addLists)|> List.map (keepLeft (List.reduce (*)))

bn.Categorical |> List.groupBy fst //|> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.head) |> List.map (keepLeft (List.reduce (*))) |> List.sumBy snd
bn2.Categorical |> List.groupBy fst// |> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.head) |> List.map (keepLeft (List.reduce (*)))
bn.Categorical |> List.map (keepLeft (List.reduce (*)))// |> List.sumBy snd

