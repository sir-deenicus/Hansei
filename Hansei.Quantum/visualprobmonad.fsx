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

//setLatex()
//Maths.adjustGrapher()

//type SimpleGraph<'a>() =
//    let edges = Dict<string, Dict<string, 'a ResizeArray>>()

//    let numberOfChildren n =
//        match edges.tryFind n with
//        | Some a -> a.Count
//        | None -> 0

//    member g.Vertices =
//        g.EdgeData
//        |> Seq.map keyValueToKey
//        |> Seq.toArray

//    member g.Edges =
//        [ for (KeyValue(e, e2s)) in edges do
//            for (KeyValue(e2, ws)) in e2s do
//                for w in ws do
//                    yield (e, e2, w) ]

//    member g.InsertVertex(s: string) =
//        let contained = edges.ContainsKey s
//        if not contained then edges.Add(s, Dict())
//        contained

//    member g.Remove(v) =
//        match (edges.tryFind v) with
//        | None -> false
//        | Some elist ->
//            for KeyValue(_, es) in edges do
//                if es.ContainsKey v then
//                    es.[v].Clear()
//                    es.Remove v |> ignore
//            elist.Clear()
//            edges.Remove v

//    member g.EdgeData = edges

//    member g.InsertEdge(v0, v1, w) =
//        maybe {
//            let! edge = edges.tryFind v0
//            match edge.tryFind v1 with
//            | None ->
//                g.InsertVertex v1 |> ignore
//                edge.Add(v1, ResizeArray([ w ]))
//                return true
//            | Some n ->
//                n.Add w
//                return true
//        }

//    ///f informs whether the node is penultimate or not.
//    member g.ModifyNodeEdges f n =
//        match edges.tryFind n with
//        | Some nodes ->
//            let keys = Seq.toArray nodes.Keys
//            let ispenultimate =
//                keys
//                |> Array.sumBy numberOfChildren = 0
//            for k in keys do
//                let currnodes = nodes.[k].ToArray()
//                nodes.[k].Clear()
//                nodes.[k] <- f ispenultimate currnodes
//        | None -> ()

//    member g.ContainsVertex v = edges.ContainsKey v

//    member g.ContainsEdge v1 v2 = maybe {
//                                      let! edges = edges.tryFind v1
//                                      return (edges.ContainsKey v2) }
//    member g.GetEdges v = maybe {
//                              let! elist = edges.tryFind v
//                              return elist |> keyValueSeqtoPairArray }

//let g = SimpleGraph<string>()


//g.InsertVertex "A"
//g.InsertVertex "B"
//g.InsertVertex "C"

//g.InsertEdge ("A","B","3")
//g.InsertEdge ("B","C","")

//g.Vertices

//g.Remove "B"

//g.Edges

//type VisualDistribution<'item, 'number when 'item: comparison> =
//    { Categorical : ('item * 'number list) list
//      Depth : int}

//let inline bindx maxdepth (dist : VisualDistribution<'item,'number>) (k : 'item -> VisualDistribution<'changeditem list,'number>) =
//    //let g = dist.G
//    let maxdepthval = Option.defaultValue Int32.MaxValue maxdepth
//    let mutable depth = -1
//    //g.InsertVertex "root"
//    let l =
//        [

//        for (x,p) in dist.Categorical do
//            //printfn "in0: %A" (depth)

//            if depth < maxdepthval then

//                depth <- depth + 1//dn.Depth
//                let dn = k(x)

//                depth <- max depth dn.Depth
//                //depth <- max depth dn.Depth
//                //printfn "in: %A" (depth)
//                //g.InsertEdge("root", string x, (List.reduce (*) p,List.head p))
//                for (y,p2) in dn.Categorical do
//                    //printfn "%A" (depth, y)
//                    //let zz = List.fold (fun (i,s) b ->
//                    //            let n = s + string b
//                    //            if s <> "" then
//                    //                g.InsertVertex s
//                    //                g.InsertVertex n
//                    //                let ps = p @ p2
//                    //                //printfn "%A" ps
//                    //                let jp = (ps |> List.take (min i ps.Length) |> List.reduce (*)), ps.[(min i ps.Length) - 1]
//                    //                g.InsertEdge(s,n, jp)
//                    //                |> ignore
//                    //            i+1, n) (1,"") y
//                    //g.InsertVertex (string y) |> ignore
//                    //g.InsertEdge (string x,string y, string(p @ p2 |> List.reduce (*))) |> ignore
//                    yield (y, p @ p2) ]

//    //printfn "%A" ps
//    {
//          Categorical = l
//          Depth = depth} 

////////////////////////////////////////////////////////////////

(**A simple wrapper around the list monad can do some book-keeping of probabilities for us by silently tracking/propagating joint probabilities*)
module SimplestWrapper =
    let inline bind (dist: list<'item * 'number>) (k: 'item -> list<'changeditem * 'number>) =
        [ for (x, p) in dist do
              for (y, p2) in k x do
                  yield (y, p * p2) ]

    let fail () = []
    let bernoulli p = [ true, p; false, 1. - p ]

    let uniform l =
        l |> List.map (fun x -> x, 1. / float l.Length)

    let inline categorical l = List.normalizeWeights l
    let bernoulliChoice a b p = [ a, p; b, 1. - p ]
    let always x = [ x, 1. ]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero() = always ()

        member d.MergeSources(xs, ys) =
            [ for (x, p) in xs do
                  for (y, p2) in ys do
                      yield ((x, y), p * p2) ]

    let dist = DistributionBuilder()

    let observe test =
        dist { if not test then return! fail () }

open SimplestWrapper
 
dist {
    let! b = bernoulli 0.5
    and! b2 = bernoulli 0.75
    and! b3 = bernoulli 0.75
    let! c = uniform [ b2; b3 ]
    and! b4 = bernoulli 0.25
    return (b, b2, b3, c, b4)
}     

dist {
    let! b = bernoulliChoice "H" "T" 0.5
    let! b2 = bernoulliChoice "H" "T" 0.5

    let! b3 =
        if b2 = "H" then
            bernoulliChoice "H" "T" 0.25
        else
            bernoulliChoice "H" "T" 0.5

    return (b, b2, b3)
}

(**There is a close relationship between control structures for non-determinism and inference. Monads show up a lot when you want to manage complex control flow using higher order functions and a simple pipelining design. Nondeterminism is about computations which have multiple possibilities and branching paths. Hence the close relation. *)
(** But what if we want to sample from a very large space? The below computation will run for a very long time. Laziness will be helpful. *)
dist {
    let! a = uniform [1..10000]
    let! b = uniform [1..10000]
    return (a + b)
} 
 
open Hansei.Backtracking
open Hansei.FSharpx.Collections
module LazyListWrapper =
    open Hansei.FSharpx.Collections.LazyList.ComputationExpression
    let inline bind (dist : LazyList<'item * 'number>)
               (k : 'item -> LazyList<'changeditem * 'number>) =

        lzlist {
            let! (x, p) = dist
            let! (y, p2) = k x
            yield (y, p * p2)
        }

    let fail() = LazyList.empty
    let bernoulli p = lzlist {yield true, p; yield false, 1.- p} 
    let always x = lzlist {yield x,1.}

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = LazyList.choice a b
        member __.Delay(f: unit -> LazyList<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

module LazySeqWrapper =
    let inline bind (dist : seq<'item * 'number>)
               (k : 'item -> seq<'changeditem * 'number>) =
        seq { for (x, p) in dist do
                for (y, p2) in k x do
                    yield (y, p * p2) }

    let fail() = Seq.empty
    let bernoulli p = seq [true, p; false, 1.- p]
    let uniform l = l |> Seq.map (fun x -> x, 1./float (Seq.length l))
    let bernoulliChoice a b p = seq [a,p;b,1.- p]
    let always x = seq [x,1.]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = Seq.concat [a;b]
        member __.Delay(f: unit -> seq<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open LazySeqWrapper
 
let qn =
    dist {
        let! a = uniform (seq [1..10000])
        let! b = uniform (seq [1..10000])
        return (a + b) % 20
    } |> Seq.take 100 |> Seq.toArray |> Array.groupBy fst |> Array.map (fun (x,ps) -> x, Array.sumBy snd ps)

let rec geom c p =
    dist {
        let! b = bernoulli p
        if b then return c
        else return! geom (c+1) p
    }

dist {
    let! i = geom 0 0.5
    do! observe (i> 3)
    let! j = geom 1 0.6
    return (i+j)
} |> Seq.take 1

geom 0 0.6 |> Seq.take 10  |> Seq.toArray

let rec infiniteFlips p = LazyListWrapper.dist {
    let! b = LazyListWrapper.bernoulli p
    return b
    return! infiniteFlips p
    }

infiniteFlips 0.001 |> LazyList.take 10 |> LazyList.toArray//|> Seq.take 10

dist {
    let! b = infiniteFlips 0.5
    let! j = geom 1 0.6
    do! observe (not b)
    return (b, j)
} 

seq { for i in 1..10 do
        for j in Seq.initInfinite id do
            if i > 5 then yield (i,j) }
|> Seq.take 1

seq { for i in 1..10 do
        for j in Seq.initInfinite id do
            yield (i,j) }
|> Seq.take 12
|> Seq.toArray

let ll1 = LazyList.ofSeq (Seq.initInfinite id)

ll1 |> LazyList.take 3 |> LazyList.toArray

seq { for i in 1..10 do
        for j in Seq.initInfinite id do
            if i > 5 then yield (i,j) }
|> Seq.take 1

lzlist {   
    let! i = LazyList.ofList [1..10]
    let! j = ll1 
    yield (i,j) }
|> LazyList.take 12
|> Seq.toArray

module BtWrapper =
    let inline bind (dist : LazyStream<'item * 'number>)
               (k : 'item -> LazyStream<'changeditem * 'number>) =
        bt { let! (x, p) = dist
             let! (y, p2) = k x
             yield (y, p * p2) }

    let fail() = Nil
    let bernoulli p = bt {yield true, p; yield false, 1.- p}
    let uniform l = l |> Seq.map (fun x -> x, 1./float (Seq.length l))
    let bernoulliChoice a b p = bt {yield a,p; yield b,1.- p}
    let always x = bt {yield x,1.}

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = choice id a b
        member __.Delay(f: unit -> LazyStream<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

//
open BtWrapper

let rec infiniteFlips p = dist {
        let! b = bernoulli p
        return b
        return! infiniteFlips p
    }

infiniteFlips 0.001 |> run 100 |> Seq.take 50 |> Seq.toArray


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

let rec runloop sep soleLast (s:string) (g:SimpleGraph<float * float>) = function
    | [a1], [b1] when soleLast ->
        if s <> "" then
            g.InsertVertex s
            if fst b1 <> 0. then 
                g.InsertEdge(s, a1, b1) |> ignore
    |  a1::ats, b1::bts ->
        let n = if s = "" then a1 else (s + sep + a1)
        if s <> "" then
            g.InsertVertex s
            if fst b1 <> 0. then 
                g.InsertEdge(s, n, b1) |> ignore
        //(((s + a1),b1)::l)
        runloop sep soleLast n g (ats,bts)
    | [], [] -> ()
    | _ -> failwith "unexpected error in runloop"

let rec runloop2 sep soleLast prev (g:SimpleGraph<float * float>) = function
| [a1], [b1] when soleLast ->
    if prev <> "" then
        g.InsertVertex prev
        if fst b1 <> 0. then 
            g.InsertEdge(prev, a1, b1) |> ignore
|  a1::ats, b1::bts -> 
    if prev <> "" then
        g.InsertVertex prev
        if fst b1 <> 0. then 
            g.InsertEdge(prev, a1, b1) |> ignore
    //(((s + a1),b1)::l)
    runloop2 sep soleLast a1 g (ats,bts)
| [], [] -> ()
| _ -> failwith "unexpected error in runloop"

let fixlen maxlen s =
    if String.length s > maxlen then s.Replace(",", "\\n").Replace("/", "\\n")
    else s



let template = System.IO.File.ReadAllText("C:\Users\cybernetic\Documents\Papers\dagre-template.txt")


let bn =
    distv {   
        let! b = bernoulliChoicev "H" "T" 0.5
        let! b2 = bernoulliChoicev "H" "T" 0.5
        let! b3 = bernoulliChoicev "H" "T" 0.25
        return [b;b2;"_" ] }


let createDagreGraph2Gen maxlen str maxw h
    (g : SimpleGraph<_>) =
    let vs =
        g.Vertices
        |> Seq.map
               (fun v ->
               sprintf "g.setNode(%A, {label:'%s', width:%d, height:%d});" v
                   (fixlen maxlen v) (min maxw ((String.length v) * 8)) h)
        |> Strings.joinToStringWith "\n"

    let es =
        g.Edges
        |> List.mapi
               (fun i (e1, e2, w) ->
               sprintf "g.setEdge(%A, %A, {label: %A}, 'e%d')" e1 e2 (str w) i)
        |> Strings.joinToStringWith "\n" 
    vs, es  

let createDagreGraph2 str maxw h = createDagreGraph2Gen 9 str maxw h


open Prelude.SimpleGraphs
let template = System.IO.File.ReadAllText("C:\Users\cybernetic\Documents\Papers\dagre-template.txt")

let disp isleftright svgid w h (vs,es) =
    let rankdir = if isleftright then """rankdir: "LR",""" else ""
    template
        .Replace("__EDGES_HERE__", es)
        .Replace("__NODES_HERE__",vs)
        .Replace("svgid", svgid)
        .Replace("svgwidth", string w)
        .Replace("svgheight", string h)
        .Replace("__RANK_DIR__", rankdir)

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

let kz =
    [ for (y, ps) in bnn2 do
        let (_, zz, p) =
            List.fold (fun (i, s, l) b ->
                let n = string b :: s
                //if s <> "" then
                //g.InsertVertex s
                //g.InsertVertex n
                //printfn "%A" ps
                let jp =
                    (ps
                     |> List.take (min i ps.Length)
                     |> List.reduce (*)),
                    ps.[(min i ps.Length) - 1]
                //g.InsertEdge(s,n, jp)
                //|> ignore
                i + 1, n, jp :: l) (1, [], []) y

        if p <> [] then yield zz, List.rev  p ]

//normalize to sum to 1

let totw = kz |> List.map snd |> List.map (List.map fst) |> List.map List.last  |> List.sum

let tot = kz |> List.map snd |> List.map List.head |> List.sumBy fst
let len = kz.Length
let kz2 =
    [for (i, (xs,ps)) in List.zip [1..kz.Length]  kz -> 
        let (p,cp),t = List.head ps, List.tail ps
        let len = List.length xs
        List.mapi (fun j x -> if j = len-1 then sprintf "%s(%d)" x i else x) 
        xs, (p/tot,cp)::t]

let xs,ps = List.head kz
let kz3 =
    [for (xs,ps) in kz ->  
        let len = List.length ps
        let ps' = List.mapi (fun j (jp, cp) -> if j = len-1 then jp/totw, cp else cp,jp) ps
        xs, ps']

kz
let g = SimpleGraph<float * float>()
g.InsertVertex "root"
for (a,b) in kz3 do
    let n = List.head a
    let w = List.head b
    if not(g.ContainsEdge "root" n |> Option.defaultValue false) then
        g.InsertEdge("root", n, w) |> ignore 
    runloop2 "," false  "" g (a,  b)
g
createDagreGraph2 (Pair.map (round 2) >> string) 100 50 g
|> disp false "nn1" 1800 6600
|> printf "%s"

|> Windows.Forms.Clipboard.SetText

bnn  |> groupit2 |> List.groupBy (fst >> List.tail)  
|> List.map (fun (x, ps) -> x, List.map snd ps |> List.sum)

|> writeStrRaw

bnn2

let inline reducer isPenultimate (n, ws) =
    if isPenultimate then Seq.reduce (fun (x,y) (u,_) -> x + u, y) ws
    else n

let inline reduceNodeWeights isPenultimate (ws:seq<_>) =
    let parts = Seq.groupBy id ws
    Seq.map (reducer isPenultimate) parts |> ResizeArray

let inline mergeNodes (g:SimpleGraph<_>) =
    Array.iter (g.ModifyNodeEdges reduceNodeWeights) g.Vertices


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

//Uses list
module ListTreeSearch = 
    type SearchSpace<'T> = list<WeightedTree<'T>>
    and WeightedTree<'T> = //Need a tree to do search right
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choices ch = List.map (fun v -> Continued(lazy([Value v]))) ch : SearchSpace<_> 
    let fail () = choices []

    let reflect tree k =  
        let rec make_choices pv = 
            List.map (function 
              | Value x -> Continued(lazy(k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv
            
        make_choices tree  : SearchSpace<_> 
    
    let exactly x = choices [x]  

    type SearchBuilder() =
        member inline d.Bind(space, f) = reflect space f
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly () 

    let search = SearchBuilder()
    let explore (maxdepth: int option) (choices: SearchSpace<'T>) =
        let rec loop depth down susp answers =
            match (down, susp, answers) with
            | (_, [], answers) -> answers
            | (_, (Value v) :: rest, (ans:ResizeArray<_>, susp)) -> 
                loop depth down rest (ans.Add v; ans, susp) 
            | (true, (Continued (Lazy t)) :: rest, answers) ->
                let down' =
                    match maxdepth with
                    | Some x -> depth < x
                    | None -> true
                loop depth true rest
                <| loop (depth + 1) down' t answers
    
            | (down, c :: rest, (ans, susp)) -> loop depth down rest (ans, c :: susp)
     
        let (ans, susp) = loop 0 true choices (ResizeArray(), [])
     
        [ yield! susp
          for v in ans -> Value v ]: SearchSpace<_>   

    let rec simpleTest i = search { 
        let! a = choices [true;false]
        if a then return i
        else return! (simpleTest (i+1))
    } 
    
    simpleTest 0 
    |> explore (Some 14)  
    |> List.filter (function Value _ -> true | _ -> false)

open Hansei.FSharpx.Collections

//uses lazy list
module TTree2 = 
    type SearchSpace<'T> = LazyList<WeightedTree<'T>>
    and WeightedTree<'T> = 
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    //if use value instead of Continued, infinite computations will fail to return/terminate
    let choicesLzy ch = LazyList.map (fun v -> Continued(lazy(LazyList.ofList [Value v]))) ch : SearchSpace<_> 
    //let choicesLzy ch = LazyList.map Value ch : SearchSpace<_> 
    let choices ch = choicesLzy (LazyList.ofList ch) : SearchSpace<_> 
    let exactly x = choicesLzy (LazyList.singleton x) : SearchSpace<_> 
    let fail () = LazyList.empty : SearchSpace<_>  

//return! error
//    visualprobmonad.fsx(815,19): error FS0001: Type mismatch. Expecting a
//    'SearchSpace<'a>'    
//but given a
//    'LazyList<WeightedTree<SearchSpace<'a>>>'    
//The types ''a' and 'SearchSpace<'a>' cannot be unified.
    let reflect2 tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
              | Value x -> Value((k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 

    let reflect tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
            | Value x -> Continued(lazy(k x))
            | Continued(Lazy t) -> Continued(lazy(make_choices t))) pv 
        make_choices tree  : SearchSpace<_>  
    
    type SearchBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly ()  
        member __.Combine(x,y) = LazyList.choice x y
        member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
        member l.Yield x = l.Return x

    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, LazyList.Nil, answers) -> 
                answers
            | (_, LazyList.Cons(Value v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add v; ans, susp)  
            | (true, LazyList.Cons(Continued (Lazy t), rest), answers) ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true  
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc + 1) (depth + 1) down' (t) answers
    
            | (down, LazyList.Cons( c,rest), (ans, susp)) ->  
                loop (hloc+1) depth down rest (ans, ( c) :: susp)
               
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        LazyList.ofList
            [ yield! susp
              for v in ans -> Value v ]: SearchSpace<_> 

//Allow Recompute, is this really needed?        
module TTree5 = 
    type SearchSpace<'T> = LazyList<WeightedTree<'T>>
    and WeightedTree<'T> = 
        | Value of (unit -> 'T)
        | ComputedValue of 'T
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choicesLzy ch = LazyList.map (fun v -> Continued(lazy(LazyList.ofList [Value (fun () -> v)]))) ch : SearchSpace<_> 
    let choices ch = choicesLzy (LazyList.ofList ch) : SearchSpace<_> 
    let exactly x = choicesLzy (LazyList.singleton x) : SearchSpace<_>
    let fail () = LazyList.empty : SearchSpace<_>  
     
//return! error
//    visualprobmonad.fsx(815,19): error FS0001: Type mismatch. Expecting a
//    'SearchSpace<'a>'    
//but given a
//    'LazyList<WeightedTree<SearchSpace<'a>>>'    
//The types ''a' and 'SearchSpace<'a>' cannot be unified.
    let reflect2 tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
              | Value x -> Value((k x)) 
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 

    let reflect tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
            | Value f -> Continued(lazy(k (f())))
            | ComputedValue x -> Continued(lazy(k x))
            | Continued(Lazy t) -> Continued(lazy(make_choices t))) pv 
        make_choices tree  : SearchSpace<_> 
    
    
    type SearchBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly (fun () -> ())
        member __.Combine(x,y) = LazyList.choice x y
        member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
        member l.Yield x = l.Return x

    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, LazyList.Nil, answers) -> 
                answers
            | (_, LazyList.Cons(ComputedValue v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add v; ans, susp)  
            | (_, LazyList.Cons(Value v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add (v()); ans, susp)  
            | (true, LazyList.Cons(Continued (Lazy t), rest), answers) ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true  
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc + 1) (depth + 1) down' (t) answers
    
            | (down, LazyList.Cons( c,rest), (ans, susp)) ->  
                loop (hloc+1) depth down rest (ans, ( c) :: susp)
               
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        LazyList.ofList
            [ yield! susp
              for v in ans -> ComputedValue v ]: SearchSpace<_>   

open TTree2 

//Using seq is too slow.
module TTree4 =
    //if take away continuation, lose flexibility in how easily can represent general
    type SearchSpace<'T> = seq<WeightedTree<'T>>
    and WeightedTree<'T> = 
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choices ch = Seq.map (fun v -> Continued(lazy(seq [Value v]))) ch : SearchSpace<_> 
    //let choices ch = Seq.map Value ch : SearchSpace<_> 
    let fail () = choices Seq.empty

//return! error
//    visualprobmonad.fsx(815,19): error FS0001: Type mismatch. Expecting a
//    'SearchSpace<'a>'    
//but given a
//    'LazyList<WeightedTree<SearchSpace<'a>>>'    
//The types ''a' and 'SearchSpace<'a>' cannot be unified.
    let reflect2 tree k =  
        let rec make_choices pv = 
            Seq.map (function 
              | Value x -> Value((k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 

    let reflect tree k =  
        let rec make_choices pv = 
            Seq.map (function 
            | Value x -> Continued(lazy(k x))
            | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 
    
    let exactly x = choices (Seq.singleton x)
    
    type SearchBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly ()  
        member __.Combine(x,y) = Seq.choice x y
        member __.Delay(f: unit -> seq<_>) = Seq.delay f 
        member l.Yield x = l.Return x

    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, Seq.Nil, answers) -> 
                answers
            | (_, Seq.Cons(Value v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add v; ans, susp)  
            | (true, Seq.Cons(Continued (Lazy t), rest), answers) ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true  
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc + 1) (depth + 1) down' (t) answers
    
            | (down, Seq.Cons( c,rest), (ans, susp)) ->  
                loop (hloc+1) depth down rest (ans, ( c) :: susp)
               
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        seq
            [ yield! susp
              for v in ans -> Value v ]: SearchSpace<_>   

//fiddleing Search algorithm
module TTree3 = 
    type SearchSpace<'T> = list<WeightedTree<'T>>
    and WeightedTree<'T> = //Need a tree to do search right
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choices ch = List.map (fun v -> Continued(lazy([Value v]))) ch : SearchSpace<_> 
    let fail () = choices []

    let reflect tree k =  
        let rec make_choices pv = 
            List.map (function 
              | Value x -> Continued(lazy(k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv
            
        make_choices tree  : SearchSpace<_> 
    
    let exactly x = choices [x]  

    type SearchBuilder() =
        member inline d.Bind(space, f) = reflect space f
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly () 
        member __.Combine(x,y) = List.append x y
        member __.Delay(f: unit -> List<_>) =  f ()
        member l.Yield x = l.Return x
        
    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, [], answers) -> answers
            | (_, (Value v) :: rest, (ans:ResizeArray<_>, susp)) -> 
                loop (hloc+1) depth down rest (ans.Add v; ans, susp) 
            | (true, (Continued (Lazy t)) :: rest, answers) ->
                let down' =
                    match maxdepth with
                    | Some x -> depth < x
                    | None -> true
                
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc+1) (depth + 1) down' (t) answers
                //loop depth true rest
                //<| loop (depth + 1) down' (t) answers
    
            | (down, ( c) :: rest, (ans, susp)) -> loop (hloc+1) depth down rest (ans, ( c) :: susp)
    
        //let (ans, susp) = loop 1.0 0 true choices (Map.empty, [])
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        //Map.fold (fun a v p -> (p, Value v)::a) susp ans : ProbabilitySpace<'T>
        [ yield! susp
          for v in ans -> Value v ]: SearchSpace<_>   

open TTree2

let rec ss i = search { 
    let! a = choices ([true;false])
    if a then return i
    else return! (ss (i+1))
} 

ss 0 |> explore None (Some 10) 
|> LazyList.takeOrMax 10  |> LazyList.toList

open TTree2
//Using list yields to Stack overflow here
let rec ss2 i = search { 
    yield i
    yield -i
    return! ss2 (i+1)
} 

let qz = ss2 0 

qz |> explore (Some 500) (Some 10)  
|> LazyList.takeOrMax 100
|> LazyList.toList

|> List.filter (function Value _ -> true | _ -> false)