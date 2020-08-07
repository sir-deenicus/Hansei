#r "netstandard"
#I @"C:\Users\cybernetic\.nuget\packages\"
#r @"C:\Users\cybernetic\source\repos\Prelude\Prelude\bin\Release\net47\Prelude.dll"
#r @"mathnet.numerics\4.9.0\lib\net40\MathNet.Numerics.dll"
#I @"C:\Users\cybernetic\Documents\Papers"  
//#load @"disputil.fsx"
#load @"literate.fsx"  
#r @"LiterateUtils\LiterateUtils\bin\Release\net47\LiterateUtils.dll"  

open Parseutils
open Prelude.Common
open System
   
type SimpleGraph<'a>() =
    let edges = Dict<string, Dict<string, 'a ResizeArray>>()

    let numberOfChildren n =
        match edges.tryFind n with
        | Some a -> a.Count
        | None -> 0

    member g.Vertices =
        g.EdgeData
        |> Seq.map keyValueToKey
        |> Seq.toArray

    member g.Edges =
        [ for (KeyValue(e, e2s)) in edges do
            for (KeyValue(e2, ws)) in e2s do
                for w in ws do
                    yield (e, e2, w) ]

    member g.InsertVertex(s: string) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained

    member g.Remove(v) =
        match (edges.tryFind v) with
        | None -> false
        | Some elist ->
            for KeyValue(_, es) in edges do
                if es.ContainsKey v then
                    es.[v].Clear()
                    es.Remove v |> ignore
            elist.Clear()
            edges.Remove v

    member g.EdgeData = edges

    member g.InsertEdge(v0, v1, w) =
        maybe {
            let! edge = edges.tryFind v0
            match edge.tryFind v1 with
            | None ->
                g.InsertVertex v1 |> ignore
                edge.Add(v1, ResizeArray([ w ]))
                return true
            | Some n ->
                n.Add w
                return true
        }
    
    ///f informs whether the node is penultimate or not.
    member g.ModifyNodeEdges f n =
        match edges.tryFind n with
        | Some nodes ->
            let keys = Seq.toArray nodes.Keys
            let ispenultimate =
                keys
                |> Array.sumBy numberOfChildren = 0
            for k in keys do
                let currnodes = nodes.[k].ToArray()
                nodes.[k].Clear()
                nodes.[k] <- f ispenultimate currnodes
        | None -> ()

    member g.ContainsVertex v = edges.ContainsKey v

    member g.ContainsEdge v1 v2 = maybe {
                                      let! edges = edges.tryFind v1
                                      return (edges.ContainsKey v2) }
    member g.GetEdges v = maybe {
                              let! elist = edges.tryFind v
                              return elist |> keyValueSeqtoPairArray }

let g = SimpleGraph<string>()

 
g.InsertVertex "A"
g.InsertVertex "B"
g.InsertVertex "C"

g.InsertEdge ("A","B","3")
g.InsertEdge ("B","C","")

g.Vertices

g.Remove "B"

g.Edges

type VisualDistribution<'item, 'number when 'item: comparison> =
    { Categorical : ('item * 'number list) list 
      Depth : int}

let inline bindx maxdepth (dist : VisualDistribution<'item,'number>) (k : 'item -> VisualDistribution<'changeditem list,'number>) =
    //let g = dist.G
    let maxdepthval = Option.defaultValue Int32.MaxValue maxdepth
    let mutable depth = -1 
    //g.InsertVertex "root"
    let l =
        [
        
        for (x,p) in dist.Categorical do
            //printfn "in0: %A" (depth) 
            
            if depth < maxdepthval then 
                
                depth <- depth + 1//dn.Depth
                let dn = k(x)
                
                depth <- max depth dn.Depth
                //depth <- max depth dn.Depth
                //printfn "in: %A" (depth)
                //g.InsertEdge("root", string x, (List.reduce (*) p,List.head p)) 
                for (y,p2) in dn.Categorical do 
                    //printfn "%A" (depth, y)
                    //let zz = List.fold (fun (i,s) b -> 
                    //            let n = s + string b
                    //            if s <> "" then
                    //                g.InsertVertex s
                    //                g.InsertVertex n
                    //                let ps = p @ p2
                    //                //printfn "%A" ps
                    //                let jp = (ps |> List.take (min i ps.Length) |> List.reduce (*)), ps.[(min i ps.Length) - 1]
                    //                g.InsertEdge(s,n, jp)
                    //                |> ignore
                    //            i+1, n) (1,"") y
                    //g.InsertVertex (string y) |> ignore
                    //g.InsertEdge (string x,string y, string(p @ p2 |> List.reduce (*))) |> ignore 
                    yield (y, p @ p2) ] 
                    
    //printfn "%A" ps
    {  
          Categorical = l 
          Depth = depth}

(**Non-determism, Combinatorics, Learning, Probability and Quantum Mechanics*)
(*Coin flips*)
let generatePossibilities alphabet n = 
    let rec iterate product i = seq { 
        if i = 0 then yield List.rev product
        else
            for symbol in alphabet do
                yield! iterate (symbol::product) (i-1)
        }
    iterate [] n

[|for c1 in ["H";"T"] do for c2 in ["H";"T"] -> c1,c2|]

let powerset (items : _ []) =
    let n = items.Length
    seq {
        for bitpattern in generatePossibilities [ false; true ] n do
            yield [| for i in 0..n - 1 do
                        if bitpattern.[i] then yield items.[i] |]
    }

let powerset2 n (items : _ seq) =
    seq {
        for bitpattern in generatePossibilities [ false; true ] n do
            yield [| for i in 0..n - 1 do
                        if bitpattern.[i] then
                            yield Seq.head (Seq.skip i items) |]
    }

powerset [|"A";"B";"C";"D";"E"|] |> powerset2 (int(2. ** 5.)) |> Seq.take 100 |> Seq.toArray
2. ** (2. ** 5.)
Seq.initInfinite id |> Seq.length

Seq.skip 1 [|1..10|]
powerset [|1..15|] |> Seq.take 100 |> Seq.toArray
generatePossibilities [false;true] 20 |> Seq.take 200 |> Seq.toArray

2. ** 30.

generatePossibilities ["H";"T"] 3 |> Seq.toArray// |> Array.length
generatePossibilities [1..6] 2 |> Seq.toArray |> Array.map List.sum
//let generatePossibilities alphabet n =
//    tensorProduct alphabet n
//    |> Seq.removeDuplicates
 

let permutations items takeN =
    generatePossibilities items takeN
    |> Seq.map List.removeDuplicates
    |> Seq.filter (fun l -> l.Length = takeN)

permutations ["A";"B"; "C";"D"] 2// |> Seq.length

let combinations items takeN =
    permutations items takeN
    |> Seq.map List.sort
    |> Seq.removeDuplicates

let combinationsWithRepeats items takeN =
    generatePossibilities items takeN
    |> Seq.map List.sort
    |> Seq.removeDuplicates 


generatePossibilities ["A";"B";"B";"B"] 2 = generatePossibilities ["A";"A";"A";"B"] 2

tensorProduct ["A";"B";"B";"B"] 2


combinations ['A'..'Z'] 5 |> Seq.take 30 |> Seq.toArray


combinationsWithRepeats ["A";"B"] 2

combinationsWithRepeats ["b"; "c"; "l"; "s"; "v";] 3 
permutations ["A";"B";"C"; "D"] 2 //|> List.length
combinations ["A";"B";"A";"B"] 2 //|> List.length

(**A simple wrapper around the list monad can do some book-keeping of probabilities for us by silently tracking/propagating joint probabilities*)
module SimplestWrapper =
    let inline bind (dist : list<'item * 'number>)
               (k : 'item -> list<'changeditem * 'number>) =
        [ for (x, p) in dist do
              for (y, p2) in k x do
                  yield (y, p * p2) ]
                   
    let fail() = []
    let bernoulli p = [true, p; false, 1.- p] 
    let uniform l = l |> List.map (fun x -> x, 1./float l.Length) 
    let bernoulliChoice a b p = [a,p;b,1.- p]
    let always x = [x,1.] 
     
    type DistributionBuilder() =  
        member inline d.Bind(dist, f) = bind dist f 
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        
    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open SimplestWrapper  

dist {  
    let! b = bernoulliChoice "H" "T" 0.5  
    let! b2 = bernoulliChoice "H" "T" 0.5    
    return (b,b2) 
}
         
dist {
    let! b = bernoulliChoice "H" "T" 0.5
    let! b2 = bernoulliChoice "H" "T" 0.5
    let! b3 = if b2 = "H" then bernoulliChoice "H" "T" 0.25
                else bernoulliChoice "H" "T" 0.5
    return (b, b2, b3)
}
(**The relationship between control structures for nondetermiism and inference. Monads show up a lot when you want to manage complex control flow using higher order functionas and a simple pipelining design. Nondeterminism is about computations which have multiple possiblities and branching paths. Hence the relation with *)
(** But what if we want to sample from a very large space? Laziness will be helpful. *)
dist {
    let! a = uniform [1..10000]
    let! b = uniform [1..10000]
    return (a + b)
} 
  

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

let rec infiniteFlips p = dist {
        let! b = bernoulli p
        return b
        return! infiniteFlips p
    }

infiniteFlips 0.001 |> Seq.take 10

dist {
    let! b = infiniteFlips 0.5 
    let! j = geom 1 0.6
    do! observe (not b)
    return (b, j)
} |> Seq.take 10      

seq {   for i in 1..10 do
            for j in Seq.initInfinite id do
              if i > 5 then yield (i,j) }
|> Seq.take 1

#r @"bin\Debug\net47\Hansei.Core.dll"
open Hansei.Backtracking

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
        member d.Combine(a,b) = choice a b
        member __.Delay(f: unit -> LazyStream<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open BtWrapper

let rec infiniteFlips p = dist {
        let! b = bernoulli p
        return b
        return! infiniteFlips p
    }

infiniteFlips 0.001 |> run (Some 100) |> Seq.take 50 |> Seq.toArray

let bn =
    dist
        {   let! b = bernoulliChoice "H" "T" 0.5 
            do! observe (b = "H")
            let! b2 = bernoulliChoice "H" "T" 0.5  
            let! b3 = if b2 = "H" then bernoulliChoicev "H" "T" 0.25 else bernoulliChoicev "H" "T" 0.5    
            let! b4 = bernoulliChoicev "H" "T" 0.75
            return [b; "(b=H)"; b2;"_";b4] }
let bn =
    distv 
        {   let! b = bernoulliChoicev "H" "T" 0.5 
            if b = "H" then
                let! b2 = bernoulliChoicev "H" "T" 0.5  
                let! b3 = if b2 = "H" then bernoulliChoicev "H" "T" 0.25 else bernoulliChoicev "H" "T" 0.5    
                let! b4 = bernoulliChoicev "H" "T" 0.75
                return [b; b2;"_";b4]
            else return []}

let bn =
    distv 
        {   let! b = bernoulliChoicev "H" "T" 0.5  
            let! b2 = bernoulliChoicev "H" "T" 0.5   
            return [ b;b2;string( b=b2 && b = "H")] }
let bn =
    distv 
        {   let! b = bernoulliChoicev "H" "T" 0.5  
            let! b2 = bernoulliChoicev "H" "T" 0.5   
            do! observe (not ( b=b2 && b = "H"))
            return [b;b2;"b=b2 && b = H"] }
             



let bn = distv {
    let! child1 = bernoulliChoicev "boy" "girl" 0.5  
    let! child2 = bernoulliChoicev "boy" "girl" 0.5//ProbList.bernoulliChoice p ("boy2", "girl2") 
    let! seefirst = uniformv [child1; child2]
    do! observe (seefirst.Contains "boy")
    return [child1;child2; "see " + seefirst]
}


let hasHeads count = List.filter ((=) "H") >> List.length >> ((<=) count)

let rec atleast_n_in_m_flips count flips n = distv {
    if n = 0 then
     if hasHeads count flips then return List.rev(string true :: flips)
     else return List.rev(string false :: flips)
    else let! flip = bernoulliChoicev "H" "T" 0.5  
         return! atleast_n_in_m_flips count (flip::flips) (n-1) }

let bn = atleast_n_in_m_flips 2 [] 3

let rec runloop sep soleLast (s:string) (g:SimpleGraph<float * float>) = function  
    | [a1], [b1] when soleLast ->     
        if s <> "" then
            g.InsertVertex s
            g.InsertEdge(s, a1, b1) |> ignore 
    |  a1::ats, b1::bts -> 
        let n = if s = "" then a1 else (s + sep + a1)     
        if s <> "" then
            g.InsertVertex s
            g.InsertEdge(s, n, b1) |> ignore
        //(((s + a1),b1)::l)
        runloop sep soleLast n g (ats,bts)
    | [], [] -> ()
    | _ -> failwith "unexpected error in runloop"
let fixlen maxlen s =
    if String.length s > maxlen then s.Replace(",", "\\n").Replace("/", "\\n")
    else s 



let template = System.IO.File.ReadAllText("C:\Users\cybernetic\Documents\Papers\dagre-template.txt")


let bn =
    distv 
        {   let! b = bernoulliChoicev "H" "T" 0.5  
            let! b2 = bernoulliChoicev "H" "T" 0.5 
            let! b3 = bernoulliChoicev "H" "T" 0.25 
            return [b;b2;"_" ] }   
let kz = 
    [for (y,ps) in bn.Categorical do  
                        let (_,zz,p) = 
                            List.fold (fun (i,s,l) b -> 
                                    let n = string b::s
                                    //if s <> "" then
                                        //g.InsertVertex s
                                        //g.InsertVertex n
                                        //printfn "%A" ps
                                    let jp = (ps |> List.take (min i ps.Length) |> List.reduce (*)), ps.[(min i ps.Length) - 1]
                                        //g.InsertEdge(s,n, jp)
                                        //|> ignore
                                    i+1, n,jp::l) (1,[],[]) y
                        if p <> [] then yield List.rev zz,p
                        //g.InsertVertex (string y) |> ignore
                        //g.InsertEdge (string x,string y, string(p @ p2 |> List.reduce (*))) |> ignore 

    ]


let g = SimpleGraph<float * float>()
g.InsertVertex "root"
for (a,b) in kz do 
    let n = List.head a
    let w = List.last b
    if not(g.ContainsEdge "root" n |> Option.defaultValue false) then
        g.InsertEdge("root", n, w) |> ignore
    runloop "," true "" g (a,List.rev b)   


createDagreGraph2 string 90 50 g
|> disp false "nn1" 1200 600
|> writeStrRaw


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