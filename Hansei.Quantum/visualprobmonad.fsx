#r "netstandard"
#I @"C:\Users\cybernetic\.nuget\packages\"
#r @"prelude\1.0.19\lib\net47\Prelude.dll"
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
            

let inline bindy maxdepth (dist : list<'item * 'number>) (k : 'item -> list<'changeditem * 'number>) = 
        [
        
            for (x,p) in dist do   
                for (y,p2) in k(x) do  
                    yield (y, p * p2) ]  


let fail() = {Categorical = []; Depth = 0; }
let bernoulliv p = {Categorical = [true,[p];false, [1.- p]]; Depth = 0; }
let uniformv l = {Categorical = l |> List.map (fun x -> x, [1./float l.Length]) ; Depth = 0}
let bernoulliChoicev a b p = {Categorical = [a,[p];b, [1.- p]]; Depth = 0; }
let alwaysv x = {Categorical = [x,[1.]]; Depth = 0;}
     
type DistributionBuilderV(?maxdepth) =  
    member inline d.Bind(dist, f) = bindx maxdepth dist f 
    member d.Return v = alwaysv v
    member d.ReturnFrom vs = vs
    member d.Zero () = alwaysv ()

let distvb maxdepth = DistributionBuilderV(maxdepth)
let distv = DistributionBuilderV()

let observe test = distv {if not test then return! fail()}
  
let bn =
    distv 
        {   let! b = bernoulliChoicev "H" "T" 0.5 
            do! observe (b = "H")
            let! b2 = bernoulliChoicev "H" "T" 0.5  
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
            let! b3 = if b2 = "H" then bernoulliChoicev "H" "T" 0.25 else bernoulliChoicev "H" "T" 0.5    
            return [b; b2;b3]
            }

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

let bn =
    distv 
        {   let! b = bernoulliChoicev "H" "T" 0.5  
            let! b2 = bernoulliChoicev "H" "T" 0.5    
            return [b;b2 ] }
            



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


let tensorProduct alphabet n =
    let rec iterate product i =
        seq {
            for symbol in alphabet do
                if i = 0 then yield List.rev product
                else yield! iterate (symbol::product) (i-1)
        }
    iterate [] n

let generatePossibilities alphabet n =
    tensorProduct alphabet n
    |> Seq.removeDuplicates

let permutations items takeN =
    generatePossibilities items takeN
    |> Seq.map List.removeDuplicates
    |> Seq.filter (fun l -> l.Length = takeN)

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