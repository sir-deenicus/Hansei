module Hansei.Visualization

open Prelude.Common

let fixlen maxlen s =
    if String.length s > maxlen then s.Replace(",", "\\n").Replace("/", "\\n")
    else s

let createDagreGraph w h
    (g : Prelude.SimpleDirectedGraphs.WeightedDirectedGraph<_>) =
    let vs =
        g.Vertices
        |> Seq.map
               (fun v ->
               sprintf "g.setNode(%A, {label:'%s', width:%d, height:%d});" v
                   (fixlen 9 v) w h)
        |> Strings.joinToStringWith "\n"

    let es =
        g.Edges
        |> Array.map
               (fun ((e1, e2), w) ->
               sprintf "g.setEdge(%A, %A, {label: %A})" e1 e2 w)
        |> Strings.joinToStringWith "\n"

    vs, es
    
let createDagreGraph2Gen maxlen str maxw h
    (g : Prelude.SimpleDirectedGraphs.LabeledDirectedGraph<_,_>) =
    let vs =
        g.Vertices
        |> Seq.map
               (fun v ->
               sprintf "g.setNode(%A, {label:'%s', width:%d, height:%d});" v
                   (fixlen maxlen v) (min maxw ((String.length v) * 8)) h)
        |> Strings.joinToStringWith "\n"

    let es =
        g.Edges
        |> Array.map
               (fun ((e1, e2), w) ->
               sprintf "g.setEdge(%A, %A, {label: %A})" e1 e2 (str w))
        |> Strings.joinToStringWith "\n"

    vs, es  
 
let createDagreGraph2 str maxw h = createDagreGraph2Gen maxw str maxw h
 
let aggregateGen join joinlast sep items =
    let len = List.length items - 1
    items
    |> List.fold (fun (acc, i, builtstr) (s,p) ->
            let sep = if builtstr = "" then "" else sep
            let str = if not joinlast && i = len then s else builtstr </join/> sep </join/> s
            (str,p) :: acc, i + 1, str) ([], 0,"")
    |> fst3 |> List.rev

let aggregate l = aggregateGen (+) l
    
let createGraph problist =
    let graph = Prelude.SimpleDirectedGraphs.WeightedDirectedGraph(true)
    graph.InsertVertex "root" |> ignore
    problist
    |> List.map (fun probpath ->
            probpath
            |> List.fold (fun (ps, prevnode) (n, p) ->
                    let ev =
                        if isNumber n then sprintf " (EV = %A)" (float n * p * ps)
                        else ""
                    graph.InsertVertex(n + ev) |> ignore
                    graph.InsertEdge(prevnode, (n + ev), p) |> ignore
                    p * ps, n + ev) (1., "root"))
    |> ignore
    graph

let inline createGraph2 dispjoint str one f problist =
    let graph = Prelude.SimpleDirectedGraphs.LabeledDirectedGraph()
    graph.InsertVertex "root" |> ignore
    problist
    |> List.map (fun probpath ->
            probpath
            |> List.fold (fun (ps, prevnode) (n, p) ->
                    let ev =
                        if isNumber n then
                            sprintf " (EV = %s)" (str (f n * p * ps))
                        else ""
                    graph.InsertVertex(n + ev) |> ignore
                    let prob =
                        if dispjoint then p * ps
                        else p 
                    graph.InsertEdge(prevnode, (n + ev), prob) |> ignore 
                    p * ps, n + ev) (one, "root"))
    |> ignore
    graph

let probDist problist =
    problist
    |> List.map (fun l ->
            let xs, ps = l |> List.unzip
            xs, List.fold ( * ) 1. ps)
    |> List.groupBy fst
    |> List.map (fun (key, ps) -> key, ps |> List.sumBy snd)    

let inline groupAndSum l =
    List.groupBy fst l
    |> List.map (fun (key, ps) -> key, ps |> List.sumBy snd) 

let inline probDist2 one problist =
    problist
    |> List.map (fun l ->
            let xs, ps = l |> List.unzip
            xs, List.fold ( * ) one ps)
    |> List.groupBy fst
    |> List.map (fun (key, ps) -> key, ps |> List.sumBy snd)    

let inline normalize l = 
    let sum = List.sumBy snd l
    List.map (fun (x,p) -> x, p / sum) l

let lblfst2 k path =
    List.mapi (fun i l ->
        let str =
            if i = 0 then ""
            else "_path_" + string (char (k + 97)) + string (i + 1)
        List.mapi (fun j (s, p) ->
            if j = List.length l - 1 then s, p
            else s + str, p) l) path

let lblfst path =
    List.mapi (fun i l ->
        let str =
            if i = 0 then ""
            else "_path_" + string (i + 1)
        List.mapi (fun j (s, p) ->
            if j = List.length l - 1 then s, p
            else s + str, p) l) path

let altpath paths =
    paths
    |> List.groupBy (List.map fst)
    |> List.collect (snd >> lblfst)

let template = System.IO.File.ReadAllText("dagre-template.txt")

let disp isleftright svgid w h (vs,es) =
    let rankdir = if isleftright then """rankdir: "LR",""" else ""
    template
        .Replace("__EDGES_HERE__", es)
        .Replace("__NODES_HERE__",vs)
        .Replace("svgid", svgid)
        .Replace("svgwidth", string w)
        .Replace("svgheight", string h)
        .Replace("__RANK_DIR__", rankdir)
      
type ListMonad() = 
    member __.Bind        (m, f)  = List.collect f m  
    member __.Return      x       = [x]
    member __.ReturnFrom  l       = l : _ list
    member __.Zero        ()      = [] 

let listM = ListMonad()