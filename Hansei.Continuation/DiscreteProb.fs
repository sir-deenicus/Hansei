module Hansei.DiscreteProb

open Prelude.Common
open Prelude.SimpleDirectedGraphs

module ListProb =  
    let inline aggregate items =
        items
        |> List.groupBy fst
        |> List.map (keepLeft (List.sumBy snd))

    let inline bind (dist: list<'item * 'number>) (k: 'item -> list<'changeditem * 'number>) =
        [ for (x, p) in dist do
            for (y, p2) in k x do
                yield (y, p * p2) ]

    let fail () = []

    let inline always one x = [ x, one ]

    type DistributionBuilder<'T>(one : 'T) =
        member inline d.Bind(dist, f) = bind dist f |> aggregate
        member d.Return v = always one v
        member d.ReturnFrom vs = vs
        member d.Zero() = always one ()

        member inline d.MergeSources(xs: list<'item * 'number>, ys: list<'item * 'number>) =
            [ for (x, p) in xs do
                for (y, p2) in ys do
                    yield ((x,y), p * p2) ] 
            |> aggregate

    let dist one = DistributionBuilder(one)

    let inline bernoulli one p = [ true, p; false, one - p ]

    let inline uniform ofInt one l =
        l |> List.map (fun x -> x, one / ofInt l.Length)

    let inline categorical l = List.normalizeWeights l

    let inline bernoulliChoice one a b p = [ a, p; b, one - p ]
     
    let observe one test =
        dist one { if not test then return! fail () }
         

    module Float =
        let dist = dist 1.
        let uniform l = uniform float 1. l
        let observe = observe 1.
        let bernoulli p = bernoulli 1. p
        let bernoulliChoice a b p = bernoulliChoice 1. a b p
     


module VisualProb = 

    let inline bind (dist : list<'item * 'number list>)
               (k : 'item -> list<'changeditem list * 'number list>) =
        [ for (x, p) in dist do
              for (y, p2) in k x do
                  yield (y, p @ p2 ) ]

    let fail() = []
    let inline bernoulli one p = [true, [p]; false, [one - p]]
    let inline uniform ofInt one l = l |> List.map (fun x -> x, [one/ofInt l.Length])
    let categorical l = l |> List.map (fun (x,p) -> x, [p])
    let inline bernoulliChoice one a b p = [a,[p];b,[one - p]]
    let inline always one x = [x,[one]]

    type DistributionBuilder<'T>(one:'T) =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always one v
        member d.ReturnFrom vs = vs
        member d.Zero () = always one ()
        member inline d.MergeSources(xs: list<'item * 'number list>, ys: list<'item * 'number list>) =
            [ for (x, p) in xs do
                for (y, p2) in ys do
                    yield ((x,y), p @ p2 ) ]  

    let dist one = DistributionBuilder(one)

    let observe one test = dist one {if not test then return! fail()}

    module Float =
        let dist = dist 1.
        let uniform l = uniform float 1. l
        let observe = observe 1.
        let constrain = observe
        let bernoulli p = bernoulli 1. p
        let bernoulliChoice a b p = bernoulliChoice 1. a b p
        let always x = always 1. x
     
    module Helper = 
        let rec editProbGraph trackhistory zero sep soleLast (prev: string) (g: DirectedMultiGraph<_, _>) =
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
        
                editProbGraph trackhistory zero sep soleLast node' g (nodes, ws)
            | [], [] -> ()
            | _ -> failwith "unexpected error"

        
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
                      yield List.rev nodelist, List.rev jointprobs ]

        
    let inline reducer isPenultimate (n, ws) =
        if isPenultimate then  
            Seq.reduce (fun (x,y) (u,_) -> x + u, y) ws
        else n

    let inline reduceNodeWeights isPenultimate (ws:seq<_>) =
        let parts = Seq.groupBy id ws 
        Seq.map (reducer isPenultimate) parts |> ResizeArray

    let inline mergeNodes (g:DirectedMultiGraph<_, _>) =
        Array.iter (g.ModifyNodeEdges reduceNodeWeights) g.Vertices 
      
    let inline buildGraph trackHistory soleLast normalizeJoint zero string probmonad =
        let preprocessed = Helper.processProbMonad string probmonad
        
        let normalize p =
            if normalizeJoint then 
                let totw = 
                    preprocessed 
                    |> List.sumBy (snd >> List.last >> fst) 
                p / totw 
            else p
            
    
        let processed =
            [ for (nodes, ps) in preprocessed ->
                  let len = List.length ps
    
                  let ps' =
                      ps
                      |> List.mapi (fun j (jointprob, condprob) ->
                          if j = len - 1 then normalize jointprob, condprob
                          else condprob, jointprob)
    
                  nodes, ps' ]
        
        let g = DirectedMultiGraph<_, _>()
        
        g.InsertVertex "root" |> ignore
        
        for (nodelist,problist) in processed do
            let n = List.head nodelist
            let w = List.head problist
            if not(g.ContainsEdge ("root", n) |> Option.defaultValue false) then
                g.InsertEdge("root", n, w) |> ignore 
            Helper.editProbGraph trackHistory zero "," soleLast  "" g (nodelist, problist)
        g
    

    type GraphBuilder() =         
        static member inline Render(probmonad, zero, ?TrackHistory, ?SoleLast, ?tostring, ?normalizeJoint) =
            let trackHistory = defaultArg TrackHistory true 
            let soleLast = defaultArg SoleLast false
            let string = defaultArg tostring string
            let normalizejoint = defaultArg normalizeJoint false
            
            buildGraph trackHistory soleLast normalizejoint zero string probmonad 