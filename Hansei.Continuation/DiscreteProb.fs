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
        member inline d.Bind(dist, f) = f |> bind dist |> aggregate
        member d.Return v = always one v
        member d.Yield v = always one v
        member d.ReturnFrom vs = vs
        member d.Zero() = []
        member d.Combine(l1,l2) = List.append l1 l2
        member d.Delay f = f ()

        member inline d.MergeSources(xs: list<'item * 'number>, ys: list<'item * 'number>) =
            [ for (x, p) in xs do
                for (y, p2) in ys do
                    yield ((x,y), p * p2) ] 
            |> aggregate
     
    let dist one = DistributionBuilder(one) 
     
    let observe one test =
        dist one { if not test then return! fail () else return ()}
         
    let inline map f l = 
        List.map (fun (x, p) -> f x, p) l 
        |> List.groupBy fst 
        |> List.map (fun (x, xs) -> x, List.sumBy snd xs)

    let inline bernoulli one p = [ true, p; false, one - p ]

    let inline uniform ofInt one l =
        l |> List.map (fun x -> x, one / ofInt l.Length)

    let inline categorical l = List.normalizeWeights l

    let inline bernoulliChoice one a b p = [ a, p; b, one - p ]
      

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
                   
    let inline bernoulli one p = [true, [p]; false, [one - p]]
    let inline uniform ofInt one l = l |> List.map (fun x -> x, [one/ofInt l.Length])
    let inline bernoulliChoice one a b p = [a,[p];b,[one - p]]
    let inline always one x = [x,[one]]
    let categorical l = l |> List.map (fun (x,p) -> x, [p])
    let fail() = []

    type DistributionBuilder<'T>(one:'T) =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always one v
        member d.ReturnFrom vs = vs
        member d.Zero () = []
        member d.Combine(l1,l2) = List.append l1 l2
        member d.Delay f = f ()
        member inline d.MergeSources(xs: list<'item * 'number list>, ys: list<'item * 'number list>) =
            [ for (x, p) in xs do
                for (y, p2) in ys do
                    yield ((x,y), p @ p2 ) ]  

    let dist one = DistributionBuilder(one)

    let observe one test = dist one {if not test then return! fail() else return ()}

    module Float =
        let dist = dist 1.
        let uniform l = uniform float 1. l
        let observe = observe 1.
        let constrain = observe
        let bernoulli p = bernoulli 1. p
        let bernoulliChoice a b p = bernoulliChoice 1. a b p
        let always x = always 1. x
      
    let inline collapseProbs (probs: ('a list * 'b list) list) =
        [ for (items, problist) in probs do
                [ List.last items ], [ List.reduce (*) problist ] ]

    let inline flattenProbs (probs: ('a list * 'b list) list) =
        [ for (items, problist) in probs do
                List.last items, List.reduce (*) problist ]
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, List.sumBy snd v)   

    
    ///When an observation is made, possibilities are collapsed and a branch is selected with p=1.
    ///Sometimes we want to erase the earlier branches as this becomes confusing to visualize in general.
    ///Assuming no singleton distribution is sampled, there will only be conditional observations generating p=1 in the list.
    ///This function removes sections that are before the last p=1.
    let adjustForConditioning l =
        let rec findOne targetn n i = function
            | _ when targetn = 0 -> -1
            | [] -> -1
            | p::ps' -> 
                if p = 1. && n = targetn then i 
                elif p = 1. then findOne targetn (n+1) (i-1) ps'
                else findOne targetn n (i-1) ps'
        [for (xs:list<'a>, ps) in l do  
            //Calculate the target index to reached by counting the number of 1s
            //The purpose of targetn is to determine how many elements to skip until the final 1 is reached.
            let targetn = (List.filter ((=) 1.) ps |> List.length) - 1 
            let i = findOne targetn 0 (List.length ps - 1) (List.rev ps) 
            if i = -1 then
                xs, ps
            else
                xs.[i..], ps.[i+targetn..]]  
   
    let inline buildGraph keepJointProbOnSameSide trackHistory soleLast normalizeJoint zero string probmonad =
        //This code takes a list consisting of a list of items paired with a list of weights (conditional probabilities)
        //and returns a string list, based on the item list and a list of joint and conditional probabilities
        let inline processProbMonad (str: 'a -> string) probm =
            [ for (nodes, ps) in probm do
                let (_, nodelist, jointAndCondprobs) =
                    ((1, [], []), nodes)
                    ||> List.fold (fun (i, nodelist, plist) node ->
                        let n = str node :: nodelist
                    
                        //The joint probability is the product of the conditional probabilities
                        //To calculate the joint probability, we take the product of the first i conditional probabilities which 
                        //are the ones relevant to the current node
                        let jointAndCondProb =
                            (ps
                            |> List.take (min i ps.Length)
                            |> List.reduce (*)),
                            ps.[(min i ps.Length) - 1]
        
                        i + 1, n, jointAndCondProb :: plist)
        
                if jointAndCondprobs <> [] then
                    yield List.rev nodelist, List.rev jointAndCondprobs ]

        // This function takes a directed multi-graph, a boolean indicating whether to track the history of nodes, a value representing the zero probability, a separator used to separate nodes in the history, a boolean indicating whether the last node should be treated as a sole node, a string representing the previous node, a list of nodes, and a list of weights.
        // It recursively edits the graph by inserting vertices and edges based on the nodes and weights.
        let rec editProbGraph trackhistory zero sep soleLast (prev: string) (g: DirectedMultiGraph<_, _>) =
            function
            | [ node ], [ w ] when soleLast -> // If the last node should be treated as a sole node, no need to the node history trace
    
                if prev <> "" then
                    g.InsertNode prev |> ignore
    
                    if fst w <> zero then
                        g.InsertEdge(prev, node, w) |> ignore
            | node :: nodes, w :: ws ->
                // Otherwise, create a new node by concatenating the previous node and the current node with the separator, and insert a vertex for the previous node and an edge for the current node and weight.
                let node' =
                    if not trackhistory || prev = "" then node
                    else (prev + sep + node)
    
                if prev <> "" then
                    g.InsertNode prev |> ignore
    
                    if fst w <> zero then
                        g.InsertEdge(prev, node', w) |> ignore
    
                editProbGraph trackhistory zero sep soleLast node' g (nodes, ws)
            | [], [] -> ()
            | _ -> failwith "unexpected error"

        let preprocessed = processProbMonad string probmonad
         
        let processed = 
            //At the terminal depth the joint probability over nodes at this depth can optionally be normalized by the sum of the weights of the nodes at this depth.
            let normalize p =
                if normalizeJoint then 
                    let totw = 
                        preprocessed 
                        |> List.sumBy (snd >> List.last >> fst) 
                    p / totw 
                else p
            
            [ for (nodes, ps) in preprocessed ->
                  let len = List.length ps
    
                  let ps' =
                      ps
                      |> List.mapi (fun j (jointprob, condprob) ->
                          if j = len - 1 then normalize jointprob, condprob
                          else 
                            if keepJointProbOnSameSide then jointprob, condprob
                            else condprob, jointprob)
    
                  nodes, ps' ]
        
        let g = DirectedMultiGraph<_, _>()
        
        g.InsertNode "root" |> ignore
        
        for (nodelist,problist) in processed do
            let n = List.head nodelist
            let w = List.head problist
            if not(g.ContainsEdge ("root", n) |> Option.defaultValue false) then
                g.InsertEdge("root", n, w) |> ignore 
            editProbGraph trackHistory zero "," soleLast  "" g (nodelist, problist)
        g
    

    type GraphBuilder() =   
        
        static member inline Render(probmonad, zero, ?MergeNodes, ?TrackHistory, ?SoleLast, ?tostring, ?normalizeJoint, ?keepJointProbOnSameSide) =
            
            let inline reducer isPenultimate (n, ws) =
                if isPenultimate then  
                    Seq.reduce (fun (x,y) (u,_) -> x + u, y) ws
                else n

            let inline reduceNodeWeights isPenultimate (ws:seq<_>) =
                let parts = Seq.groupBy id ws 
                Seq.map (reducer isPenultimate) parts |> ResizeArray

            let inline mergeNodes (g:DirectedMultiGraph<_, _>) =
                Array.iter (g.ModifyNodeEdges reduceNodeWeights) g.Vertices; g
      
            let trackHistory = defaultArg TrackHistory true 
            let soleLast = defaultArg SoleLast false
            let string = defaultArg tostring string
            let normalizejoint = defaultArg normalizeJoint false
            let keepJointProbOnSameSide = defaultArg keepJointProbOnSameSide false
            
            let g = buildGraph keepJointProbOnSameSide trackHistory soleLast normalizejoint zero string probmonad 
            match MergeNodes with Some true -> mergeNodes g | _ -> g