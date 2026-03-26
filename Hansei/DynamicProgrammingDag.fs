module Hansei.DynamicProgrammingDag

open System
open System.Collections.Generic

type GraphShape =
    | LinearChain
    | RootedTree

type FiniteDagNode<'Obs, 'State when 'State: comparison> =
    {
        Id: int
        ParentId: int option
        States: 'State[]
        InitialWeight: ('State -> float) option
        TransitionWeight: ('State -> 'State -> float) option
        Observation: ('Obs * ('State -> 'Obs -> float)) option
    }

type FiniteDag<'Obs, 'State, 'Label when 'State: comparison and 'Label: comparison> =
    {
        Nodes: FiniteDagNode<'Obs, 'State>[]
        RootId: int
        QueryId: int
        Label: 'State -> 'Label
        Shape: GraphShape
    }

type GraphInferenceEngine =
    | SumProduct
    | BeliefPropagation
    | MaxProduct

type ExactPosteriorResult<'Label when 'Label: comparison> =
    {
        Posterior: Map<'Label, float>
        Evidence: float
        LogEvidence: float
    }

type MaxPosteriorResult<'Label> =
    {
        BestLabel: 'Label
        Score: float
        LogScore: float
    }

type GraphInferenceResult<'Label when 'Label: comparison> =
    | ExactPosterior of ExactPosteriorResult<'Label>
    | MaxPosterior of MaxPosteriorResult<'Label>

type PreparedDag<'Label when 'Label: comparison> internal (inferCore: GraphInferenceEngine -> GraphInferenceResult<'Label>) =
    member _.Infer(engine) = inferCore engine

    member _.ExactPosterior() =
        match inferCore SumProduct with
        | ExactPosterior result -> result
        | MaxPosterior _ -> failwith "Expected an exact posterior result."

    member _.BeliefPropagation() =
        match inferCore BeliefPropagation with
        | ExactPosterior result -> result
        | MaxPosterior _ -> failwith "Expected an exact posterior result."

    member _.MaxPosterior() =
        match inferCore MaxProduct with
        | MaxPosterior result -> result
        | ExactPosterior _ -> failwith "Expected a max-posterior result."

    member this.Evaluate() =
        this.ExactPosterior().Posterior

let internal negativeInfinity = Double.NegativeInfinity

let internal logWeight weight =
    if weight <= 0.0 then negativeInfinity else log weight

let internal logSumExp (values: float[]) =
    if values.Length = 0 then
        negativeInfinity
    else
        let maxValue = values |> Array.max

        if Double.IsNegativeInfinity maxValue then
            negativeInfinity
        else
            maxValue + log (values |> Array.sumBy (fun value -> exp (value - maxValue)))

let internal addLogWeight key logValue acc =
    Map.change key (fun existing ->
        match existing with
        | None -> Some logValue
        | Some current -> Some (logSumExp [| current; logValue |])) acc

let internal exactPosteriorResultFromLog entries =
    let rawLog =
        entries
        |> Seq.fold (fun acc (label, logValue) -> addLogWeight label logValue acc) Map.empty

    let logEvidence = rawLog |> Seq.map (fun kv -> kv.Value) |> Seq.toArray |> logSumExp

    let posterior =
        rawLog
        |> Map.map (fun _ logValue ->
            if Double.IsNegativeInfinity logValue || Double.IsNegativeInfinity logEvidence then 0.0
            else exp (logValue - logEvidence))

    let evidence =
        if Double.IsNegativeInfinity logEvidence then
            0.0
        else
            exp logEvidence

    {
        Posterior = posterior
        Evidence = evidence
        LogEvidence = logEvidence
    }

let internal maxPosteriorResultFromLog entries =
    let rawLog =
        entries
        |> Seq.fold (fun acc (label, logValue) ->
            Map.change label (fun existing ->
                match existing with
                | None -> Some logValue
                | Some current -> Some (max current logValue)) acc) Map.empty

    let bestLabel, bestLogScore =
        rawLog
        |> Map.toList
        |> List.maxBy snd

    let score =
        if Double.IsNegativeInfinity bestLogScore then
            0.0
        else
            exp bestLogScore

    {
        BestLabel = bestLabel
        Score = score
        LogScore = bestLogScore
    }

type ChainStateToken<'State> =
    | ChainStateToken of int

type ChainQuery<'State, 'Label> =
    | ChainQuery of ChainStateToken<'State> * ('State -> 'Label)

type FiniteNode<'State> =
    {
        Id: int
        States: 'State[]
    }

type ChainAction<'Obs, 'State> =
    | ObserveAction of int * ('State -> 'Obs -> float) * 'Obs
    | TransitionAction of int * int * ('State -> 'State -> float)

type ChainProgramState<'Obs, 'State> =
    {
        Hidden: FiniteNode<'State> option
        InitialWeight: ('State -> float) option
        CurrentToken: int option
        NextToken: int
        Actions: ResizeArray<ChainAction<'Obs, 'State>>
    }

type ChainProgram<'Obs, 'State, 'T> =
    | ChainProgram of (ChainProgramState<'Obs, 'State> -> ChainProgramState<'Obs, 'State> * 'T)

type ChainProgramBuilder() =
    member _.Bind(ChainProgram run, binder) =
        ChainProgram(fun state ->
            let nextState, value = run state
            let (ChainProgram nextRun) = binder value
            nextRun nextState)

    member _.Return value = ChainProgram(fun state -> state, value)

    member _.ReturnFrom program = program

    member _.Zero() = ChainProgram(fun state -> state, ())

    member _.Delay(generator) =
        ChainProgram(fun state ->
            let (ChainProgram run) = generator ()
            run state)

let chain = ChainProgramBuilder()

type TreeToken<'State> =
    | TreeToken of int * 'State[]

type TreeQuery<'State, 'Label> =
    | TreeQuery of TreeToken<'State> * ('State -> 'Label)

type TreeObservation<'Obs, 'State> =
    {
        Observation: 'Obs
        ObservationWeight: 'State -> 'Obs -> float
    }

type TreeNodeSpec<'Obs, 'State when 'State: comparison> =
    {
        Id: int
        ParentId: int option
        States: 'State[]
        InitialWeight: ('State -> float) option
        TransitionWeight: ('State -> 'State -> float) option
        Observation: TreeObservation<'Obs, 'State> option
    }

type TreeProgramState<'Obs, 'State when 'State: comparison> =
    {
        Nodes: Map<int, TreeNodeSpec<'Obs, 'State>>
        RootId: int option
        NextToken: int
    }

type TreeProgram<'Obs, 'State, 'T when 'State: comparison> =
    | TreeProgram of (TreeProgramState<'Obs, 'State> -> TreeProgramState<'Obs, 'State> * 'T)

type TreeProgramBuilder() =
    member _.Bind(TreeProgram run, binder) =
        TreeProgram(fun state ->
            let nextState, value = run state
            let (TreeProgram nextRun) = binder value
            nextRun nextState)

    member _.Return value = TreeProgram(fun state -> state, value)

    member _.ReturnFrom program = program

    member _.Zero() = TreeProgram(fun state -> state, ())

    member _.Delay(generator) =
        TreeProgram(fun state ->
            let (TreeProgram run) = generator ()
            run state)

let dag = TreeProgramBuilder()

let internal finiteState _name states initialWeight =
    ChainProgram(fun state ->
        match state.Hidden with
        | Some _ -> failwith "Only one finite hidden chain is supported in the structured chain builder."
        | None ->
            let tokenId = state.NextToken

            {
                state with
                    Hidden = Some { Id = tokenId; States = states }
                    InitialWeight = Some initialWeight
                    CurrentToken = Some tokenId
                    NextToken = tokenId + 1
            },
            ChainStateToken tokenId)

let internal observeFinite (ChainStateToken tokenId) observationWeight observation =
    ChainProgram(fun state ->
        match state.CurrentToken with
        | Some currentToken when currentToken = tokenId ->
            state.Actions.Add (ObserveAction(tokenId, observationWeight, observation))
            state, ()
        | _ -> failwith "Chain.observe must apply to the current state token.")

let internal transitionFinite (ChainStateToken sourceToken) transitionWeight =
    ChainProgram(fun state ->
        match state.CurrentToken with
        | Some currentToken when currentToken = sourceToken ->
            let nextToken = state.NextToken
            state.Actions.Add (TransitionAction(sourceToken, nextToken, transitionWeight))

            {
                state with
                    CurrentToken = Some nextToken
                    NextToken = nextToken + 1
            },
            ChainStateToken nextToken
        | _ -> failwith "Chain.transition must apply to the current state token.")

let internal finishFinite token label =
    ChainProgram(fun state -> state, ChainQuery(token, label))

let internal rootVar states initialWeight =
    TreeProgram(fun state ->
        match state.RootId with
        | Some _ -> failwith "Only one root is supported in the DAG builder."
        | None ->
            let tokenId = state.NextToken

            let node =
                {
                    Id = tokenId
                    ParentId = None
                    States = states
                    InitialWeight = Some initialWeight
                    TransitionWeight = None
                    Observation = None
                }

            {
                state with
                    Nodes = Map.add tokenId node state.Nodes
                    RootId = Some tokenId
                    NextToken = tokenId + 1
            },
            TreeToken(tokenId, states))

let internal childVarWithStates (TreeToken(parentId, _)) states transitionWeight =
    TreeProgram(fun state ->
        if not (Map.containsKey parentId state.Nodes) then
            failwith "Dag.childWithStates references an unknown parent token."
        else
            let tokenId = state.NextToken

            let node =
                {
                    Id = tokenId
                    ParentId = Some parentId
                    States = states
                    InitialWeight = None
                    TransitionWeight = Some transitionWeight
                    Observation = None
                }

            {
                state with
                    Nodes = Map.add tokenId node state.Nodes
                    NextToken = tokenId + 1
            },
            TreeToken(tokenId, states))

let internal childVar (TreeToken(_, parentStates) as parent) transitionWeight =
    childVarWithStates parent parentStates transitionWeight

let internal observeVar (TreeToken(tokenId, _)) observationWeight observation =
    TreeProgram(fun state ->
        let node =
            state.Nodes
            |> Map.tryFind tokenId
            |> Option.defaultWith (fun () -> failwith "Dag.observe references an unknown token.")

        let updatedNode =
            {
                node with
                    Observation = Some { Observation = observation; ObservationWeight = observationWeight }
            }

        { state with Nodes = Map.add tokenId updatedNode state.Nodes }, ())

let internal finishTree token label =
    TreeProgram(fun state -> state, TreeQuery(token, label))

let internal compileChainProgram (program: ChainProgram<'Obs, 'State, ChainQuery<'State, 'Label>>) : FiniteDag<'Obs, 'State, 'Label> =
    let initialState =
        {
            Hidden = None
            InitialWeight = None
            CurrentToken = None
            NextToken = 0
            Actions = ResizeArray()
        }

    let (ChainProgram run) = program
    let finalState, ChainQuery(ChainStateToken finalToken, label) = run initialState

    let hidden =
        finalState.Hidden
        |> Option.defaultWith (fun () -> failwith "No hidden state was declared in the chain builder.")

    let initialWeight =
        finalState.InitialWeight
        |> Option.defaultWith (fun () -> failwith "No initial distribution was declared in the chain builder.")

    let actions = finalState.Actions |> Seq.toArray

    let rootNode : FiniteDagNode<'Obs, 'State> =
        {
            Id = hidden.Id
            ParentId = None
            States = hidden.States
            InitialWeight = Some initialWeight
            TransitionWeight = None
            Observation = None
        }

    let rec build (nodes: Map<int, FiniteDagNode<'Obs, 'State>>) currentToken index =
        if index >= actions.Length then
            nodes, currentToken
        else
            let observedToken, observationWeight, observation =
                match actions.[index] with
                | ObserveAction(tokenId, obsWeight, obs) when tokenId = currentToken -> tokenId, obsWeight, obs
                | _ -> failwith "Expected an observation on the current chain token."

            let observedNode =
                nodes
                |> Map.tryFind observedToken
                |> Option.defaultWith (fun () -> failwith "Observed chain token was not allocated.")

            let updatedNodes : Map<int, FiniteDagNode<'Obs, 'State>> =
                nodes
                |> Map.add observedToken { observedNode with Observation = Some (observation, observationWeight) }

            let nextIndex = index + 1

            if nextIndex < actions.Length then
                match actions.[nextIndex] with
                | TransitionAction(sourceToken, destinationToken, transitionWeight) when sourceToken = observedToken ->
                    let nextNode : FiniteDagNode<'Obs, 'State> =
                        {
                            Id = destinationToken
                            ParentId = Some sourceToken
                            States = hidden.States
                            InitialWeight = None
                            TransitionWeight = Some transitionWeight
                            Observation = None
                        }

                    build (updatedNodes |> Map.add destinationToken nextNode) destinationToken (nextIndex + 1)
                | _ -> build updatedNodes observedToken nextIndex
            else
                updatedNodes, observedToken

    let nodes, currentToken = build (Map.ofList [ hidden.Id, rootNode ]) hidden.Id 0

    if currentToken <> finalToken then
        failwith "The chain builder finished on a different token than the compiled chain tail."

    {
        Nodes = nodes |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd |> Seq.toArray
        RootId = hidden.Id
        QueryId = finalToken
        Label = label
        Shape = LinearChain
    }

let internal compileTreeProgram (program: TreeProgram<'Obs, 'State, TreeQuery<'State, 'Label>>) : FiniteDag<'Obs, 'State, 'Label> =
    let initialState : TreeProgramState<'Obs, 'State> =
        {
            Nodes = Map.empty
            RootId = None
            NextToken = 0
        }

    let (TreeProgram run) = program
    let finalState, TreeQuery(TreeToken(queryToken, _), label) = run initialState

    let rootId =
        finalState.RootId
        |> Option.defaultWith (fun () -> failwith "No root variable was declared in the DAG builder.")

    let compiledNodes : FiniteDagNode<'Obs, 'State>[] =
        finalState.Nodes
        |> Map.toSeq
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.map (fun (node: TreeNodeSpec<'Obs, 'State>) ->
            let compiledObservation =
                node.Observation
                |> Option.map (fun obs -> obs.Observation, obs.ObservationWeight)

            ({
                Id = node.Id
                ParentId = node.ParentId
                States = node.States
                InitialWeight = node.InitialWeight
                TransitionWeight = node.TransitionWeight
                Observation = compiledObservation
            }: FiniteDagNode<'Obs, 'State>))
        |> Seq.toArray

    {
        Nodes = compiledNodes
        RootId = rootId
        QueryId = queryToken
        Label = label
        Shape = RootedTree
    }

let internal compileFiniteChain observations states initialWeight transitionWeight observationWeight label =
    let rootObservation =
        observations
        |> Array.tryHead
        |> Option.map (fun observed -> observed, observationWeight)

    let nodes =
        [|
            yield
                ({
                    Id = 0
                    ParentId = None
                    States = states
                    InitialWeight = Some initialWeight
                    TransitionWeight = None
                    Observation = rootObservation
                }: FiniteDagNode<'Obs, 'State>)

            for nodeId in 1 .. observations.Length do
                let observation =
                    if nodeId < observations.Length then
                        Some (observations.[nodeId], observationWeight)
                    else
                        None

                yield
                    ({
                        Id = nodeId
                        ParentId = Some (nodeId - 1)
                        States = states
                        InitialWeight = None
                        TransitionWeight = Some transitionWeight
                        Observation = observation
                    }: FiniteDagNode<'Obs, 'State>)
        |]

    {
        Nodes = nodes
        RootId = 0
        QueryId = observations.Length
        Label = label
        Shape = LinearChain
    }

module Chain =
    let finite name states initialWeight = finiteState name states initialWeight
    let observe token observationWeight observation = observeFinite token observationWeight observation
    let transition token transitionWeight = transitionFinite token transitionWeight
    let query token label = finishFinite token label
    let compile program = compileChainProgram program
    let compileFinite observations states initialWeight transitionWeight observationWeight label =
        compileFiniteChain observations states initialWeight transitionWeight observationWeight label

module Dag =
    let finite _name states initialWeight = rootVar states initialWeight
    let root name states initialWeight = finite name states initialWeight
    let child parent _name transitionWeight = childVar parent transitionWeight
    let childWithStates parent _name states transitionWeight = childVarWithStates parent states transitionWeight
    let observe token observationWeight observation = observeVar token observationWeight observation
    let query token label = finishTree token label
    let compile program = compileTreeProgram program

type internal ChildSubproblemShape =
    {
        ChildKey: int
        TransitionWeights: int64[]
    }

type internal RootedSubproblemShape =
    {
        StateCount: int
        ObservationWeights: int64[]
        Children: ChildSubproblemShape[]
    }

type internal CompiledChildEdge<'Obs, 'State when 'State: comparison> =
    {
        ChildId: int
        ChildStateCount: int
        TransitionLogWeights: float[]
        TransitionWeights: int64[]
    }

type internal CompiledRootedTreeNode<'Obs, 'State when 'State: comparison> =
    {
        Node: FiniteDagNode<'Obs, 'State>
        ObservationLogWeights: float[]
        ObservationWeights: int64[]
        Children: CompiledChildEdge<'Obs, 'State>[]
    }

type internal CompiledRootedTreeContext<'Obs, 'State, 'Label when 'State: comparison and 'Label: comparison> =
    {
        Dag: FiniteDag<'Obs, 'State, 'Label>
        NodesById: Dictionary<int, CompiledRootedTreeNode<'Obs, 'State>>
        SubtreeKeysById: Dictionary<int, int>
    }

type internal CompiledParentPatternContributions =
    {
        PrefixContributions: float[][]
        SuffixContributions: float[][]
    }

type internal CompiledLinearChainStep<'Obs, 'State when 'State: comparison> =
    {
        Node: FiniteDagNode<'Obs, 'State>
        ObservationLogWeights: float[]
        TransitionLogWeights: float[] option
    }

type internal CompiledLinearChainContext<'Obs, 'State, 'Label when 'State: comparison and 'Label: comparison> =
    {
        Dag: FiniteDag<'Obs, 'State, 'Label>
        Steps: CompiledLinearChainStep<'Obs, 'State>[]
        RootInitialLogWeights: float[]
    }

type internal CompiledDag<'Obs, 'State, 'Label when 'State: comparison and 'Label: comparison> =
    | LinearChainDag of CompiledLinearChainContext<'Obs, 'State, 'Label>
    | RootedTreeDag of CompiledRootedTreeContext<'Obs, 'State, 'Label>

let internal nodeObservationWeight (node: FiniteDagNode<'Obs, 'State>) stateValue =
    match node.Observation with
    | None -> 1.0
    | Some (observed, observationWeight) -> observationWeight stateValue observed

let internal nodeLogObservationWeight node stateValue =
    nodeObservationWeight node stateValue |> logWeight

let internal compiledTransitionLogWeight (edge: CompiledChildEdge<'Obs, 'State>) parentStateIndex childStateIndex =
    edge.TransitionLogWeights.[parentStateIndex * edge.ChildStateCount + childStateIndex]

let internal childrenByParent (nodes: FiniteDagNode<'Obs, 'State>[]) =
    nodes
    |> Array.choose (fun node -> node.ParentId |> Option.map (fun parentId -> parentId, node.Id))
    |> Array.groupBy fst
    |> Array.map (fun (parentId, children) -> parentId, children |> Array.map snd)
    |> Map.ofArray

let internal nodesById (nodes: FiniteDagNode<'Obs, 'State>[]) =
    nodes
    |> Array.map (fun node -> node.Id, node)
    |> Map.ofArray

let internal compileRootedTreeContext (dag: FiniteDag<'Obs, 'State, 'Label>) =
    let rawNodesById = Dictionary<int, FiniteDagNode<'Obs, 'State>>()
    let childBuilders = Dictionary<int, ResizeArray<int>>()

    for node in dag.Nodes do
        rawNodesById.[node.Id] <- node

        match node.ParentId with
        | Some parentId ->
            let children =
                match childBuilders.TryGetValue parentId with
                | true, existing -> existing
                | _ ->
                    let created = ResizeArray<int>()
                    childBuilders.[parentId] <- created
                    created

            children.Add node.Id
        | None -> ()

    let childrenByParentMap = Dictionary<int, int[]>()

    for KeyValue(parentId, childIds) in childBuilders do
        childrenByParentMap.[parentId] <- childIds.ToArray()

    let compiledNodesById = Dictionary<int, CompiledRootedTreeNode<'Obs, 'State>>()

    for node in dag.Nodes do
        let childIds =
            match childrenByParentMap.TryGetValue node.Id with
            | true, existing -> existing
            | _ -> [||]

        let compiledChildren =
            childIds
            |> Array.map (fun childId ->
                let childNode = rawNodesById.[childId]

                let transitionWeight =
                    childNode.TransitionWeight
                    |> Option.defaultWith (fun () -> failwith "Non-root tree nodes must have a transition weight.")

                let transitionWeights =
                    [|
                        for parentState in node.States do
                            for childState in childNode.States do
                                yield BitConverter.DoubleToInt64Bits (transitionWeight parentState childState)
                    |]

                {
                    ChildId = childId
                    ChildStateCount = childNode.States.Length
                    TransitionLogWeights = transitionWeights |> Array.map BitConverter.Int64BitsToDouble |> Array.map logWeight
                    TransitionWeights = transitionWeights
                })

        compiledNodesById.[node.Id] <-
            {
                Node = node
                ObservationLogWeights = node.States |> Array.map (nodeLogObservationWeight node)
                ObservationWeights = node.States |> Array.map (nodeObservationWeight node) |> Array.map BitConverter.DoubleToInt64Bits
                Children = compiledChildren
            }

    let subtreeKeysById = Dictionary<int, int>()
    let interner = Dictionary<RootedSubproblemShape, int>()
    let mutable nextKey = 0

    let rec compute nodeId =
        match subtreeKeysById.TryGetValue nodeId with
        | true, cached -> cached
        | _ ->
            let node = compiledNodesById.[nodeId]

            let childShapes =
                node.Children
                |> Array.map (fun child ->
                    {
                        ChildKey = compute child.ChildId
                        TransitionWeights = child.TransitionWeights
                    })

            Array.sortInPlace childShapes

            let shape =
                {
                    StateCount = node.Node.States.Length
                    ObservationWeights = node.ObservationWeights
                    Children = childShapes
                }

            let key =
                match interner.TryGetValue shape with
                | true, internedKey -> internedKey
                | _ ->
                    let internedKey = nextKey
                    nextKey <- nextKey + 1
                    interner.[shape] <- internedKey
                    internedKey

            subtreeKeysById.[nodeId] <- key
            key

    for node in dag.Nodes do
        compute node.Id |> ignore

    {
        Dag = dag
        NodesById = compiledNodesById
        SubtreeKeysById = subtreeKeysById
    }

let internal linearChainOrder (dag: FiniteDag<'Obs, 'State, 'Label>) =
    let children = childrenByParent dag.Nodes

    let rec loop acc nodeId =
        if nodeId = dag.QueryId then
            List.rev (nodeId :: acc)
        else
            match children |> Map.tryFind nodeId |> Option.defaultValue [||] with
            | [| childId |] -> loop (nodeId :: acc) childId
            | [||] -> failwith "Linear chain query is not reachable from the root."
            | _ -> failwith "Linear chain graphs must have at most one child per node."

    loop [] dag.RootId |> List.toArray

let internal compileLinearChainContext (dag: FiniteDag<'Obs, 'State, 'Label>) =
    let graphNodes = nodesById dag.Nodes
    let order = linearChainOrder dag

    let steps =
        order
        |> Array.map (fun nodeId ->
            let node = graphNodes.[nodeId]

            let transitionLogWeights =
                match node.ParentId, node.TransitionWeight with
                | None, None -> None
                | None, Some _ -> failwith "Linear chain root must not have a transition weight."
                | Some _, None -> failwith "Linear chain child nodes must have a transition weight."
                | Some parentId, Some transitionWeight ->
                    let parentNode = graphNodes.[parentId]

                    Some
                        [|
                            for parentState in parentNode.States do
                                for childState in node.States do
                                    yield logWeight (transitionWeight parentState childState)
                        |]

            {
                Node = node
                ObservationLogWeights = node.States |> Array.map (nodeLogObservationWeight node)
                TransitionLogWeights = transitionLogWeights
            })

    let root = steps.[0]

    let prior =
        root.Node.InitialWeight
        |> Option.defaultWith (fun () -> failwith "Linear chain root must carry an initial distribution.")

    {
        Dag = dag
        Steps = steps
        RootInitialLogWeights = root.Node.States |> Array.map (fun stateValue -> logWeight (prior stateValue))
    }

let internal compileDag dag =
    match dag.Shape with
    | LinearChain -> LinearChainDag (compileLinearChainContext dag)
    | RootedTree -> RootedTreeDag (compileRootedTreeContext dag)

let internal inferLinearChainSumProduct (context: CompiledLinearChainContext<'Obs, 'State, 'Label>) =
    let dag = context.Dag
    let root = context.Steps.[0]

    let mutable current =
        root.Node.States
        |> Array.mapi (fun stateIndex _ -> context.RootInitialLogWeights.[stateIndex] + root.ObservationLogWeights.[stateIndex])

    for stepIndex in 1 .. context.Steps.Length - 1 do
        let step = context.Steps.[stepIndex]
        let next = Array.zeroCreate<float> step.Node.States.Length

        let transitionLogWeights =
            step.TransitionLogWeights
            |> Option.defaultWith (fun () -> failwith "Linear chain child nodes must have a transition weight.")

        for nextStateIndex in 0 .. step.Node.States.Length - 1 do
            let totals =
                [| for sourceStateIndex in 0 .. current.Length - 1 ->
                    current.[sourceStateIndex] + transitionLogWeights.[sourceStateIndex * step.Node.States.Length + nextStateIndex] |]

            next.[nextStateIndex] <- logSumExp totals + step.ObservationLogWeights.[nextStateIndex]

        current <- next

    let queryStep = context.Steps.[context.Steps.Length - 1]

    [ for stateIndex in 0 .. queryStep.Node.States.Length - 1 -> dag.Label queryStep.Node.States.[stateIndex], current.[stateIndex] ]
    |> exactPosteriorResultFromLog

let internal inferLinearChainMaxProduct (context: CompiledLinearChainContext<'Obs, 'State, 'Label>) =
    let dag = context.Dag
    let root = context.Steps.[0]

    let mutable current =
        root.Node.States
        |> Array.mapi (fun stateIndex _ -> context.RootInitialLogWeights.[stateIndex] + root.ObservationLogWeights.[stateIndex])

    for stepIndex in 1 .. context.Steps.Length - 1 do
        let step = context.Steps.[stepIndex]
        let next = Array.zeroCreate<float> step.Node.States.Length

        let transitionLogWeights =
            step.TransitionLogWeights
            |> Option.defaultWith (fun () -> failwith "Linear chain child nodes must have a transition weight.")

        for nextStateIndex in 0 .. step.Node.States.Length - 1 do
            let mutable best = negativeInfinity

            for sourceStateIndex in 0 .. current.Length - 1 do
                let score = current.[sourceStateIndex] + transitionLogWeights.[sourceStateIndex * step.Node.States.Length + nextStateIndex]

                if score > best then
                    best <- score

            next.[nextStateIndex] <- best + step.ObservationLogWeights.[nextStateIndex]

        current <- next

    let queryStep = context.Steps.[context.Steps.Length - 1]

    [ for stateIndex in 0 .. queryStep.Node.States.Length - 1 -> dag.Label queryStep.Node.States.[stateIndex], current.[stateIndex] ]
    |> maxPosteriorResultFromLog

let internal inferLinearChain engine context =
    match engine with
    | SumProduct -> inferLinearChainSumProduct context |> ExactPosterior
    | BeliefPropagation -> inferLinearChainSumProduct context |> ExactPosterior
    | MaxProduct -> inferLinearChainMaxProduct context |> MaxPosterior

let internal inferRootedTreeSumProduct (context: CompiledRootedTreeContext<'Obs, 'State, 'Label>) =
    let dag = context.Dag
    let upwards = Dictionary<int, float[]>()
    let sharedUpwards = Dictionary<int, float[]>()
    let sharedParentPatterns = Dictionary<int, CompiledParentPatternContributions>()

    let rec computeUpward nodeId =
        match upwards.TryGetValue nodeId with
        | true, cached -> cached
        | _ ->
            let key = context.SubtreeKeysById.[nodeId]

            match sharedUpwards.TryGetValue key with
            | true, shared ->
                upwards.[nodeId] <- shared
                shared
            | _ ->
                let node = context.NodesById.[nodeId]

                let message =
                    node.Node.States
                    |> Array.mapi (fun stateIndex _ ->
                        let childTerms =
                            node.Children
                            |> Array.sumBy (fun child ->
                                let childUpward = computeUpward child.ChildId

                                [| for childIndex in 0 .. child.ChildStateCount - 1 ->
                                    compiledTransitionLogWeight child stateIndex childIndex + childUpward.[childIndex] |]
                                |> logSumExp)

                        node.ObservationLogWeights.[stateIndex] + childTerms)

                sharedUpwards.[key] <- message
                upwards.[nodeId] <- message
                message

    computeUpward dag.RootId |> ignore

    let root = context.NodesById.[dag.RootId]

    let prior =
        root.Node.InitialWeight
        |> Option.defaultWith (fun () -> failwith "Root node must have an initial distribution.")

    if dag.QueryId = dag.RootId then
        let rootUpward = upwards.[dag.RootId]

        let rootBelief =
            root.Node.States
            |> Array.mapi (fun index stateValue -> logWeight (prior stateValue) + rootUpward.[index])

        root.Node.States
        |> Array.mapi (fun index stateValue -> dag.Label stateValue, rootBelief.[index])
        |> exactPosteriorResultFromLog
    else
        let outsides = Dictionary<int, float[]>()
        let rootOutside = root.Node.States |> Array.map (fun stateValue -> logWeight (prior stateValue))
        outsides.[dag.RootId] <- rootOutside

        let getParentPattern nodeId =
            let key = context.SubtreeKeysById.[nodeId]

            match sharedParentPatterns.TryGetValue key with
            | true, cached -> cached
            | _ ->
                let node = context.NodesById.[nodeId]
                let stateCount = node.Node.States.Length

                let childContributions =
                    node.Children
                    |> Array.map (fun child ->
                        let childUpward = upwards.[child.ChildId]

                        node.Node.States
                        |> Array.mapi (fun parentIndex _ ->
                            [| for childIndex in 0 .. child.ChildStateCount - 1 ->
                                compiledTransitionLogWeight child parentIndex childIndex + childUpward.[childIndex] |]
                            |> logSumExp))

                let prefix = Array.init (childContributions.Length + 1) (fun _ -> Array.zeroCreate<float> stateCount)

                for childIndex in 0 .. childContributions.Length - 1 do
                    for stateIndex in 0 .. stateCount - 1 do
                        prefix.[childIndex + 1].[stateIndex] <- prefix.[childIndex].[stateIndex] + childContributions.[childIndex].[stateIndex]

                let suffix = Array.init (childContributions.Length + 1) (fun _ -> Array.zeroCreate<float> stateCount)

                for childIndex in childContributions.Length - 1 .. -1 .. 0 do
                    for stateIndex in 0 .. stateCount - 1 do
                        suffix.[childIndex].[stateIndex] <- suffix.[childIndex + 1].[stateIndex] + childContributions.[childIndex].[stateIndex]

                let cached =
                    {
                        PrefixContributions = prefix
                        SuffixContributions = suffix
                    }

                sharedParentPatterns.[key] <- cached
                cached

        let rec propagate nodeId =
            let node = context.NodesById.[nodeId]
            let nodeOutside = outsides.[nodeId]
            let childEdges = node.Children
            let pattern = getParentPattern nodeId

            for childIndex in childEdges.Length - 1 .. -1 .. 0 do
                let child = childEdges.[childIndex]
                let childNode = context.NodesById.[child.ChildId]

                let childOutside =
                    childNode.Node.States
                    |> Array.mapi (fun childStateIndex _ ->
                        node.Node.States
                        |> Array.mapi (fun parentIndex _ ->
                            nodeOutside.[parentIndex]
                            + node.ObservationLogWeights.[parentIndex]
                            + pattern.PrefixContributions.[childIndex].[parentIndex]
                            + pattern.SuffixContributions.[childIndex + 1].[parentIndex]
                            + compiledTransitionLogWeight child parentIndex childStateIndex)
                        |> logSumExp)

                outsides.[child.ChildId] <- childOutside
                propagate child.ChildId

        propagate dag.RootId

        let queryNode = context.NodesById.[dag.QueryId]
        let queryUpward = upwards.[dag.QueryId]
        let queryOutside = outsides.[dag.QueryId]
        let logBelief = Array.mapi (fun index value -> queryOutside.[index] + value) queryUpward

        queryNode.Node.States
        |> Array.mapi (fun index stateValue -> dag.Label stateValue, logBelief.[index])
        |> exactPosteriorResultFromLog

let internal inferRootedTreeMaxProduct (context: CompiledRootedTreeContext<'Obs, 'State, 'Label>) =
    let dag = context.Dag
    let upwards = Dictionary<int, float[]>()
    let sharedUpwards = Dictionary<int, float[]>()
    let sharedParentPatterns = Dictionary<int, CompiledParentPatternContributions>()

    let rec computeUpward nodeId =
        match upwards.TryGetValue nodeId with
        | true, cached -> cached
        | _ ->
            let key = context.SubtreeKeysById.[nodeId]

            match sharedUpwards.TryGetValue key with
            | true, shared ->
                upwards.[nodeId] <- shared
                shared
            | _ ->
                let node = context.NodesById.[nodeId]

                let message =
                    node.Node.States
                    |> Array.mapi (fun stateIndex _ ->
                        let childTerms =
                            node.Children
                            |> Array.sumBy (fun child ->
                                let childUpward = computeUpward child.ChildId

                                [| for childIndex in 0 .. child.ChildStateCount - 1 ->
                                    compiledTransitionLogWeight child stateIndex childIndex + childUpward.[childIndex] |]
                                |> Array.max)

                        node.ObservationLogWeights.[stateIndex] + childTerms)

                sharedUpwards.[key] <- message
                upwards.[nodeId] <- message
                message

    computeUpward dag.RootId |> ignore

    let root = context.NodesById.[dag.RootId]

    let prior =
        root.Node.InitialWeight
        |> Option.defaultWith (fun () -> failwith "Root node must have an initial distribution.")

    if dag.QueryId = dag.RootId then
        let rootUpward = upwards.[dag.RootId]

        let rootBelief =
            root.Node.States
            |> Array.mapi (fun index stateValue -> logWeight (prior stateValue) + rootUpward.[index])

        root.Node.States
        |> Array.mapi (fun index stateValue -> dag.Label stateValue, rootBelief.[index])
        |> maxPosteriorResultFromLog
    else
        let outsides = Dictionary<int, float[]>()
        let rootOutside = root.Node.States |> Array.map (fun stateValue -> logWeight (prior stateValue))
        outsides.[dag.RootId] <- rootOutside

        let getParentPattern nodeId =
            let key = context.SubtreeKeysById.[nodeId]

            match sharedParentPatterns.TryGetValue key with
            | true, cached -> cached
            | _ ->
                let node = context.NodesById.[nodeId]
                let stateCount = node.Node.States.Length

                let childContributions =
                    node.Children
                    |> Array.map (fun child ->
                        let childUpward = upwards.[child.ChildId]

                        node.Node.States
                        |> Array.mapi (fun parentIndex _ ->
                            [| for childIndex in 0 .. child.ChildStateCount - 1 ->
                                compiledTransitionLogWeight child parentIndex childIndex + childUpward.[childIndex] |]
                            |> Array.max))

                let prefix = Array.init (childContributions.Length + 1) (fun _ -> Array.zeroCreate<float> stateCount)

                for childIndex in 0 .. childContributions.Length - 1 do
                    for stateIndex in 0 .. stateCount - 1 do
                        prefix.[childIndex + 1].[stateIndex] <- prefix.[childIndex].[stateIndex] + childContributions.[childIndex].[stateIndex]

                let suffix = Array.init (childContributions.Length + 1) (fun _ -> Array.zeroCreate<float> stateCount)

                for childIndex in childContributions.Length - 1 .. -1 .. 0 do
                    for stateIndex in 0 .. stateCount - 1 do
                        suffix.[childIndex].[stateIndex] <- suffix.[childIndex + 1].[stateIndex] + childContributions.[childIndex].[stateIndex]

                let cached =
                    {
                        PrefixContributions = prefix
                        SuffixContributions = suffix
                    }

                sharedParentPatterns.[key] <- cached
                cached

        let rec propagate nodeId =
            let node = context.NodesById.[nodeId]
            let nodeOutside = outsides.[nodeId]
            let childEdges = node.Children
            let pattern = getParentPattern nodeId

            for childIndex in childEdges.Length - 1 .. -1 .. 0 do
                let child = childEdges.[childIndex]
                let childNode = context.NodesById.[child.ChildId]

                let childOutside =
                    childNode.Node.States
                    |> Array.mapi (fun childStateIndex _ ->
                        node.Node.States
                        |> Array.mapi (fun parentIndex _ ->
                            nodeOutside.[parentIndex]
                            + node.ObservationLogWeights.[parentIndex]
                            + pattern.PrefixContributions.[childIndex].[parentIndex]
                            + pattern.SuffixContributions.[childIndex + 1].[parentIndex]
                            + compiledTransitionLogWeight child parentIndex childStateIndex)
                        |> Array.max)

                outsides.[child.ChildId] <- childOutside
                propagate child.ChildId

        propagate dag.RootId

        let queryNode = context.NodesById.[dag.QueryId]
        let queryUpward = upwards.[dag.QueryId]
        let queryOutside = outsides.[dag.QueryId]
        let logBelief = Array.mapi (fun index value -> queryOutside.[index] + value) queryUpward

        queryNode.Node.States
        |> Array.mapi (fun index stateValue -> dag.Label stateValue, logBelief.[index])
        |> maxPosteriorResultFromLog

let internal inferRootedTree engine context =
    match engine with
    | SumProduct -> inferRootedTreeSumProduct context |> ExactPosterior
    | BeliefPropagation -> inferRootedTreeSumProduct context |> ExactPosterior
    | MaxProduct -> inferRootedTreeMaxProduct context |> MaxPosterior

let internal inferCompiledDag engine compiledDag =
    match compiledDag with
    | LinearChainDag context -> inferLinearChain engine context
    | RootedTreeDag context -> inferRootedTree engine context

let prepareDag dag =
    let compiledDag = compileDag dag
    PreparedDag(fun engine -> inferCompiledDag engine compiledDag)

let inferPreparedDag engine (preparedDag: PreparedDag<'Label>) =
    preparedDag.Infer engine

let inferPreparedDagByBeliefPropagation (preparedDag: PreparedDag<'Label>) =
    preparedDag.Infer BeliefPropagation

let inferFiniteDag engine dag =
    prepareDag dag |> inferPreparedDag engine

let evaluatePreparedDag (preparedDag: PreparedDag<'Label>) =
    preparedDag.Evaluate()

let evaluateDag dag =
    prepareDag dag |> evaluatePreparedDag