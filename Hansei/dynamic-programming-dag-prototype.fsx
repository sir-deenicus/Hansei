#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open Hansei.Core.List
open Hansei.Core.List.Distributions

type private FiniteNode<'State> =
    {
        Id: int
        States: 'State[]
    }

type internal GraphShape =
    | LinearChain
    | RootedTree

type internal FiniteDagNode<'Obs, 'State when 'State: comparison> =
    {
        Id: int
        ParentId: int option
        States: 'State[]
        InitialWeight: ('State -> float) option
        TransitionWeight: ('State -> 'State -> float) option
        Observation: ('Obs * ('State -> 'Obs -> float)) option
    }

type internal FiniteDag<'Obs, 'State when 'State: comparison> =
    {
        Nodes: FiniteDagNode<'Obs, 'State>[]
        RootId: int
        QueryId: int
        Label: 'State -> string
        Shape: GraphShape
    }

type internal GraphInferenceEngine =
    | SumProduct
    | BeliefPropagation
    | MaxProduct

type internal ExactPosteriorResult =
    {
        Posterior: Map<string, float>
        Evidence: float
        LogEvidence: float
    }

type internal MaxPosteriorResult =
    {
        BestLabel: string
        Score: float
        LogScore: float
    }

type internal GraphInferenceResult =
    | ExactPosterior of ExactPosteriorResult
    | MaxPosterior of MaxPosteriorResult

type private ChildSubproblemShape =
    {
        ChildKey: int
        TransitionWeights: int64[]
    }

type private RootedSubproblemShape =
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

type internal CompiledDiagnostics =
    {
        mutable PreparedContextUses: int
        mutable SharedUpwardHits: int
        mutable ParentPatternHits: int
    }

type private CompiledDiagnosticsSnapshot =
    {
        PreparedContextUses: int
        SharedUpwardHits: int
        ParentPatternHits: int
    }

type internal CompiledRootedTreeContext<'Obs, 'State when 'State: comparison> =
    {
        Dag: FiniteDag<'Obs, 'State>
        NodesById: System.Collections.Generic.Dictionary<int, CompiledRootedTreeNode<'Obs, 'State>>
        ChildrenByParent: System.Collections.Generic.Dictionary<int, int[]>
        SubtreeKeysById: System.Collections.Generic.Dictionary<int, int>
        Diagnostics: CompiledDiagnostics
    }

type internal CompiledParentPatternContributions =
    {
        ChildContributions: float[][]
        PrefixContributions: float[][]
        SuffixContributions: float[][]
    }

type internal CompiledLinearChainStep<'Obs, 'State when 'State: comparison> =
    {
        Node: FiniteDagNode<'Obs, 'State>
        ObservationLogWeights: float[]
        TransitionLogWeights: float[] option
    }

type internal CompiledLinearChainContext<'Obs, 'State when 'State: comparison> =
    {
        Dag: FiniteDag<'Obs, 'State>
        Steps: CompiledLinearChainStep<'Obs, 'State>[]
        RootInitialLogWeights: float[]
        Diagnostics: CompiledDiagnostics
    }

type internal CompiledDag<'Obs, 'State when 'State: comparison> =
    | LinearChainDag of CompiledLinearChainContext<'Obs, 'State>
    | RootedTreeDag of CompiledRootedTreeContext<'Obs, 'State>

type private StateToken<'State> =
    private
    | StateToken of int

type private ModelResult<'State> =
    private
    | ModelResult of StateToken<'State> * ('State -> string)

type private ChainAction<'Obs, 'State> =
    | ObserveAction of int * ('State -> 'Obs -> float) * 'Obs
    | TransitionAction of int * int * ('State -> 'State -> float)

type private ChainProgramState<'Obs, 'State> =
    {
        Hidden: FiniteNode<'State> option
        InitialWeight: ('State -> float) option
        CurrentToken: int option
        NextToken: int
        Actions: ResizeArray<ChainAction<'Obs, 'State>>
    }

type private ChainProgram<'Obs, 'State, 'T> =
    ChainProgram of (ChainProgramState<'Obs, 'State> -> ChainProgramState<'Obs, 'State> * 'T)

type private ChainProgramBuilder() =
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

let private chain = ChainProgramBuilder()

type private TreeToken<'State> =
    private
    | TreeToken of int * 'State[]

type private TreeResult<'State> =
    private
    | TreeResult of TreeToken<'State> * ('State -> string)

type private TreeObservation<'Obs, 'State> =
    {
        Observation: 'Obs
        ObservationWeight: 'State -> 'Obs -> float
    }

type private TreeNodeSpec<'Obs, 'State when 'State: comparison> =
    {
        Id: int
        ParentId: int option
        States: 'State[]
        InitialWeight: ('State -> float) option
        TransitionWeight: ('State -> 'State -> float) option
        Observation: TreeObservation<'Obs, 'State> option
    }

type private TreeProgramState<'Obs, 'State when 'State: comparison> =
    {
        Nodes: Map<int, TreeNodeSpec<'Obs, 'State>>
        RootId: int option
        NextToken: int
    }

type private TreeProgram<'Obs, 'State, 'T when 'State: comparison> =
    TreeProgram of (TreeProgramState<'Obs, 'State> -> TreeProgramState<'Obs, 'State> * 'T)

type private TreeProgramBuilder() =
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

let private dag = TreeProgramBuilder()

let private aggregateValues (dist: ProbabilitySpace<'T>) =
    dist
    |> List.fold (fun acc (node, p) ->
        match node with
        | Value value -> Map.change value (fun existing -> Some (p + defaultArg existing 0.0)) acc
        | ContinuedSubTree _ -> acc) Map.empty

let private normalizeMap (dist: Map<'T, float>) =
    let total = dist |> Seq.sumBy (fun kv -> kv.Value)
    if total <= 0.0 then dist else dist |> Map.map (fun _ p -> p / total)

let private buildProbabilityLookup<'a when 'a: comparison> (entries: ('a * float) list) =
    let table = entries |> Map.ofList
    fun key -> Map.tryFind key table |> Option.defaultValue 0.0

let private buildPairProbabilityLookup<'a, 'b when 'a: comparison and 'b: comparison> (entries: (('a * 'b) * float) list) =
    let table = entries |> Map.ofList
    fun source target -> Map.tryFind (source, target) table |> Option.defaultValue 0.0

let private negativeInfinity = Double.NegativeInfinity

let private logWeight weight =
    if weight <= 0.0 then negativeInfinity else log weight

let private logSumExp (values: float[]) =
    if values.Length = 0 then
        negativeInfinity
    else
        let maxValue = values |> Array.max

        if Double.IsNegativeInfinity maxValue then
            negativeInfinity
        else
            maxValue + log (values |> Array.sumBy (fun value -> exp (value - maxValue)))

let private addLogWeight key logValue acc =
    Map.change key (fun existing ->
        match existing with
        | None -> Some logValue
        | Some current -> Some (logSumExp [| current; logValue |])) acc

let private exactPosteriorResultFromLog entries =
    let rawLog =
        entries
        |> Seq.fold (fun acc (label, logValue) -> addLogWeight label logValue acc) Map.empty

    let logEvidence = rawLog |> Seq.map (fun kv -> kv.Value) |> Seq.toArray |> logSumExp
    let posterior =
        rawLog
        |> Map.map (fun _ logValue ->
            if Double.IsNegativeInfinity logValue || Double.IsNegativeInfinity logEvidence then 0.0
            else exp (logValue - logEvidence))

    let evidence = if Double.IsNegativeInfinity logEvidence then 0.0 else exp logEvidence

    {
        Posterior = posterior
        Evidence = evidence
        LogEvidence = logEvidence
    }

let private maxPosteriorResultFromLog entries =
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

    let score = if Double.IsNegativeInfinity bestLogScore then 0.0 else exp bestLogScore

    {
        BestLabel = bestLabel
        Score = score
        LogScore = bestLogScore
    }

let private normalizedValues dist = dist |> aggregateValues |> normalizeMap

let private unionKeys (a: Map<'T, float>) (b: Map<'T, float>) =
    Set.union (a |> Map.keys |> Set.ofSeq) (b |> Map.keys |> Set.ofSeq)

let private l1Distance (expected: Map<'T, float>) (actual: Map<'T, float>) =
    unionKeys expected actual
    |> Seq.sumBy (fun key ->
        let expectedP = Map.tryFind key expected |> Option.defaultValue 0.0
        let actualP = Map.tryFind key actual |> Option.defaultValue 0.0
        abs (expectedP - actualP))

let private benchmark repeats action =
    action () |> ignore
    let timer = Stopwatch.StartNew()
    for _ in 1 .. repeats do
        action () |> ignore
    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private diagnosticsOf (compiledDag: CompiledDag<'Obs, 'State>) : CompiledDiagnostics =
    match compiledDag with
    | LinearChainDag context -> context.Diagnostics
    | RootedTreeDag context -> context.Diagnostics

let private resetCompiledDiagnostics compiledDag =
    let diagnostics = diagnosticsOf compiledDag
    diagnostics.PreparedContextUses <- 0
    diagnostics.SharedUpwardHits <- 0
    diagnostics.ParentPatternHits <- 0

let private snapshotCompiledDiagnostics compiledDag =
    let diagnostics = diagnosticsOf compiledDag

    {
        PreparedContextUses = diagnostics.PreparedContextUses
        SharedUpwardHits = diagnostics.SharedUpwardHits
        ParentPatternHits = diagnostics.ParentPatternHits
    }

let private printTop title count (dist: Map<string, float>) =
    printfn "%s" title
    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.truncate count
    |> List.iter (fun (label, p) -> printfn "  %-26s %.6f" label p)

let private topKEntries count (dist: Map<string, float>) =
    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.truncate count

let private printTopKSegmentations title count (dist: Map<string, float>) =
    printfn "%s" title

    topKEntries count dist
    |> List.iteri (fun index (label, probability) ->
        printfn "  #%d %-23s %.6f" (index + 1) label probability)

let private exact distribution = Model.ExactInfer distribution |> normalizedValues

let private hiddenMarkovPosterior (observations: bool[]) =
    let rec loop step hiddenState =
        dist {
            if step >= observations.Length then
                return if hiddenState then "final=Hot" else "final=Cold"
            else
                let emissionP = if hiddenState then 0.85 else 0.15
                let! emitted = bernoulli emissionP
                do! observe (emitted = observations.[step])

                let transitionP = if hiddenState then 0.85 else 0.15
                let! nextState = bernoulli transitionP
                return! loop (step + 1) nextState
        }

    dist {
        let! initial = bernoulli 0.5
        return! loop 0 initial
    }

let private finiteState _name states initialWeight =
    ChainProgram(fun state ->
        match state.Hidden with
        | Some _ -> failwith "Only one finite hidden chain is supported in this Phase 1 prototype."
        | None ->
            let tokenId = state.NextToken
            let nextState =
                {
                    state with
                        Hidden = Some { Id = tokenId; States = states }
                        InitialWeight = Some initialWeight
                        CurrentToken = Some tokenId
                        NextToken = tokenId + 1
                }

            nextState, StateToken tokenId)

let private observeFinite (StateToken tokenId) observationWeight observation =
    ChainProgram(fun state ->
        match state.CurrentToken with
        | Some currentToken when currentToken = tokenId ->
            state.Actions.Add (ObserveAction(tokenId, observationWeight, observation))
            state, ()
        | _ -> failwith "observeFinite must apply to the current state token in the linear chain builder.")

let private transitionFinite (StateToken sourceToken) transitionWeight =
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
            StateToken nextToken
        | _ -> failwith "transitionFinite must apply to the current state token in the linear chain builder.")

let private finishFinite token label =
    ChainProgram(fun state -> state, ModelResult(token, label))

let private rootVar states initialWeight =
    TreeProgram(fun state ->
        match state.RootId with
        | Some _ -> failwith "Only one root is supported in this Phase 1 tree prototype."
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

let private childVarWithStates (TreeToken(parentId, _)) states transitionWeight =
    TreeProgram(fun state ->
        if not (Map.containsKey parentId state.Nodes) then
            failwith "childVarWithStates references an unknown parent token."
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

let private childVar (TreeToken(_, parentStates) as parent) transitionWeight =
    childVarWithStates parent parentStates transitionWeight

let private observeVar (TreeToken(tokenId, _)) observationWeight observation =
    TreeProgram(fun state ->
        let node = state.Nodes |> Map.tryFind tokenId |> Option.defaultWith (fun () -> failwith "observeVar references an unknown token.")
        let updatedNode = { node with Observation = Some { Observation = observation; ObservationWeight = observationWeight } }
        { state with Nodes = Map.add tokenId updatedNode state.Nodes }, ())

let private finishTree token label =
    TreeProgram(fun state -> state, TreeResult(token, label))

let private compileChainProgram (program: ChainProgram<'Obs, 'State, ModelResult<'State>>) =
    let initialState =
        {
            Hidden = None
            InitialWeight = None
            CurrentToken = None
            NextToken = 0
            Actions = ResizeArray()
        }

    let (ChainProgram run) = program
    let finalState, ModelResult(StateToken finalToken, label) = run initialState

    let hidden = finalState.Hidden |> Option.defaultWith (fun () -> failwith "No hidden state was declared in the chain builder.")
    let initialWeight = finalState.InitialWeight |> Option.defaultWith (fun () -> failwith "No initial distribution was declared in the chain builder.")
    let actions = finalState.Actions |> Seq.toArray
    let rootNode: FiniteDagNode<'Obs, 'State> =
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
                | ObserveAction(tokenId, observationWeight, observation) when tokenId = currentToken ->
                    tokenId, observationWeight, observation
                | _ -> failwith "Expected an observation on the current chain token."

            let observedNode: FiniteDagNode<'Obs, 'State> =
                nodes
                |> Map.tryFind observedToken
                |> Option.defaultWith (fun () -> failwith "Observed chain token was not allocated.")

            let updatedNodes: Map<int, FiniteDagNode<'Obs, 'State>> =
                nodes
                |> Map.add observedToken { observedNode with Observation = Some (observation, observationWeight) }

            let nextIndex = index + 1

            if nextIndex < actions.Length then
                match actions.[nextIndex] with
                | TransitionAction(sourceToken, destinationToken, transitionWeight) when sourceToken = observedToken ->
                    let nextNode: FiniteDagNode<'Obs, 'State> =
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
        failwith "The chain builder finished on a different state token than the compiled chain tail."

    {
        Nodes = nodes |> Map.toSeq |> Seq.sortBy fst |> Seq.map snd |> Seq.toArray
        RootId = hidden.Id
        QueryId = finalToken
        Label = label
        Shape = LinearChain
    }

let private compileTreeProgram (program: TreeProgram<'Obs, 'State, TreeResult<'State>>) =
    let initialState =
        {
            Nodes = Map.empty
            RootId = None
            NextToken = 0
        }

    let (TreeProgram run) = program
    let finalState, TreeResult(TreeToken(queryToken, _), label) = run initialState
    let rootId = finalState.RootId |> Option.defaultWith (fun () -> failwith "No root variable was declared in the DAG builder.")

    {
        Nodes =
            finalState.Nodes
            |> Map.toSeq
            |> Seq.sortBy fst
            |> Seq.map snd
            |> Seq.map (fun node ->
                let compiledObservation = node.Observation |> Option.map (fun obs -> obs.Observation, obs.ObservationWeight)
                ({
                    Id = node.Id
                    ParentId = node.ParentId
                    States = node.States
                    InitialWeight = node.InitialWeight
                    TransitionWeight = node.TransitionWeight
                    Observation = compiledObservation
                }: FiniteDagNode<'Obs, 'State>))
            |> Seq.toArray
        RootId = rootId
        QueryId = queryToken
        Label = label
        Shape = RootedTree
    }

module private ChainDsl =
    let finite name states initialWeight = finiteState name states initialWeight
    let observe token observationWeight observation = observeFinite token observationWeight observation
    let transition token transitionWeight = transitionFinite token transitionWeight
    let query token label = finishFinite token label

module private DagDsl =
    let finite _name states initialWeight = rootVar states initialWeight
    let root name states initialWeight = finite name states initialWeight
    let child parent _name transitionWeight = childVar parent transitionWeight
    let childWithStates parent _name states transitionWeight = childVarWithStates parent states transitionWeight
    let observe token observationWeight observation = observeVar token observationWeight observation
    let query token label = finishTree token label

let private compileFiniteChain observations states initialWeight transitionWeight observationWeight label =
    let rootObservation: ('Obs * ('State -> 'Obs -> float)) option =
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

module private StructuredChainCompiler =
    open ChainDsl

    let compileFiniteChain (observations: 'Obs[]) states initialWeight transitionWeight observationWeight label =
        if observations.Length = 0 then
            compileChainProgram
                (chain {
                    let! initialState = finite "HiddenState" states initialWeight
                    return! query initialState label
                })
        else
            let rec loop index currentState =
                chain {
                    do! observe currentState observationWeight observations.[index]

                    let! nextState = transition currentState transitionWeight

                    if index = observations.Length - 1 then
                        return nextState
                    else
                        return! loop (index + 1) nextState
                }

            compileChainProgram
                (chain {
                    let! initialState = finite "HiddenState" states initialWeight
                    let! finalState = loop 0 initialState
                    return! query finalState label
                })

let private compileStructuredFiniteChain (observations: 'Obs[]) states initialWeight transitionWeight observationWeight label =
    StructuredChainCompiler.compileFiniteChain observations states initialWeight transitionWeight observationWeight label

let private compileBinaryHmmChain observations =
    let states = [| false; true |]
    let initialWeight _ = 0.5
    let observationWeight state observed =
        match state, observed with
        | true, true
        | false, false -> 0.85
        | _ -> 0.15

    let transitionWeight source target =
        match source, target with
        | true, true
        | false, false -> 0.85
        | _ -> 0.15

    let label state = if state then "final=Hot" else "final=Cold"
    compileFiniteChain observations states initialWeight transitionWeight observationWeight label

let private compileStructuredBinaryHmmChain (observations: bool[]) =
    let states = [| false; true |]
    let initialWeight _ = 0.5
    let observationWeight state observed =
        match state, observed with
        | true, true
        | false, false -> 0.85
        | _ -> 0.15

    let transitionWeight source target =
        match source, target with
        | true, true
        | false, false -> 0.85
        | _ -> 0.15

    let label state = if state then "final=Hot" else "final=Cold"
    compileStructuredFiniteChain observations states initialWeight transitionWeight observationWeight label

let private nodeObservationWeight (node: FiniteDagNode<'Obs, 'State>) stateValue =
    match node.Observation with
    | None -> 1.0
    | Some (observed, observationWeight) -> observationWeight stateValue observed

let private nodeLogObservationWeight node stateValue = nodeObservationWeight node stateValue |> logWeight

let private compiledTransitionLogWeight (edge: CompiledChildEdge<'Obs, 'State>) parentStateIndex childStateIndex =
    edge.TransitionLogWeights.[parentStateIndex * edge.ChildStateCount + childStateIndex]

let private childrenByParent (nodes: FiniteDagNode<'Obs, 'State>[]) =
    nodes
    |> Array.choose (fun node -> node.ParentId |> Option.map (fun parentId -> parentId, node.Id))
    |> Array.groupBy fst
    |> Array.map (fun (parentId, children) -> parentId, children |> Array.map snd)
    |> Map.ofArray

let private nodesById (nodes: FiniteDagNode<'Obs, 'State>[]) = nodes |> Array.map (fun node -> node.Id, node) |> Map.ofArray

let private compileRootedTreeContext (dag: FiniteDag<'Obs, 'State>) =
    let rawNodesById = System.Collections.Generic.Dictionary<int, FiniteDagNode<'Obs, 'State>>()
    let childBuilders = System.Collections.Generic.Dictionary<int, ResizeArray<int>>()

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

    let childrenByParent = System.Collections.Generic.Dictionary<int, int[]>()

    for KeyValue(parentId, childIds) in childBuilders do
        childrenByParent.[parentId] <- childIds.ToArray()

    let compiledNodesById = System.Collections.Generic.Dictionary<int, CompiledRootedTreeNode<'Obs, 'State>>()

    for node in dag.Nodes do
        let childIds =
            match childrenByParent.TryGetValue node.Id with
            | true, existing -> existing
            | _ -> [||]

        let compiledChildren =
            childIds
            |> Array.map (fun childId ->
                let childNode = rawNodesById.[childId]
                let transitionWeight = childNode.TransitionWeight |> Option.defaultWith (fun () -> failwith "Non-root tree nodes must have a transition weight.")
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

    let subtreeKeysById = System.Collections.Generic.Dictionary<int, int>()
    let interner = System.Collections.Generic.Dictionary<RootedSubproblemShape, int>()
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
        let _ = compute node.Id
        ()

    {
        Dag = dag
        NodesById = compiledNodesById
        ChildrenByParent = childrenByParent
        SubtreeKeysById = subtreeKeysById
        Diagnostics = { PreparedContextUses = 0; SharedUpwardHits = 0; ParentPatternHits = 0 }
    }

let private rootedSubproblemKeyStats (dag: FiniteDag<'Obs, 'State>) =
    let context = compileRootedTreeContext dag
    let keys = dag.Nodes |> Array.map (fun node -> context.SubtreeKeysById.[node.Id])
    let uniqueKeyCount = keys |> Array.distinct |> Array.length
    keys.Length, uniqueKeyCount

let private rootedSubproblemKeyStatsOfCompiled compiledDag =
    match compiledDag with
    | RootedTreeDag context ->
        let keys = context.Dag.Nodes |> Array.map (fun node -> context.SubtreeKeysById.[node.Id])
        let uniqueKeyCount = keys |> Array.distinct |> Array.length
        keys.Length, uniqueKeyCount
    | LinearChainDag context -> context.Dag.Nodes.Length, context.Dag.Nodes.Length

let private linearChainOrder (dag: FiniteDag<'Obs, 'State>) =
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

let private compileLinearChainContext (dag: FiniteDag<'Obs, 'State>) =
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
    let prior = root.Node.InitialWeight |> Option.defaultWith (fun () -> failwith "Linear chain root must carry an initial distribution.")

    {
        Dag = dag
        Steps = steps
        RootInitialLogWeights = root.Node.States |> Array.map (fun stateValue -> logWeight (prior stateValue))
        Diagnostics = { PreparedContextUses = 0; SharedUpwardHits = 0; ParentPatternHits = 0 }
    }

let private compileDag (dag: FiniteDag<'Obs, 'State>) =
    match dag.Shape with
    | LinearChain -> LinearChainDag (compileLinearChainContext dag)
    | RootedTree -> RootedTreeDag (compileRootedTreeContext dag)

let private inferLinearChainSumProduct (context: CompiledLinearChainContext<'Obs, 'State>) =
    let dag = context.Dag
    let root = context.Steps.[0]
    let mutable current =
        root.Node.States
        |> Array.mapi (fun stateIndex _ -> context.RootInitialLogWeights.[stateIndex] + root.ObservationLogWeights.[stateIndex])

    for stepIndex in 1 .. context.Steps.Length - 1 do
        let step = context.Steps.[stepIndex]
        let next = Array.zeroCreate<float> step.Node.States.Length
        let transitionLogWeights = step.TransitionLogWeights |> Option.defaultWith (fun () -> failwith "Linear chain child nodes must have a transition weight.")

        for nextStateIndex in 0 .. step.Node.States.Length - 1 do
            let totals =
                [| for sourceStateIndex in 0 .. current.Length - 1 ->
                    current.[sourceStateIndex] + transitionLogWeights.[sourceStateIndex * step.Node.States.Length + nextStateIndex] |]

            next.[nextStateIndex] <- logSumExp totals + step.ObservationLogWeights.[nextStateIndex]

        current <- next

    let queryStep = context.Steps.[context.Steps.Length - 1]

    [ for stateIndex in 0 .. queryStep.Node.States.Length - 1 -> dag.Label queryStep.Node.States.[stateIndex], current.[stateIndex] ]
    |> exactPosteriorResultFromLog

let private inferLinearChainMaxProduct (context: CompiledLinearChainContext<'Obs, 'State>) =
    let dag = context.Dag
    let root = context.Steps.[0]
    let mutable current =
        root.Node.States
        |> Array.mapi (fun stateIndex _ -> context.RootInitialLogWeights.[stateIndex] + root.ObservationLogWeights.[stateIndex])

    for stepIndex in 1 .. context.Steps.Length - 1 do
        let step = context.Steps.[stepIndex]
        let next = Array.zeroCreate<float> step.Node.States.Length
        let transitionLogWeights = step.TransitionLogWeights |> Option.defaultWith (fun () -> failwith "Linear chain child nodes must have a transition weight.")

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

let private inferLinearChain engine dag =
    match engine with
    | SumProduct -> inferLinearChainSumProduct dag |> ExactPosterior
    | BeliefPropagation -> inferLinearChainSumProduct dag |> ExactPosterior
    | MaxProduct -> inferLinearChainMaxProduct dag |> MaxPosterior

let private inferRootedTreeSumProduct (context: CompiledRootedTreeContext<'Obs, 'State>) =
    let dag = context.Dag
    let upwards = System.Collections.Generic.Dictionary<int, float[]>()
    let sharedUpwards = System.Collections.Generic.Dictionary<int, float[]>()
    let sharedParentPatterns = System.Collections.Generic.Dictionary<int, CompiledParentPatternContributions>()

    let rec computeUpward nodeId =
        match upwards.TryGetValue nodeId with
        | true, cached -> cached
        | _ ->
            let key = context.SubtreeKeysById.[nodeId]

            match sharedUpwards.TryGetValue key with
            | true, shared ->
                context.Diagnostics.SharedUpwardHits <- context.Diagnostics.SharedUpwardHits + 1
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

    let _ = computeUpward dag.RootId
    let root = context.NodesById.[dag.RootId]
    let prior = root.Node.InitialWeight |> Option.defaultWith (fun () -> failwith "Root node must have an initial distribution.")

    if dag.QueryId = dag.RootId then
        let rootUpward = upwards.[dag.RootId]
        let rootBelief =
            root.Node.States
            |> Array.mapi (fun index stateValue -> logWeight (prior stateValue) + rootUpward.[index])

        root.Node.States
        |> Array.mapi (fun index stateValue -> dag.Label stateValue, rootBelief.[index])
        |> exactPosteriorResultFromLog
    else
        let outsides = System.Collections.Generic.Dictionary<int, float[]>()
        let rootOutside = root.Node.States |> Array.map (fun stateValue -> logWeight (prior stateValue))
        outsides.[dag.RootId] <- rootOutside

        let getParentPattern nodeId =
            let key = context.SubtreeKeysById.[nodeId]

            match sharedParentPatterns.TryGetValue key with
            | true, cached ->
                context.Diagnostics.ParentPatternHits <- context.Diagnostics.ParentPatternHits + 1
                cached
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
                        ChildContributions = childContributions
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

let private inferRootedTreeMaxProduct (context: CompiledRootedTreeContext<'Obs, 'State>) =
    let dag = context.Dag
    let upwards = System.Collections.Generic.Dictionary<int, float[]>()
    let sharedUpwards = System.Collections.Generic.Dictionary<int, float[]>()
    let sharedParentPatterns = System.Collections.Generic.Dictionary<int, CompiledParentPatternContributions>()

    let rec computeUpward nodeId =
        match upwards.TryGetValue nodeId with
        | true, cached -> cached
        | _ ->
            let key = context.SubtreeKeysById.[nodeId]

            match sharedUpwards.TryGetValue key with
            | true, shared ->
                context.Diagnostics.SharedUpwardHits <- context.Diagnostics.SharedUpwardHits + 1
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

    let root = context.NodesById.[dag.RootId]
    let prior = root.Node.InitialWeight |> Option.defaultWith (fun () -> failwith "Root node must have an initial distribution.")
    let _ = computeUpward dag.RootId

    if dag.QueryId = dag.RootId then
        let rootUpward = upwards.[dag.RootId]
        let rootBelief =
            root.Node.States
            |> Array.mapi (fun index stateValue -> logWeight (prior stateValue) + rootUpward.[index])

        root.Node.States
        |> Array.mapi (fun index stateValue -> dag.Label stateValue, rootBelief.[index])
        |> maxPosteriorResultFromLog
    else
        let outsides = System.Collections.Generic.Dictionary<int, float[]>()
        let rootOutside = root.Node.States |> Array.map (fun stateValue -> logWeight (prior stateValue))
        outsides.[dag.RootId] <- rootOutside

        let getParentPattern nodeId =
            let key = context.SubtreeKeysById.[nodeId]

            match sharedParentPatterns.TryGetValue key with
            | true, cached ->
                context.Diagnostics.ParentPatternHits <- context.Diagnostics.ParentPatternHits + 1
                cached
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
                        ChildContributions = childContributions
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

let private inferRootedTree engine context =
    match engine with
    | SumProduct -> inferRootedTreeSumProduct context |> ExactPosterior
    | BeliefPropagation -> inferRootedTreeSumProduct context |> ExactPosterior
    | MaxProduct -> inferRootedTreeMaxProduct context |> MaxPosterior

let private inferCompiledDag engine compiledDag =
    let diagnostics = diagnosticsOf compiledDag
    diagnostics.PreparedContextUses <- diagnostics.PreparedContextUses + 1

    match compiledDag with
    | LinearChainDag dag -> inferLinearChain engine dag
    | RootedTreeDag context -> inferRootedTree engine context

let private inferFiniteDag engine dag =
    compileDag dag |> inferCompiledDag engine

let private exactPosteriorOf result =
    match result with
    | ExactPosterior posterior -> posterior
    | MaxPosterior _ -> failwith "Expected an exact posterior result."

let private maxPosteriorOf result =
    match result with
    | MaxPosterior posterior -> posterior
    | ExactPosterior _ -> failwith "Expected a max-posterior result."

let private evaluateCompiledDag compiledDag =
    inferCompiledDag SumProduct compiledDag |> exactPosteriorOf |> fun result -> result.Posterior

let private benchmarkCompiledDag repeats compiledDag action =
    resetCompiledDiagnostics compiledDag
    let millisecondsPerRun = benchmark repeats (fun () -> action compiledDag)
    let diagnostics = snapshotCompiledDiagnostics compiledDag
    millisecondsPerRun, diagnostics

let private printCompiledBenchmark label repeats compiledDag =
    let millisecondsPerRun, diagnostics = benchmarkCompiledDag repeats compiledDag evaluateCompiledDag
    printfn "DP DAG ms/run (%s) %.3f" label millisecondsPerRun
    printfn "Prepared context uses (%s) %d" label diagnostics.PreparedContextUses

    if diagnostics.SharedUpwardHits > 0 || diagnostics.ParentPatternHits > 0 then
        printfn "Cache hits (%s): upward=%d parent-pattern=%d" label diagnostics.SharedUpwardHits diagnostics.ParentPatternHits

let internal prepareDag dag =
    compileDag dag

let internal evaluatePreparedDag compiledDag =
    evaluateCompiledDag compiledDag

let internal inferPreparedDag engine compiledDag =
    inferCompiledDag engine compiledDag

let internal inferPreparedDagByBeliefPropagation compiledDag =
    inferCompiledDag BeliefPropagation compiledDag

let private evaluateDag (dag: FiniteDag<'Obs, 'State>) =
    dag |> compileDag |> evaluateCompiledDag

let private printPhase1Case label observations compareAgainstHansei =
    let directDag = compileBinaryHmmChain observations
    let structuredDag = compileStructuredBinaryHmmChain observations
    let compiledDirectDag = compileDag directDag
    let compiledStructuredDag = compileDag structuredDag
    let directPosterior = evaluateCompiledDag compiledDirectDag
    let structuredPosterior = evaluateCompiledDag compiledStructuredDag
    let directEvidence = inferCompiledDag SumProduct compiledDirectDag |> exactPosteriorOf
    let structuredMax = inferCompiledDag MaxProduct compiledStructuredDag |> maxPosteriorOf
    printfn "\n=== %s ===" label
    printfn "Observation length: %d" observations.Length
    printTop "DP DAG posterior (direct IR)" 2 directPosterior
    printTop "DP DAG posterior (opt-in structured subset)" 2 structuredPosterior
    printfn "Evidence (sum-product, direct IR) = %.6f" directEvidence.Evidence
    printfn "Log evidence (sum-product, direct IR) = %.6f" directEvidence.LogEvidence
    printfn "MAP label (max-product, structured subset) = %s (score %.6f)" structuredMax.BestLabel structuredMax.Score
    printfn "MAP log-score (max-product, structured subset) = %.6f" structuredMax.LogScore
    printfn "L1(direct IR, opt-in structured subset) = %.6f" (l1Distance directPosterior structuredPosterior)

    if compareAgainstHansei then
        let hanseiPosterior = hiddenMarkovPosterior observations |> exact
        printTop "Hansei exact posterior" 2 hanseiPosterior
        printfn "L1(opt-in structured subset, Hansei exact) = %.6f" (l1Distance structuredPosterior hanseiPosterior)
        printCompiledBenchmark "direct IR" 50 compiledDirectDag
        printCompiledBenchmark "opt-in structured subset" 50 compiledStructuredDag
        printfn "Hansei exact ms/run      %.3f" (benchmark 10 (fun () -> hiddenMarkovPosterior observations |> exact))
    else
        printCompiledBenchmark "direct IR" 200 compiledDirectDag
        printCompiledBenchmark "opt-in structured subset" 200 compiledStructuredDag

let private printWeatherChainCase () =
    let observations = [| "umbrella"; "umbrella"; "no-umbrella"; "umbrella" |]
    let states = [| "sunny"; "cloudy"; "rainy" |]
    let initialWeight state =
        match state with
        | "sunny" -> 0.5
        | "cloudy" -> 0.3
        | "rainy" -> 0.2
        | _ -> 0.0

    let transitionWeight source target =
        match source, target with
        | "sunny", "sunny" -> 0.7
        | "sunny", "cloudy" -> 0.2
        | "sunny", "rainy" -> 0.1
        | "cloudy", "sunny" -> 0.2
        | "cloudy", "cloudy" -> 0.5
        | "cloudy", "rainy" -> 0.3
        | "rainy", "sunny" -> 0.1
        | "rainy", "cloudy" -> 0.3
        | "rainy", "rainy" -> 0.6
        | _ -> 0.0

    let observationWeight state observation =
        match state, observation with
        | "sunny", "umbrella" -> 0.1
        | "sunny", "no-umbrella" -> 0.9
        | "cloudy", "umbrella" -> 0.5
        | "cloudy", "no-umbrella" -> 0.5
        | "rainy", "umbrella" -> 0.9
        | "rainy", "no-umbrella" -> 0.1
        | _ -> 0.0

    let label state = sprintf "final=%s" state
    let directDag = compileFiniteChain observations states initialWeight transitionWeight observationWeight label
    let structuredDag = compileStructuredFiniteChain observations states initialWeight transitionWeight observationWeight label
    let compiledDirectDag = compileDag directDag
    let compiledStructuredDag = compileDag structuredDag
    let directPosterior = evaluateCompiledDag compiledDirectDag
    let structuredPosterior = evaluateCompiledDag compiledStructuredDag
    let directEvidence = inferCompiledDag SumProduct compiledDirectDag |> exactPosteriorOf
    let directMax = inferCompiledDag MaxProduct compiledDirectDag |> maxPosteriorOf

    printfn "\n=== Weather Chain Example ==="
    printfn "Observation length: %d" observations.Length
    printTop "DP DAG posterior (direct IR)" 3 directPosterior
    printTop "DP DAG posterior (opt-in structured subset)" 3 structuredPosterior
    printfn "Evidence (sum-product, direct IR) = %.6f" directEvidence.Evidence
    printfn "Log evidence (sum-product, direct IR) = %.6f" directEvidence.LogEvidence
    printfn "MAP label (max-product, direct IR) = %s (score %.6f)" directMax.BestLabel directMax.Score
    printfn "MAP log-score (max-product, direct IR) = %.6f" directMax.LogScore
    printfn "L1(direct IR, opt-in structured subset) = %.6f" (l1Distance directPosterior structuredPosterior)
    printCompiledBenchmark "direct IR" 200 compiledDirectDag
    printCompiledBenchmark "opt-in structured subset" 200 compiledStructuredDag

let private compileBranchingWeatherDag () =
    let states = [| "sunny"; "cloudy"; "rainy" |]
    let initialWeight state =
        match state with
        | "sunny" -> 0.5
        | "cloudy" -> 0.3
        | "rainy" -> 0.2
        | _ -> 0.0

    let sensorTransition source target =
        match source, target with
        | "sunny", "sunny" -> 0.8
        | "sunny", "cloudy" -> 0.15
        | "sunny", "rainy" -> 0.05
        | "cloudy", "sunny" -> 0.2
        | "cloudy", "cloudy" -> 0.6
        | "cloudy", "rainy" -> 0.2
        | "rainy", "sunny" -> 0.05
        | "rainy", "cloudy" -> 0.25
        | "rainy", "rainy" -> 0.7
        | _ -> 0.0

    let umbrellaWeight state observation =
        match state, observation with
        | "sunny", true -> 0.1
        | "sunny", false -> 0.9
        | "cloudy", true -> 0.5
        | "cloudy", false -> 0.5
        | "rainy", true -> 0.9
        | "rainy", false -> 0.1
        | _ -> 0.0

    let trafficWeight state observation =
        match state, observation with
        | "sunny", true -> 0.2
        | "sunny", false -> 0.8
        | "cloudy", true -> 0.6
        | "cloudy", false -> 0.4
        | "rainy", true -> 0.85
        | "rainy", false -> 0.15
        | _ -> 0.0

    {
        Nodes =
            [|
                {
                    Id = 0
                    ParentId = None
                    States = states
                    InitialWeight = Some initialWeight
                    TransitionWeight = None
                    Observation = None
                }
                {
                    Id = 1
                    ParentId = Some 0
                    States = states
                    InitialWeight = None
                    TransitionWeight = Some sensorTransition
                    Observation = Some (true, umbrellaWeight)
                }
                {
                    Id = 2
                    ParentId = Some 0
                    States = states
                    InitialWeight = None
                    TransitionWeight = Some sensorTransition
                    Observation = Some (true, trafficWeight)
                }
            |]
        RootId = 0
        QueryId = 0
        Label = fun state -> sprintf "weather=%s" state
        Shape = RootedTree
    }

module private StructuredDagCompiler =
    open DagDsl

    let compileBranchingWeatherDag () =
        let states = [| "sunny"; "cloudy"; "rainy" |]
        let initialWeight state =
            match state with
            | "sunny" -> 0.5
            | "cloudy" -> 0.3
            | "rainy" -> 0.2
            | _ -> 0.0

        let sensorTransition source target =
            match source, target with
            | "sunny", "sunny" -> 0.8
            | "sunny", "cloudy" -> 0.15
            | "sunny", "rainy" -> 0.05
            | "cloudy", "sunny" -> 0.2
            | "cloudy", "cloudy" -> 0.6
            | "cloudy", "rainy" -> 0.2
            | "rainy", "sunny" -> 0.05
            | "rainy", "cloudy" -> 0.25
            | "rainy", "rainy" -> 0.7
            | _ -> 0.0

        let umbrellaWeight state observation =
            match state, observation with
            | "sunny", true -> 0.1
            | "sunny", false -> 0.9
            | "cloudy", true -> 0.5
            | "cloudy", false -> 0.5
            | "rainy", true -> 0.9
            | "rainy", false -> 0.1
            | _ -> 0.0

        let trafficWeight state observation =
            match state, observation with
            | "sunny", true -> 0.2
            | "sunny", false -> 0.8
            | "cloudy", true -> 0.6
            | "cloudy", false -> 0.4
            | "rainy", true -> 0.85
            | "rainy", false -> 0.15
            | _ -> 0.0

        compileTreeProgram
            (dag {
                let! weather = finite "Weather" states initialWeight
                let! umbrellaSensor = child weather "UmbrellaSensor" sensorTransition
                let! trafficSensor = child weather "TrafficSensor" sensorTransition
                do! observe umbrellaSensor umbrellaWeight true
                do! observe trafficSensor trafficWeight true
                return! query weather (fun state -> sprintf "weather=%s" state)
            })

let private compileStructuredBranchingWeatherDag () =
    StructuredDagCompiler.compileBranchingWeatherDag ()

let private compileRepeatedSubtreeDag repeatedChildCount =
    let states = [| "sunny"; "cloudy"; "rainy" |]
    let initialWeight =
        buildProbabilityLookup
            [ "sunny", 0.5
              "cloudy", 0.3
              "rainy", 0.2 ]

    let sensorTransition =
        buildPairProbabilityLookup
            [ ("sunny", "sunny"), 0.8
              ("sunny", "cloudy"), 0.15
              ("sunny", "rainy"), 0.05
              ("cloudy", "sunny"), 0.2
              ("cloudy", "cloudy"), 0.6
              ("cloudy", "rainy"), 0.2
              ("rainy", "sunny"), 0.05
              ("rainy", "cloudy"), 0.25
              ("rainy", "rainy"), 0.7 ]

    let repeatedObservationWeight =
        buildPairProbabilityLookup
            [ ("sunny", "alert"), 0.2
              ("sunny", "quiet"), 0.8
              ("cloudy", "alert"), 0.6
              ("cloudy", "quiet"), 0.4
              ("rainy", "alert"), 0.85
              ("rainy", "quiet"), 0.15 ]

    {
        Nodes =
            [|
                yield
                    {
                        Id = 0
                        ParentId = None
                        States = states
                        InitialWeight = Some initialWeight
                        TransitionWeight = None
                        Observation = None
                    }

                for childIndex in 1 .. repeatedChildCount do
                    yield
                        {
                            Id = childIndex
                            ParentId = Some 0
                            States = states
                            InitialWeight = None
                            TransitionWeight = Some sensorTransition
                            Observation = Some ("alert", repeatedObservationWeight)
                        }
            |]
        RootId = 0
        QueryId = 0
        Label = fun state -> sprintf "weather=%s" state
        Shape = RootedTree
    }

type private TrieMatch =
    {
        EndExclusive: int
        WordId: int
        LogWeight: float
    }

type private TrieWord =
    {
        Id: int
        Text: string
        LogWeight: float
    }

type private TrieNode =
    {
        Id: int
        ChildCharacters: char[]
        ChildIds: int[]
        TerminalWordId: int option
    }

type private TrieLexicon =
    {
        Nodes: TrieNode[]
        RootId: int
        Words: TrieWord[]
    }

type private TrieTokenCatalog =
    {
        Lexicon: TrieLexicon
        DynamicWordIds: System.Collections.Generic.Dictionary<string, int>
        DynamicWords: ResizeArray<string>
    }

type private UnknownTokenGenerator = TrieTokenCatalog -> string -> int -> TrieMatch list

type private UnknownCharacterClass =
    | Letter
    | Digit
    | WhiteSpace
    | Punctuation
    | Other

type private TrieSegmentationCandidate =
    {
        WordIds: int list
        LogScore: float
    }

type private CompiledTrieBoundaryWeights =
    {
        StartLogWeights: float[]
        TransitionLogWeights: float[]
        FallbackLogWeight: string option -> string -> float
    }

type private TrieSegmentationModel =
    {
        Lexicon: TrieLexicon
        BoundaryWeights: CompiledTrieBoundaryWeights
        UnknownTokenGenerator: UnknownTokenGenerator
    }

type private PreparedTrieSegmentationContext =
    {
        Model: TrieSegmentationModel
        Text: string
        Catalog: TrieTokenCatalog
        KnownMatchesByStart: TrieMatch[][]
        UnknownMatchesByStart: System.Collections.Generic.Dictionary<int, TrieMatch list>
        BoundaryMemo: System.Collections.Generic.Dictionary<struct (int option * int), float>
    }

type private BoundaryLogWeightTemplate = string option -> string -> float

let private buildTrieLexicon (entries: (string * float) list) =
    let childrenById = System.Collections.Generic.Dictionary<int, System.Collections.Generic.Dictionary<char, int>>()
    let terminalsById = System.Collections.Generic.Dictionary<int, int>()
    let mutable nextId = 1
    let rootId = 0
    childrenById.[rootId] <- System.Collections.Generic.Dictionary<char, int>()

    let words =
        entries
        |> List.mapi (fun wordId (word, weight) ->
            {
                Id = wordId
                Text = word
                LogWeight = logWeight weight
            })
        |> List.toArray

    for wordEntry in words do
        let mutable nodeId = rootId

        for character in wordEntry.Text do
            let children = childrenById.[nodeId]

            let childId =
                match children.TryGetValue character with
                | true, existing -> existing
                | _ ->
                    let created = nextId
                    nextId <- nextId + 1
                    children.[character] <- created
                    childrenById.[created] <- System.Collections.Generic.Dictionary<char, int>()
                    created

            nodeId <- childId

        terminalsById.[nodeId] <- wordEntry.Id

    let nodes =
        Array.init nextId (fun nodeId ->
            let children = childrenById.[nodeId] |> Seq.map (fun (KeyValue(character, childId)) -> character, childId) |> Seq.sortBy fst |> Seq.toArray
            let terminalWordId =
                match terminalsById.TryGetValue nodeId with
                | true, entry -> Some entry
                | _ -> None

            {
                Id = nodeId
                ChildCharacters = children |> Array.map fst
                ChildIds = children |> Array.map snd
                TerminalWordId = terminalWordId
            })

    {
        Nodes = nodes
        RootId = rootId
        Words = words
    }

let private noBoundaryCost _ _ = 0.0

let private noUnknownTokenGenerator : UnknownTokenGenerator = fun _ _ _ -> []

let private classifyUnknownCharacter (character: char) =
    if Char.IsLetter character then
        Letter
    elif Char.IsDigit character then
        Digit
    elif Char.IsWhiteSpace character then
        WhiteSpace
    elif Char.IsPunctuation character then
        Punctuation
    else
        Other

let private combineBoundaryCostTemplates (templates: BoundaryLogWeightTemplate list) : BoundaryLogWeightTemplate =
    fun previousWord currentWord ->
        templates |> List.sumBy (fun template -> template previousWord currentWord)

let private buildPerBreakLogWeight weight : BoundaryLogWeightTemplate =
    let breakLogWeight = logWeight weight
    fun _ _ -> breakLogWeight

let private buildSimpleBigramWordModelLogWeight defaultWeight (entries: (((string option) * string) * float) list) : BoundaryLogWeightTemplate =
    let table = entries |> Map.ofList
    let defaultLogWeight = logWeight defaultWeight

    fun previousWord currentWord ->
        table
        |> Map.tryFind (previousWord, currentWord)
        |> Option.map logWeight
        |> Option.defaultValue defaultLogWeight

let private createTrieTokenCatalog lexicon =
    {
        Lexicon = lexicon
        DynamicWordIds = System.Collections.Generic.Dictionary<string, int>()
        DynamicWords = ResizeArray<string>()
    }

let private knownTrieWordCount (catalog: TrieTokenCatalog) =
    catalog.Lexicon.Words.Length

let private trieTokenText (catalog: TrieTokenCatalog) wordId =
    if wordId < knownTrieWordCount catalog then
        catalog.Lexicon.Words.[wordId].Text
    else
        catalog.DynamicWords.[wordId - knownTrieWordCount catalog]

let private internDynamicTrieToken (catalog: TrieTokenCatalog) tokenText =
    match catalog.DynamicWordIds.TryGetValue tokenText with
    | true, cached -> cached
    | _ ->
        let wordId = knownTrieWordCount catalog + catalog.DynamicWords.Count
        catalog.DynamicWords.Add tokenText
        catalog.DynamicWordIds.[tokenText] <- wordId
        wordId

let private compileTrieBoundaryWeights (lexicon: TrieLexicon) (boundaryLogWeight: BoundaryLogWeightTemplate) =
    let words = lexicon.Words
    let wordCount = words.Length

    {
        StartLogWeights = words |> Array.map (fun word -> boundaryLogWeight None word.Text)
        TransitionLogWeights =
            [|
                for previousWord in words do
                    for currentWord in words do
                        yield boundaryLogWeight (Some previousWord.Text) currentWord.Text
            |]
        FallbackLogWeight = boundaryLogWeight
    }

let private trieBoundaryLogWeight (model: TrieSegmentationModel) (catalog: TrieTokenCatalog) previousWordId currentWordId =
    let knownWordCount = model.Lexicon.Words.Length

    if currentWordId < knownWordCount then
        match previousWordId with
        | None -> model.BoundaryWeights.StartLogWeights.[currentWordId]
        | Some previousKnownWordId when previousKnownWordId < knownWordCount ->
            model.BoundaryWeights.TransitionLogWeights.[previousKnownWordId * knownWordCount + currentWordId]
        | Some previousWordId ->
            model.BoundaryWeights.FallbackLogWeight (Some (trieTokenText catalog previousWordId)) model.Lexicon.Words.[currentWordId].Text
    else
        model.BoundaryWeights.FallbackLogWeight (previousWordId |> Option.map (trieTokenText catalog)) (trieTokenText catalog currentWordId)

let private buildUnknownCharacterLogWeight defaultClassWeight (classEntries: (UnknownCharacterClass * float) list) (characterEntries: (char * float) list) =
    let classTable = classEntries |> Map.ofList
    let characterTable = characterEntries |> Map.ofList
    let defaultClassLogWeight = logWeight defaultClassWeight

    fun character ->
        characterTable
        |> Map.tryFind character
        |> Option.map logWeight
        |> Option.defaultWith (fun () ->
            classTable
            |> Map.tryFind (classifyUnknownCharacter character)
            |> Option.map logWeight
            |> Option.defaultValue defaultClassLogWeight)

let private buildUnknownEndLogWeight defaultWeight (classEntries: (UnknownCharacterClass * float) list) (characterEntries: (char * float) list) =
    let classTable = classEntries |> Map.ofList
    let characterTable = characterEntries |> Map.ofList
    let defaultLogWeight = logWeight defaultWeight

    fun (terminalCharacter: char option) ->
        match terminalCharacter with
        | None -> defaultLogWeight
        | Some character ->
            characterTable
            |> Map.tryFind character
            |> Option.map logWeight
            |> Option.defaultWith (fun () ->
                classTable
                |> Map.tryFind (classifyUnknownCharacter character)
                |> Option.map logWeight
                |> Option.defaultValue defaultLogWeight)

let private buildBoundedUnknownUnigramTokenGeneratorWithScorers maxLength tokenWeight characterLogWeight endLogWeight : UnknownTokenGenerator =
    let tokenLogWeight = logWeight tokenWeight

    fun catalog text startIndex ->
        let maxEndExclusive = min text.Length (startIndex + maxLength)

        [ startIndex + 1 .. maxEndExclusive ]
        |> List.map (fun endExclusive ->
            let chunk = text.Substring(startIndex, endExclusive - startIndex)
            let charLogWeight = chunk |> Seq.sumBy characterLogWeight
            let endOfUnknownLogWeight =
                if String.IsNullOrEmpty chunk then endLogWeight None else endLogWeight (Some chunk.[chunk.Length - 1])

            {
                EndExclusive = endExclusive
                WordId = internDynamicTrieToken catalog ("<unk:" + chunk + ">")
                LogWeight = tokenLogWeight + charLogWeight + endOfUnknownLogWeight
            })

let private buildBoundedUnknownBigramTokenGeneratorWithScorers maxLength tokenWeight transitionLogWeight endLogWeight : UnknownTokenGenerator =
    let tokenLogWeight = logWeight tokenWeight

    fun catalog text startIndex ->
        let maxEndExclusive = min text.Length (startIndex + maxLength)

        [ startIndex + 1 .. maxEndExclusive ]
        |> List.map (fun endExclusive ->
            let chunk = text.Substring(startIndex, endExclusive - startIndex)

            let previousCharacter, transitionScore =
                chunk
                |> Seq.fold
                    (fun (previousCharacter, accLogWeight) character ->
                        Some character, accLogWeight + transitionLogWeight previousCharacter character)
                    (None, tokenLogWeight)

            {
                EndExclusive = endExclusive
                WordId = internDynamicTrieToken catalog ("<unk:" + chunk + ">")
                LogWeight = transitionScore + endLogWeight previousCharacter
            })

let private buildClassWeightedUnknownUnigramTokenGenerator maxLength tokenWeight defaultCharacterClassWeight classEntries characterEntries defaultEndWeight endClassEntries endCharacterEntries : UnknownTokenGenerator =
    let characterLogWeight = buildUnknownCharacterLogWeight defaultCharacterClassWeight classEntries characterEntries
    let endLogWeight = buildUnknownEndLogWeight defaultEndWeight endClassEntries endCharacterEntries
    buildBoundedUnknownUnigramTokenGeneratorWithScorers maxLength tokenWeight characterLogWeight endLogWeight

let private buildClassWeightedUnknownBigramTokenGenerator maxLength tokenWeight defaultTransitionWeight (entries: (((char option) * char) * float) list) defaultEndWeight endClassEntries endCharacterEntries : UnknownTokenGenerator =
    let table = entries |> Map.ofList
    let defaultTransitionLogWeight = logWeight defaultTransitionWeight

    let transitionLogWeight previousCharacter character =
        table
        |> Map.tryFind (previousCharacter, character)
        |> Option.map logWeight
        |> Option.defaultValue defaultTransitionLogWeight

    let endLogWeight = buildUnknownEndLogWeight defaultEndWeight endClassEntries endCharacterEntries
    buildBoundedUnknownBigramTokenGeneratorWithScorers maxLength tokenWeight transitionLogWeight endLogWeight

let private buildBoundedUnknownUnigramTokenGenerator maxLength tokenWeight defaultCharacterWeight (entries: (char * float) list) : UnknownTokenGenerator =
    buildClassWeightedUnknownUnigramTokenGenerator
        maxLength
        tokenWeight
        defaultCharacterWeight
        []
        entries
        1.0
        []
        []

let private buildBoundedUnknownBigramTokenGenerator maxLength tokenWeight defaultTransitionWeight (entries: (((char option) * char) * float) list) : UnknownTokenGenerator =
    buildClassWeightedUnknownBigramTokenGenerator
        maxLength
        tokenWeight
        defaultTransitionWeight
        entries
        1.0
        []
        []

let private buildTrieSegmentationModelWithUnknown lexiconEntries boundaryLogWeight unknownTokenGenerator =
    let lexicon = buildTrieLexicon lexiconEntries

    {
        Lexicon = lexicon
        BoundaryWeights = compileTrieBoundaryWeights lexicon boundaryLogWeight
        UnknownTokenGenerator = unknownTokenGenerator
    }

let private buildTrieSegmentationModel lexiconEntries boundaryLogWeight =
    buildTrieSegmentationModelWithUnknown lexiconEntries boundaryLogWeight noUnknownTokenGenerator

let private buildWordTransitionLogWeight defaultWeight (entries: (((string option) * string) * float) list) =
    buildSimpleBigramWordModelLogWeight defaultWeight entries

let private tryFindTrieChildId (node: TrieNode) character =
    let index = Array.BinarySearch(node.ChildCharacters, character)

    if index >= 0 then
        Some node.ChildIds.[index]
    else
        None

let private trieMatchesAt (model: TrieSegmentationModel) (text: string) startIndex =
    let mutable nodeId = model.Lexicon.RootId
    let matches = ResizeArray<TrieMatch>()
    let mutable index = startIndex
    let mutable keepScanning = true

    while keepScanning && index < text.Length do
        let node = model.Lexicon.Nodes.[nodeId]

        match tryFindTrieChildId node text.[index] with
        | Some childId ->
            nodeId <- childId
            index <- index + 1

            match model.Lexicon.Nodes.[nodeId].TerminalWordId with
            | Some wordId ->
                matches.Add
                    {
                        EndExclusive = index
                        WordId = wordId
                        LogWeight = model.Lexicon.Words.[wordId].LogWeight
                    }
            | None -> ()
        | None ->
            keepScanning <- false

    matches |> Seq.toList

let private tokenMatchesAt (model: TrieSegmentationModel) (catalog: TrieTokenCatalog) (text: string) startIndex =
    let knownMatches = trieMatchesAt model text startIndex

    match knownMatches with
    | _ :: _ -> knownMatches
    | [] -> model.UnknownTokenGenerator catalog text startIndex

let private buildKnownTrieMatchesByStart (model: TrieSegmentationModel) (text: string) =
    Array.init (text.Length + 1) (fun startIndex ->
        if startIndex < text.Length then
            trieMatchesAt model text startIndex |> List.toArray
        else
            [||])

let private prepareTrieSegmentationContext (model: TrieSegmentationModel) (text: string) =
    {
        Model = model
        Text = text
        Catalog = createTrieTokenCatalog model.Lexicon
        KnownMatchesByStart = buildKnownTrieMatchesByStart model text
        UnknownMatchesByStart = System.Collections.Generic.Dictionary<int, TrieMatch list>()
        BoundaryMemo = System.Collections.Generic.Dictionary<struct (int option * int), float>()
    }

let private preparedTrieTokenMatches (context: PreparedTrieSegmentationContext) startIndex =
    let knownMatches = context.KnownMatchesByStart.[startIndex]

    if knownMatches.Length > 0 then
        knownMatches |> Array.toList
    else
        match context.UnknownMatchesByStart.TryGetValue startIndex with
        | true, cached -> cached
        | _ ->
            let matches = context.Model.UnknownTokenGenerator context.Catalog context.Text startIndex
            context.UnknownMatchesByStart.[startIndex] <- matches
            matches

let private preparedTrieBoundaryLogWeight (context: PreparedTrieSegmentationContext) previousWordId currentWordId =
    let knownWordCount = context.Model.Lexicon.Words.Length

    if currentWordId < knownWordCount then
        match previousWordId with
        | None -> context.Model.BoundaryWeights.StartLogWeights.[currentWordId]
        | Some previousKnownWordId when previousKnownWordId < knownWordCount ->
            context.Model.BoundaryWeights.TransitionLogWeights.[previousKnownWordId * knownWordCount + currentWordId]
        | _ ->
            let key = struct (previousWordId, currentWordId)

            match context.BoundaryMemo.TryGetValue key with
            | true, cached -> cached
            | _ ->
                let computed = trieBoundaryLogWeight context.Model context.Catalog previousWordId currentWordId
                context.BoundaryMemo.[key] <- computed
                computed
    else
        let key = struct (previousWordId, currentWordId)

        match context.BoundaryMemo.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let computed = trieBoundaryLogWeight context.Model context.Catalog previousWordId currentWordId
            context.BoundaryMemo.[key] <- computed
            computed

let private trieLabelOfWordIds (catalog: TrieTokenCatalog) wordIds =
    wordIds
    |> List.map (trieTokenText catalog)
    |> String.concat " | "

let private trieExactPosteriorResultFromLog (catalog: TrieTokenCatalog) entries =
    let rawLog =
        entries
        |> Seq.fold (fun acc (wordIds, logValue) -> addLogWeight wordIds logValue acc) Map.empty

    let logEvidence = rawLog |> Seq.map (fun kv -> kv.Value) |> Seq.toArray |> logSumExp
    let posterior =
        rawLog
        |> Seq.map (fun (KeyValue(wordIds, logValue)) ->
            let probability =
                if Double.IsNegativeInfinity logValue || Double.IsNegativeInfinity logEvidence then 0.0
                else exp (logValue - logEvidence)

            trieLabelOfWordIds catalog wordIds, probability)
        |> Map.ofSeq

    let evidence = if Double.IsNegativeInfinity logEvidence then 0.0 else exp logEvidence

    {
        Posterior = posterior
        Evidence = evidence
        LogEvidence = logEvidence
    }

let private inferPreparedTrieSegmentationExact (context: PreparedTrieSegmentationContext) =
    let memo = System.Collections.Generic.Dictionary<struct (int * int option), (int list * float) list>()

    let rec segmentationsFrom startIndex previousWord =
        let key = struct (startIndex, previousWord)

        match memo.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let segmentations =
                if startIndex = context.Text.Length then
                    [ [], 0.0 ]
                else
                    preparedTrieTokenMatches context startIndex
                    |> List.collect (fun trieMatch ->
                        let localLogWeight = preparedTrieBoundaryLogWeight context previousWord trieMatch.WordId + trieMatch.LogWeight

                        segmentationsFrom trieMatch.EndExclusive (Some trieMatch.WordId)
                        |> List.map (fun (suffixWordIds, suffixLogWeight) ->
                            trieMatch.WordId :: suffixWordIds, localLogWeight + suffixLogWeight))

            memo.[key] <- segmentations
            segmentations

    segmentationsFrom 0 None |> trieExactPosteriorResultFromLog context.Catalog

let private inferTrieSegmentationExact (model: TrieSegmentationModel) (text: string) =
    prepareTrieSegmentationContext model text |> inferPreparedTrieSegmentationExact

let private inferPreparedTrieSegmentationTopKMaxProduct topK (context: PreparedTrieSegmentationContext) =
    let memo = System.Collections.Generic.Dictionary<struct (int * int option), TrieSegmentationCandidate list>()

    let rec topKFrom startIndex previousWord =
        let key = struct (startIndex, previousWord)

        match memo.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let candidates =
                if startIndex = context.Text.Length then
                    [ { WordIds = []; LogScore = 0.0 } ]
                else
                    preparedTrieTokenMatches context startIndex
                    |> List.collect (fun trieMatch ->
                        let localLogWeight = preparedTrieBoundaryLogWeight context previousWord trieMatch.WordId + trieMatch.LogWeight

                        topKFrom trieMatch.EndExclusive (Some trieMatch.WordId)
                        |> List.map (fun suffix ->
                            {
                                WordIds = trieMatch.WordId :: suffix.WordIds
                                LogScore = localLogWeight + suffix.LogScore
                            }))
                    |> List.distinctBy (fun candidate -> candidate.WordIds)
                    |> List.sortByDescending (fun candidate -> candidate.LogScore)
                    |> List.truncate topK

            memo.[key] <- candidates
            candidates

    topKFrom 0 None
    |> List.map (fun candidate ->
        let label = trieLabelOfWordIds context.Catalog candidate.WordIds
        let score = if Double.IsNegativeInfinity candidate.LogScore then 0.0 else exp candidate.LogScore

        {
            BestLabel = label
            Score = score
            LogScore = candidate.LogScore
        })

let private inferTrieSegmentationTopKMaxProduct topK (model: TrieSegmentationModel) (text: string) =
    prepareTrieSegmentationContext model text |> inferPreparedTrieSegmentationTopKMaxProduct topK

let private inferTrieSegmentationMaxProduct (model: TrieSegmentationModel) (text: string) =
    match inferTrieSegmentationTopKMaxProduct 1 model text with
    | best :: _ -> best
    | [] ->
        {
            BestLabel = "<no segmentation>"
            Score = 0.0
            LogScore = negativeInfinity
        }

type private TriePartialCandidate =
    {
        Position: int
        PreviousWordId: int option
        WordIdsReversed: int list
        LogScore: float
    }

let private inferPreparedTrieSegmentationBeamApprox beamWidth (context: PreparedTrieSegmentationContext) =
    let rec loop frontier completed =
        match frontier with
        | [] -> completed |> trieExactPosteriorResultFromLog context.Catalog
        | _ ->
            let expandedBuffer = ResizeArray<TriePartialCandidate>()
            let completedBuffer = ResizeArray<int list * float>()

            for candidate in frontier do
                if candidate.Position = context.Text.Length then
                    completedBuffer.Add (List.rev candidate.WordIdsReversed, candidate.LogScore)
                else
                    for trieMatch in preparedTrieTokenMatches context candidate.Position do
                        let nextLogScore = candidate.LogScore + preparedTrieBoundaryLogWeight context candidate.PreviousWordId trieMatch.WordId + trieMatch.LogWeight

                        expandedBuffer.Add
                            {
                                Position = trieMatch.EndExclusive
                                PreviousWordId = Some trieMatch.WordId
                                WordIdsReversed = trieMatch.WordId :: candidate.WordIdsReversed
                                LogScore = nextLogScore
                            }

            let nextFrontier =
                if expandedBuffer.Count = 0 then
                    []
                else
                    let expanded = expandedBuffer.ToArray()
                    System.Array.Sort(expanded, System.Comparison<TriePartialCandidate>(fun left right -> compare right.LogScore left.LogScore))
                    expanded
                    |> Seq.truncate beamWidth
                    |> Seq.toList

            let allCompleted =
                if completedBuffer.Count = 0 then
                    completed
                else
                    completedBuffer |> Seq.toList |> List.append completed

            loop nextFrontier allCompleted

    loop [ { Position = 0; PreviousWordId = None; WordIdsReversed = []; LogScore = 0.0 } ] []

let private inferTrieSegmentationBeamApprox beamWidth (model: TrieSegmentationModel) (text: string) =
    prepareTrieSegmentationContext model text |> inferPreparedTrieSegmentationBeamApprox beamWidth

let private prepareTrieSegmentation model text =
    prepareTrieSegmentationContext model text

let private inferPreparedTrieSegmentationExactContext prepared =
    inferPreparedTrieSegmentationExact prepared

let private inferPreparedTrieSegmentationTopKContext topK prepared =
    inferPreparedTrieSegmentationTopKMaxProduct topK prepared

let private inferPreparedTrieSegmentationBeamContext beamWidth prepared =
    inferPreparedTrieSegmentationBeamApprox beamWidth prepared

let private trieSegmentationExactSubmodelForModel (model: TrieSegmentationModel) (text: string) : ProbabilitySpace<string> =
    let posterior = prepareTrieSegmentationContext model text |> inferPreparedTrieSegmentationExact

    posterior.Posterior
    |> Map.toList
    |> categorical

let internal trieSegmentationExactSubmodel lexiconEntries boundaryLogWeight (text: string) : ProbabilitySpace<string> =
    let model = buildTrieSegmentationModel lexiconEntries boundaryLogWeight
    trieSegmentationExactSubmodelForModel model text

let private topKMapLabels topK (model: TrieSegmentationModel) (text: string) =
    prepareTrieSegmentationContext model text |> inferPreparedTrieSegmentationTopKMaxProduct topK

let private printTopKMapSegmentations title topK candidates =
    printfn "%s" title

    candidates
    |> List.truncate topK
    |> List.iteri (fun index candidate ->
        printfn "  #%d %-23s log=%.6f score=%.6f" (index + 1) candidate.BestLabel candidate.LogScore candidate.Score)

let private printTrieSegmentationSummary title topK beamWidth repeats (model: TrieSegmentationModel) (text: string) =
    let prepared = prepareTrieSegmentationContext model text
    let exactSegmentation = inferPreparedTrieSegmentationExact prepared
    let approximateSegmentation = inferPreparedTrieSegmentationBeamApprox beamWidth prepared
    let topKMap = inferPreparedTrieSegmentationTopKMaxProduct topK prepared
    let maxSegmentation = topKMap |> List.tryHead |> Option.defaultValue { BestLabel = "<no segmentation>"; Score = 0.0; LogScore = negativeInfinity }
    let adapterPosterior = trieSegmentationExactSubmodelForModel model text |> exact
    let benchmarkPrepared = prepareTrieSegmentationContext model text

    printfn "\n=== %s ===" title
    printfn "Observed text: %s" text
    printfn "Total segmentations: %d" exactSegmentation.Posterior.Count
    printTopKSegmentations (sprintf "Top-%d trie segmentations" topK) topK exactSegmentation.Posterior
    printTopKSegmentations (sprintf "Top-%d trie beam-approx segmentations (beam=%d)" topK beamWidth) topK approximateSegmentation.Posterior
    printTopKMapSegmentations (sprintf "Top-%d trie MAP paths" topK) topK topKMap
    printfn "Evidence (trie exact) = %.6f" exactSegmentation.Evidence
    printfn "Log evidence (trie exact) = %.6f" exactSegmentation.LogEvidence
    printfn "MAP segmentation (trie max-product) = %s (score %.6f)" maxSegmentation.BestLabel maxSegmentation.Score
    printfn "MAP log-score (trie max-product) = %.6f" maxSegmentation.LogScore
    printfn "L1(trie exact, Hansei adapter exact) = %.6f" (l1Distance exactSegmentation.Posterior adapterPosterior)
    printfn "L1(trie exact, trie beam approx) = %.6f" (l1Distance exactSegmentation.Posterior approximateSegmentation.Posterior)
    printfn "Trie exact ms/run %.3f" (benchmark repeats (fun () -> inferPreparedTrieSegmentationExact benchmarkPrepared |> ignore))
    printfn "Trie beam-approx ms/run %.3f" (benchmark repeats (fun () -> inferPreparedTrieSegmentationBeamApprox beamWidth benchmarkPrepared |> ignore))

let private printTrieSegmentationCase () =
    let model =
        buildTrieSegmentationModel
            [ "the", 0.55
              "rain", 0.35
              "therain", 0.15
              "he", 0.05
              "ran", 0.02 ]
            noBoundaryCost

    printTrieSegmentationSummary "Trie Segmentation Example" 4 6 500 model "therain"

let private printTrieSegmentationOovFallbackCase () =
    let boundaryLogWeight = combineBoundaryCostTemplates [ buildPerBreakLogWeight 0.98 ]

    let unknownTokenGenerator =
        buildClassWeightedUnknownUnigramTokenGenerator
            4
            0.015
            0.28
            ([ (Letter, 0.48)
               (Digit, 0.09)
               (Punctuation, 0.12)
               (Other, 0.18) ])
            ([ ('a', 0.60)
               ('e', 0.62)
               ('h', 0.44)
               ('i', 0.58)
               ('n', 0.55)
               ('r', 0.53)
               ('t', 0.56)
               ('x', 0.18)
               ('1', 0.08) ])
            0.82
            ([ (Letter, 0.92)
               (Digit, 0.35)
               (Punctuation, 0.45)
               (Other, 0.55) ])
            []

    let model =
        buildTrieSegmentationModelWithUnknown
            [ ("the", 0.55)
              ("rain", 0.35)
              ("therain", 0.15)
              ("he", 0.05)
              ("ran", 0.02) ]
            boundaryLogWeight
            unknownTokenGenerator

    printTrieSegmentationSummary "Trie Segmentation With OOV Fallback" 5 8 300 model "thera1n"

let private printLargeTrieSegmentationCase () =
    let boundaryLogWeight =
        combineBoundaryCostTemplates
            [ buildPerBreakLogWeight 0.97
              (buildSimpleBigramWordModelLogWeight
                  0.85
                  [ (None, "the"), 1.0
                    (Some "the", "rain"), 0.78
                    (Some "the", "rainbow"), 0.96
                    (Some "the", "rainbowman"), 0.88
                    (Some "rain", "bow"), 0.65
                    (Some "rain", "bowman"), 0.93
                    (Some "rainbow", "man"), 0.97
                    (Some "bow", "man"), 0.72
                    (Some "man", "and"), 0.94
                    (Some "and", "the"), 1.0
                    (Some "and", "therain"), 0.74 ]) ]

    let model =
        buildTrieSegmentationModel
            [ "the", 0.58
              "rain", 0.42
              "therain", 0.12
              "bow", 0.18
              "rainbow", 0.33
              "man", 0.32
              "bowman", 0.24
              "rainbowman", 0.09
              "and", 0.45
              "andthe", 0.08
              "there", 0.07
              "her", 0.04
              "he", 0.05
              "ran", 0.03
              "ant", 0.02 ]
            boundaryLogWeight

    printTrieSegmentationSummary "Larger Trie Segmentation Benchmark" 6 12 300 model "therainbowmanandtherain"

let private printRepeatedSubtreeCase () =
    let repeatedChildCount = 24
    let repeatedDag = compileRepeatedSubtreeDag repeatedChildCount
    let compiledRepeatedDag = compileDag repeatedDag
    let repeatedPosterior = evaluateCompiledDag compiledRepeatedDag
    let repeatedEvidence = inferCompiledDag SumProduct compiledRepeatedDag |> exactPosteriorOf
    let repeatedMax = inferCompiledDag MaxProduct compiledRepeatedDag |> maxPosteriorOf
    let totalNodes, uniqueSubproblems = rootedSubproblemKeyStatsOfCompiled compiledRepeatedDag

    printfn "\n=== Repeated Subtree Benchmark ==="
    printfn "Repeated children: %d" repeatedChildCount
    printfn "Rooted subproblems: %d total, %d unique structural keys" totalNodes uniqueSubproblems
    printTop "DP DAG posterior (shared repeated subtrees)" 3 repeatedPosterior
    printfn "Evidence (sum-product) = %.6f" repeatedEvidence.Evidence
    printfn "Log evidence (sum-product) = %.6f" repeatedEvidence.LogEvidence
    printfn "MAP label (max-product) = %s (score %.6f)" repeatedMax.BestLabel repeatedMax.Score
    printfn "MAP log-score (max-product) = %.6f" repeatedMax.LogScore
    printCompiledBenchmark "shared repeated subtrees" 500 compiledRepeatedDag

let private printBranchingDagCase () =
    let directDag = compileBranchingWeatherDag ()
    let structuredDag = compileStructuredBranchingWeatherDag ()
    let compiledDirectDag = compileDag directDag
    let compiledStructuredDag = compileDag structuredDag
    let directPosterior = evaluateCompiledDag compiledDirectDag
    let structuredPosterior = evaluateCompiledDag compiledStructuredDag
    let directEvidence = inferCompiledDag SumProduct compiledDirectDag |> exactPosteriorOf
    let structuredMax = inferCompiledDag MaxProduct compiledStructuredDag |> maxPosteriorOf
    let directChildQueryDag = { directDag with QueryId = 1; Label = fun state -> sprintf "umbrella-sensor=%s" state }
    let structuredChildQueryDag = { structuredDag with QueryId = 1; Label = fun state -> sprintf "umbrella-sensor=%s" state }
    let compiledDirectChildQueryDag = compileDag directChildQueryDag
    let compiledStructuredChildQueryDag = compileDag structuredChildQueryDag
    let directChildPosterior = evaluateCompiledDag compiledDirectChildQueryDag
    let structuredChildPosterior = evaluateCompiledDag compiledStructuredChildQueryDag

    printfn "\n=== Branching Weather DAG Example ==="
    printTop "DP DAG posterior (direct IR)" 3 directPosterior
    printTop "DP DAG posterior (opt-in structured subset)" 3 structuredPosterior
    printfn "Evidence (sum-product, direct IR) = %.6f" directEvidence.Evidence
    printfn "Log evidence (sum-product, direct IR) = %.6f" directEvidence.LogEvidence
    printfn "MAP label (max-product, structured subset) = %s (score %.6f)" structuredMax.BestLabel structuredMax.Score
    printfn "MAP log-score (max-product, structured subset) = %.6f" structuredMax.LogScore
    printfn "L1(direct IR, opt-in structured subset) = %.6f" (l1Distance directPosterior structuredPosterior)
    printTop "DP DAG child-query posterior (direct IR)" 3 directChildPosterior
    printTop "DP DAG child-query posterior (opt-in structured subset)" 3 structuredChildPosterior
    printfn "L1(child query direct IR, opt-in structured subset) = %.6f" (l1Distance directChildPosterior structuredChildPosterior)
    printCompiledBenchmark "direct IR" 500 compiledDirectDag
    printCompiledBenchmark "opt-in structured subset" 500 compiledStructuredDag

let shortObservations = [| true; true; true; false; true; true; false; true |]
let longObservations =
    [|
        true; true; true; false; true; true; false; true
        false; false; true; false; true; false; false; true
        true; false; true; true; false; true; false; false
        true; true; true; false; true; false; true; true
    |]

printfn "=== Dynamic Programming DAG Prototype ==="
printfn "Phase 1 prototype: explicit finite graph IR plus an opt-in structured subset that compiles into the same evaluator family."
printPhase1Case "Short HMM chain" shortObservations true
printPhase1Case "Long HMM chain" longObservations false
printWeatherChainCase ()
printBranchingDagCase ()
printRepeatedSubtreeCase ()
printTrieSegmentationCase ()
printTrieSegmentationOovFallbackCase ()
printLargeTrieSegmentationCase ()