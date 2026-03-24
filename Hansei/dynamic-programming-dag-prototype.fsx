#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open Hansei.Core.List
open Hansei.Core.List.Distributions

type private SamplerResult =
    { Name: string; Dist: Map<string, float> }

type private FiniteNode<'State> =
    {
        Id: int
        States: 'State[]
    }

type private FiniteChainStep<'Obs, 'State> =
    {
        Observation: 'Obs
        ObservationWeight: 'State -> 'Obs -> float
        TransitionWeight: ('State -> 'State -> float) option
    }

type private FiniteChainDag<'Obs, 'State when 'State: comparison> =
    {
        Hidden: FiniteNode<'State>
        InitialWeight: 'State -> float
        Steps: FiniteChainStep<'Obs, 'State>[]
        Label: 'State -> string
    }

type private FiniteTreeNode<'Obs, 'State when 'State: comparison> =
    {
        Id: int
        ParentId: int option
        States: 'State[]
        InitialWeight: ('State -> float) option
        TransitionWeight: ('State -> 'State -> float) option
        Observation: ('Obs * ('State -> 'Obs -> float)) option
    }

type private FiniteTreeDag<'Obs, 'State when 'State: comparison> =
    {
        Nodes: FiniteTreeNode<'Obs, 'State>[]
        RootId: int
        Label: 'State -> string
        IsLoweredChain: bool
    }

type private CompiledFiniteGraph<'Obs, 'State when 'State: comparison> =
    | ChainGraph of FiniteChainDag<'Obs, 'State>
    | TreeGraph of FiniteTreeDag<'Obs, 'State>

type private GraphInferenceEngine =
    | SumProduct
    | MaxProduct

type private ExactPosteriorResult =
    {
        Posterior: Map<string, float>
        Evidence: float
    }

type private MaxPosteriorResult =
    {
        BestLabel: string
        Score: float
    }

type private GraphInferenceResult =
    | ExactPosterior of ExactPosteriorResult
    | MaxPosterior of MaxPosteriorResult

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

let private exactPosteriorResult (raw: Map<string, float>) =
    let evidence = raw |> Seq.sumBy (fun kv -> kv.Value)

    {
        Posterior = normalizeMap raw
        Evidence = evidence
    }

let private maxPosteriorResult (raw: Map<string, float>) =
    let bestLabel, bestScore =
        raw
        |> Map.toList
        |> List.maxBy snd

    {
        BestLabel = bestLabel
        Score = bestScore
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

let private printTop title count (dist: Map<string, float>) =
    printfn "%s" title
    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.truncate count
    |> List.iter (fun (label, p) -> printfn "  %-26s %.6f" label p)

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
    let mutable index = 0
    let mutable currentToken = hidden.Id
    let steps = ResizeArray<FiniteChainStep<'Obs, 'State>>()

    while index < actions.Length do
        let stepObservation, stepObservationWeight =
            match actions.[index] with
            | ObserveAction(tokenId, observationWeight, observation) when tokenId = currentToken ->
                index <- index + 1
                observation, observationWeight
            | _ -> failwith "Expected an observation on the current chain token."

        let stepTransition =
            if index < actions.Length then
                match actions.[index] with
                | TransitionAction(sourceToken, destinationToken, transitionWeight) when sourceToken = currentToken ->
                    index <- index + 1
                    currentToken <- destinationToken
                    Some transitionWeight
                | _ -> None
            else
                None

        steps.Add
            {
                Observation = stepObservation
                ObservationWeight = stepObservationWeight
                TransitionWeight = stepTransition
            }

    if currentToken <> finalToken then
        failwith "The chain builder finished on a different state token than the compiled chain tail."

    {
        Hidden = hidden
        InitialWeight = initialWeight
        Steps = steps.ToArray()
        Label = label
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

    if rootId <> queryToken then
        failwith "This Phase 1 tree evaluator currently expects the query token to be the root variable."

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
                }: FiniteTreeNode<'Obs, 'State>))
            |> Seq.toArray
        RootId = rootId
        Label = label
        IsLoweredChain = false
    }

let private lowerChainToDag (chainDag: FiniteChainDag<'Obs, 'State>) =
    let chainNodes =
        [|
            yield
                ({
                    Id = 0
                    ParentId = None
                    States = chainDag.Hidden.States
                    InitialWeight = Some chainDag.InitialWeight
                    TransitionWeight = None
                    Observation = None
                }: FiniteTreeNode<'Obs, 'State>)

            for stepIndex in 0 .. chainDag.Steps.Length - 1 do
                let step = chainDag.Steps.[stepIndex]
                yield
                    ({
                        Id = stepIndex + 1
                        ParentId = Some stepIndex
                        States = chainDag.Hidden.States
                        InitialWeight = None
                        TransitionWeight = step.TransitionWeight
                        Observation = Some (step.Observation, step.ObservationWeight)
                    }: FiniteTreeNode<'Obs, 'State>)
        |]

    {
        Nodes = chainNodes
        RootId = 0
        Label = chainDag.Label
        IsLoweredChain = true
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
    {
        Hidden = { Id = 0; States = states }
        InitialWeight = initialWeight
        Steps =
            observations
            |> Array.map (fun observed ->
                {
                    Observation = observed
                    ObservationWeight = observationWeight
                    TransitionWeight = Some transitionWeight
                })
        Label = label
    }

module private StructuredChainCompiler =
    open ChainDsl

    let compileFiniteChain (observations: 'Obs[]) states initialWeight transitionWeight observationWeight label =
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

let private inferHmmChainSumProduct (dag: FiniteChainDag<'Obs, 'State>) =
    let states = dag.Hidden.States
    let mutable current = Array.zeroCreate<float> states.Length

    for stateIndex in 0 .. states.Length - 1 do
        current.[stateIndex] <- dag.InitialWeight states.[stateIndex]

    for step in dag.Steps do
        let emitted = Array.zeroCreate<float> states.Length

        for stateIndex in 0 .. states.Length - 1 do
            emitted.[stateIndex] <- current.[stateIndex] * step.ObservationWeight states.[stateIndex] step.Observation

        match step.TransitionWeight with
        | None -> current <- emitted
        | Some transitionWeight ->
            let next = Array.zeroCreate<float> states.Length

            for nextStateIndex in 0 .. states.Length - 1 do
                let mutable total = 0.0

                for sourceStateIndex in 0 .. states.Length - 1 do
                    total <- total + emitted.[sourceStateIndex] * transitionWeight states.[sourceStateIndex] states.[nextStateIndex]

                next.[nextStateIndex] <- total

            current <- next

    let raw =
        [ for stateIndex in 0 .. states.Length - 1 -> dag.Label states.[stateIndex], current.[stateIndex] ]
        |> Map.ofList

    exactPosteriorResult raw

let private inferHmmChainMaxProduct (dag: FiniteChainDag<'Obs, 'State>) =
    let states = dag.Hidden.States
    let mutable current = Array.zeroCreate<float> states.Length

    for stateIndex in 0 .. states.Length - 1 do
        current.[stateIndex] <- dag.InitialWeight states.[stateIndex]

    for step in dag.Steps do
        let emitted = Array.zeroCreate<float> states.Length

        for stateIndex in 0 .. states.Length - 1 do
            emitted.[stateIndex] <- current.[stateIndex] * step.ObservationWeight states.[stateIndex] step.Observation

        match step.TransitionWeight with
        | None -> current <- emitted
        | Some transitionWeight ->
            let next = Array.zeroCreate<float> states.Length

            for nextStateIndex in 0 .. states.Length - 1 do
                let mutable best = 0.0

                for sourceStateIndex in 0 .. states.Length - 1 do
                    let score = emitted.[sourceStateIndex] * transitionWeight states.[sourceStateIndex] states.[nextStateIndex]

                    if score > best then
                        best <- score

                next.[nextStateIndex] <- best

            current <- next

    let raw =
        [ for stateIndex in 0 .. states.Length - 1 -> dag.Label states.[stateIndex], current.[stateIndex] ]
        |> Map.ofList

    maxPosteriorResult raw

let private inferHmmChain engine dag =
    match engine with
    | SumProduct -> inferHmmChainSumProduct dag |> ExactPosterior
    | MaxProduct -> inferHmmChainMaxProduct dag |> MaxPosterior

let private lowerGraphChain (dag: FiniteTreeDag<'Obs, 'State>) =
    let root = dag.Nodes |> Array.find (fun node -> node.Id = dag.RootId)
    let steps =
        dag.Nodes
        |> Array.filter (fun node -> node.ParentId.IsSome)
        |> Array.sortBy (fun node -> node.Id)
        |> Array.map (fun node ->
            let observation, observationWeight =
                node.Observation |> Option.defaultWith (fun () -> failwith "Lowered chain nodes must carry an observation.")

            {
                Observation = observation
                ObservationWeight = observationWeight
                TransitionWeight = node.TransitionWeight
            })

    {
        Hidden = { Id = root.Id; States = root.States }
        InitialWeight = root.InitialWeight |> Option.defaultWith (fun () -> failwith "Lowered chain root must carry an initial distribution.")
        Steps = steps
        Label = dag.Label
    }

let private inferTreeDagSumProduct (dag: FiniteTreeDag<'Obs, 'State>) =
    let nodesById = dag.Nodes |> Array.map (fun node -> node.Id, node) |> Map.ofArray
    let childrenByParent =
        dag.Nodes
        |> Array.choose (fun node -> node.ParentId |> Option.map (fun parentId -> parentId, node.Id))
        |> Array.groupBy fst
        |> Array.map (fun (parentId, children) -> parentId, children |> Array.map snd)
        |> Map.ofArray

    let rec subtreeMessage nodeId stateValue =
        let node = nodesById.[nodeId]
        let localObsWeight =
            match node.Observation with
            | None -> 1.0
            | Some (observed, observationWeight) -> observationWeight stateValue observed

        let childWeight =
            childrenByParent
            |> Map.tryFind nodeId
            |> Option.defaultValue [||]
            |> Array.fold (fun acc childId ->
                let childNode = nodesById.[childId]
                let transitionWeight = childNode.TransitionWeight |> Option.defaultWith (fun () -> failwith "Non-root tree nodes must have a transition weight.")
                let childMass =
                    childNode.States
                    |> Array.sumBy (fun childState -> transitionWeight stateValue childState * subtreeMessage childId childState)

                acc * childMass) 1.0

        localObsWeight * childWeight

    let root = nodesById.[dag.RootId]
    let prior = root.InitialWeight |> Option.defaultWith (fun () -> failwith "Root node must have an initial distribution.")
    let raw =
        root.States
        |> Array.map (fun stateValue -> dag.Label stateValue, prior stateValue * subtreeMessage dag.RootId stateValue)
        |> Map.ofArray

    exactPosteriorResult raw

let private inferTreeDagMaxProduct (dag: FiniteTreeDag<'Obs, 'State>) =
    let nodesById = dag.Nodes |> Array.map (fun node -> node.Id, node) |> Map.ofArray
    let childrenByParent =
        dag.Nodes
        |> Array.choose (fun node -> node.ParentId |> Option.map (fun parentId -> parentId, node.Id))
        |> Array.groupBy fst
        |> Array.map (fun (parentId, children) -> parentId, children |> Array.map snd)
        |> Map.ofArray

    let rec subtreeBest nodeId stateValue =
        let node = nodesById.[nodeId]
        let localObsWeight =
            match node.Observation with
            | None -> 1.0
            | Some (observed, observationWeight) -> observationWeight stateValue observed

        let childScore =
            childrenByParent
            |> Map.tryFind nodeId
            |> Option.defaultValue [||]
            |> Array.fold (fun acc childId ->
                let childNode = nodesById.[childId]
                let transitionWeight = childNode.TransitionWeight |> Option.defaultWith (fun () -> failwith "Non-root tree nodes must have a transition weight.")
                let bestChildMass =
                    childNode.States
                    |> Array.maxBy (fun childState -> transitionWeight stateValue childState * subtreeBest childId childState)
                    |> fun childState -> transitionWeight stateValue childState * subtreeBest childId childState

                acc * bestChildMass) 1.0

        localObsWeight * childScore

    let root = nodesById.[dag.RootId]
    let prior = root.InitialWeight |> Option.defaultWith (fun () -> failwith "Root node must have an initial distribution.")
    let raw =
        root.States
        |> Array.map (fun stateValue -> dag.Label stateValue, prior stateValue * subtreeBest dag.RootId stateValue)
        |> Map.ofArray

    maxPosteriorResult raw

let private inferTreeDag engine dag =
    match engine with
    | SumProduct -> inferTreeDagSumProduct dag |> ExactPosterior
    | MaxProduct -> inferTreeDagMaxProduct dag |> MaxPosterior

let private inferCompiledGraph engine graph =
    match graph with
    | ChainGraph dag -> inferHmmChain engine dag
    | TreeGraph dag when dag.IsLoweredChain -> lowerGraphChain dag |> inferHmmChain engine
    | TreeGraph dag -> inferTreeDag engine dag

let private exactPosteriorOf result =
    match result with
    | ExactPosterior posterior -> posterior
    | MaxPosterior _ -> failwith "Expected an exact posterior result."

let private maxPosteriorOf result =
    match result with
    | MaxPosterior posterior -> posterior
    | ExactPosterior _ -> failwith "Expected a max-posterior result."

let private evaluateHmmChain (dag: FiniteChainDag<'Obs, 'State>) =
    inferCompiledGraph SumProduct (ChainGraph dag) |> exactPosteriorOf |> fun result -> result.Posterior

let private evaluateTreeDag (dag: FiniteTreeDag<'Obs, 'State>) =
    inferCompiledGraph SumProduct (TreeGraph dag) |> exactPosteriorOf |> fun result -> result.Posterior

let private evaluateGraphDag (dag: FiniteTreeDag<'Obs, 'State>) =
    inferCompiledGraph SumProduct (TreeGraph dag) |> exactPosteriorOf |> fun result -> result.Posterior

let private printPhase1Case label observations compareAgainstHansei =
    let directDag = compileBinaryHmmChain observations
    let structuredDag = compileStructuredBinaryHmmChain observations
    let loweredDirectDag = lowerChainToDag directDag
    let loweredStructuredDag = lowerChainToDag structuredDag
    let directPosterior = evaluateHmmChain directDag
    let structuredPosterior = evaluateHmmChain structuredDag
    let loweredDirectPosterior = evaluateGraphDag loweredDirectDag
    let loweredStructuredPosterior = evaluateGraphDag loweredStructuredDag
    let directEvidence = inferCompiledGraph SumProduct (ChainGraph directDag) |> exactPosteriorOf
    let structuredMax = inferCompiledGraph MaxProduct (ChainGraph structuredDag) |> maxPosteriorOf
    printfn "\n=== %s ===" label
    printfn "Observation length: %d" observations.Length
    printTop "DP DAG posterior (direct IR)" 2 directPosterior
    printTop "DP DAG posterior (opt-in structured subset)" 2 structuredPosterior
    printfn "Evidence (sum-product, direct IR) = %.6f" directEvidence.Evidence
    printfn "MAP label (max-product, structured subset) = %s (score %.6f)" structuredMax.BestLabel structuredMax.Score
    printfn "L1(direct IR, opt-in structured subset) = %.6f" (l1Distance directPosterior structuredPosterior)
    printfn "L1(chain evaluator, lowered direct DAG) = %.6f" (l1Distance directPosterior loweredDirectPosterior)
    printfn "L1(chain evaluator, lowered structured DAG) = %.6f" (l1Distance structuredPosterior loweredStructuredPosterior)

    if compareAgainstHansei then
        let hanseiPosterior = hiddenMarkovPosterior observations |> exact
        printTop "Hansei exact posterior" 2 hanseiPosterior
        printfn "L1(opt-in structured subset, Hansei exact) = %.6f" (l1Distance structuredPosterior hanseiPosterior)
        printfn "DP DAG ms/run (direct IR)       %.3f" (benchmark 50 (fun () -> evaluateHmmChain directDag))
        printfn "DP DAG ms/run (opt-in structured subset) %.3f" (benchmark 50 (fun () -> evaluateHmmChain structuredDag))
        printfn "DP DAG ms/run (lowered direct DAG) %.3f" (benchmark 50 (fun () -> evaluateGraphDag loweredDirectDag))
        printfn "DP DAG ms/run (lowered structured DAG) %.3f" (benchmark 50 (fun () -> evaluateGraphDag loweredStructuredDag))
        printfn "Hansei exact ms/run      %.3f" (benchmark 10 (fun () -> hiddenMarkovPosterior observations |> exact))
    else
        printfn "DP DAG ms/run (direct IR)       %.3f" (benchmark 200 (fun () -> evaluateHmmChain directDag))
        printfn "DP DAG ms/run (opt-in structured subset) %.3f" (benchmark 200 (fun () -> evaluateHmmChain structuredDag))
        printfn "DP DAG ms/run (lowered direct DAG) %.3f" (benchmark 200 (fun () -> evaluateGraphDag loweredDirectDag))
        printfn "DP DAG ms/run (lowered structured DAG) %.3f" (benchmark 200 (fun () -> evaluateGraphDag loweredStructuredDag))

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
    let directPosterior = evaluateHmmChain directDag
    let structuredPosterior = evaluateHmmChain structuredDag
    let directEvidence = inferCompiledGraph SumProduct (ChainGraph directDag) |> exactPosteriorOf
    let directMax = inferCompiledGraph MaxProduct (ChainGraph directDag) |> maxPosteriorOf

    printfn "\n=== Weather Chain Example ==="
    printfn "Observation length: %d" observations.Length
    printTop "DP DAG posterior (direct IR)" 3 directPosterior
    printTop "DP DAG posterior (opt-in structured subset)" 3 structuredPosterior
    printfn "Evidence (sum-product, direct IR) = %.6f" directEvidence.Evidence
    printfn "MAP label (max-product, direct IR) = %s (score %.6f)" directMax.BestLabel directMax.Score
    printfn "L1(direct IR, opt-in structured subset) = %.6f" (l1Distance directPosterior structuredPosterior)
    printfn "DP DAG ms/run (direct IR)       %.3f" (benchmark 200 (fun () -> evaluateHmmChain directDag))
    printfn "DP DAG ms/run (opt-in structured subset) %.3f" (benchmark 200 (fun () -> evaluateHmmChain structuredDag))

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
        Label = fun state -> sprintf "weather=%s" state
        IsLoweredChain = false
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

let private printBranchingDagCase () =
    let directDag = compileBranchingWeatherDag ()
    let structuredDag = compileStructuredBranchingWeatherDag ()
    let directPosterior = evaluateTreeDag directDag
    let structuredPosterior = evaluateTreeDag structuredDag
    let directEvidence = inferCompiledGraph SumProduct (TreeGraph directDag) |> exactPosteriorOf
    let structuredMax = inferCompiledGraph MaxProduct (TreeGraph structuredDag) |> maxPosteriorOf

    printfn "\n=== Branching Weather DAG Example ==="
    printTop "DP DAG posterior (direct IR)" 3 directPosterior
    printTop "DP DAG posterior (opt-in structured subset)" 3 structuredPosterior
    printfn "Evidence (sum-product, direct IR) = %.6f" directEvidence.Evidence
    printfn "MAP label (max-product, structured subset) = %s (score %.6f)" structuredMax.BestLabel structuredMax.Score
    printfn "L1(direct IR, opt-in structured subset) = %.6f" (l1Distance directPosterior structuredPosterior)
    printfn "DP DAG ms/run (direct IR)       %.3f" (benchmark 500 (fun () -> evaluateTreeDag directDag))
    printfn "DP DAG ms/run (opt-in structured subset) %.3f" (benchmark 500 (fun () -> evaluateTreeDag structuredDag))

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