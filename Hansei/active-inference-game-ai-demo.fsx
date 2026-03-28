#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open Hansei
open Hansei.DynamicProgrammingDag
open Hansei.Probability

type Archetype =
    | Cautious
    | Bold

type Aggression =
    | Reserved
    | Assertive

type PlayerStyle =
    | Frontal
    | Flanker

type ThreatLevel =
    | Low
    | Medium
    | High

type Morale =
    | Steady
    | Shaken

type Goal =
    | HoldPosition
    | Probe
    | Engage
    | Retreat

type AgentAction =
    | Patrol
    | Investigate
    | TakeCover
    | Attack
    | Flee

type Observation =
    {
        Name: string
        PlayerDistance: float
        HeardGunshot: bool
        SawFlank: bool
        Health: float
        AllyNearby: bool
    }

type InferredWorld =
    {
        Aggression: Aggression
        PlayerStyle: PlayerStyle
        Threat: ThreatLevel
        Morale: Morale
        Goal: Goal
        Action: AgentAction
    }

type AgentBeliefState =
    {
        ThreatBelief: Map<ThreatLevel, float>
        MoraleBelief: Map<Morale, float>
        GoalBelief: Map<Goal, float>
    }

type DagDecisionState =
    | AggressionState of Aggression
    | AggressionStyleState of Aggression * PlayerStyle
    | AggressionStyleThreatState of Aggression * PlayerStyle * ThreatLevel
    | WorldState of Aggression * PlayerStyle * ThreatLevel * Morale
    | GoalWorldState of Aggression * PlayerStyle * ThreatLevel * Morale * Goal
    | ActionState of AgentAction

let private boolLikelihood whenTrue eventOccurred =
    if eventOccurred then
        whenTrue
    else
        1.0 - whenTrue

let private peaked expected scale observed =
    exp (-abs (observed - expected) / scale)

let private threatInterpretationBias aggression threat =
    match aggression, threat with
    | Reserved, Low -> 0.82
    | Reserved, Medium -> 1.12
    | Reserved, High -> 1.36
    | Assertive, Low -> 1.18
    | Assertive, Medium -> 0.92
    | Assertive, High -> 0.68

let private threatObservationLikelihood obs aggression threat =
    let (distanceWeight,
         gunshotWeight,
         flankWeight,
         healthWeight,
         allyWeight) =
        match threat with
        | Low -> 30.0, 0.15, 0.10, peaked 90.0 35.0 obs.Health, if obs.AllyNearby then 1.15 else 0.85
        | Medium -> 14.0, 0.55, 0.35, peaked 65.0 25.0 obs.Health, if obs.AllyNearby then 1.0 else 0.95
        | High -> 5.0, 0.85, 0.80, peaked 30.0 18.0 obs.Health, if obs.AllyNearby then 0.9 else 1.1

    peaked distanceWeight (if threat = High then 5.0 else 11.0) obs.PlayerDistance
    * boolLikelihood gunshotWeight obs.HeardGunshot
    * boolLikelihood flankWeight obs.SawFlank
    * healthWeight
    * allyWeight
    * threatInterpretationBias aggression threat
    |> max 1e-9

let private moraleObservationLikelihood obs threat morale =
    match morale with
    | Steady ->
        match threat with
        | Low -> 1.15 * peaked 85.0 28.0 obs.Health * (if obs.AllyNearby then 1.2 else 0.8)
        | Medium -> 0.95 * peaked 70.0 24.0 obs.Health * (if obs.AllyNearby then 1.1 else 0.9)
        | High -> 0.65 * peaked 55.0 22.0 obs.Health * (if obs.AllyNearby then 1.0 else 0.75)
    | Shaken ->
        match threat with
        | Low -> 0.35 * peaked 35.0 20.0 obs.Health * (if obs.AllyNearby then 0.85 else 1.1)
        | Medium -> 0.85 * peaked 40.0 22.0 obs.Health * (if obs.AllyNearby then 0.9 else 1.1)
        | High -> 1.25 * peaked 25.0 18.0 obs.Health * (if obs.AllyNearby then 0.95 else 1.2)
    |> max 1e-9

let private playerStyleLikelihood obs style =
    match style with
    | Frontal ->
        boolLikelihood 0.18 obs.SawFlank
        * boolLikelihood 0.6 obs.HeardGunshot
        * peaked 12.0 12.0 obs.PlayerDistance
    | Flanker ->
        boolLikelihood 0.82 obs.SawFlank
        * boolLikelihood 0.5 obs.HeardGunshot
        * peaked 9.0 10.0 obs.PlayerDistance
    |> max 1e-9

let private aggressionPrior archetype =
    match archetype with
    | Cautious -> distribution [ Reserved, 0.84; Assertive, 0.16 ]
    | Bold -> distribution [ Reserved, 0.14; Assertive, 0.86 ]

let private playerStylePrior =
    distribution [ Frontal, 0.62; Flanker, 0.38 ]

let private baseThreatBelief =
    Map.ofList [ Low, 0.45; Medium, 0.35; High, 0.20 ]

let private baseMoraleBelief =
    Map.ofList [ Steady, 0.65; Shaken, 0.35 ]

let private baseGoalBelief =
    Map.ofList [ HoldPosition, 0.30; Probe, 0.30; Engage, 0.22; Retreat, 0.18 ]

let private dagAggressionStates =
    [| AggressionState Reserved; AggressionState Assertive |]

let private dagAggressionStyleStates =
    [| for aggression in [ Reserved; Assertive ] do
           for style in [ Frontal; Flanker ] do
               yield AggressionStyleState(aggression, style) |]

let private dagAggressionStyleThreatStates =
    [| for aggression in [ Reserved; Assertive ] do
           for style in [ Frontal; Flanker ] do
               for threat in [ Low; Medium; High ] do
                   yield AggressionStyleThreatState(aggression, style, threat) |]

let private dagWorldStates =
    [| for aggression in [ Reserved; Assertive ] do
           for style in [ Frontal; Flanker ] do
               for threat in [ Low; Medium; High ] do
                   for morale in [ Steady; Shaken ] do
                       yield WorldState(aggression, style, threat, morale) |]

let private dagGoalWorldStates =
    [| for aggression in [ Reserved; Assertive ] do
           for style in [ Frontal; Flanker ] do
               for threat in [ Low; Medium; High ] do
                   for morale in [ Steady; Shaken ] do
                       for goal in [ HoldPosition; Probe; Engage; Retreat ] do
                           yield GoalWorldState(aggression, style, threat, morale, goal) |]

let private dagActionStates =
    [| ActionState Patrol; ActionState Investigate; ActionState TakeCover; ActionState Attack; ActionState Flee |]

let private moraleBeliefFor aggression =
    match aggression with
    | Reserved -> Map.ofList [ Steady, 0.58; Shaken, 0.42 ]
    | Assertive -> Map.ofList [ Steady, 0.74; Shaken, 0.26 ]

let private weightOf key (weights: Map<'T, float>) =
    weights |> Map.tryFind key |> Option.defaultValue 0.0

let private normalizeMap (dist: Map<'T, float>) =
    let total = dist |> Seq.sumBy (fun kv -> kv.Value)

    dist
    |> Map.map (fun _ weight ->
        if total <= 0.0 then 0.0 else weight / total)

let private blendBeliefs carryWeight (previous: Map<'T, float>) (fresh: Map<'T, float>) =
    Seq.append (previous |> Map.toSeq |> Seq.map fst) (fresh |> Map.toSeq |> Seq.map fst)
    |> Set.ofSeq
    |> Seq.map (fun key ->
        let priorWeight = previous |> Map.tryFind key |> Option.defaultValue 0.0
        let freshWeight = fresh |> Map.tryFind key |> Option.defaultValue 0.0
        key, carryWeight * priorWeight + (1.0 - carryWeight) * freshWeight)
    |> Map.ofSeq
    |> normalizeMap

let private beliefDistribution belief =
    belief
    |> Map.toList
    |> distribution

let private moralePrior aggression =
    aggression
    |> moraleBeliefFor
    |> beliefDistribution

let private initialBeliefState =
    {
        ThreatBelief = normalizeMap baseThreatBelief
        MoraleBelief = normalizeMap baseMoraleBelief
        GoalBelief = normalizeMap baseGoalBelief
    }

let private goalWeights archetype aggression threat morale =
    match archetype, aggression, threat, morale with
    | Cautious, Reserved, Low, Steady -> [ HoldPosition, 0.56; Probe, 0.30; Engage, 0.06; Retreat, 0.08 ]
    | Cautious, Assertive, Low, Steady -> [ HoldPosition, 0.42; Probe, 0.36; Engage, 0.14; Retreat, 0.08 ]
    | Bold, Reserved, Low, Steady -> [ HoldPosition, 0.26; Probe, 0.50; Engage, 0.18; Retreat, 0.06 ]
    | Bold, Assertive, Low, Steady -> [ HoldPosition, 0.12; Probe, 0.34; Engage, 0.48; Retreat, 0.06 ]
    | Cautious, Reserved, Low, Shaken -> [ HoldPosition, 0.28; Probe, 0.18; Engage, 0.04; Retreat, 0.50 ]
    | Cautious, Assertive, Low, Shaken -> [ HoldPosition, 0.24; Probe, 0.24; Engage, 0.10; Retreat, 0.42 ]
    | Bold, Reserved, Low, Shaken -> [ HoldPosition, 0.18; Probe, 0.28; Engage, 0.14; Retreat, 0.40 ]
    | Bold, Assertive, Low, Shaken -> [ HoldPosition, 0.10; Probe, 0.28; Engage, 0.30; Retreat, 0.32 ]
    | Cautious, Reserved, Medium, Steady -> [ HoldPosition, 0.38; Probe, 0.32; Engage, 0.10; Retreat, 0.20 ]
    | Cautious, Assertive, Medium, Steady -> [ HoldPosition, 0.24; Probe, 0.32; Engage, 0.24; Retreat, 0.20 ]
    | Bold, Reserved, Medium, Steady -> [ HoldPosition, 0.16; Probe, 0.34; Engage, 0.30; Retreat, 0.20 ]
    | Bold, Assertive, Medium, Steady -> [ HoldPosition, 0.08; Probe, 0.18; Engage, 0.60; Retreat, 0.14 ]
    | Cautious, Reserved, Medium, Shaken -> [ HoldPosition, 0.16; Probe, 0.10; Engage, 0.04; Retreat, 0.70 ]
    | Cautious, Assertive, Medium, Shaken -> [ HoldPosition, 0.14; Probe, 0.14; Engage, 0.12; Retreat, 0.60 ]
    | Bold, Reserved, Medium, Shaken -> [ HoldPosition, 0.10; Probe, 0.14; Engage, 0.18; Retreat, 0.58 ]
    | Bold, Assertive, Medium, Shaken -> [ HoldPosition, 0.06; Probe, 0.12; Engage, 0.34; Retreat, 0.48 ]
    | Cautious, Reserved, High, Steady -> [ HoldPosition, 0.26; Probe, 0.04; Engage, 0.06; Retreat, 0.64 ]
    | Cautious, Assertive, High, Steady -> [ HoldPosition, 0.18; Probe, 0.04; Engage, 0.18; Retreat, 0.60 ]
    | Bold, Reserved, High, Steady -> [ HoldPosition, 0.12; Probe, 0.06; Engage, 0.28; Retreat, 0.54 ]
    | Bold, Assertive, High, Steady -> [ HoldPosition, 0.06; Probe, 0.06; Engage, 0.56; Retreat, 0.32 ]
    | Cautious, Reserved, High, Shaken -> [ HoldPosition, 0.06; Probe, 0.02; Engage, 0.02; Retreat, 0.90 ]
    | Cautious, Assertive, High, Shaken -> [ HoldPosition, 0.06; Probe, 0.02; Engage, 0.08; Retreat, 0.84 ]
    | Bold, Reserved, High, Shaken -> [ HoldPosition, 0.04; Probe, 0.02; Engage, 0.14; Retreat, 0.80 ]
    | Bold, Assertive, High, Shaken -> [ HoldPosition, 0.03; Probe, 0.02; Engage, 0.25; Retreat, 0.70 ]

let private goalWeight archetype aggression threat morale goal =
    goalWeights archetype aggression threat morale
    |> List.tryPick (fun (candidate, weight) -> if candidate = goal then Some weight else None)
    |> Option.defaultValue 0.0

let private clamp low high value =
    max low (min high value)

let private goalCarryWeight archetype aggression obs threat morale =
    let baseCarry =
        match threat, morale with
        | High, Shaken -> 0.28
        | High, Steady -> 0.46
        | Medium, Shaken -> 0.54
        | Medium, Steady -> 0.74
        | Low, Shaken -> 0.70
        | Low, Steady -> 0.90

    let (archetypeAdjustment,
         aggressionAdjustment) =
        match archetype, aggression with
        | Cautious, Reserved -> -0.16, -0.10
        | Cautious, Assertive -> -0.12, 0.00
        | Bold, Reserved -> 0.08, -0.02
        | Bold, Assertive -> 0.14, 0.12

    let healthAdjustment = if obs.Health < 35.0 then -0.14 elif obs.Health < 55.0 then -0.07 else 0.0
    let flankAdjustment = if obs.SawFlank then -0.12 else 0.0
    let isolationAdjustment = if not obs.AllyNearby then -0.07 else 0.0

    clamp 0.20 0.94 (baseCarry + archetypeAdjustment + aggressionAdjustment + healthAdjustment + flankAdjustment + isolationAdjustment)

let private goalBeliefFor beliefState archetype aggression threat morale obs =
    let freshGoalBelief =
        goalWeights archetype aggression threat morale
        |> Map.ofList
        |> normalizeMap

    blendBeliefs (goalCarryWeight archetype aggression obs threat morale) beliefState.GoalBelief freshGoalBelief

let private actionWeights archetype goal aggression style threat morale =
    match goal, archetype, aggression, style, threat, morale with
    | HoldPosition, _, _, _, Low, Steady -> [ Patrol, 0.54; Investigate, 0.24; TakeCover, 0.10; Attack, 0.07; Flee, 0.05 ]
    | Probe, _, _, _, Low, Steady -> [ Investigate, 0.48; Patrol, 0.32; TakeCover, 0.08; Attack, 0.08; Flee, 0.04 ]
    | Engage, _, _, _, Low, Steady -> [ Attack, 0.34; Investigate, 0.28; Patrol, 0.22; TakeCover, 0.10; Flee, 0.06 ]
    | Retreat, _, _, _, Low, Steady -> [ TakeCover, 0.34; Patrol, 0.24; Flee, 0.20; Investigate, 0.16; Attack, 0.06 ]
    | HoldPosition, _, _, _, Low, Shaken -> [ TakeCover, 0.34; Patrol, 0.28; Investigate, 0.16; Flee, 0.16; Attack, 0.06 ]
    | Probe, _, _, _, Low, Shaken -> [ Investigate, 0.36; TakeCover, 0.24; Patrol, 0.18; Flee, 0.16; Attack, 0.06 ]
    | Engage, _, _, _, Low, Shaken -> [ Attack, 0.22; Investigate, 0.26; TakeCover, 0.22; Flee, 0.20; Patrol, 0.10 ]
    | Retreat, _, _, _, Low, Shaken -> [ Flee, 0.36; TakeCover, 0.30; Patrol, 0.16; Investigate, 0.12; Attack, 0.06 ]
    | HoldPosition, Cautious, Reserved, _, Medium, Steady -> [ TakeCover, 0.40; Patrol, 0.24; Investigate, 0.18; Flee, 0.12; Attack, 0.06 ]
    | HoldPosition, Cautious, Assertive, _, Medium, Steady -> [ TakeCover, 0.34; Investigate, 0.22; Patrol, 0.18; Attack, 0.18; Flee, 0.08 ]
    | HoldPosition, Bold, Reserved, _, Medium, Steady -> [ TakeCover, 0.30; Investigate, 0.24; Attack, 0.20; Patrol, 0.18; Flee, 0.08 ]
    | HoldPosition, Bold, Assertive, _, Medium, Steady -> [ Attack, 0.32; TakeCover, 0.24; Investigate, 0.20; Patrol, 0.16; Flee, 0.08 ]
    | Probe, Cautious, Reserved, _, Medium, Steady -> [ Investigate, 0.44; TakeCover, 0.24; Patrol, 0.18; Flee, 0.10; Attack, 0.04 ]
    | Probe, Cautious, Assertive, _, Medium, Steady -> [ Investigate, 0.36; TakeCover, 0.22; Attack, 0.18; Patrol, 0.16; Flee, 0.08 ]
    | Probe, Bold, Reserved, _, Medium, Steady -> [ Investigate, 0.34; Attack, 0.24; TakeCover, 0.20; Patrol, 0.14; Flee, 0.08 ]
    | Probe, Bold, Assertive, _, Medium, Steady -> [ Attack, 0.34; Investigate, 0.30; TakeCover, 0.18; Patrol, 0.12; Flee, 0.06 ]
    | Engage, Cautious, Reserved, Frontal, Medium, Steady -> [ Attack, 0.26; TakeCover, 0.30; Investigate, 0.20; Flee, 0.16; Patrol, 0.08 ]
    | Engage, Cautious, Assertive, Frontal, Medium, Steady -> [ Attack, 0.40; TakeCover, 0.22; Investigate, 0.18; Flee, 0.12; Patrol, 0.08 ]
    | Engage, Bold, Reserved, Frontal, Medium, Steady -> [ Attack, 0.44; Investigate, 0.22; TakeCover, 0.16; Flee, 0.10; Patrol, 0.08 ]
    | Engage, Bold, Assertive, Frontal, Medium, Steady -> [ Attack, 0.60; Investigate, 0.16; TakeCover, 0.12; Flee, 0.06; Patrol, 0.06 ]
    | Engage, Cautious, Reserved, Flanker, Medium, Steady -> [ TakeCover, 0.36; Investigate, 0.24; Flee, 0.18; Attack, 0.14; Patrol, 0.08 ]
    | Engage, Cautious, Assertive, Flanker, Medium, Steady -> [ Attack, 0.26; TakeCover, 0.28; Investigate, 0.22; Flee, 0.16; Patrol, 0.08 ]
    | Engage, Bold, Reserved, Flanker, Medium, Steady -> [ Attack, 0.30; TakeCover, 0.24; Investigate, 0.22; Flee, 0.14; Patrol, 0.10 ]
    | Engage, Bold, Assertive, Flanker, Medium, Steady -> [ Attack, 0.42; TakeCover, 0.22; Investigate, 0.18; Flee, 0.10; Patrol, 0.08 ]
    | Retreat, Cautious, _, _, Medium, Steady -> [ TakeCover, 0.40; Flee, 0.30; Investigate, 0.14; Patrol, 0.10; Attack, 0.06 ]
    | Retreat, Bold, _, _, Medium, Steady -> [ Flee, 0.34; TakeCover, 0.30; Investigate, 0.16; Attack, 0.12; Patrol, 0.08 ]
    | HoldPosition, Cautious, _, _, Medium, Shaken -> [ TakeCover, 0.38; Flee, 0.28; Patrol, 0.16; Investigate, 0.12; Attack, 0.06 ]
    | HoldPosition, Bold, _, _, Medium, Shaken -> [ TakeCover, 0.30; Flee, 0.24; Attack, 0.18; Investigate, 0.16; Patrol, 0.12 ]
    | Probe, Cautious, _, _, Medium, Shaken -> [ TakeCover, 0.30; Investigate, 0.26; Flee, 0.24; Patrol, 0.14; Attack, 0.06 ]
    | Probe, Bold, _, _, Medium, Shaken -> [ Attack, 0.22; Investigate, 0.24; TakeCover, 0.24; Flee, 0.18; Patrol, 0.12 ]
    | Engage, Cautious, _, _, Medium, Shaken -> [ TakeCover, 0.34; Flee, 0.28; Attack, 0.18; Investigate, 0.12; Patrol, 0.08 ]
    | Engage, Bold, _, _, Medium, Shaken -> [ Attack, 0.34; TakeCover, 0.24; Flee, 0.20; Investigate, 0.14; Patrol, 0.08 ]
    | Retreat, Cautious, _, _, Medium, Shaken -> [ Flee, 0.50; TakeCover, 0.30; Investigate, 0.08; Patrol, 0.08; Attack, 0.04 ]
    | Retreat, Bold, _, _, Medium, Shaken -> [ Flee, 0.40; TakeCover, 0.28; Investigate, 0.12; Attack, 0.12; Patrol, 0.08 ]
    | HoldPosition, Cautious, _, _, High, Steady -> [ TakeCover, 0.42; Flee, 0.28; Attack, 0.12; Investigate, 0.10; Patrol, 0.08 ]
    | HoldPosition, Bold, _, _, High, Steady -> [ TakeCover, 0.30; Attack, 0.28; Flee, 0.20; Investigate, 0.14; Patrol, 0.08 ]
    | Probe, Cautious, _, _, High, Steady -> [ TakeCover, 0.34; Investigate, 0.24; Flee, 0.22; Attack, 0.12; Patrol, 0.08 ]
    | Probe, Bold, _, _, High, Steady -> [ Attack, 0.28; TakeCover, 0.24; Investigate, 0.22; Flee, 0.18; Patrol, 0.08 ]
    | Engage, Cautious, Reserved, _, High, Steady -> [ TakeCover, 0.34; Flee, 0.24; Attack, 0.18; Investigate, 0.16; Patrol, 0.08 ]
    | Engage, Cautious, Assertive, _, High, Steady -> [ Attack, 0.30; TakeCover, 0.28; Flee, 0.18; Investigate, 0.16; Patrol, 0.08 ]
    | Engage, Bold, Reserved, _, High, Steady -> [ Attack, 0.36; TakeCover, 0.24; Flee, 0.18; Investigate, 0.14; Patrol, 0.08 ]
    | Engage, Bold, Assertive, _, High, Steady -> [ Attack, 0.54; TakeCover, 0.20; Flee, 0.10; Investigate, 0.10; Patrol, 0.06 ]
    | Retreat, Cautious, _, _, High, Steady -> [ Flee, 0.42; TakeCover, 0.38; Investigate, 0.08; Patrol, 0.08; Attack, 0.04 ]
    | Retreat, Bold, _, _, High, Steady -> [ Flee, 0.34; TakeCover, 0.30; Attack, 0.16; Investigate, 0.12; Patrol, 0.08 ]
    | HoldPosition, Cautious, _, _, High, Shaken -> [ Flee, 0.40; TakeCover, 0.36; Investigate, 0.10; Patrol, 0.08; Attack, 0.06 ]
    | HoldPosition, Bold, _, _, High, Shaken -> [ TakeCover, 0.32; Flee, 0.28; Attack, 0.20; Investigate, 0.12; Patrol, 0.08 ]
    | Probe, Cautious, _, _, High, Shaken -> [ Flee, 0.36; TakeCover, 0.32; Investigate, 0.16; Attack, 0.10; Patrol, 0.06 ]
    | Probe, Bold, _, _, High, Shaken -> [ Attack, 0.24; TakeCover, 0.28; Flee, 0.24; Investigate, 0.16; Patrol, 0.08 ]
    | Engage, Cautious, _, _, High, Shaken -> [ TakeCover, 0.36; Flee, 0.30; Attack, 0.18; Investigate, 0.10; Patrol, 0.06 ]
    | Engage, Bold, _, _, High, Shaken -> [ Attack, 0.34; TakeCover, 0.24; Flee, 0.24; Investigate, 0.10; Patrol, 0.08 ]
    | Retreat, Cautious, _, _, High, Shaken -> [ Flee, 0.58; TakeCover, 0.28; Investigate, 0.06; Patrol, 0.04; Attack, 0.04 ]
    | Retreat, Bold, _, _, High, Shaken -> [ Flee, 0.46; TakeCover, 0.24; Attack, 0.14; Investigate, 0.10; Patrol, 0.06 ]

let private actionWeight archetype goal aggression style threat morale action =
    actionWeights archetype goal aggression style threat morale
    |> List.tryPick (fun (candidate, weight) -> if candidate = action then Some weight else None)
    |> Option.defaultValue 0.0

let private goalModel archetype aggression threat morale =
    distribution (goalWeights archetype aggression threat morale)

let private actionModel archetype goal aggression style threat morale =
    distribution (actionWeights archetype goal aggression style threat morale)

let private inferWorld beliefState archetype obs =
    dist {
        let! aggression = aggressionPrior archetype
        let! style = playerStylePrior
        do! factor (playerStyleLikelihood obs style)

        let threatPriorBelief = blendBeliefs 0.70 beliefState.ThreatBelief baseThreatBelief
        let! threat = beliefDistribution threatPriorBelief
        do! factor (threatObservationLikelihood obs aggression threat)

        let moralePriorBelief = blendBeliefs 0.65 beliefState.MoraleBelief (moraleBeliefFor aggression)
        let! morale = beliefDistribution moralePriorBelief
        do! factor (moraleObservationLikelihood obs threat morale)

        let goalPriorBelief = goalBeliefFor beliefState archetype aggression threat morale obs
        let! goal = beliefDistribution goalPriorBelief

        let! action = actionModel archetype goal aggression style threat morale

        return
            {
                Aggression = aggression
                PlayerStyle = style
                Threat = threat
                Morale = morale
                Goal = goal
                Action = action
            }
    }
    |> Model.ExactInfer

let private buildFiniteChainDag beliefState archetype obs : FiniteDag<Observation, DagDecisionState, AgentAction> =
    let threatPriorBelief = blendBeliefs 0.70 beliefState.ThreatBelief baseThreatBelief
    let aggressionWeight =
        match archetype with
        | Cautious -> function | Reserved -> 0.84 | Assertive -> 0.16
        | Bold -> function | Reserved -> 0.14 | Assertive -> 0.86

    {
        Nodes =
            [|
                {
                    Id = 0
                    ParentId = None
                    States = dagAggressionStates
                    InitialWeight = Some (fun state ->
                        match state with
                        | AggressionState aggression -> aggressionWeight aggression
                        | _ -> 0.0)
                    TransitionWeight = None
                    Observation = None
                }
                {
                    Id = 1
                    ParentId = Some 0
                    States = dagAggressionStyleStates
                    InitialWeight = None
                    TransitionWeight = Some (fun parentState childState ->
                        match parentState, childState with
                        | AggressionState aggression, AggressionStyleState (childAggression, style) when aggression = childAggression ->
                            match style with
                            | Frontal -> 0.62
                            | Flanker -> 0.38
                        | _ -> 0.0)
                    Observation = Some (obs, fun state observation ->
                        match state with
                        | AggressionStyleState (_, style) -> playerStyleLikelihood observation style
                        | _ -> 0.0)
                }
                {
                    Id = 2
                    ParentId = Some 1
                    States = dagAggressionStyleThreatStates
                    InitialWeight = None
                    TransitionWeight = Some (fun parentState childState ->
                        match parentState, childState with
                        | AggressionStyleState (aggression, style), AggressionStyleThreatState (childAggression, childStyle, threat)
                            when aggression = childAggression && style = childStyle ->
                            weightOf threat threatPriorBelief
                        | _ -> 0.0)
                    Observation = Some (obs, fun state observation ->
                        match state with
                        | AggressionStyleThreatState (aggression, _, threat) -> threatObservationLikelihood observation aggression threat
                        | _ -> 0.0)
                }
                {
                    Id = 3
                    ParentId = Some 2
                    States = dagWorldStates
                    InitialWeight = None
                    TransitionWeight = Some (fun parentState childState ->
                        match parentState, childState with
                        | AggressionStyleThreatState (aggression, style, threat), WorldState (childAggression, childStyle, childThreat, morale)
                            when aggression = childAggression && style = childStyle && threat = childThreat ->
                            let moralePriorBelief = blendBeliefs 0.65 beliefState.MoraleBelief (moraleBeliefFor aggression)
                            weightOf morale moralePriorBelief
                        | _ -> 0.0)
                    Observation = Some (obs, fun state observation ->
                        match state with
                        | WorldState (_, _, threat, morale) -> moraleObservationLikelihood observation threat morale
                        | _ -> 0.0)
                }
                {
                    Id = 4
                    ParentId = Some 3
                    States = dagGoalWorldStates
                    InitialWeight = None
                    TransitionWeight = Some (fun parentState childState ->
                        match parentState, childState with
                        | WorldState (aggression, style, threat, morale), GoalWorldState (childAggression, childStyle, childThreat, childMorale, goal)
                            when aggression = childAggression && style = childStyle && threat = childThreat && morale = childMorale ->
                            let goalPriorBelief = goalBeliefFor beliefState archetype aggression threat morale obs
                            weightOf goal goalPriorBelief
                        | _ -> 0.0)
                    Observation = None
                }
                {
                    Id = 5
                    ParentId = Some 4
                    States = dagActionStates
                    InitialWeight = None
                    TransitionWeight = Some (fun parentState childState ->
                        match parentState, childState with
                        | GoalWorldState (aggression, style, threat, morale, goal), ActionState action ->
                            actionWeight archetype goal aggression style threat morale action
                        | _ -> 0.0)
                    Observation = None
                }
            |]
        RootId = 0
        QueryId = 5
        Label = function | ActionState action -> action | _ -> failwith "Query label expected an action state."
        Shape = LinearChain
    }

let private aggregateMarginal projector worlds =
    worlds
    |> Seq.fold (fun acc (tree, weight) ->
        match tree with
        | Value world ->
            let key = projector world
            Map.change key (fun current -> Some (weight + Option.defaultValue 0.0 current)) acc
        | ContinuedSubTree _ -> acc) Map.empty

let private posteriorBelief projector worlds =
    worlds
    |> aggregateMarginal projector
    |> normalizeMap

let private actionPosterior worlds =
    worlds
    |> aggregateMarginal (fun world -> world.Action)
    |> normalizeMap

let private unionKeys (left: Map<'T, float>) (right: Map<'T, float>) =
    Set.union (left |> Map.keys |> Set.ofSeq) (right |> Map.keys |> Set.ofSeq)

let private l1Distance left right =
    unionKeys left right
    |> Seq.sumBy (fun key ->
        abs (weightOf key left - weightOf key right))

let private updateBeliefState worlds =
    {
        ThreatBelief = posteriorBelief (fun world -> world.Threat) worlds
        MoraleBelief = posteriorBelief (fun world -> world.Morale) worlds
        GoalBelief = posteriorBelief (fun world -> world.Goal) worlds
    }

let private actionUtility archetype world =
    match archetype, world.Goal, world.Action, world.Threat, world.Morale, world.PlayerStyle with
    | Cautious, HoldPosition, Patrol, Low, Steady, _ -> 1.28
    | Cautious, HoldPosition, TakeCover, High, _, _ -> 1.30
    | Cautious, HoldPosition, Flee, Low, _, _ -> 0.35
    | Cautious, Probe, Investigate, _, _, _ -> 1.18
    | Cautious, Probe, Attack, High, _, _ -> 0.62
    | Cautious, Probe, Flee, _, _, _ -> 0.82
    | Cautious, Engage, Attack, Medium, Steady, Frontal -> 1.12
    | Cautious, Engage, Attack, High, Steady, _ -> 0.92
    | Cautious, Engage, Attack, High, Shaken, _ -> 0.54
    | Cautious, Engage, TakeCover, High, _, _ -> 1.22
    | Cautious, Retreat, Flee, High, _, _ -> 1.38
    | Cautious, Retreat, TakeCover, High, _, _ -> 1.18
    | Cautious, Retreat, Attack, _, _, _ -> 0.40
    | Bold, HoldPosition, Patrol, Low, Steady, _ -> 1.08
    | Bold, HoldPosition, Attack, Medium, _, _ -> 1.10
    | Bold, HoldPosition, TakeCover, High, _, _ -> 1.12
    | Bold, HoldPosition, Flee, Low, _, _ -> 0.52
    | Bold, Probe, Investigate, _, _, _ -> 1.12
    | Bold, Probe, Attack, Medium, _, _ -> 1.16
    | Bold, Probe, Attack, High, _, _ -> 1.06
    | Bold, Probe, Flee, _, _, _ -> 0.58
    | Bold, Engage, Attack, Medium, Steady, Frontal -> 1.34
    | Bold, Engage, Attack, High, Steady, _ -> 1.30
    | Bold, Engage, Attack, High, Shaken, _ -> 0.96
    | Bold, Engage, TakeCover, High, _, Flanker -> 1.08
    | Bold, Retreat, Flee, High, _, _ -> 1.18
    | Bold, Retreat, TakeCover, High, _, _ -> 1.02
    | Bold, Retreat, Attack, High, Steady, _ -> 0.78
    | _, _, Patrol, High, _, _ -> 0.30
    | _, _, _, _, _, _ -> 0.95

let private utilityWeightedActionPosterior archetype worlds =
    worlds
    |> Seq.fold (fun acc (tree, weight) ->
        match tree with
        | Value world ->
            let adjustedWeight = weight * actionUtility archetype world
            Map.change world.Action (fun current -> Some (adjustedWeight + Option.defaultValue 0.0 current)) acc
        | ContinuedSubTree _ -> acc) Map.empty
    |> normalizeMap

let private printProbabilityMap title dist =
    printfn "%s" title

    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.iter (fun (label, probability) ->
        printfn "  %-12s %.4f" (string label) probability)

    printfn ""

let private warmUp repeatCount action =
    for _ in 1 .. repeatCount do
        action () |> ignore

let private measureAverageMs repeatCount action =
    let timer = Stopwatch.StartNew()

    for _ in 1 .. repeatCount do
        action () |> ignore

    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeatCount

let private benchmark warmupCount averageRepeatCount action =
    warmUp warmupCount action

    let timer = Stopwatch.StartNew()
    let result = action ()
    timer.Stop()

    let averageMs = measureAverageMs averageRepeatCount action
    result, timer.Elapsed.TotalMilliseconds, averageMs

let private printMarginal title projector worlds =
    let normalized = worlds |> aggregateMarginal projector |> normalizeMap

    printProbabilityMap title normalized

let private bestAction worlds =
    worlds
    |> actionPosterior
    |> Map.toList
    |> List.maxBy snd

let private bestUtilityWeightedAction archetype worlds =
    worlds
    |> utilityWeightedActionPosterior archetype
    |> Map.toList
    |> List.maxBy snd

let private compareFiniteDag beliefState archetype obs =
    let genericPosterior, genericSingleMs, genericAverageMs =
        benchmark 5 250 (fun () ->
            inferWorld beliefState archetype obs
            |> actionPosterior)

    let dagFactory () = buildFiniteChainDag beliefState archetype obs

    let dagPosteriorDirect, dagDirectSingleMs, dagDirectAverageMs =
        benchmark 5 250 (fun () -> dagFactory () |> Model.EvaluateFiniteDag)

    let preparedDag = dagFactory () |> Model.PrepareFiniteDag

    let dagPosteriorPrepared, dagPreparedSingleMs, dagPreparedAverageMs =
        benchmark 5 250 (fun () -> preparedDag.Evaluate())

    printfn "Finite-DAG comparison (full latent chain approximation)"
    printfn "  generic exact tree:      single=%.4f ms  avg(250)=%.4f ms" genericSingleMs genericAverageMs
    printfn "  direct DAG eval:         single=%.4f ms  avg(250)=%.4f ms" dagDirectSingleMs dagDirectAverageMs
    printfn "  prepared DAG reuse:      single=%.4f ms  avg(250)=%.4f ms" dagPreparedSingleMs dagPreparedAverageMs
    printfn "  posterior L1(tree, dag)= %.6f" (l1Distance genericPosterior dagPosteriorPrepared)
    printfn ""

    printProbabilityMap "  Generic tree action posterior" genericPosterior
    printProbabilityMap "  Prepared DAG action posterior" dagPosteriorPrepared

let private runScenario beliefState archetype obs =
    let worlds, singleMs, averageMs = benchmark 5 250 (fun () -> inferWorld beliefState archetype obs)

    let action, actionProb = bestAction worlds
    let utilityAction, utilityActionProb = bestUtilityWeightedAction archetype worlds
    let nextBeliefState = updateBeliefState worlds

    printfn "=== %s | archetype=%A ===" obs.Name archetype
    printfn "distance=%.1f  gunshot=%b  flank=%b  health=%.0f  allyNearby=%b" obs.PlayerDistance obs.HeardGunshot obs.SawFlank obs.Health obs.AllyNearby
    printfn "chosen action (argmax posterior): %A  %.4f" action actionProb
    printfn "chosen action (utility-weighted): %A  %.4f" utilityAction utilityActionProb
    printfn "timing: single-run=%.4f ms  avg(250)=%.4f ms" singleMs averageMs
    printfn ""

    printMarginal "Action posterior" (fun world -> world.Action) worlds
    printProbabilityMap "Utility-weighted action posterior" (utilityWeightedActionPosterior archetype worlds)
    printMarginal "Aggression posterior" (fun world -> world.Aggression) worlds
    printMarginal "Threat posterior" (fun world -> world.Threat) worlds
    printMarginal "Morale posterior" (fun world -> world.Morale) worlds
    printMarginal "Goal posterior" (fun world -> world.Goal) worlds
    printMarginal "Player-style posterior" (fun world -> world.PlayerStyle) worlds
    printMarginal "Carried threat belief -> next tick" (fun world -> world.Threat) worlds
    printMarginal "Carried morale belief -> next tick" (fun world -> world.Morale) worlds
    printMarginal "Carried goal belief -> next tick" (fun world -> world.Goal) worlds
    compareFiniteDag beliefState archetype obs

    nextBeliefState

let scenarios =
    [ { Name = "Quiet patrol contact"
        PlayerDistance = 28.0
        HeardGunshot = false
        SawFlank = false
        Health = 96.0
        AllyNearby = true }
      { Name = "Close frontal pressure"
        PlayerDistance = 6.0
        HeardGunshot = true
        SawFlank = false
        Health = 82.0
        AllyNearby = true }
      { Name = "Wounded and flanked"
        PlayerDistance = 7.0
        HeardGunshot = true
        SawFlank = true
        Health = 26.0
        AllyNearby = false } ]

printfn "Active-Inference-inspired game AI demo"
printfn "====================================="
printfn ""

for archetype in [ Cautious; Bold ] do
    printfn "--- Sequential ticks for archetype=%A ---" archetype
    printfn ""

    let mutable beliefState = initialBeliefState

    for scenario in scenarios do
        beliefState <- runScenario beliefState archetype scenario