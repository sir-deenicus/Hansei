# Fun Diversions

## Active-Inference-Inspired Game AI

### The Idea

Replace FSMs, HFSMs, and behaviour trees with a small **probabilistic generative model per agent**. The agent maintains beliefs over latent world-state variables *and* its own internal dispositions, conditions on observations each tick, and selects actions by posterior inference — loosely inspired by Karl Friston's Active Inference / Free Energy Principle.

The agent doesn't follow a fixed policy graph. It *infers* what to do by asking: "given what I've observed, what latent state am I probably in, and which action has the highest expected value under that belief?"

### Why Not FSMs / BTs

| Approach | Limitation |
|---|---|
| FSM / HFSM | Combinatorial explosion of hand-authored transitions; brittle thresholds |
| Behaviour Tree | Still hand-coded priority/fallback logic; no soft evidence weighting |
| AD-Tree per state | Hard to tune conjunction weights; grows unwieldy with variables |

A probabilistic model gives **soft composition** (evidence from multiple variables combines naturally), **implicit personality** (latent priors differ per agent), and **automatic adaptation** (posterior shifts with observations) — all from one compact model definition.

### Architecture

```
  Observations (per tick)
       │
       ▼
┌─────────────────────┐
│  Generative Model   │   A small factored HMM / dynamic Bayes net
│                     │   - Latent: ThreatLevel, Alertness, Aggression, ...
│  prior ──► latent   │   - Observed: PlayerDistance, Health, Noise, ...
│  latent ──► obs     │   - Action: Attack, Flee, Patrol, Investigate, ...
│  latent ──► action  │
└─────────────────────┘
       │
       ▼
  Posterior over Actions
       │
       ▼
  Sample / argmax ──► Execute action
```

Each tick:
1. Condition the model on current observations.
2. Run inference (exact DP for small models, beam/importance for larger ones).
3. Read off the posterior over actions; pick one (sample for variety, argmax for determinism).
4. Optionally carry forward the latent posterior as the prior for the next tick (filtering).

### Optimizations for Real-Time

**Compile to DP DAG.** If the model is a finite discrete chain or rooted tree (which a small agent model typically is), the `FiniteDag` IR + compiled evaluation context runs exact inference in **microseconds** — well within a 16 ms frame budget. This is the primary path.

**Pre-compile the evaluation context.** `prepareDag` builds transition log-tables, observation log-weights, and child arrays once. Repeated per-tick evaluation reuses everything except conditioning on new observations. For a model with $N$ states and $K$ variables, the per-tick cost is $O(K \cdot N^2)$ — trivial for $N \leq 20$, $K \leq 8$.

**Tier the inference frequency.** Not every variable needs updating every frame:
- **Fast tier (every tick):** reactive variables — `ThreatLevel`, `PlayerProximity`.
- **Slow tier (every ~0.5 s):** strategic variables — `Aggression`, `PatrolGoal`.
- **Event-driven:** variables that only change on discrete events — `HasWeapon`, `AllyCount`.

**Pool and reuse models.** Agents with the same archetype share the same compiled DAG context; only the per-agent prior/observation vectors differ.

**Approximate fallback.** If a model outgrows exact DP (too many states or complex dependencies), the stochastic beam searcher provides bounded-time approximate inference with tunable width.

### Non-Handcoded Interestingness

Three mechanisms make behaviour emergent rather than scripted:

**Soft composition.** In an FSM: `if health < 30 && enemyNear then flee`. In the model: low health *shifts the posterior* toward flee-like actions but doesn't hard-gate. An agent at 31 health near a weak enemy might still attack — inference balanced the evidence. Graded, context-sensitive behaviour without enumerating cases.

**Latent personality.** If `Aggression` is a latent variable with a prior, different posterior realisations produce different behavioural profiles from the *same* model. A spectrum of cautious-to-reckless agents from one definition. The latent can drift over time — an agent *becomes* more cautious after repeated damage, without explicit morale code.

**Automatic adaptation.** The agent conditions on player behaviour. If the player always flanks left, the posterior over `PlayerApproachDirection` shifts, and covering-left actions become more probable — emergent counter-strategy, not a scripted one.

### Why This May Be Easier To Author

There is a common claim that FSMs, HFSMs, and BTs are easier to author because every branch is explicit. That is often true only while the system is still small. Once the behaviour graph grows, explicitness turns into a large pile of special cases, thresholds, priority hacks, and transitions that are individually understandable but collectively hard to reason about.

A compact probabilistic model can be easier to control in practice because the real knobs are few and meaningful: priors, likelihood strengths, transition tables, and action conditionals. Instead of hand-authoring every conjunction, we describe a small set of hidden causes and how they influence observations and actions. The result is less literal branch-by-branch control, but often more *global* control because the whole system still fits in our head.

This puts the approach closer to **utility AI** and **GOAP / planners** than to FSMs on the controllability-flexibility spectrum. Utility AI makes action scores explicit; planners make goals and symbolic effects explicit; a probabilistic latent-state model makes uncertainty and hidden causes explicit. All three are attempts to replace large enumerative control graphs with a smaller, more compositional decision structure. The probabilistic version's extra strength is that it handles partial observability and internal hidden state directly rather than treating them as ad hoc blackboard values.

There is also a stronger claim: this style can be viewed as a **generalization** of both utility AI and GOAP. Utility systems already say "combine several influences into a compact action score instead of enumerating branches." Planners already say "represent high-level goals explicitly instead of burying them inside reactive logic." A probabilistic model can absorb both ideas into one representation: latent goals, latent world state, uncertain observations, and action preferences all live inside the same compositional structure. In that sense it is not a completely different point on the spectrum so much as a cleaner expression of the same impulse: compress the authored logic, let behaviour emerge from structure, and keep the knobs few but meaningful.

One practical way to see this is to separate a **strategic goal variable** from the tactical action distribution. The strategic variable plays the role GOAP-style intent would normally play, while action reweighting by expected utility plays the role a utility system would normally play. The probabilistic model then becomes a common substrate under both.

The practical objection is therefore usually not uncontrollability. It is tooling and familiarity: designers are used to trees and scores, not priors and likelihoods. With good visualisation of posteriors, top contributing factors, and action comparisons, a probabilistic model can be both compact and highly tunable while still admitting interesting behaviour.

### Personality, Goals, And Utility

These pieces are not competing layers so much as different kinds of bias acting at different levels.

**Personality priors** are the slowest and most agent-specific. They say what sort of agent this is before the current situation is observed: cautious vs bold, stubborn vs flexible, curious vs disciplined. In the model, these usually appear as priors over latent traits such as `Aggression`, `RiskTolerance`, `Discipline`, or `Curiosity`. They do not choose an action directly; they bias how the agent interprets the world and which goals become plausible.

**Goals** are slower strategic state. They answer "what am I trying to achieve right now?" rather than "what do I do this frame?" In a probabilistic model, goals are usually latent variables conditioned on personality and current inferred world state. A bold agent under medium threat may infer `Engage`; a cautious one with the same observations may infer `Probe` or `HoldPosition`.

**Utility weighting** is the final preference layer over tactical action. Once the model has inferred a posterior over hidden state and strategic goal, utility weighting can re-rank or reweight candidate actions by what is currently desirable. This is where notions like self-preservation, squad cohesion, mission importance, or ammunition conservation can enter.

One useful way to think about the stack is:

1. Personality priors shape the latent interpretation of the situation.
2. Inferred world state plus personality shape the strategic goal posterior.
3. Strategic goal plus inferred world state shape the tactical action posterior.
4. Utility weighting applies a final preference adjustment over those tactical actions.

That makes the architecture feel like a common generalization of utility AI and GOAP: personality priors replace a lot of hand-authored per-agent branching, goals play the role of planner intent, and utility weighting remains as the final local decision pressure.

### Code Sketch

Below is a self-contained sketch using Hansei's `ProbabilitySpace` builder and the `FiniteDag` IR to define and run a small agent model. This is illustrative — a real game would wrap this in a per-agent tick loop.

```fsharp
open Hansei.Probability
open Hansei.DynamicProgrammingDag

// ---------- 1. Define the state/action spaces ----------

type ThreatLevel = Low | Medium | High
type Alertness   = Calm | Wary | Alert
type AgentAction = Patrol | Investigate | Attack | Flee

let threatStates = [| Low; Medium; High |]
let alertStates  = [| Calm; Wary; Alert |]
let actionSpace  = [| Patrol; Investigate; Attack; Flee |]

// ---------- 2. Define a compiled FiniteDag model ----------
// A two-variable chain: ThreatLevel -> Alertness -> Action
// with observations conditioning each step.

let agentDag (observedPlayerDist: float) (observedHealth: float) : FiniteDag<float, string, string> =
    // Encode states as strings for the generic DAG; a real version
    // would use a specialised state type or int-indexed arrays.
    let threatLabels = [| "Low"; "Medium"; "High" |]
    let alertLabels  = [| "Calm"; "Wary"; "Alert" |]
    let actionLabels = [| "Patrol"; "Investigate"; "Attack"; "Flee" |]

    { Nodes =
        [| // Node 0: ThreatLevel (root)
           { Id = 0; ParentId = None; States = threatLabels
             InitialWeight = Some (fun s ->
                 match s with
                 | "Low" -> 0.5 | "Medium" -> 0.35 | _ -> 0.15)
             TransitionWeight = None
             // Observation: closer player -> higher threat likelihood
             Observation = Some (observedPlayerDist, fun s obs ->
                 match s with
                 | "High"   -> exp(-(obs / 5.0))     // high threat more likely when close
                 | "Medium" -> exp(-(obs / 15.0))
                 | _        -> exp(-(obs / 40.0) |> max 0.01 |> min 1.0)) }

           // Node 1: Alertness (child of ThreatLevel)
           { Id = 1; ParentId = Some 0; States = alertLabels
             InitialWeight = None
             TransitionWeight = Some (fun parentState childState ->
                 // P(Alertness | ThreatLevel)
                 match parentState, childState with
                 | "High", "Alert" -> 0.8  | "High", "Wary"  -> 0.15 | "High", _      -> 0.05
                 | "Medium", "Wary" -> 0.6 | "Medium", "Alert" -> 0.25 | "Medium", _ -> 0.15
                 | _, "Calm" -> 0.7        | _, "Wary" -> 0.2         | _, _          -> 0.1)
             Observation = None }

           // Node 2: Action (child of Alertness) — the query node
           { Id = 2; ParentId = Some 1; States = actionLabels
             InitialWeight = None
             TransitionWeight = Some (fun alertState action ->
                 // P(Action | Alertness) — the "policy" is just a conditional table
                 match alertState, action with
                 | "Alert", "Attack" -> 0.5 | "Alert", "Flee"        -> 0.3
                 | "Alert", "Investigate" -> 0.15 | "Alert", _       -> 0.05
                 | "Wary", "Investigate" -> 0.5   | "Wary", "Attack" -> 0.2
                 | "Wary", "Patrol" -> 0.2        | "Wary", _        -> 0.1
                 | _, "Patrol" -> 0.7  | _, "Investigate" -> 0.2     | _, _ -> 0.05)
             // Observation: low health penalises aggressive actions
             Observation = Some (observedHealth, fun action hp ->
                 match action with
                 | "Attack" -> 0.3 + 0.7 * (hp / 100.0)  // attack less likely when hurt
                 | "Flee"   -> 1.0 - 0.7 * (hp / 100.0)  // flee more likely when hurt
                 | _ -> 0.5) } |]
      RootId = 0
      QueryId = 2          // we want the posterior over actions
      Label = id
      Shape = RootedTree }


// ---------- 3. Per-tick inference ----------

let inferAction (playerDistance: float) (agentHealth: float) =
    let dag = agentDag playerDistance agentHealth
    let prepared = Hansei.Model.PrepareFiniteDag(dag)
    let result = Hansei.Model.EvaluateFiniteDag(dag)
    result


// ---------- 4. Alternative: generic ProbabilitySpace model ----------
// For prototyping or richer models that don't fit the DAG IR.

let prob = ProbabilitySpaceBuilder()

let agentModel (playerDist: float) (health: float) =
    prob {
        // Latent threat level
        let! threat =
            distribution [ Low, 0.5; Medium, 0.35; High, 0.15 ]

        // Condition on observed player distance
        let threatLikelihood =
            match threat with
            | High   -> exp(-(playerDist / 5.0))
            | Medium -> exp(-(playerDist / 15.0))
            | Low    -> exp(-(playerDist / 40.0)) |> max 0.01

        // Soft evidence via weighted subtree
        let! _ = distribution [ (), threatLikelihood; (), 0.0 ]

        // Latent alertness given threat
        let! alertness =
            match threat with
            | High   -> distribution [ Alert, 0.8; Wary, 0.15; Calm, 0.05 ]
            | Medium -> distribution [ Wary, 0.6; Alert, 0.25; Calm, 0.15 ]
            | Low    -> distribution [ Calm, 0.7; Wary, 0.2; Alert, 0.1 ]

        // Action given alertness
        let! action =
            match alertness with
            | Alert -> distribution [ Attack, 0.5; Flee, 0.3; Investigate, 0.15; Patrol, 0.05 ]
            | Wary  -> distribution [ Investigate, 0.5; Attack, 0.2; Patrol, 0.2; Flee, 0.1 ]
            | Calm  -> distribution [ Patrol, 0.7; Investigate, 0.2; Attack, 0.05; Flee, 0.05 ]

        // Condition on health: penalise attack when hurt, boost flee
        let healthFactor =
            match action with
            | Attack -> 0.3 + 0.7 * (health / 100.0)
            | Flee   -> 1.0 - 0.7 * (health / 100.0)
            | _      -> 0.5

        let! _ = distribution [ (), healthFactor; (), 0.0 ]

        return action
    }

// Run exact inference on the generic model
let inferActionGeneric playerDist health =
    agentModel playerDist health
    |> Model.ExactInfer
    |> List.map (fun (Value v, p) -> v, p)
    // Normalize
    |> fun xs ->
        let total = xs |> List.sumBy snd
        xs |> List.map (fun (v, p) -> v, p / total)


// ---------- 5. Tick loop sketch ----------

type AgentState =
    { PriorBeliefs: Map<ThreatLevel, float>  // carried forward for filtering
      LastAction: AgentAction option }

let tick (state: AgentState) (playerDist: float) (health: float) =
    let posterior = inferActionGeneric playerDist health
    // Sample for variety, or argmax for determinism
    let chosen =
        posterior
        |> List.maxBy snd
        |> fst
    { state with LastAction = Some chosen }, chosen
```

### Extensions

- **Temporal filtering:** carry the posterior from tick $t$ as the prior for tick $t+1$, creating belief persistence and momentum.
- **Utility weighting:** multiply action likelihoods by a utility function (expected reward) to blend inference with goal-directed planning — this is the full Active Inference loop.
- **Hierarchical models:** nest a strategic-level model (goal selection) above a tactical-level model (action selection), each running at different tick rates.
- **Personality priors:** give each agent archetype a different prior over `Aggression`, `Curiosity`, etc. Same model, different emergent behaviour.
- **Online learning:** update transition tables slowly based on observed outcomes, so agents learn player patterns over a play session.
