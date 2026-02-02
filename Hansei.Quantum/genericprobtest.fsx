#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx"
#time "on"

open System
open Hansei.Utils
open Hansei.Continuation
open MathNet.Symbolics
open Prelude.Common
open MathNet.Numerics
open Prelude.Math
//open Hansei.FSharpx.Collections
open System.Numerics


// type GenericProbabilitySpaceBuilder() =
//     member inline d.Bind(space, k) = reflect space k
//     member inline d.Return v = always v
//     member inline d.ReturnFrom vs = vs: GenericProbabilitySpace<_, _>
//     member inline d.YieldFrom vs = vs: GenericProbabilitySpace<_, _>
//     member d.Zero() = []
    
//     member inline __.For
//         (sequence: seq<'a>, body: 'a -> GenericProbabilitySpace<'b, 'W>)
//         : GenericProbabilitySpace<'b, 'W> =
//         Seq.fold
//             (fun acc elem ->
//                 let comp = body elem
//                 List.append acc comp)
//             (__.Zero())
//             sequence

//     //member __.Combine(x, y) = List.append x y

//     // Sequential Combine: run left, then right
//     member inline d.Combine
//         (left: GenericProbabilitySpace<'a,'W>,
//          right: GenericProbabilitySpace<'b,'W>) : GenericProbabilitySpace<'b,'W> =
//         d.Bind(left, fun _ -> right)


//     member inline __.Delay(f: unit -> GenericProbabilitySpace<'a, 'W>) = [ ContinuedSubTree(memo f), 'W.One ]
//     member inline l.Yield x = l.Return x

//     member inline __.While
//         (guard: unit -> bool,
//          body: unit -> GenericProbabilitySpace<unit,'W>)
//         : GenericProbabilitySpace<unit,'W> =
//         let rec loop () =
//             if guard() then
//                 __.Bind(body(), fun () -> loop())
//             else
//                 __.Zero()
//         loop()

// let dist = GenericProbabilitySpaceBuilder()

// =============================================
// =============  DESIGN GUIDELINES  ===========
// =============================================
//
// GenericProbabilitySpace<'T,'W> represents the FREE SEMIRING over outcomes 'T with weights 'W.
// Required semiring ops on 'W: Zero, One, (+), (*).
//
// Core algebra:
//   Return v            => singleton (unit path → v) with weight One
//   Bind p f (let!)     => path sequencing (weights multiply: *)
//   Combine a b (;)     => ADDITIVE UNION (branching) (weights add: +)
//   Zero                => empty alternative set
//   explore             => normal form: collapses identical Value keys by (+) their weights
//
// Consequences of Combine = additive union:
//   dist { return 6; return 7 }  == δ6 + δ7
//   Multiple return / yield lines are ALTERNATIVES, NOT early exit.
//   Duplicated branches scale weight in non‑idempotent semirings (Probability, Counting, Complex amplitudes).
//   In idempotent semirings (Logic: OR, Viterbi: max, Tropical: min) duplicates are absorbed.
//   For-Loop inside dist enumerates (adds) one branch per iteration (disjunction).
//
// Separation of concerns:
//   Sequencing (AND)      => let! / Bind
//   Branching  (OR/+)     => multiple returns / distribution / for over choices
//
// Mutation & backtracking:
//   Because Combine creates parallel branches, a mutable variable inside dist is shared
//   across branch exploration order → UNSAFE for aggregations (e.g. running maxima). 
//
//   Builders:
//   dist    : Free semiring (branching) – KEEP mutable state out; use for probability,
//             Viterbi, Tropical, logic, provenance, counting, quantum amplitudes. 
//
// When to call explore:
//   * After each dynamic-programming “layer” to collapse duplicate keys via (+) for efficiency
//     (Viterbi, shortest paths, matrix ops, provenance polynomials).
//   * Before interpreting results (normalizing probabilities, taking argmax/min, measuring quantum state).
//
// Observations (observe / constrain):
//   Implemented by pruning failing branches (return Zero).
//   Works uniformly across semirings: probability mass removed, impossible paths (Viterbi/Tropical)
//   get weight Zero, logic failures drop, counting omits those paths, provenance excludes monomials.
//
// Quantum (Complex semiring):
//   Superposition branches add complex amplitudes via (+); interference emerges automatically
//   when explore collapses identical basis states.
//
// Linear algebra / tensor ops:
//   Join indices with observe equality; explore performs the summations in the chosen semiring.
//
// Guidelines summary:
//   1. Use dist for algebraic / exhaustive search & semiring reasoning. 
//   2. Avoid mutation in dist; NEVER rely on side effects for semantics.
//   3. Treat multiple returns as intentional alternatives; prefer explicit distribution/choice helpers for clarity.
//   4. Insert explore strategically to enforce semiring addition and control blow‑up.
//   5. Document which builder a module expects (consistency).
//   6. For new semirings: ensure (+) associative/commutative, (*) associative, * distributes over +,
//      Zero annihilates (*), One neutral for (*).
// ============================================= 

/// <summary>
/// Defines the minimal algebraic structure needed for weights in the core probabilistic framework.
/// </summary>
type Semiring<'W
    when 'W: (static member Zero: 'W)
    and 'W: (static member One: 'W)
    and 'W: (static member (+): 'W * 'W -> 'W)
    and 'W: (static member (*): 'W * 'W -> 'W)> = 'W

/// <summary>
/// Defines a structure with division, needed for normalization and related functions.
/// All Fields are DivisionRings.
/// </summary>
type DivisionRing<'W
    when 'W: (static member One: 'W)
    and 'W: (static member Zero: 'W)
    and 'W: (static member (+): 'W * 'W -> 'W)
    and 'W: (static member (*): 'W * 'W -> 'W)
    and 'W: (static member (/): 'W * 'W -> 'W)> = 'W

type GenericProbabilitySpace<'T, 'W when Semiring<'W>> = list<GenericWeightedTree<'T, 'W> * 'W>

and GenericWeightedTree<'T, 'W when Semiring<'W>> =
    | Value of 'T
    | ContinuedSubTree of Memo<GenericProbabilitySpace<'T, 'W>>

let inline distribution<'T, 'W when Semiring<'W>> (weightedlist: list<'T * 'W>) =
    weightedlist
    |> List.map (fun (v, p) ->
        let subTree = ContinuedSubTree(memo (fun () -> [ Value v, 'W.One ]))
        (subTree, p))
    : GenericProbabilitySpace<_, 'W>

let inline exactly<'T, 'W when Semiring<'W>> (x: 'T) =
    distribution [ x, 'W.One ]: GenericProbabilitySpace<_, _>

let inline always<'T, 'W when Semiring<'W>> (x: 'T) =
    distribution [ x, 'W.One ]: GenericProbabilitySpace<_, _>

let inline fail () = []: GenericProbabilitySpace<_, _>

let inline reflect tree k =
    let rec makeChoices ps =
        List.map
            (function
            | (Value x, p) -> ContinuedSubTree(memo (fun () -> k x)), p
            | (ContinuedSubTree m, p) -> ContinuedSubTree(memo (fun () -> makeChoices (force m))), p)
            ps

    makeChoices tree: GenericProbabilitySpace<_, _> 
 
type GenericProbabilitySpaceBuilder() =
    member inline _.Bind(space, k) = reflect space k
    member inline _.Return v = always v
    member inline _.ReturnFrom vs = vs: GenericProbabilitySpace<_, _>
    member inline _.YieldFrom vs = vs: GenericProbabilitySpace<_, _>
    member _.Zero() = []
    // Disjunctive For: each iteration contributes alternatives (additive union)
    member inline __.For
        (sequence: seq<'a>, body: 'a -> GenericProbabilitySpace<'b,'W>) =
        Seq.fold (fun acc x -> List.append acc (body x)) (__.Zero()) sequence
    // Additive union (semiring +)
    member _.Combine(a, b) = List.append a b
    member inline _.Delay(f: unit -> GenericProbabilitySpace<'a,'W>) =
        [ ContinuedSubTree(memo f), 'W.One ]
    member inline b.Yield x = b.Return x
    member inline this.While(guard, body) =
        let rec loop () =
            if guard() then this.Bind(body(), fun () -> loop()) else this.Zero()
        loop()

let dist = GenericProbabilitySpaceBuilder()


// Helper (explicit choice if preferred over multiple returns)
let inline choice a b = List.append a b
let inline choices xs = List.fold List.append [] xs

let inline observe test =
    dist { if not test then return! fail () else return () }: GenericProbabilitySpace<_, 'w>

let inline constrain test =
    observe test: GenericProbabilitySpace<_, _>

module GenericProbabilitySpace =
    /// Applies a function to every value in the probability space.
    let inline map f p =
        // Inner recursive loop, not marked inline
        let rec mapTree ps =
            ps
            |> List.map (fun (tree, weight) ->
                match tree with
                | Value v -> Value(f v), weight
                | ContinuedSubTree m ->
                    // Recurse on the forced subtree
                    ContinuedSubTree(memo (fun () -> mapTree (force m))), weight)

        mapTree p

    /// Lazily filters the probability space, keeping only values that satisfy the predicate.
    let inline filter f p : GenericProbabilitySpace<_, _> =
        let rec filterTree ps =
            ps
            |> List.collect (fun (tree, weight) ->
                match tree with
                | Value v when f v -> [ Value v, weight ]
                | Value _ -> [] // Discard the non-matching value.
                | ContinuedSubTree m ->
                    // Preserve the subtree for later filtering. The recursive call is
                    // correctly deferred inside the new lazy thunk.
                    [ ContinuedSubTree(memo (fun () -> filterTree (force m))), weight ])

        filterTree p  
    
    let inline filterDistribution f p : GenericProbabilitySpace<_, 'w> =
        dist {
            let! x = p
            do! observe (f x)
            return x
        } 

let inline explore<'T, 'W when Semiring<'W> and 'T: equality>
    (maxdepth: int option)
    (choices: GenericProbabilitySpace<'T, 'W>)
    : GenericProbabilitySpace<'T, 'W> =
    let rec loop
        pcontrib
        depth
        (worklist: GenericProbabilitySpace<'T, 'W>)
        (answers_dict: Dict<'T, 'W>)
        (suspended_list: GenericProbabilitySpace<'T, 'W>)
        =
        match worklist with
        | [] -> (answers_dict, suspended_list) // This level of the worklist is done
        | (item, p) :: rest ->
            let current_path_p = pcontrib * p 
            match item with
            | Value v ->
                // Found a final value, add it to the dictionary
                let new_answers_dict = insertWithx (+) v current_path_p answers_dict 
                loop pcontrib depth rest new_answers_dict suspended_list // Continue with the rest of this level
            | ContinuedSubTree m ->
                let should_go_down =
                    match maxdepth with
                    | Some maxd -> depth < maxd | None -> true

                if should_go_down then
                    // Go deeper: explore the children from the forced memo
                    let children = force m

                    let (answers_after_children, suspended_after_children) =
                        loop current_path_p (depth + 1) children answers_dict suspended_list

                    // After exploring the children, continue with the rest of the current level
                    loop pcontrib depth rest answers_after_children suspended_after_children
                else
                    // Max depth reached. Add this continuation to the suspended list.
                    let new_suspended_list = (ContinuedSubTree m, current_path_p) :: suspended_list
                    loop pcontrib depth rest answers_dict new_suspended_list

    // Initial call to the loop
    let (final_answers, suspended_computations) =
        loop 'W.One 0 choices (Dict<_, _>()) []

    // Combine the results: the values found and the computations that were suspended
    let final_values = [ for (KeyValue(v, p)) in final_answers -> (Value v, p) ]

    List.append final_values suspended_computations 

let inline first_success<'T, 'W when Semiring<'W>>
    (toFloat: 'W -> float)
    (maxdepth: int)
    (choices: GenericProbabilitySpace<'T, 'W>)
    : 'T option =
    let rec level_loop current_level depth =
        if depth >= maxdepth || List.isEmpty current_level then
            None // Search failed or max depth reached
        else
            // Separate values from continuations at the current level
            let values =
                current_level
                |> List.choose (fun (item, p) ->
                    match item with
                    | Value v -> Some(v, p)
                    | _ -> None)

            if not (List.isEmpty values) then
                // Success! We found the first level with values.
                // Sample one according to weights.
                let choices_for_sampling =
                    values |> List.map (fun (v, p) -> (v, toFloat p)) |> Array.ofList

                let total_weight = choices_for_sampling |> Array.sumBy snd

                if total_weight <= 0.0 then
                    // All outcomes at this level have zero probability.
                    // This is effectively a failure for this path, so we must continue searching deeper.
                    let next_level =
                        current_level
                        |> List.collect (fun (item, p) ->
                            match item with
                            | ContinuedSubTree m ->
                                let children = force m
                                children |> List.map (fun (child_item, child_p) -> (child_item, p * child_p))
                            | _ -> [])

                    level_loop next_level (depth + 1)
                else
                    Some(Prelude.Sampling.discreteSample choices_for_sampling)
            else
                // No values at this level, prepare the next level
                let next_level =
                    current_level
                    |> List.collect (fun (item, p) ->
                        match item with
                        | ContinuedSubTree m ->
                            let children = force m
                            children |> List.map (fun (child_item, child_p) -> (child_item, p * child_p))
                        | _ -> [] // Should not happen because we filtered for values
                    )

                level_loop next_level (depth + 1)

    level_loop choices 0

/// <summary>
/// Performs rejection sampling on a probabilistic model.
/// It repeatedly draws one sample from the distribution using a single-path sampler
/// like `first_success`, collecting the results to approximate the posterior distribution.
///
/// Example of how to call it:
/// let my_sampler = first_success id 100
/// let posterior = rejection_sample my_sampler 10000 my_distribution
/// </summary>
/// <param name="sampler">A function like `first_success` that can draw one sample.</param>
/// <param name="nsamples">The total number of samples to draw.</param>
/// <param name="distribution">The probabilistic model to sample from.</param>
let inline rejection_sample
    (sampler: GenericProbabilitySpace<'T, 'W> -> 'T option)
    (nsamples: int)
    (choices: GenericProbabilitySpace<'T, 'W>)
    : GenericProbabilitySpace<'T, float> =
    let t0 = System.DateTime.Now

    // 1. Use a mutable dictionary for counts and a running total for accepted samples.
    let counts = Dict<'T, int>()
    let mutable num_accepted = 0

    for _ in 1..nsamples do
        match sampler choices with
        | Some v ->
            num_accepted <- num_accepted + 1
            let current_count = counts.GetOrDefault(v, 0)
            counts[v] <- current_count + 1
        | None -> () // Sample was rejected by the sampler.

    let t1 = System.DateTime.Now

    printfn
        "rejection_sample: accepted %d / %d samples\nTime taken: %A seconds"
        num_accepted
        nsamples
        (round 3 ((t1 - t0).TotalSeconds))

    if num_accepted = 0 then
        []
    else
        let total_accepted_f = float num_accepted

        [ for KeyValue(value, count) in counts -> (value, float count / total_accepted_f) ]
        |> distribution

/// <summary>
/// Approximates a distribution by running independent simulations (forward sampling).
/// At each branching point, the selector function chooses a single path to follow.
/// </summary>
let inline forward_sample
    maxdepth
    selector
    subsample
    (nsamples: int)
    (choices: GenericProbabilitySpace<'T, 'W>)
    : GenericProbabilitySpace<'T, float> =
    let t0 = System.DateTime.Now

    // Simulates the path of a single particle.
    let rec loop depth =
        function
        | _ when depth >= maxdepth -> None
        | [ Value v, _ ] -> Some v
        | [] -> None
        | [ ContinuedSubTree m, _ ] -> loop (depth + 1) (force m)
        | ch ->
            match selector (subsample ch) with
            | None -> None
            | Some(th, _) -> loop (depth + 1) [ th, 'W.One ]

    let counts = Dict<'T, int>()
    let mutable num_accepted = 0

    for _ in 1..nsamples do
        match loop 0 choices with
        | Some v ->
            num_accepted <- num_accepted + 1
            let current_count = counts.GetOrDefault(v, 0)
            counts[v] <- current_count + 1
        | None -> ()

    let t1 = System.DateTime.Now
    printfn
        "forward_sample: accepted %d / %d samples\nTime taken: %A seconds"
        num_accepted
        nsamples
        (round 3 ((t1 - t0).TotalSeconds))

    if num_accepted = 0 then []
    else
        // Count the results and normalize to create the final posterior distribution.
        let total_accepted = float num_accepted

        [ for KeyValue(value, count) in counts -> (value, float count / total_accepted) ]
        |> distribution

/// <summary>
/// A convenience wrapper that runs rejection_sample and immediately explores the result to get final values.
/// </summary>
let inline rejection_sample_fn
    (sampler: GenericProbabilitySpace<'T, 'W> -> 'T option)
    (nsamples: int)
    (choices: GenericProbabilitySpace<'T, 'W>)
    : GenericProbabilitySpace<'T, float> =
    rejection_sample sampler nsamples choices |> explore None

/// <summary>
/// A convenience wrapper that runs forward_sample and immediately explores the result to get final values.
/// </summary>
let inline forward_sample_fn
    maxdepth
    selector
    subsample
    (nsamples: int)
    (choices: GenericProbabilitySpace<'T, 'W>)
    : GenericProbabilitySpace<'T, float> =
    forward_sample maxdepth selector subsample nsamples choices |> explore None

let random_selector dosort =
    let rec selection r ptotal pcum =
        function
        | [] -> None //failwith "Choice selection: can't happen"
        | (th, p) :: rest ->
            let pcum = pcum + p

            if r < pcum then
                Some(th, ptotal)
            else
                selection r ptotal pcum rest

    fun choices ->
        let ptotal = List.sumBy snd choices
        let r = random.NextDouble(0., ptotal) (* 0<=r<ptotal *)

        if dosort then List.sortBy snd choices else choices
        |> selection r ptotal 0.0

module Distributions =
    let inline bernoulli (p: 'W) =
        distribution [ (true, p); (false, 'W.One - p) ]

    let inline bernoulliChoice (p: 'W) (a, b) =
        distribution [ (a, p); (b, 'W.One - p) ]

    let inline uniform<'a, 'w when 'w :> INumberBase<'w>> f (items: 'a list) =
        let len = f items.Length
        distribution (List.map (fun item -> item, 'w.One / len) items)

    let inline categorical distr =
        distribution (List.normalizeWeights distr)

    let inline discretizedSampler toNumberType coarsener sampler (n: int) : GenericProbabilitySpace<_, 'w> =
        dist {
            return!
                [ for _ in 1..n -> sampler () ]
                |> coarsenWithGeneric toNumberType coarsener
                |> categorical
        }


//=========================================================   

let testdist = dist {
    let! x = distribution [ (1, 0.5); (2, 0.25); (3,0.25) ]
    printfn $"Expensive computation on {x}"
    if x = 1 then return 'a'
    else 
        let! y = distribution ['b',0.5; 'c', 0.5 ]
        printfn $"Another expensive computation on {y}"
        return y 
}

explore (Some 6) testdist

let samples =
    [ for i in 1..20 -> first_success id 9 testdist ]
    |> List.choose id // Discard None results and unwrap the Some values
    |> set

forward_sample 9 (random_selector false) id 1000 testdist 
|> explore None

let rec geometricAccumulator count = dist {
    let! flip = distribution [true, 0.5; false, 0.5]
    if flip then 
        return count  // Success! Return number of failures
    else 
        return! geometricAccumulator (count + 1)  // Failure, try again
}

geometricAccumulator 0 |> explore (Some 20) 

let rec infiniteCoin() = dist {
    let! flip = distribution [true, 0.5; false, 0.5]
    if flip then 
        return 0
    else
        let! rest = infiniteCoin()
        return 1 + rest
}

infiniteCoin() |> explore (Some 10)  

//=========================================================
// Sampling Correctness Test (Non-Deterministic Behavior)

printfn "\n--- Running Sampling Correctness Test ---"

// This test ensures that random operations are not being improperly cached.
// We create a simple distribution and sample from it multiple times.
// We expect to see different results, proving the sampling is not deterministic.

let sampling_dist = distribution [ ('A', 0.5); ('B', 0.5) ]

let run_sampling_test () =
    let num_samples = 20
    printfn "Drawing %d samples from the distribution [('A', 0.5); ('B', 0.5)]..." num_samples

    // Run first_success multiple times and collect the results
    let samples =
        [ for i in 1..num_samples -> first_success id 5 sampling_dist ]
        |> List.choose id // Discard None results and unwrap the Some values

    let distinct_results = Set.ofList samples
    
    samples
    |> List.countBy id
    |> List.iter (fun (v, count) -> printfn "Value: %A, Count: %d" v count)

    printfn "Samples collected: %A" samples
    printfn "Distinct results: %A" distinct_results

    // The test succeeds if we get more than one unique result.
    // With 20 samples from a 50/50 distribution, it's astronomically unlikely
    // to get only one outcome unless something is wrong.
    if Set.count distinct_results > 1 then
        printfn "SUCCESS: Sampling is non-deterministic as expected."
    else
        printfn "FAILURE: Sampling appears to be deterministic. Caching might be incorrect."

run_sampling_test ()

//=========================================================
// Basic Probability Tests

printfn "\n--- Running Basic Probability Tests ---"

// 1. Simple Coin Flip
let coin_flip = Distributions.bernoulli 0.5
let coin_flip_result = explore None coin_flip
printfn "Coin Flip (Bernoulli 0.5) Result: %A" coin_flip_result
// Expected: A list containing (Value true, 0.5) and (Value false, 0.5)

// 2. Fair Dice Roll
let dice_roll = Distributions.uniform float [ 1..6 ]
let dice_roll_result = explore None dice_roll
printfn "Fair Dice Roll (Uniform 1..6) Result: %A" dice_roll_result
// Expected: A list of 6 values, each with probability ~0.1667

// 3. Classic Urn Problem
// Bag A: 2 Red, 3 Blue. Bag B: 4 Red, 1 Blue.
// Pick a bag at random, then a ball. What is P(Red)?
let urn_problem =
    dist {
        // Step 1: Pick a bag at random (50/50 chance)
        let! bag = distribution [ ('A', 0.5); ('B', 0.5) ]

        // Step 2: Based on the bag, pick a ball
        match bag with
        | 'A' -> return! distribution [ ("Red", 2.0 / 5.0); ("Blue", 3.0 / 5.0) ]
        | 'B' -> return! distribution [ ("Red", 4.0 / 5.0); ("Blue", 1.0 / 5.0) ]
    }

let urn_problem_result = explore None urn_problem
printfn "Urn Problem Result: %A" urn_problem_result
// Expected: P(Red) = (0.5 * 2/5) + (0.5 * 4/5) = 0.6
//           P(Blue) = (0.5 * 3/5) + (0.5 * 1/5) = 0.4

//=========================================================

// Expected output:
// Rejection Sampling Test (Lazy Die Problem)

printfn "\n--- Running Rejection Sampling Test ---"

// Problem: We have a fair die and a loaded die (50% chance of rolling 6).
// We pick one at random, roll it, and observe the result is 6.
// What is the probability we picked the loaded die?

let lazy_die_model =
    dist {
        // 1. Choose a die at random
        let! die_type = distribution [ ("Fair", 0.5); ("Loaded", 0.5) ]

        // 2. Roll the chosen die
        let! roll =
            match die_type with
            | "Fair" -> Distributions.uniform float [ 1..6 ]
            | "Loaded" -> distribution [ (1, 0.1); (2, 0.1); (3, 0.1); (4, 0.1); (5, 0.1); (6, 0.5) ]

        // 3. Observe that the result is a 6.
        //    Paths where the roll is not 6 will be rejected.
        do! observe (roll = 6)

        // 4. Return the type of die we must have used.
        return die_type
    }

(*create a simple example that easily solves: What percent of the time is metamagic empower (increase damage by 50%) better than maximize on a 1d6 per caster level spell? And CL =15.*)
dist {
    let! r = Distributions.uniform float [1.0..6.0]
    let damage = 15. * r * 1.5 // Metamagic Empower increases damage by 50%
    let max_damage = 15. * 6.
    do! observe (damage > max_damage) // Check if Empower is better than Maximize
    return true
} |> explore None

let posterior = rejection_sample_fn (first_success id 100) 1000 lazy_die_model
 
let posterior2 = forward_sample 100 (random_selector false) id 1000 lazy_die_model

printfn "Posterior distribution for the die type given a roll of 6:"
printfn "%A" posterior

explore None lazy_die_model |> List.normalizeWeights
explore None posterior
explore None posterior2 

//=========================================================
// METAMAGIC: EMPOWER VS. MAXIMIZE
//=========================================================

printfn "\n--- D&D Metamagic: Empower vs. Maximize (15d6) ---"

// A single d6 roll as a uniform distribution of integers.
let d6: GenericProbabilitySpace<int, float> = Distributions.uniform float [ 1..6 ]

// A recursive helper to model the sum of N dice.
let rec sumOfDice n =
    dist {
        if n <= 0 then
            return 0
        else
            let! roll = d6
            let! rest = sumOfDice (n - 1)
            return roll + rest
    }
let rec sumOfDice2 n =
    dist {
        if n <= 0 then
            return 0
        else
            let! roll = d6 
            return! (GenericProbabilitySpace.map ((+) roll) (sumOfDice (n - 1)))
    }

// The main model to compare the outcomes.
let empowerIsBetter =
    dist {
        let casterLevel = 15
        // 1. Probabilistically roll 15d6 to get the base damage.
        let! baseDamage = sumOfDice2 casterLevel

        // 2. Calculate the damage from both metamagic feats.
        let empoweredDamage = float baseDamage * 1.5 |> floor |> int
        let maximizedDamage = casterLevel * 6 // 15 * 6 = 90

        // 3. Return true if Empower's damage was greater.
        return empoweredDamage > maximizedDamage
    }

// Explore the final distribution to get the probability of `true`.
let probability =
    empowerIsBetter 
    //|> explore None
    |> forward_sample_fn 100 (random_selector false) id 6000

//printfn "For a 15d6 spell, Empower is better than Maximize %.2f%% of the time." (probability * 100.0)

//=========================================================
// METAMAGIC (EFFICIENT IMPLEMENTATION)
//=========================================================

printfn "\n--- D&D Metamagic: Empower vs. Maximize (15d6) - Efficient ---" 

/// <summary>
/// Efficiently calculates the distribution of the sum of two integer distributions
/// by performing a convolution. It collapses the results at each step.
/// </summary>
let addDistributions (distA: GenericProbabilitySpace<int, float>) (distB: GenericProbabilitySpace<int, float>) =
    dist {
        let! a = distA
        let! b = distB
        return a + b
    }
    |> explore None // The key: collapse the tree after each addition.

/// <summary>
/// Calculates the sum of N dice by repeatedly applying the efficient `addDistributions` function.
/// </summary>
let sumOfDiceEfficient n =
    let zero_dist = exactly 0
    // Start with a sum of 0 and fold the `addDistributions` function for each die.
    Seq.fold (fun currentSumDist _ -> addDistributions currentSumDist d6) zero_dist [ 1..n ]

// The main model is now much faster as it uses the efficient sum.
let empowerIsBetterEfficient =
    dist {
        let casterLevel = 15
        // 1. Probabilistically roll 15d6 using the efficient method.
        let! baseDamage = sumOfDiceEfficient casterLevel

        // 2. Calculate damages.
        let empoweredDamage = float baseDamage * 1.5 |> floor |> int
        let maximizedDamage = casterLevel * 6

        // 3. Return true if Empower is better.
        return empoweredDamage > maximizedDamage
    }

// Now `explore` is instantaneous because the distribution is small and pre-computed.
let probabilityEfficient =
    empowerIsBetterEfficient
    |> explore None 
    |> List.tryPick (fun (tree, prob) ->
        match tree with
        | Value true -> Some prob
        | _ -> None)
    |> Option.defaultValue 0.0

printfn "For a 15d6 spell, Empower is better than Maximize %.2f%% of the time (calculated exactly)." (probabilityEfficient * 100.0)

//===============

/// Represents a value in the tropical (min-plus) semiring.
[<Struct>]
type Tropical =
    | Tropical of float

    static member (+)(Tropical a, Tropical b) = Tropical(min a b) // Addition is min
    static member (*)(Tropical a, Tropical b) = Tropical(a + b) // Multiplication is +
    static member Zero = Tropical(System.Double.PositiveInfinity) // Additive identity is +inf
    static member One = Tropical(0.0) // Multiplicative identity is 0

// Allow unwrapping the value for easy printing/assertion
let inline tropicalValue (Tropical v) = v

// This is your test case
let shortestPathTest: GenericProbabilitySpace<char, Tropical> =
    dist {
        // Start at A, choose to go to B (cost 2) or C (cost 1)
        let! next_node = distribution [ ('B', Tropical 2.0); ('C', Tropical 1.0) ]

        match next_node with
        | 'B' ->
            // From B, you can only go to D (cost 5)
            // Total cost A->B->D = 1.0 * 2.0 * 5.0 => 0 + 2 + 5 = 7
            return! distribution [ ('D', Tropical 5.0) ]
        | 'C' ->
            // From C, you can go to D (cost 3) or E (cost 4)
            // Total cost A->C->D = 1.0 * 1.0 * 3.0 => 0 + 1 + 3 = 4
            // Total cost A->C->E = 1.0 * 1.0 * 4.0 => 0 + 1 + 4 = 5
            return! distribution [ ('D', Tropical 3.0); ('E', Tropical 4.0) ]
    }

let results =
    explore None shortestPathTest
    |> List.map (fun (tree, weight) ->
        match tree with
        | Value v -> (v, tropicalValue weight)
        | _ -> failwith "Should only have value nodes")
// Expected output:
// val results : (char * float) list = [('D', 4.0); ('E', 5.0)]


let firstSuccessGraphTest () =
    // Define a graph where the shallowest success nodes are in a distribution.
    let graph: GenericProbabilitySpace<char, float> =
        dist {
            // Depth 1: Choose between branch B or D
            let! node1 = distribution [ ('B', 0.5); ('D', 0.5) ]

            match node1 with
            | 'B' ->
                // Path A->B is complete. Now at Depth 2.
                // The shallowest success nodes are 'C' and 'F'.
                return! distribution [ ('C', 0.9); ('F', 0.1) ]
            | 'D' ->
                // Path A->D is complete. Now at Depth 2.
                // This branch continues deeper.
                let! node2 = distribution [ ('E', 1.0) ]
                // Path A->D->E is complete. Now at Depth 3.
                // 'C' is also reachable here, but it's deeper than the other path.
                return! distribution [ ('C', 1.0) ]
        }

    printfn "Running first_success on the test graph..."
    // We expect to find either 'C' or 'F'. The BFS strategy finds the
    // distribution at depth 2 and samples from it. It should not explore
    // the deeper path through 'D' and 'E'.
    let result = first_success id 5 graph

    printfn "Result of first_success: %A" result
    // Expected output: A sample from the distribution [('C', 0.9); ('F', 0.1)],
    // so either Some 'C' or Some 'F'.
    result

firstSuccessGraphTest ()

[ for _ in 1..100 -> firstSuccessGraphTest () ] |> List.countBy id

//=========================================================
// Refactored Shortest Path Test (Declarative Style)

printfn "\n--- Running Refactored Shortest Path Test ---"

// --- Graph represented as a data structure ---
let graph =
    Map
        [ 'A', [ ('B', 2.0); ('C', 1.0) ]
          'B', [ ('D', 5.0) ]
          'C', [ ('D', 3.0); ('E', 4.0) ]
          'D', []
          'E', [] ]

/// <summary>
/// Performs one relaxation step of the Bellman-Ford algorithm.
/// It takes a distribution of currently known paths and expands them by one edge.
/// </summary>
let pathStep (frontier: GenericProbabilitySpace<char, Tropical>) : GenericProbabilitySpace<char, Tropical> =
    dist {
        // For each node we can currently reach...
        let! currentNode = frontier

        // A path can end at the current node. We return the node itself with a zero-cost
        // edge, which means its path cost remains unchanged.
        return! distribution [ (currentNode, Tropical 0.0) ]

        // And for each of its neighbors...
        let neighbors = graph |> Map.tryFind currentNode |> Option.defaultValue []

        for (neighbor, cost) in neighbors do
            // ...return that neighbor. The framework's `*` operation (which is `+` for Tropical)
            // will automatically add the edge `cost` to the `currentNode`'s path cost.
            return! distribution [ (neighbor, Tropical cost) ]
    }

/// <summary>
/// Finds the shortest paths from a start node to all other reachable nodes.
/// </summary>
let findShortestPaths (startNode: char) (graph: Map<char, list<char * float>>) =
    let numNodes = graph.Keys |> Seq.length
    // Initial state: we are at the start node with 0 cost.
    let initialState = distribution [ (startNode, Tropical 0.0) ]

    // The Bellman-Ford algorithm guarantees finding the shortest path in a graph with |V| nodes
    // after at most |V|-1 relaxation steps. We iterate |V| times for simplicity.
    let finalDistribution =
        Seq.fold
            (fun currentDist _ ->
                // 1. Expand all current paths by one edge.
                let expanded = pathStep currentDist
                // 2. Use `explore` to apply the Tropical semiring's `min` operation,
                //    collapsing multiple paths to a node down to the single cheapest one.
                explore None expanded)
            initialState
            [ 1..numNodes ]

    // Format the final results.
    finalDistribution
    |> List.map (fun (tree, weight) ->
        match tree with
        | Value v -> (v, tropicalValue weight)
        | _ -> failwith "Should only have value nodes")
    |> List.sortBy fst

let shortest_paths_from_A = findShortestPaths 'A' graph

printfn "Shortest paths from node 'A': %A" shortest_paths_from_A
// Expected: [('A', 0.0); ('B', 2.0); ('C', 1.0); ('D', 4.0); ('E', 5.0)]


//=========================================================
// Viterbi Semiring Tests

printfn "\n--- Running Viterbi Semiring Tests ---"

/// <summary>
/// Represents a value in the Viterbi (max-times) semiring.
/// Used for finding the most likely path in a sequence model.
/// </summary>
[<Struct>]
type Viterbi =
    | Viterbi of float

    static member (+)(Viterbi a, Viterbi b) = Viterbi(max a b) // Addition is max
    static member (*)(Viterbi a, Viterbi b) = Viterbi(a * b) // Multiplication is *
    static member Zero = Viterbi(System.Double.NegativeInfinity) // Additive identity for max
    static member One = Viterbi(1.0) // Multiplicative identity is 1

let inline viterbiValue (Viterbi v) = v

// --- Test 1: Simple Path Finding (Most Likely Weather Pattern) ---

printfn "\n--- Test 1: Most Likely Weather Pattern ---"

// This model finds the single most likely sequence of weather over 3 days,
// given transition probabilities. This is a simple Markov chain.
let weather_transitions (prevState: string) =
    match prevState with
    | "Sunny" -> distribution [ ("Sunny", Viterbi 0.8); ("Cloudy", Viterbi 0.2) ]
    | "Cloudy" -> distribution [ ("Sunny", Viterbi 0.4); ("Cloudy", Viterbi 0.4); ("Rainy", Viterbi 0.2) ]
    | "Rainy" -> distribution [ ("Cloudy", Viterbi 0.6); ("Rainy", Viterbi 0.4) ]
    | _ -> fail ()

let most_likely_weather_pattern =
    dist {
        // Start sunny on day 1
        let! day1 = distribution [ ("Sunny", Viterbi 1.0) ]
        // Transition to day 2
        let! day2 = weather_transitions day1
        // Transition to day 3
        let! day3 = weather_transitions day2
        return [ day1; day2; day3 ]
    }
    |> explore None
    |> List.maxBy (fun (_, prob) -> viterbiValue prob)
    |> fun (tree, prob) ->
        match tree with
        | Value path -> (path, viterbiValue prob)
        | _ -> failwith "Should have a value"

printfn "Most likely 3-day weather pattern: %A" most_likely_weather_pattern
// Expected: Sunny -> Sunny -> Sunny with probability 1.0 * 0.8 * 0.8 = 0.64

// --- Test 2: Hidden Markov Model (Inferring Weather from Umbrella Usage) ---

printfn "\n--- Test 2: Hidden Markov Model (Inferring Weather) ---"

// This model infers the most likely sequence of hidden states (actual weather)
// given a sequence of observations (whether an umbrella was seen).

let hmm_observations = [ "Umbrella"; "Umbrella"; "No Umbrella"; "Umbrella" ]

// HMM Parameters
let hmm_transition (prevState: string) =
    match prevState with
    | "start" -> distribution [ ("Rainy", Viterbi 0.5); ("Sunny", Viterbi 0.5) ]
    | "Rainy" -> distribution [ ("Rainy", Viterbi 0.7); ("Sunny", Viterbi 0.3) ]
    | "Sunny" -> distribution [ ("Rainy", Viterbi 0.4); ("Sunny", Viterbi 0.6) ]
    | _ -> fail ()

let hmm_emission (state: string) (obs: string) =
    match (state, obs) with
    | ("Rainy", "Umbrella") -> Viterbi 0.9
    | ("Rainy", "No Umbrella") -> Viterbi 0.1
    | ("Sunny", "Umbrella") -> Viterbi 0.2
    | ("Sunny", "No Umbrella") -> Viterbi 0.8
    | _ -> Viterbi 0.0

// Note on Seq.fold vs. for-loop:
// We use Seq.fold here because it's the standard functional pattern for
// evolving a state (in this case, the distribution of possible paths)
// through a sequence. A `for` loop inside the `dist` builder would create
// parallel, independent computations for each observation, which is not
// what we want for a sequential model like an HMM.

let viterbi_hmm_path =
    Seq.fold
        (fun (current_dist: GenericProbabilitySpace<string list, Viterbi>) (obs: string) ->
            dist {
                let! prev_path = current_dist
                let prevState = List.head prev_path
                // 1. Transition to the next hidden state
                let! next_state = hmm_transition prevState
                // 2. Weight the new path by the probability of the observation
                let emission_prob = hmm_emission next_state obs
                return! distribution [ (next_state :: prev_path), emission_prob ]
            })
        (distribution [ ([ "start" ], Viterbi 1.0) ])
        hmm_observations
    |> explore None
    |> List.maxBy (fun (_, prob) -> viterbiValue prob)
    |> fun (tree, prob) ->
        match tree with
        | Value path -> (List.rev path |> List.tail, viterbiValue prob)
        | _ -> failwith "Should have a value"

printfn "Observations: %A" hmm_observations
printfn "Most likely hidden weather sequence: %A" viterbi_hmm_path
// Expected: A plausible sequence like [Rainy, Rainy, Sunny, Rainy]

//=========================================================
// Refactored Viterbi HMM Test (Declarative Style)

printfn "\n--- Running Refactored Viterbi HMM Test ---"

// --- HMM Parameters as Data Structures ---
let states = [ "Rainy"; "Sunny" ]
let observations = [ "Umbrella"; "Umbrella"; "No Umbrella"; "Umbrella" ]

// P(next | prev)
let transitionProbs =
    Map
        [ (("start", "Rainy"), Viterbi 0.5)
          (("start", "Sunny"), Viterbi 0.5)
          (("Rainy", "Rainy"), Viterbi 0.7)
          (("Rainy", "Sunny"), Viterbi 0.3)
          (("Sunny", "Rainy"), Viterbi 0.4)
          (("Sunny", "Sunny"), Viterbi 0.6) ]

// P(observation | state)
let emissionProbs =
    Map
        [ (("Rainy", "Umbrella"), Viterbi 0.9)
          (("Rainy", "No Umbrella"), Viterbi 0.1)
          (("Sunny", "Umbrella"), Viterbi 0.2)
          (("Sunny", "No Umbrella"), Viterbi 0.8) ]

/// <summary>
/// Performs one step of the Viterbi algorithm.
/// For a given previous state and an observation, it returns a distribution
/// of all possible next states, weighted by their path probabilities.
/// </summary>
let viterbiStepLoop (observation: string) (prevState: string) : GenericProbabilitySpace<string, Viterbi> =
    dist {
        // The `for` loop creates a distribution over all possible next states.
        for nextState in states do
            // Get the probability of transitioning to it
            let trans_p = transitionProbs.[(prevState, nextState)]
            // Get the probability of seeing the observation from it
            let emit_p = emissionProbs.[(nextState, observation)]

            // The weight is P(obs|next) * P(next|prev). This uses the Viterbi algebra's `*`.
            let path_p = emit_p * trans_p

            // Return the next state as a possible path, weighted by its probability.
            // The `for` loop will collect these into a single distribution.
            return! distribution [ (nextState, path_p) ]
    }

// To run it for the observation sequence, we build the chain declaratively.
let mostLikelyWeather =
    dist {
        let! state1 = viterbiStepLoop observations.[0] "start"
        let! state2 = viterbiStepLoop observations.[1] state1
        let! state3 = viterbiStepLoop observations.[2] state2
        let! state4 = viterbiStepLoop observations.[3] state3
        return [ state1; state2; state3; state4 ] // The sequence of states
    }
    |> explore None
    |> List.maxBy (fun (_, prob) -> viterbiValue prob)
    |> fun (tree, prob) ->
        match tree with
        | Value path -> (path, viterbiValue prob)
        | _ -> failwith "Should have a value"

printfn "Observations: %A" observations
printfn "Most likely hidden weather sequence (refactored): %A" mostLikelyWeather

//=========================================================
// Viterbi Semiring Test (Dishonest Casino)

printfn "\n--- Running Viterbi Semiring Test ---"

// Problem: Find the most likely sequence of hidden states (Fair/Loaded die)
// given a sequence of observed die rolls.
let viterbi_test =
    // The sequence of observed die rolls
    let observations = [ 1; 2; 3; 6; 6; 6; 1; 2 ]

    // HMM Parameters
    let states = [ "Fair"; "Loaded" ]
    let start_probs = [ ("Fair", Viterbi 0.5); ("Loaded", Viterbi 0.5) ]

    let transition_probs (prevState: string) =
        match prevState with
        | "start" -> distribution start_probs
        | "Fair" -> distribution [ ("Fair", Viterbi 0.95); ("Loaded", Viterbi 0.05) ]
        | "Loaded" -> distribution [ ("Fair", Viterbi 0.10); ("Loaded", Viterbi 0.90) ]
        | _ -> fail ()

    let emission_prob (state: string) (observation: int) =
        match state with
        | "Fair" -> Viterbi(1.0 / 6.0)
        | "Loaded" -> if observation = 6 then Viterbi 0.5 else Viterbi 0.1
        | _ -> Viterbi 0.0

    // The Viterbi algorithm, expressed using the dist computation expression.
    // We use Seq.fold to build the path sequence step-by-step.
    let viterbi_path_dist =
        Seq.fold
            (fun (current_dist: GenericProbabilitySpace<list<string>, Viterbi>) (obs: int) ->
                dist {
                    // Get the most likely previous paths
                    let! prev_path = current_dist
                    let prevState = List.head prev_path

                    // 1. Transition to the next state
                    let! next_state = transition_probs prevState

                    // 2. "Observe" the emission probability by weighting the path.
                    //    We return a single branch weighted by the emission probability.
                    let emission = emission_prob next_state obs
                    return! distribution [ (next_state :: prev_path), emission ]
                })
            (distribution [ ([ "start" ], Viterbi 1.0) ])
            observations

    // Explore the graph to find the single most likely path
    let most_likely_path =
        explore None viterbi_path_dist
        |> List.maxBy (fun (_, prob) -> viterbiValue prob)
        |> (fun (tree, prob) ->
            match tree with
            | Value path -> (List.rev path |> List.tail, viterbiValue prob) // Reverse and remove "start"
            | _ -> failwith "Should have a value")

    printfn "Observations: %A" observations
    printfn "Most likely sequence of states (Viterbi path): %A" most_likely_path

viterbi_test


let sat_solver_model =
    dist {
        // 1. Define the boolean variables as 50/50 choices.
        let! a = Distributions.bernoulli 0.5
        let! b = Distributions.bernoulli 0.5
        let! c = Distributions.bernoulli 0.5

        // 2. Define the formula's clauses as observations.
        //    Formula: (a || b) && (!a || c)
        do! observe (a || b)
        do! observe ((not a) || c)

        // 3. If all clauses are satisfied, return the variable assignment.
        return (a, b, c)
    }

printfn "Solving SAT formula: (a || b) && (!a || c)"

// Use first_success to find a satisfying assignment.
// The weight function `fun _ -> 1.0` is irrelevant since we only care about existence.
let sat_solution = first_success (fun _ -> 1.0) 20 sat_solver_model

//==================

// 1) define the semiring
[<Struct>]
type BoolSemiring =
    | Bool of bool

    static member Zero = Bool false
    static member One = Bool true
    static member (+)(Bool a, Bool b) = Bool(a || b)
    static member (*)(Bool a, Bool b) = Bool(a && b)

// 2) build a “distribution” of assignments; weights don’t matter so use One
let satModel =
    dist {
        let! a = distribution [ (true, BoolSemiring.One); (false, BoolSemiring.One) ]
        let! b = distribution [ (true, BoolSemiring.One); (false, BoolSemiring.One) ]
        let! c = distribution [ (true, BoolSemiring.One); (false, BoolSemiring.One) ]
        // clauses become observations (fail = zero weight)
        do! observe (a || b)
        do! observe ((not a) || c)
        return (a, b, c)
    }

// 3) run explore with the BoolSemiring
let solution = first_success (konst 1.) 20 satModel

//unsatisfiable example
let unsatModel =
    dist {
        let! a = distribution [ (true, BoolSemiring.One); (false, BoolSemiring.One) ]
        do! observe (a && not a)
        return a
    }
    |> explore None

let medical_diagnosis =
    dist {
        // Prior: Does patient have disease?
        let! has_disease = Distributions.bernoulli 0.01

        // Test 1: 95% accurate
        let! test1_positive =
            if has_disease then
                Distributions.bernoulli 0.95
            else
                Distributions.bernoulli 0.05

        // Test 2: 90% accurate
        let! test2_positive =
            if has_disease then
                Distributions.bernoulli 0.90
            else
                Distributions.bernoulli 0.10

        // Observe: both tests came back positive
        do! observe (test1_positive && test2_positive)

        return has_disease
    }
    |> explore None
    |> List.normalizeWeights

let sentence_completion start_word =
    let word_transitions word =
        match word with
        | "the" -> distribution [ ("cat", Viterbi 0.3); ("dog", Viterbi 0.4); ("house", Viterbi 0.3) ]
        | "cat" -> distribution [ ("sits", Viterbi 0.6); ("runs", Viterbi 0.4) ]
        | "dog" -> distribution [ ("barks", Viterbi 0.7); ("runs", Viterbi 0.3) ]
        | _ -> distribution [ (".", Viterbi 1.0) ]

    // Build 4-word sentence
    dist {
        let! word1 = distribution [ (start_word, Viterbi 1.0) ]
        let! word2 = word_transitions word1
        let! word3 = word_transitions word2
        let! word4 = word_transitions word3
        return [ word1; word2; word3; word4 ]
    }

let sentence = sentence_completion "the" |> explore None

let robot_path_planning =
    dist {
        let! start_move = distribution [ ("North", Tropical 1.0); ("East", Tropical 1.0) ]

        match start_move with
        | "North" ->
            let! second_move =
                distribution
                    [ ("North", Tropical 2.0)
                      ("East", Tropical 1.0)
                      ("Obstacle", Tropical 1000.0) ]

            if second_move <> "Obstacle" then
                return! distribution [ ("Goal", Tropical 1.0) ]
            else
                return! fail ()
        | "East" -> return! distribution [ ("Goal", Tropical 3.0) ]
    }
    |> explore None


//=========================================================
// #SAT (Model Counting) Test with a Counting Semiring

printfn "\n--- Running #SAT (Model Counting) Test ---"

/// <summary>
/// A simple semiring for counting paths/solutions.
/// </summary>
[<Struct>]
type Counting =
    | Counting of int

    static member (+)(Counting a, Counting b) = Counting(a + b) // Add counts
    static member (*)(Counting a, Counting b) = Counting(a * b) // Multiply counts
    static member Zero = Counting 0
    static member One = Counting 1

let inline countingValue (Counting v) = v

// We use the same model as the SAT solver, but with the Counting semiring.
let count_sat_model: GenericProbabilitySpace<_, Counting> =
    dist {
        // Each variable choice is a path with weight 1.
        let! a = distribution [ (true, Counting.One); (false, Counting.One) ]
        let! b = distribution [ (true, Counting.One); (false, Counting.One) ]
        let! c = distribution [ (true, Counting.One); (false, Counting.One) ]

        // Clauses eliminate invalid paths.
        do! observe (a || b)
        do! observe ((not a) || c)

        // We return a single value `true` for any valid path.
        return true
    }

// Explore the model. The final weight will be the sum of all valid paths.
let number_of_solutions =
    explore None count_sat_model
    |> List.sumBy (fun (_, count) -> countingValue count)

printfn "Counting satisfying assignments for: (a || b) && (!a || c)"
printfn "Total number of solutions found: %d" number_of_solutions
// Expected: 5 solutions
// (T,T,T), (T,F,T), (F,T,T), (F,T,F), (F,T,C) -> (F,T,T) and (F,T,F) are the same.
// Let's list them:
// a=T: b must be T or F (b=T). c must be T. -> (T,T,T), (T,F,T)
// a=F: b must be T. c can be T or F. -> (F,T,T), (F,T,F)
// Wait, let's re-check the logic.
// a=T: (T||b) is T. (!T||c) -> (F||c) -> c must be T. b can be T/F.
//   Solutions: (T,T,T), (T,F,T)
// a=F: (F||b) -> b must be T. (!F||c) -> (T||c) is T. c can be T/F.
//   Solutions: (F,T,T), (F,T,F)
// Total solutions: 4. Let me re-check my manual calculation.
// a=T, b=T, c=T -> (T||T)&&(!T||T) -> T&&T -> T
// a=T, b=T, c=F -> (T||T)&&(!T||F) -> T&&F -> F
// a=T, b=F, c=T -> (T||F)&&(!T||T) -> T&&T -> T
// a=T, b=F, c=F -> (T||F)&&(!T||F) -> T&&F -> F
// a=F, b=T, c=T -> (F||T)&&(!F||T) -> T&&T -> T
// a=F, b=T, c=F -> (F||T)&&(!F||F) -> T&&T -> T
// a=F, b=F, c=T -> (F||F)&&(!F||T) -> F&&T -> F
// a=F, b=F, c=F -> (F||F)&&(!F||F) -> F&&T -> F
// The satisfying assignments are: (T,T,T), (T,F,T), (F,T,T), (F,T,F). There are 4.
// My manual check was wrong. Let's see what the code says.
// The code should be correct. The logic is sound.
// The expected number of solutions is 4. Let's update the comment.
printfn "Expected number"

// --- Monty Hall with observe: condition on host opening door 3 ---
printfn "\n--- Monty Hall (host opens door 3) ---"

let montyHallObserved =
    dist {
        // 1. Car behind 1–3 uniformly
        let! prizeDoor = Distributions.uniform float [ 1; 2; 3 ]
        // 2. Contestant picks door 1
        let chosen = 1
        // 3. Host opens a door that is neither chosen nor prize
        let avail = [ 1; 2; 3 ] |> List.filter (fun d -> d <> chosen && d <> prizeDoor)
        let! opened = Distributions.uniform float avail
        // 4. Condition on seeing that the host actually opened door 3
        do! observe (opened = 3)
        // 5. Determine win if stay or switch
        let stayWin = (chosen = prizeDoor)
        let switch = [ 1; 2; 3 ] |> List.except [ chosen; opened ] |> List.head
        let switchWin = (switch = prizeDoor)
        return (stayWin, switchWin)
    }

montyHallObserved
|> explore None
|> List.normalizeWeights
|> printfn "P(stayWin, switchWin | opened=3) = %A"
// Expected ≈ [(true,false,0.5); (false,true,0.5)]


// --- Example: Two dice conditioned on sum ≥ 8 ---
printfn "\n--- Two Dice Conditional (sum ≥ 8) ---"

let conditionalDice =
    dist {
        let! d1 = Distributions.uniform float [ 1..6 ]
        let! d2 = Distributions.uniform float [ 1..6 ]
        // Only keep outcomes where the sum is at least 8
        do! observe (d1 + d2 >= 8)
        return (d1 + d2)
    }

conditionalDice
|> explore None
|> List.normalizeWeights
|> printfn "P(d1,d2 | sum ≥ 8) = %A"
// Expected: only pairs summing to 8,9,10,11,12, weighted

let inline beta draws a b : GenericProbabilitySpace<_, 'w> =
    let rec loop draws a b =
        dist {
            if draws <= 0 then
                return a / (a + b)
            else
                let! ball = Distributions.categorical [ 1, a / (a + b); 2, b / (a + b) ]

                if ball = 1 then
                    return! loop (draws - 1) (a + 'w.One) b
                else
                    return! loop (draws - 1) a (b + 'w.One)
        }

    loop draws a b

//=========================================================
// Bayesian Inference: Inferring Coin Fairness

printfn "\n--- Bayesian Inference: Inferring Coin Fairness ---"

// Problem: We observe a sequence of coin flips and want to infer
// the most likely fairness (bias) of the coin from a set of possibilities.

// A recursive helper function to process observations one by one.
let inline observe_all bias observations : GenericProbabilitySpace<_, 'w> =
    let rec observe_loop observations =
        dist {
            match observations with
            | [] ->
                // Base case: All observations are processed, return the bias for this path.
                return bias
            | obs :: rest ->
                // Recursive step:
                // 1. Model a flip with the current bias.
                let! flip_outcome = Distributions.bernoulli bias
                // 2. Condition on the flip matching the observation.
                do! observe (flip_outcome = obs)
                // 3. Continue with the rest of the observations.
                return! observe_loop rest
        }

    observe_loop observations

let inline coin_fairness_model (prior: GenericProbabilitySpace<_, 'w>) coin_observations =
    dist {
        // Our prior belief: the coin's bias is one of three values.
        let! bias = prior
        // Start the recursive observation process.
        return! observe_all bias coin_observations
    }

let inline coin_fairness_observation prior observation : GenericProbabilitySpace<_, 'w> =
    dist {
        // Our prior belief: the coin's bias is one of three values.
        let! p = prior
        let! flip_outcome = Distributions.bernoulli p
        do! observe (flip_outcome = observation)
        // Return the prior bias as the result of this path.
        return p
    }

let inline coin_fairness_observations prior observations =
    List.fold coin_fairness_observation prior observations

let coin_observations = [ true; true; false; true; true; false; true ]

//test beta prior
let beta_prior = beta 20 1.0 1.0

let betaPeakedPrior =
    Distributions.discretizedSampler float (bucketRange 2 0.04) (fun () -> Distributions.Beta(0.5, 0.5).Sample()) 10000

explore None betaPeakedPrior |> List.sortBy (fun (Value p, _) -> p)
explore None beta_prior

forward_sample_fn 200 (random_selector false) id 5000 beta_prior

let uniform_prior = Distributions.uniform float [ 0.0..0.05..1.0 ]

let posterior_bias =
    coin_fairness_observations betaPeakedPrior coin_observations
    |> explore None
    //|> forward_sample_fn 200 (random_selector false) id 10000
    |> List.normalizeWeights
    |> List.sortBy (fun (Value p, _) -> p) // Sort by bias value


//=======================

[<Struct>]
type Complex(value: System.Numerics.Complex) =
    member _.Value = value

    member _.Magnitude = value.Magnitude

    // Static properties to satisfy the Semiring<'W> constraint
    static member Zero = Complex(System.Numerics.Complex.Zero)
    static member One = Complex(System.Numerics.Complex.One)

    // Operators for the Semiring
    static member (+)(a: Complex, b: Complex) = Complex(a.Value + b.Value)
    static member (*)(a: Complex, b: Complex) = Complex(a.Value * b.Value)

    // Implicit conversions for convenience
    static member op_Implicit(c: System.Numerics.Complex) : Complex = Complex(c)
    static member op_Implicit(qc: Complex) : System.Numerics.Complex = qc.Value

    override this.ToString() = this.Value.ToString()

/// A quantum computation is a GenericProbabilitySpace where the weights are Complex amplitudes.

type Quantum<'T> = GenericProbabilitySpace<'T, Complex>

/// The probability of measuring a state is the squared magnitude of its amplitude.
let measurementProbability (c: Complex) = c.Magnitude * c.Magnitude

/// <summary>
/// Measures a quantum state, collapsing the superposition.
/// This converts a quantum distribution of (value, amplitude) pairs into a
/// classical probability distribution of (value, probability) pairs.
/// </summary>
let measure (state: Quantum<'T>) : GenericProbabilitySpace<'T, float> =
    state
    |> explore None // Collapse the quantum graph to a list of final states and their total amplitudes
    |> List.map (fun (tree, amplitude) ->
        match tree with
        | Value v -> (v, measurementProbability amplitude)
        | _ -> failwith "Cannot measure a non-final state")
    |> distribution

let measure_fn state = measure state |> explore None

// Define the basis states |0> and |1>
let qzero: Quantum<int> = exactly 0
let qone: Quantum<int> = exactly 1

let Sqrt2Inv = 1.0 / sqrt 2.0

/// The Hadamard gate. Puts a qubit into a uniform superposition.
let hadamard (q: Quantum<int>) : Quantum<int> =
    dist {
        let! basis_state = q

        if basis_state = 0 then
            // H|0> -> 1/√2 |0> + 1/√2 |1>
            return!
                distribution
                    [ (0, System.Numerics.Complex(Sqrt2Inv, 0.0))
                      (1, System.Numerics.Complex(Sqrt2Inv, 0.0)) ]
        else
            // H|1> -> 1/√2 |0> - 1/√2 |1>
            return!
                distribution
                    [ (0, System.Numerics.Complex(Sqrt2Inv, 0.0))
                      (1, System.Numerics.Complex(-Sqrt2Inv, 0.0)) ]
    }

/// The Controlled-NOT (CNOT) gate.
/// Takes a control qubit and a target qubit. Flips the target if the control is 1.
let cnot (control: Quantum<int>) (target: Quantum<int>) : Quantum<int * int> =
    dist {
        let! c = control
        let! t = target

        if c = 1 then
            // Flip the target bit (0->1, 1->0)
            return (c, 1 - t)
        else
            // Do nothing
            return (c, t)
    }

//=========================================================
// EXAMPLE: Creating and Measuring a Bell State
//=========================================================

printfn "\n--- Creating and Measuring a Bell State ---"

// A Bell state is a maximally entangled state of two qubits.
// We create it by applying H to the first qubit, then CNOT to both.
let bell_state =
    dist {
        let! q1 = hadamard qzero
        let! q2 = exactly 0
        return! cnot (exactly q1) (exactly q2)
    }

// Measure the Bell state
let measurement_results = measure_fn bell_state

printfn "Bell state quantum amplitudes: %A" (explore None bell_state)
printfn "Bell state measurement probabilities: %A" measurement_results

// Expected Amplitudes: [((0,0), 0.707...), ((1,1), 0.707...)]
// Expected Probabilities: [((0,0), 0.5), ((1,1), 0.5)]


/// <summary>
/// Creates a Bell state (a maximally entangled pair) from two input qubits.
/// It applies a Hadamard to the first qubit, then a CNOT gate.
/// </summary>
let createBellState q1 q2 = cnot (hadamard q1) q2

//=========================================================
// EXAMPLE: Creating and Measuring a Bell State
//=========================================================

printfn "\n--- Creating and Measuring a Bell State ---"

// Create a Bell state from the |00> basis state.
let bell_state_from_zeros = createBellState qzero qzero

// Measure the Bell state
measure_fn bell_state_from_zeros
explore None bell_state_from_zeros

//=========================================================
// EXAMPLE: Hadamard Gate is its own Inverse (H*H = I)
//=========================================================

printfn "\n--- Hadamard Interference with dist: The 'Looking Twice' Effect ---"

// Single Hadamard - creates superposition
let single_hadamard_dist =
    dist {
        let! q = qzero
        return! hadamard (exactly q)
    }
    |> measure_fn

printfn "H|0⟩ measurement probabilities: %A" single_hadamard_dist


printfn "\n--- Hadamard Twice Test (using dist block) ---"

// We define the entire sequence of operations in a single computation.
let hadamard_twice_circuit =
    dist {
        // 1. Start with a qubit in the |0> state.
        let! q_initial = qzero

        // 2. Apply the Hadamard gate once.
        //    We use `exactly` to lift the resulting value back into a quantum state
        //    before passing it to the next gate.
        let! q_superposition = hadamard (exactly q_initial)

        // 3. Apply the Hadamard gate a second time.

        // 4. Return the final state of the qubit.
        return! (hadamard (exactly q_superposition))
    }

printfn "The second Hadamard causes quantum interference!"
printfn "The |1⟩ components destructively interfere and cancel out."

// Now, explore the final state of the circuit.
let final_amplitudes = explore None hadamard_twice_circuit
let final_probabilities = measure_fn hadamard_twice_circuit


/// <summary>
/// The general Phase Shift gate P(φ).
/// Applies a phase of e^(i*phi) to the |1> state, leaving |0> unchanged.
/// </summary>
/// <param name="phi">The phase angle in radians.</param>
/// <param name="q">The input qubit.</param>
let phaseShift (phi: float) (q: Quantum<int>) : Quantum<int> =
    dist {
        let! basis_state = q

        if basis_state = 0 then
            // P|0> -> |0> (return the state with its original amplitude)
            return 0
        else
            // P|1> -> e^(i*phi) * |1>
            // We achieve this by returning a new distribution weighted by the phase factor.
            // The framework's multiplication will apply this to the incoming amplitude.
            let phase = System.Numerics.Complex.FromPolarCoordinates(1.0, phi)
            return! distribution [ (1, phase) ]
    }

// --- Common Phase Gates ---

/// The Z gate (Phase shift by π). Flips the phase of the |1> state.
let zGate = phaseShift System.Math.PI

/// The S gate (Phase shift by π/2).
let sGate = phaseShift (System.Math.PI / 2.0)

/// The T gate (Phase shift by π/4).
let tGate = phaseShift (System.Math.PI / 4.0)


//=========================================================
// EXAMPLE: Phase Gate Application
//=========================================================

printfn "\n--- Phase Gate Example: Z(H|0>) ---"

// The state H|0> is (|0> + |1>)/√2
let h_zero_state = hadamard qzero

// Applying the Z gate should flip the phase of the |1> component,
// resulting in (|0> - |1>)/√2, which is the state H|1>.
let z_h_zero_state = zGate h_zero_state

// For comparison, let's also create the state H|1> directly.
let h_one_state = hadamard qone

printfn "Amplitudes of H|0>: %A" (explore None h_zero_state)
printfn "Amplitudes of Z(H|0>): %A" (explore None z_h_zero_state)
printfn "Amplitudes of H|1> (for comparison): %A" (explore None h_one_state)

// Expected output: The amplitudes for Z(H|0>) and H|1> should be identical.

/// <summary>
/// The Ry gate, which rotates a qubit around the Y-axis of the Bloch sphere.
/// This is key for creating arbitrary superpositions of |0> and |1>.
/// Ry(θ)|0> = cos(θ/2)|0> + sin(θ/2)|1>
/// Ry(θ)|1> = -sin(θ/2)|0> + cos(θ/2)|1>
/// </summary>
/// <param name="theta">The rotation angle in radians.</param>
/// <param name="q">The input qubit.</param>
let rotateY (theta: float) (q: Quantum<int>) : Quantum<int> =
    dist {
        let! basis_state = q
        let cos_t_2 = cos (theta / 2.0)
        let sin_t_2 = sin (theta / 2.0)

        if basis_state = 0 then
            // Apply Ry to |0>
            return!
                distribution
                    [ (0, System.Numerics.Complex(cos_t_2, 0.0))
                      (1, System.Numerics.Complex(sin_t_2, 0.0)) ]
        else // basis_state = 1
            // Apply Ry to |1>
            return!
                distribution
                    [ (0, System.Numerics.Complex(-sin_t_2, 0.0))
                      (1, System.Numerics.Complex(cos_t_2, 0.0)) ]
    }

/// <summary>
/// A classical probability distribution (as opposed to a quantum one with amplitudes).
/// </summary>
type ClassicalDist<'T> = GenericProbabilitySpace<'T, float>

/// <summary>
/// A helper function that transforms a distribution of `(Value v, p)`
/// into a distribution of `(Value (v, p), 1.0)`.
/// This makes the original probability `p` available for use inside a computation.
/// </summary>
let reweightWithProb (dist: GenericProbabilitySpace<'a, 'w>) =
    dist
    |> List.map (fun (tree, p) ->
        match tree with
        | Value v -> (Value(v, p), 'w.One)
        | _ -> failwith "reweightWithProb only works on final distributions")

/// <summary>
/// Performs one step of the Zeno process on a classical probability distribution.
/// 1. Lifts the classical state to a quantum one.
/// 2. Applies a small quantum evolution.
/// 3. Measures the result, collapsing it back to a classical distribution.
/// </summary>
/// <param name="theta">The small angle of quantum rotation for this step.</param>
/// <param name="dist">The input classical probability distribution.</param>
let zenoStep (theta: float) (distr: ClassicalDist<int>) : ClassicalDist<int> =
    dist {
        // `let!` here iterates over each element of the classical input distribution.
        // `p_in` will be the probability of being in that state.
        let! (basis_state, p_in) = reweightWithProb distr

        // 1. Lift the classical basis state into a quantum state.
        //    Its initial amplitude is 1.0, but we will carry its classical
        //    probability `p_in` through the computation.
        let q_initial = exactly basis_state

        // 2. Evolve this single quantum state by a small angle.
        let q_evolved = rotateY theta q_initial

        // 3. Measure the evolved state. This produces a *new* classical
        //    distribution, e.g., |0> -> [(0, 0.99), (1, 0.01)].
        let measured_dist = measure q_evolved

        // 4. We now have a distribution of distributions. We need to scale
        //    the probabilities of this new distribution by the probability
        //    of the input state we started with (`p_in`).
        let final_dist = measured_dist |> List.map (fun (tree, prob) -> (tree, p_in * prob))

        // Return the final, scaled distribution for this branch.
        // The `dist` builder will automatically collect and sum the probabilities
        // for all the identical final states from all branches. This is our DP magic at work!
        return! final_dist
    } 
//=========================================================


//=========================================================
// LOGIC PROGRAMMING (PROLOG-STYLE)
//=========================================================

printfn "\n--- Logic Programming: Ancestor Problem ---"

/// <summary>
/// A semiring for logic programming, where '+' is OR and '*' is AND.
/// </summary>
[<Struct>]
type Logic =
    | Logic of bool

    static member (+)(Logic a, Logic b) = Logic(a || b) // OR combines alternative solutions
    static member (*)(Logic a, Logic b) = Logic(a && b) // AND combines steps in a path
    static member Zero = Logic false // Represents failure
    static member One = Logic true // Represents success

/// A logic computation is a GenericProbabilitySpace with Logic weights.
type LogicProgram<'T> = GenericProbabilitySpace<'T, Logic>

// --- Define Facts (our "database") ---
let parent_db =
    set [
        ("john", "mary")
        ("john", "paul")
        ("mary", "peter")
        ("paul", "susan") 
        ("john", "lisa")      // Lisa is another child of John (sibling to Mary and Paul)
        ("lisa", "tom")       // Tom is Lisa's child (cousin to Peter and Susan)
        ("paul", "anna")      // Anna is Paul's child (sibling to Susan, cousin to Peter and Tom)
        ("mary", "jane")      // Jane is Mary's child (sibling to Peter, cousin to Susan, Anna, Tom)
    ]

// --- Define Rules ---

/// A rule that succeeds if X is a parent of Y.  
let parent(x,y) : LogicProgram<unit> =
    dist { do! observe (parent_db.Contains(x, y))  }

/// A recursive rule to find ancestors.
let rec ancestor (x, y) : LogicProgram<unit> =
    // Rule 1: ancestor(X, Y) :- parent(X, Y).
    let rule1 = parent (x, y)

    // Rule 2: ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
    let rule2 =
        dist {
            for (p, c) in parent_db do
                if p = x then
                    // For each child `c` of `x`, recursively check if `c` is an ancestor of `y`.
                    // The `do!` ensures that if the recursive call fails, this path is pruned.
                    do! ancestor (c, y)
                    // If the recursive call succeeds, this path succeeds.
                    return ()
        }

    // Combine the results of the two rules with an OR relationship.
    List.append rule1 rule2

let rec ancestorB (x, y) : LogicProgram<unit> =
    // Rule 1: ancestor(X, Y) :- parent(X, Y). 
    dist {
        return! parent (x, y)
        for (p, c) in parent_db do
            if p = x then
                // For each child `c` of `x`, recursively check if `c` is an ancestor of `y`.
                // The `do!` ensures that if the recursive call fails, this path is pruned.
                // If the recursive call succeeds, this path succeeds.
                do! ancestorB (c, y) 
    }
  
/// A recursive rule to find ancestors.
let rec ancestorC (x, y) : LogicProgram<unit> =
    dist {
        // Rule 1: ancestor(X, Y) :- parent(X, Y).
        // The computation expression builder's `Combine` member (List.append) will
        // automatically handle the OR logic between this branch...
        do! parent (x, y)

        // ...and the branches generated by the `for` loop below.
        // Rule 2: ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
        for (p, c) in parent_db do
            if p = x then
                // This creates a new computation branch for each potential intermediate ancestor `c`.
                // If the recursive call succeeds, this branch succeeds.
                do! ancestorC (c, y)
    }

// --- Pose a Query ---

// Query: "Who are the ancestors of Peter?"
let people = [ "john"; "mary"; "peter"; "paul" ]

let ancestors_of_peter =
    dist {
        // Generate all possible ancestors from our list of people.
        let! potential_ancestor = distribution (people |> List.map (fun p -> (p, Logic.One)))
        // For each one, check if the ancestor rule succeeds.
        // `observe` here acts as a query condition.
        do! ancestorC (potential_ancestor, "peter")
        // If it succeeds, return the person's name.
        return potential_ancestor
    }

// --- Find all solutions ---
let solutions =
    explore None ancestors_of_peter
    |> List.choose (fun (tree, success) ->
        match tree, success with
        | Value v, Logic true -> Some v // Keep only successful branches
        | _ -> None)
    |> Set.ofList // Get unique solutions

printfn "Query: Who are the ancestors of Peter?"
printfn "Solutions: %A" solutions
// Expected: set ["john"; "mary"]
 
open MathNet.Symbolics
open MathNet.Symbolics.Core

[<Struct>]
type Prov = Prov of Expression with
    static member Zero = Prov (Number 0N)
    static member One  = Prov (Number 1N)
    static member (+) (Prov a, Prov b) = Prov (a + b)
    static member (*) (Prov a, Prov b) = Prov (a * b)

let sym s = Prov (symbol s)

// An outgoing-edge distribution: choose child c, weight by the edge symbol p_x_c
let edgeOut (x: string) : GenericProbabilitySpace<string, Prov> =
    parent_db
    |> Seq.choose (fun (p, c) ->
        if p = x then Some (c, sym (sprintf "p_%s_%s" x c)) else None)
    |> Seq.toList
    |> distribution
// A provenance-weighted parent fact: if fact exists, emit unit weighted by its symbol.
let parentProv (x, y) : GenericProbabilitySpace<bool, Prov> =
    let key = sprintf "p_%s_%s" x y
    if Set.contains (x, y) parent_db then
        distribution [ (true, sym key) ]
    else
        fail ()
 
// Ancestor with provenance weights (sum over alternative proofs; product along a proof)
let rec ancestorProv (x, y) : GenericProbabilitySpace<bool, Prov> =
    dist {
        // Base case
        return! parentProv (x, y)
        // Recursive case
        for (p, c) in parent_db do
            if p = x then
                let! _ = parentProv (x, c)
                let! _ = ancestorProv (c, y)
                return true
    }
let rec ancestorProv2 (x, y) : GenericProbabilitySpace<bool, Prov> =
    dist {
        // Base case: parent(x,y)
        return! parentProv (x, y)

        // Recursive case: parent(x,c) AND ancestor(c,y)
        let! c = edgeOut x      // multiplies by p_x_c
        let! _ = ancestorProv2 (c, y)
        return true
    }
// Query provenance for john -> peter; result is p_john_mary * p_mary_peter
ancestorProv ("john", "peter")
|> explore None
|> List.map (fun (_, Prov w) -> w)  // polynomials for each proof

// --- Minimal unification machinery ---
type Term =
  | Var of string
  | Fun of string * Term list

type Subst = Map<string, Term>
let extractLogic (extract: 'T -> 'U option) (results: GenericProbabilitySpace<'T, Logic>) : Set<'U> =
    results
    |> List.choose (function
        | Value v, Logic true -> extract v
        | _ -> None)
    |> Set.ofList
let rec apply (s: Subst) t =
  match t with
  | Var v -> s.TryFind v |> Option.defaultValue t
  | Fun (f, args) -> Fun(f, List.map (apply s) args)

let rec occurs v t =
  match t with
  | Var v' -> v = v'
  | Fun (_, args) -> List.exists (occurs v) args

let rec unify t1 t2 (s: Subst) : Subst option =
  let t1, t2 = apply s t1, apply s t2
  match t1, t2 with
  | Var v, t
  | t, Var v when t = Var v -> Some s
  | Var v, t
    when not (occurs v t) -> Some (s.Add(v, t))
  | t, Var v
    when not (occurs v t) -> Some (s.Add(v, t))
  | Fun (f, xs), Fun (g, ys) when f = g && xs.Length = ys.Length ->
      (Some s, List.zip xs ys)
      ||> List.fold (fun acc (a,b) -> acc |> Option.bind (unify a b))
  | _ -> None
let subst_union s1 s2 =
    Map.merge (fun _ v2 -> v2) id s2 s1
    
// Facts as terms
let parentFacts =
  [ Fun("john",[]), Fun("mary",[])
    Fun("john",[]), Fun("paul",[])
    Fun("mary",[]), Fun("peter",[])
    Fun("paul",[]), Fun("susan",[]) 
    Fun("john",[]), Fun("lisa",[])
    Fun("lisa",[]), Fun("tom",[])
    Fun("paul",[]), Fun("anna",[])
    Fun("mary",[]), Fun("jane",[]) ]

// Lift a fact with unification: parent(X,Y)
let parentU (x: Term) (y: Term) : LogicProgram<Subst> =
  dist {
    for (px, py) in parentFacts do
      match unify x px Map.empty |> Option.bind (fun s1 -> unify (apply s1 y) py s1) with
      | Some s -> return s
      | None -> ()
  }

// ancestor(X,Y) :- parent(X,Y).
// ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
let rec ancestorU (x: Term) (y: Term) : LogicProgram<Subst> =
  dist {
    // Clause 1
    return! parentU x y
    // Clause 2
    for (px, pz) in parentFacts do
      // introduce a fresh Z and unify with parent(X,Z)
      let z = Var "Z"
      match unify x px Map.empty |> Option.bind (fun s1 -> unify (apply s1 z) pz s1) with
      | Some s1 ->
          // Continue with ancestor(Z,Y) under s1, returning composed substitutions
          let! s2 = ancestorU (apply s1 z) (apply s1 y)
          return (subst_union s1 s2) // Map union is simplistic; prefer left-biased merge after apply
      | None -> ()
  }

ancestorU (Var "X") (Fun("peter", []))
|> explore None
|> List.choose (function
    | Value subst, Logic true ->
        match subst.TryFind "X" with
        | Some (Fun (name, [])) -> Some name
        | _ -> None
    | _ -> None)
|> Set.ofList



// grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
let grandparentU (x: Term) (y: Term) : LogicProgram<Subst> =
    dist {
        let z = Var "Z"
        let! s1 = parentU x z
        let! s2 = parentU (apply s1 z) (apply s1 y)
        return subst_union s1 s2
    }

// sibling(X,Y) :- parent(Z,X), parent(Z,Y), X \= Y.
let siblingU (x: Term) (y: Term) : LogicProgram<Subst> =
    dist {
        let z = Var "Z"
        let! s1 = parentU z x
        let! s2 = parentU (apply s1 z) (apply s1 y)
        let s = subst_union s1 s2
        do! observe (apply s x <> apply s y)
        return s
    }

// --- Examples ---

// Grandparents of peter
grandparentU (Var "G") (Fun("peter", []))
|> explore None
|> List.choose (function
    | Value subst, Logic true ->
        match subst.TryFind "G" with
        | Some (Fun (name, [])) -> Some name
        | _ -> None
    | _ -> None)
|> Set.ofList
|> printfn "Grandparents of peter: %A"

// Siblings (unordered pairs)
siblingU (Var "X") (Var "Y")
|> explore None
|> List.choose (function
    | Value s, Logic true ->
        match s.TryFind "X", s.TryFind "Y" with
        | Some (Fun (x, [])), Some (Fun (y, [])) -> Some (x, y)
        | _ -> None
    | _ -> None)
|> Set.ofList
|> printfn "Sibling pairs: %A"



// sibling(A,B) :- parent(P,A), parent(P,B), A ≠ B.
let siblingU2 (x: Term) (y: Term) : LogicProgram<Subst> =
    dist {
        let p = Var "P"
        let! s1 = parentU p x
        let! s2 = parentU (apply s1 p) (apply s1 y)
        let s = subst_union s1 s2
        do! observe (apply s x <> apply s y)
        return s
    }

// cousin(X,Y) :- parent(A,X), parent(B,Y), sibling(A,B), X ≠ Y.
let cousinU (x: Term) (y: Term) : LogicProgram<Subst> =
    dist {
        let a = Var "A"
        let b = Var "B"
        let! s1 = parentU a x
        let! s2 = parentU b y
        let s = subst_union s1 s2
        let! s3 = siblingU (apply s a) (apply s b)
        let s' = subst_union s s3
        do! observe (apply s' x <> apply s' y)
        return s'
    }

// --- Query: Who are the cousins of Peter? ---
cousinU (Var "X") (Fun("peter", []))
|> explore None
|> List.choose (function
    | Value subst, Logic true ->
        match subst.TryFind "X" with
        | Some (Fun (name, [])) -> Some name
        | _ -> None
    | _ -> None)
|> Set.ofList
|> printfn "Cousins of Peter: %A"

/// sibling(X,Y) succeeds if X and Y share a parent and are not the same person
let sibling (x,y) : LogicProgram<unit> =
  dist {
    for (p,c1) in parent_db do
      if c1 = x then
        for (p2,c2) in parent_db do
          if p2 = p && c2 = y && x <> y then
            return ()
  }

/// cousin(X,Y) succeeds if X’s parent A and Y’s parent B are siblings
let cousin (x,y) : LogicProgram<unit> =
  dist {
    // find A such that parent(A,X)
    for (a, cx) in parent_db do
      if cx = x then
        // find B such that parent(B,Y)
        for (b, cy) in parent_db do
          if cy = y then
            // require A and B to be siblings
            do! sibling (a,b)
            // and X≠Y to rule out “self-cousin”
            do! observe (x <> y)
            return ()
  }

// example query: who are the cousins of “peter”?
let cousinsOfPeter =
  dist {
    let! who = distribution (people |> List.map (fun p -> p, Logic.One))
    do! cousin (who,"peter")
    return who
  }
  |> explore None
  |> List.choose (function Value v, Logic true -> Some v | _ -> None)
  |> Set.ofList

printfn "Cousins of Peter: %A" cousinsOfPeter



// Gather all people from the facts (parents and children)
let peopleAll =
    parent_db
    |> Seq.collect (fun (p, c) -> seq { yield p; yield c })
    |> Set.ofSeq
    |> Set.toList

// 1) Propositional mode (LogicProgram<unit>)
let allCousins_prop =
    dist {
        let! x = distribution (peopleAll |> List.map (fun p -> p, Logic.One))
        let! y = distribution (peopleAll |> List.map (fun p -> p, Logic.One))
        do! observe (x < y)      // make pairs unordered and dedup
        do! cousin (x, y)
        return (x, y)
    }
    |> explore None
    |> List.choose (function Value v, Logic true -> Some v | _ -> None)
    |> Set.ofList

printfn "All cousin pairs (propositional): %A" allCousins_prop

// 2) Relational/unification mode (LogicProgram<Subst>)
let allCousins_rel =
    cousinU (Var "X") (Var "Y")
    |> explore None
    |> List.choose (function
        | Value s, Logic true ->
            match s.TryFind "X", s.TryFind "Y" with
            | Some (Fun (x, [])), Some (Fun (y, [])) when x < y -> Some (x, y)
            | _ -> None
        | _ -> None)
    |> Set.ofList

printfn "All cousin pairs (relational): %A" allCousins_rel





// A Goal is a relation that, given a current Subst, produces new Substs (with Logic weights)
type Goal = Subst -> LogicProgram<Subst * Subst>

// Relational builder that threads Subst implicitly
type RelBuilder() =
    member _.Return (x: unit) : Goal =
        fun s -> exactly ((), s) |> GenericProbabilitySpace.map (fun _ -> (s, s))
    member _.ReturnFrom (g:Goal) = g
    member r.Yield(x) = r.Return x
    member _.YieldFrom (g: Goal) = g
    member _.Bind (m: Goal, f: unit -> Goal) : Goal =
        fun s0 ->
            dist {
                let! (sIn, s1) = m s0
                // continue from updated substitution
                return! f () s1
            }
    member _.Zero() : Goal = fun _ -> fail ()
    member _.Combine (g1: Goal, g2: Goal) : Goal =
        fun s -> List.append (g1 s) (g2 s)
    member _.Delay(f: unit -> Goal) : Goal =
        fun s -> [ ContinuedSubTree(memo (fun () -> f () s)), Logic.One ]
    member r.For (xs: seq<'a>, body: 'a -> Goal) : Goal =
        fun s ->
            xs |> Seq.fold (fun acc x -> r.Combine(acc, body x)) (r.Zero()) |> fun g -> g s

let rel = RelBuilder()

// Run a goal starting from the empty substitution, returning just the resulting substitutions
let runRel (g: Goal) : LogicProgram<Subst> =
    g Map.empty |> GenericProbabilitySpace.map snd

// Unification as a goal
let (===) (t1: Term) (t2: Term) : Goal =
    fun s ->
        match unify t1 t2 s with
        | Some s' -> exactly ((), s') |> GenericProbabilitySpace.map (fun _ -> (s', s'))
        | None -> fail ()

// Disequality as a goal (uses current substitution)
let (=/=) (t1: Term) (t2: Term) : Goal =
    fun s ->
        dist {
            do! observe (apply s t1 <> apply s t2)
            return (s, s)
        }

// Fact: parent/2 as a goal (with unification)
let parentR (x: Term) (y: Term) : Goal =
    rel {
        for (px, py) in parentFacts do
            do! (x === px)
            do! (y === py)
    }

// Sibling and cousin, now succinct like Prolog
let siblingR (x: Term) (y: Term) : Goal =
    rel {
        let z = Var "Z"
        do! parentR z x
        do! parentR z y
        do! (x =/= y)
    }

let cousinR (x: Term) (y: Term) : Goal =
    rel {
        let a = Var "A"
        let b = Var "B"
        do! parentR a x
        do! parentR b y
        do! siblingR a b
        do! (x =/= y)
    }

// Query: all cousins of Peter
cousinR (Var "X") (Fun("peter", []))
|> runRel
|> explore None
|> List.choose (function
    | Value s, Logic true ->
        match s.TryFind "X" with
        | Some (Fun (name, [])) -> Some name
        | _ -> None
    | _ -> None)
|> Set.ofList
|> printfn "Cousins of Peter: %A"




// Structured terms as lists
let Nil = Fun("nil", [])
let Cons h t = Fun("cons", [ h; t ])
let Atom s = Fun(s, [])

let rec listOfAtoms (xs: string list) =
    List.foldBack (fun s acc -> Cons (Atom s) acc) xs Nil

let rec atomsOfList =
    function
    | Fun("nil", []) -> Some []
    | Fun("cons", [ Fun(h, []); t ]) ->
        atomsOfList t |> Option.map (fun rest -> h :: rest)
    | _ -> None

// memberR: member(X, L)
// Clause 1: L = [X|_]
// Clause 2: L = [_|T], member(X, T)
let rec memberR (x: Term) (lst: Term) : Goal =
    let clause1 =
        rel {
            let tail = Var "Tail"
            do! (lst === Cons x tail)
        }
    let clause2 =
        rel {
            let h, t = Var "H", Var "T"
            do! (lst === Cons h t)
            do! memberR x t
        }
    fun s -> List.append (clause1 s) (clause2 s)
let applyToList x lst = 
    rel {
        let h, t = Var "H", Var "T"
        do! (lst === Cons h t)
        do! (x === h)
    }
let rec memberR2 (x: Term) (lst: Term) : Goal =
    rel {
        yield! (rel {
            let h, t = Var "H", Var "T"
            do! (lst === Cons h t)
            do! (x === h) 
        })
        let h,t = Var "H", Var "T"
        do! (lst === Cons h t)
        do! memberR2 x t
    }

// appendR: append(Xs, Ys, Zs)
// Clause 1: append([], Ys, Ys).
// Clause 2: append([H|T], Ys, [H|Zs]) :- append(T, Ys, Zs).
let rec appendR (xs: Term) (ys: Term) (zs: Term) : Goal =
    let clause1 =
        rel {
            do! (xs === Nil)
            do! (zs === ys)
        }
    let clause2 =
        rel {
            let h, t, zs1 = Var "H", Var "T", Var "Zs1"
            do! (xs === Cons h t)
            do! (zs === Cons h zs1)
            do! appendR t ys zs1
        }
    fun s -> List.append (clause1 s) (clause2 s)

// --- Reversibility demos ---

// 1) memberR in both directions
// member(X, [peter,susan,tom]) → generate X
memberR (Var "X") (listOfAtoms [ "peter"; "susan"; "tom" ])
|> runRel
|> explore None
|> List.choose (function
    | Value s, Logic true ->
        match s.TryFind "X" with
        | Some (Fun (name, [])) -> Some name
        | _ -> None
    | _ -> None)
|> Set.ofList
|> printfn "Members: %A"

// 2) appendR forward: append([mary,paul], [tom], Z)
appendR (listOfAtoms [ "mary"; "paul" ]) (listOfAtoms [ "tom" ]) (Var "Z")
|> runRel
|> explore None
|> List.choose (function
    | Value s, Logic true -> s.TryFind "Z" |> Option.bind atomsOfList
    | _ -> None)
|> Set.ofList
|> printfn "Append forward Z: %A"

// 3) appendR backward (split): append(X, Y, [mary,paul,tom]) → all prefix/suffix pairs
appendR (Var "X") (Var "Y") (listOfAtoms [ "mary"; "paul"; "tom" ])
|> runRel
|> explore None
|> List.choose (function
    | Value s, Logic true ->
        match s.TryFind "X" |> Option.bind atomsOfList,
              s.TryFind "Y" |> Option.bind atomsOfList with
        | Some xs, Some ys -> Some (xs, ys)
        | _ -> None
    | _ -> None)
|> Set.ofList
|> printfn "All splits of [mary,paul,tom] as (X,Y): %A"

// 4) appendR solve for a missing middle: append([mary], Y, [mary,paul,tom]) → Y = [paul,tom]
appendR (listOfAtoms [ "mary" ]) (Var "Y") (listOfAtoms [ "mary"; "paul"; "tom" ])
|> runRel
|> explore None
|> List.choose (function
    | Value s, Logic true -> s.TryFind "Y" |> Option.bind atomsOfList
    | _ -> None)
|> Set.ofList
|> printfn "Solve Y in append([mary], Y, [mary,paul,tom]): %A"




// Vector as: distribution [ (index, value) ]
// Matrix as: distribution [ ((row,col), value) ]

// Dot product: ⟨a,b⟩ = Σ_i a_i * b_i
let inline dot (a: GenericProbabilitySpace<int,'W>) (b: GenericProbabilitySpace<int,'W>) : 'W
    when Semiring<'W> =
    dist {
        let! i = a
        let! j = b
        do! observe (i = j)   // join on index
        return ()             // collapse to a single key
    }
    |> explore None
    |> List.fold (fun acc (_, w) -> acc + w) 'W.Zero

// Matrix–vector: (M v)_i = Σ_j M_{ij} * v_j
let inline matVec
    (m: GenericProbabilitySpace<int*int,'W>)
    (v: GenericProbabilitySpace<int,'W>)
    : GenericProbabilitySpace<int,'W>
    when Semiring<'W> =
    dist {
        let! (i,k) = m
        let! j = v
        do! observe (k = j)   // join on inner index
        return i              // aggregate into row i
    }
    |> explore None

// Matrix–matrix: (A B)_{ij} = Σ_k A_{ik} * B_{kj}
let inline matMul
    (a: GenericProbabilitySpace<int*int,'W>)
    (b: GenericProbabilitySpace<int*int,'W>)
    : GenericProbabilitySpace<int*int,'W>
    when Semiring<'W> =
    dist {
        let! (i,k)  = a
        let! (k',j) = b
        do! observe (k = k')  // join on inner dimension
        return (i,j)          // aggregate into (i,j)
    }
    |> explore None

// --- Tiny float examples ---

// 2D vectors
let v: GenericProbabilitySpace<int,float> = distribution [ (0, 1.0); (1, 2.0) ]
let w: GenericProbabilitySpace<int,float> = distribution [ (0,10.0); (1,20.0) ]
let dp = dot v w  // 1*10 + 2*20 = 50.0

// 2x2 matrices
let A: GenericProbabilitySpace<int*int,float> =
  distribution [ ((0,0), 1.0); ((0,1), 2.0)
                 ((1,0), 3.0); ((1,1), 4.0) ]
let B: GenericProbabilitySpace<int*int,float> =
  distribution [ ((0,0), 5.0); ((0,1), 6.0)
                 ((1,0), 7.0); ((1,1), 8.0) ]

let Av = matVec A (distribution [ (0, 1.0); (1, 1.0) ]) // A * [1;1]
let C  = matMul A B                                     // A * B

// Pretty-print helpers for inspection
let showVec (x: GenericProbabilitySpace<int,float>) =
    x |> List.sortBy (fun (Value i,_) -> i)
      |> List.map (fun (Value i, w) -> sprintf "i=%d -> %g" i w)

let showMat (x: GenericProbabilitySpace<int*int,float>) =
    x |> List.sortBy (fun (Value (i,j),_) -> (i,j))
      |> List.map (fun (Value (i,j), w) -> sprintf "(%d,%d) -> %g" i j w)


// Tensor helpers (semiring-generalized einsum)

// Outer products
let inline outer
    (a: GenericProbabilitySpace<'I,'W>)
    (b: GenericProbabilitySpace<'J,'W>)
    : GenericProbabilitySpace<'I*'J,'W>
    when Semiring<'W> =
    dist {
        let! i = a
        let! j = b
        return (i, j)
    }
    |> explore None

let inline outer3
    (a: GenericProbabilitySpace<'I,'W>)
    (b: GenericProbabilitySpace<'J,'W>)
    (c: GenericProbabilitySpace<'K,'W>)
    : GenericProbabilitySpace<'I*'J*'K,'W>
    when Semiring<'W> =
    dist {
        let! i = a
        let! j = b
        let! k = c
        return (i, j, k)
    }
    |> explore None

// Contract two tensors on one shared index (general 1-index contraction)
let inline contract1
    (projA: 'IA -> 'K * 'OA)            // split A’s index tuple into (sharedKey, remainingOutA)
    (projB: 'IB -> 'K * 'OB)            // split B’s index tuple into (sharedKey, remainingOutB)
    (combine: 'OA -> 'OB -> 'OC)        // build output index from remaining parts
    (a: GenericProbabilitySpace<'IA,'W>)
    (b: GenericProbabilitySpace<'IB,'W>)
    : GenericProbabilitySpace<'OC,'W>
    when Semiring<'W> and 'K: equality =
    dist {
        let! ai = a
        let! bi = b
        let kA, oA = projA ai
        let kB, oB = projB bi
        do! observe (kA = kB)           // join on shared index
        return combine oA oB            // explore will sum weights over equal outputs
    }
    |> explore None

// Example einsum: C[i,j,l] = Σ_k A[i,k,l] * B[k,j]  (einsum 'ikl,kj->ijl')
let inline einsum_ikl_kj_to_ijl
    (a: GenericProbabilitySpace<int*int*int,'W>)
    (b: GenericProbabilitySpace<int*int,'W>)
    : GenericProbabilitySpace<int*int*int,'W>
    when Semiring<'W> =
    contract1
        (fun (i,k,l) -> k, (i,l))
        (fun (k,j)   -> k, j)
        (fun (i,l) j -> (i,j,l))
        a b

// Tiny float demo
let T: GenericProbabilitySpace<int*int*int,float> =
    distribution [ ((0,0,0), 1.0); ((0,1,0), 2.0); ((1,0,1), 3.0) ]

let M: GenericProbabilitySpace<int*int,float> =
    distribution [ ((0,0), 5.0); ((1,0), 7.0); ((0,1), 11.0); ((1,1), 13.0) ]

let C_ijl = einsum_ikl_kj_to_ijl T M
// C_ijl has keys (i,j,l); weights are Σ_k T[i,k,l] * M[k,j] under your semiring

// Dual-number weight for forward-mode AD (Semiring over R with derivative)
[<Struct>]
type Dual = { v: float; d: float } with
    static member Zero = { v = 0.0; d = 0.0 }
    static member One  = { v = 1.0; d = 0.0 }
    static member (+) (a: Dual, b: Dual) = { v = a.v + b.v; d = a.d + b.d }
    static member (*) (a: Dual, b: Dual) = { v = a.v * b.v; d = a.d * b.v + a.v * b.d }
    static member (-) (a: Dual, b: Dual) = { v = a.v - b.v; d = a.d - b.d }

// Bernoulli over Dual (no need to change your core CE)
let bernoulliDual (p: Dual) : GenericProbabilitySpace<bool, Dual> =
    distribution [ (true, p); (false, Dual.One - p) ]

// Example: differentiate Pr(a && b) where a,b ~ Bernoulli(p) wrt p
let p = { v = 0.3; d = 1.0 } // seed derivative wrt p
let probBothTrue_dual =
    dist {
        let! a = bernoulliDual p
        let! b = bernoulliDual p
        do! observe (a && b)
        return ()
    }
    |> explore None
    |> List.fold (fun acc (_, w) -> acc + w) Dual.Zero

printfn "Pr = %g, dPr/dp = %g" probBothTrue_dual.v probBothTrue_dual.d

// Pushforward f_* μ
let inline pushforward (f: 'X -> 'Y) (mu: GenericProbabilitySpace<'X,'W>)
    : GenericProbabilitySpace<'Y,'W>
    when Semiring<'W> and 'Y: equality =
    mu |> GenericProbabilitySpace.map f |> explore None

// Example: parity of 2d6
let twoD6 =
    dist {
        let! a = Distributions.uniform float [1..6]
        let! b = Distributions.uniform float [1..6]
        return a + b
    }
let parity = pushforward (fun s -> if s % 2 = 0 then "even" else "odd") twoD6
// parity |> List.normalizeWeights

// Pullback of likelihood ℓ along f: reweight μ(x) by ℓ(f(x))
let inline pullbackLikelihood (f: 'X -> 'Y) (lik: 'Y -> 'W) (mu: GenericProbabilitySpace<'X,'W>)
    : GenericProbabilitySpace<'X,'W>
    when Semiring<'W> and 'X: equality =
    dist {
        let! x = mu
        return! distribution [ (x, lik (f x)) ]
    } |> explore None

// Example: infer x∈{0..10} given noisy y≈x^2
let priorX : GenericProbabilitySpace<int,float> = Distributions.uniform float [0..10]
let yObs, sigma = 25.0, 2.0
let gauss e = exp (-(e*e)/(2.0*sigma*sigma))
let posteriorX =
    pullbackLikelihood (fun x -> float (x*x)) (fun y -> gauss (y - yObs)) priorX
    |> List.normalizeWeights


//================================== 
 
module LuckDSL =
    open System 
    let inline flatten (dist: GenericProbabilitySpace<'T, float>) =
        dist
        |> explore None
        |> List.choose (fun (tree, weight) ->
            match tree with
            | Value v -> Some(v, weight)
            | _ -> None)

    let inline normalize weights =
        let total = weights |> List.sumBy snd
        if total <= 0.0 then invalidArg "weights" "Total probability must be positive."
        weights |> List.map (fun (v, p) -> v, p / total)

    let inline log2 x = Math.Log(x, 2.0)

    let klDivergence baseDist targetDist =
        let baseMap = baseDist |> dict
        targetDist
        |> List.fold (fun acc (value, p) ->
            if p <= 0.0 then acc
            else
                match baseMap.TryGetValue value with
                | true, q when q > 0.0 -> acc + p * log2 (p / q)
                | _ -> Double.PositiveInfinity) 0.0

    type LuckOption<'T> =
        { Name: string
          Distribution: GenericProbabilitySpace<'T, float>
          KL: float
          Cost: float }

    let d6 : GenericProbabilitySpace<int, float> =
        dist { return! Distributions.uniform float [ 1..6 ] }

    let alwaysSix : GenericProbabilitySpace<int, float> =
        dist { return 6 }

    let bestOfTwo : GenericProbabilitySpace<int, float> =
        dist {
            let! first = d6
            let! second = d6
            return max first second
        }

    let bestOfThree =
        dist {
            let! a = d6
            let! b = d6
            let! c = d6
            return max a (max b c)
        }

    let plusOneCapped : GenericProbabilitySpace<int, float> =
        dist {
            let! r = d6
            return min (r + 1) 6
        }

    let uniform2to6 : GenericProbabilitySpace<int, float> =
        dist { return! Distributions.uniform float [ 2..6 ] }
    
    let d20 : GenericProbabilitySpace<int, float> =
        dist { return! Distributions.uniform float [ 1..20 ] }

    let bestOfTwoD20 : GenericProbabilitySpace<int, float> =
        dist {
            let! first = d20
            let! second = d20
            return max first second
        }

    let createOption die name scaling target =
        let baseNorm = die |> flatten |> normalize
        let targetNorm = target |> flatten |> normalize
        let kl = klDivergence baseNorm targetNorm
        { Name = name
          Distribution = target
          KL = kl
          Cost = scaling * kl }

    let demo () =
        let budget = 100.0

        [ createOption d6 "Always 6" budget alwaysSix
          createOption d20 "Always 20" budget (dist { return 20 })
          createOption d6 "Best of two" budget bestOfTwo 
          createOption d6 "Best of 3" budget bestOfThree
          createOption d6 "+1 capped at 6" budget plusOneCapped
          createOption d6 "Uniform [2..6]" budget uniform2to6
          createOption d20 "Best of two d20" budget bestOfTwoD20 ]
        |> List.iter (fun option ->
            let distView =
                option.Distribution
                |> explore None
                |> List.normalizeWeights
            printfn "\nLuck option: %s" option.Name
            printfn "KL divergence (bits): %.4f | Cost: %.2f" option.KL option.Cost
            printfn "Effective distribution: %A" distView)
 
LuckDSL.demo ()
 