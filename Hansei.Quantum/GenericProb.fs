namespace Hansei.Generic

module GenericProbTest = 
    open System
    open Hansei.Utils
    open Hansei.Continuation    
    open MathNet.Symbolics
    open Prelude.Common
    open MathNet.Numerics 
    open Prelude.Math
    //open Hansei.FSharpx.Collections
    open System.Numerics
    
    /// <summary>
    /// Defines the minimal algebraic structure needed for weights in the core probabilistic framework.
    /// </summary>
    type Semiring<'W when
        'W : (static member Zero : 'W) and
        'W : (static member One : 'W) and
        'W : (static member ( + ) : 'W * 'W -> 'W) and 
        'W : (static member ( * ) : 'W * 'W -> 'W)> = 'W

    /// <summary>
    /// Defines a structure with division, needed for normalization and related functions.
    /// All Fields are DivisionRings.
    /// </summary>
    type DivisionRing<'W when
        'W : (static member One : 'W) and
        'W : (static member ( + ) : 'W * 'W -> 'W) and 
        'W : (static member ( * ) : 'W * 'W -> 'W) and
        'W : (static member ( / ) : 'W * 'W -> 'W)> = 'W

    // type Semifield<'W when  
    //     'W : (static member One : 'W) and
    //     'W : (static member ( + ) : 'W * 'W -> 'W) and 
    //     'W : (static member ( * ) : 'W * 'W -> 'W) and
    //     'W : (static member ( / ) : 'W * 'W -> 'W)> = 'W 

    type GenericProbabilitySpace<'T, 'W when Semiring<'W>> = list<GenericWeightedTree<'T, 'W> * 'W>
    and GenericWeightedTree<'T, 'W when Semiring<'W>> =
        | Value           of 'T
        | ContinuedSubTree of Memo<GenericProbabilitySpace<'T, 'W>>
 
    // type GenericProbabilitySpace<'T, 'W when Semifield<'W>> = LazyList<GenericWeightedTree<'T, 'W> * 'W>
    // and GenericWeightedTree<'T, 'W when Semifield<'W>>  = 
    //     | Value of 'T 
    //     | Continued of Lazy<GenericProbabilitySpace<'T, 'W>>      
    
//     let inline distributionOfLazy<'T, 'W when Semiring<'W>>(weightedlist: LazyList<'T * 'W>) = 
//         weightedlist
//         |> Lazy
            
// //            Continued(lazy(LazyList.ofList [Value v, 'W.One ])), p) weightedlist 
//         : GenericProbabilitySpace<_, _> 

    let inline distribution<'T, 'W when Semiring<'W>> (weightedlist: list<'T * 'W>) =
        weightedlist
        |> List.map (fun (v, p) ->
            let subTree = ContinuedSubTree(memo (fun () -> [Value v, 'W.One]))
            (subTree, p)) : GenericProbabilitySpace<_, 'W>
 
    let inline exactly<'T, 'W when Semiring<'W>>(x: 'T) = distribution [x, 'W.One] : GenericProbabilitySpace<_, _>    

    let inline always<'T, 'W when Semiring<'W>>(x: 'T) = distribution [x, 'W.One] : GenericProbabilitySpace<_, _>
    
    let inline fail () = [] : GenericProbabilitySpace<_, _>   

    // let inline reflect tree k =  
    //     let rec make_choices pv = 
    //         LazyList.map (function 
    //         | (Value x, p) -> Continued(lazy(k x)), p
    //         | (Continued(Lazy t), p) -> Continued(lazy(make_choices t)), p) pv 
    //     make_choices tree : GenericProbabilitySpace<_, _>  

    // let reflect2 tree k =
    //     let rec makeChoices ps =
    //         List.map (function
    //             | (ValueB x          , p) -> ContinuedSubTreeB (memo (fun () -> k x))   , p
    //             | (ContinuedSubTreeB m, p) -> ContinuedSubTreeB (memo (fun () -> makeChoices (force m))) , p)
    //         ps
    //     makeChoices tree

    [<TailCall>]
    let inline reflect tree k = 
        let rec makeChoices ps =
            List.map (function 
            | (Value x, p) -> ContinuedSubTree(memo (fun () -> k x)), p
            | (ContinuedSubTree m, p) -> ContinuedSubTree(memo (fun () -> makeChoices (force m))), p) ps
        makeChoices tree : GenericProbabilitySpace<_, _> 
        
    type GenericProbabilitySpaceBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member inline d.Return v = always v
        member inline d.ReturnFrom vs = vs : GenericProbabilitySpace<_, _>  
        member d.Zero () = []
        member inline __.For(sequence: seq<'a>, body: 'a -> GenericProbabilitySpace<'b, 'W>) : GenericProbabilitySpace<'b, 'W> =
            Seq.fold (fun acc elem ->
                let comp = body elem 
                List.append acc comp
            ) (__.Zero()) sequence
        member __.Combine(x,y) = List.append x y
        member inline __.Delay(f: unit -> GenericProbabilitySpace<'a, 'W>) = [ContinuedSubTree (memo f), 'W.One] 
        member inline l.Yield x = l.Return x  

    let dist = GenericProbabilitySpaceBuilder()

    let inline observe test = dist { if not test then return! fail () else return () }  : GenericProbabilitySpace<_,'w>

    let inline constrain test = observe test : GenericProbabilitySpace<_,_>

    let inline filterDistribution f p : GenericProbabilitySpace<_,'w> = dist {
        let! x = p
        do! observe (f x)
        return x
    }    

    let inline explore<'T, 'W when Semiring<'W> and 'T : equality> (maxdepth: int option) (choices: GenericProbabilitySpace<'T,'W>) : GenericProbabilitySpace<'T,'W> =
        let rec loop pcontrib depth (worklist: GenericProbabilitySpace<'T,'W>) (answers_dict: Dict<'T, 'W>) (suspended_list: GenericProbabilitySpace<'T,'W>) =
            match worklist with
            | [] ->
                // This level of the worklist is done
                (answers_dict, suspended_list)

            | (item, p) :: rest ->
                let current_path_p = pcontrib * p
                match item with
                | Value v ->
                    // Found a final value, add it to the dictionary
                    let new_answers_dict = insertWithx (+) v current_path_p answers_dict
                    // Continue with the rest of this level
                    loop pcontrib depth rest new_answers_dict suspended_list

                | ContinuedSubTree m ->
                    let should_go_down =
                        match maxdepth with
                        | Some maxd -> depth < maxd
                        | None -> true

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
        let (final_answers, suspended_computations) = loop 'W.One 0 choices (Dict<_,_>()) []

        // Combine the results: the values found and the computations that were suspended
        let final_values =
            [ for (KeyValue(v, p)) in final_answers -> (Value v, p) ]

        List.append final_values suspended_computations

    //let inline explore (maxdepth: int option) (choices: GenericProbabilitySpace<_,'w>) =
    //    let rec loop p depth down susp answers =
    //        match (down, susp, answers) with
    //        | _, LazyList.Nil, answers -> answers
    //        | _, LazyList.Cons((Value v, pt), rest), (ans, susp) -> 
    //            loop p depth down rest (insertWithx (+) v (pt * p) ans, susp) 
    //        | true, LazyList.Cons((Continued (Lazy t), pt), rest), answers ->
    //            let down' =
    //                Option.map (fun x -> depth < x) maxdepth 
    //                |> Option.defaultValue true   
         
    //            loop (pt * p) (depth + 1) down' t answers
    //            |> loop p depth true rest 

    //        | (down, LazyList.Cons((c, pt), rest), (ans, susp)) -> 
    //            loop p depth down rest (ans, (c, pt * p) :: susp)
         
    //    let (ans, susp) = loop 'w.One 0 true choices (Dict(), [])
      
    //    [ yield! susp
    //      for (KeyValue (v, p)) in ans -> Value v, p ] 

    //let inline first_success maxdepth ch = 
    //    let rec loop maxdepth = function
    //        | LazyList.Nil -> None
    //        | _ when maxdepth = 0 -> None
    //        | LazyList.Cons((Value _, _), _) as l -> 
    //            let choices =
    //                [|for (v, p) in l do 
    //                    match v with
    //                    | Value x -> yield (x, p)
    //                    | _ -> () |] 
    //            if choices.Length = 0 then None else Some(Array.sampleOne choices)
    //        | LazyList.Cons((Continued (Lazy t), pt), rest) -> (* Unclear: expand and do BFS *)
    //            loop (maxdepth - 1) (LazyList.choice rest (LazyList.map (fun (v, p) -> (v, pt * p)) t))
    //    loop maxdepth ch

    let inline first_success<'T, 'W when Semiring<'W>> (toFloat: 'W -> float) (maxdepth: int) (choices: GenericProbabilitySpace<'T,'W>) : 'T option =
        let rec level_loop current_level depth =
            if depth >= maxdepth || List.isEmpty current_level then
                None // Search failed or max depth reached
            else
                // Separate values from continuations at the current level
                let values = 
                    current_level |> List.choose (fun (item, p) ->
                        match item with
                        | Value v -> Some (v, p)
                        | _ -> None)

                if not (List.isEmpty values) then
                    // Success! We found the first level with values.
                    // Sample one according to weights.
                    let choices_for_sampling =
                        values
                        |> List.map (fun (v, p) -> (v, toFloat p))
                        |> Array.ofList
                    
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
                                | _ -> []
                            )
                        level_loop next_level (depth + 1)
                    else
                        Some (Prelude.Sampling.discreteSample choices_for_sampling) 
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
    

    module ProbabilitySpace =
        let inline expectedValue r f ps = 
            ps
            |> List.mapi (fun u a ->
                match a with
                | (Value x, p) -> f x * p
                | (_, p) -> r u * p)
            |> List.sum

        let inline map f l =
            [ for (v, p) in l do
                match v with
                | Value x -> yield (Value(f x), p)
                | _ -> yield (v, p) ]

        let inline mapValues f l =
            [ for (v, p) in l do
                match v with
                | Value x -> yield (f x, p)
                | _ -> () ]

        let inline mapValuesProb pf f l =
            [ for (v, p) in l do
                match v with
                | Value x -> yield (f x, pf p)
                | _ -> () ]

    let inline normalizeGeneric sumWith (choices: list<'a * 'w>) =
        let sum = sumWith snd choices
        List.map (fun (v, p) -> (v, p / sum)) choices

    let inline normalize distr = normalizeGeneric List.sumBy distr 

    module Distributions =
        let inline bernoulli (p:'W) =
            distribution [ (true, p); (false, 'W.One - p) ]

        let inline bernoulliChoice (p:'W) (a, b) =
            distribution [ (a, p); (b, 'W.One - p) ]

        let inline uniform<'a,'w when 'w :> INumberBase<'w>> f (items: 'a list) =
            let len = f items.Length
            distribution (List.map (fun item -> item, 'w.One / len) items) 

        let inline categorical distr =
            distribution (List.normalizeWeights distr) 

        let inline geometric bernoulli n p : GenericProbabilitySpace<_, 'w> =
            let rec loop n =
                dist {
                    let! a = bernoulli p

                    if a then return n
                    else return! (loop (n + 1))
                }
            loop n

        let inline discretizedSampler toNumberType coarsener sampler (n: int) : GenericProbabilitySpace<_, 'w> =
            dist {
                return!
                    [ for _ in 1..n -> sampler () ] 
                    |> coarsenWithGeneric toNumberType coarsener
                    |> categorical  
            }

        let inline beta draws a b =
            let one = a/a
            let rec loop draws a b = dist {
                if draws <= 0 then
                    return a / (a + b)
                else
                    let! ball = categorical [ 1, a / (a + b); 2, b / (a + b) ]

                    if ball = 1 then
                        return! loop (draws - 1) (a + one) b
                    else
                        return! loop (draws - 1) a (b + one)
                }

            loop draws a b: GenericProbabilitySpace<_, 'w>

        let inline dirichlet one draws d =
            let rec loop draws d = dist {
                let t = List.sum d

                if draws <= 0 then
                    return (List.map (fun a -> (a / t)) d)
                else
                    let ps = List.mapi (fun i a -> i, (a / t)) d
                    let! ball = categorical ps
                    let d' = List.mapi (fun i a -> if i = ball then a + one else a) d
                    return! loop (draws - 1) d'
            }

            loop draws d: GenericProbabilitySpace<_, 'w>
           
  
module GenericProb =
    open System
    open Hansei.Utils
    open Hansei.Continuation    
    open MathNet.Symbolics
    open Prelude.Common
    open MathNet.Numerics 
    open Prelude.Math
    open Hansei.FSharpx.Collections
     
    type GenericProbabilitySpace<'T, 'w> = LazyList<GenericWeightedTree<'T, 'w> * 'w>
    and GenericWeightedTree<'T, 'w> = 
        | Value of 'T 
        | Continued of Lazy<GenericProbabilitySpace<'T, 'w>>      
     
    //if use value instead of Continued, infinite computations will fail to return/terminate
    let distributionOfLazy (one:'w) weightedlist = 
        LazyList.map (fun (v, p) -> 
            Continued(lazy(LazyList.ofList [Value v, one])), p) weightedlist 
        : GenericProbabilitySpace<_, _> 

    let distribution one weightedlist = distributionOfLazy one (LazyList.ofList weightedlist) : GenericProbabilitySpace<_, 'w>  
  
    let always one x = distributionOfLazy one (LazyList.singleton (x,one)) : GenericProbabilitySpace<_, _> 
    
    let exactly one x = distribution one [x, one] 
    
    let fail () = LazyList.empty : GenericProbabilitySpace<_, _>  
  
    let reflect tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
            | (Value x, p) -> Continued(lazy(k x)), p
            | (Continued(Lazy t), p) -> Continued(lazy(make_choices t)), p) pv 
        make_choices tree : GenericProbabilitySpace<_, _>  
      
    type GenericProbabilitySpaceBuilder<'w>(one:'w) =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = always one v
        member d.ReturnFrom vs = vs : GenericProbabilitySpace<_, _>  
        member d.Zero () = LazyList.empty  
        member __.Combine(x,y) = LazyList.choice x y
        member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
        member l.Yield x = l.Return x  
     
    let dist one = GenericProbabilitySpaceBuilder(one)

    let observe one test = dist one { if not test then return! fail () else return () }  : GenericProbabilitySpace<_,'w>

    let constrain one test = observe one test : GenericProbabilitySpace<_,'w>
    
    let filterDistribution one f p : GenericProbabilitySpace<_,'w> = dist one {
        let! x = p
        do! observe one (f x)
        return x
    }    
    
    let inline explore one (maxdepth: int option) (choices: GenericProbabilitySpace<_,'w>) =
        let rec loop p depth down susp answers =
            match (down, susp, answers) with
            | _, LazyList.Nil, answers -> answers
            | _, LazyList.Cons((Value v, pt), rest), (ans, susp) -> 
                loop p depth down rest (insertWithx (+) v (pt * p) ans, susp) 
            | true, LazyList.Cons((Continued (Lazy t), pt), rest), answers ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true   
             
                loop (pt * p) (depth + 1) down' t answers
                |> loop p depth true rest 

            | (down, LazyList.Cons((c, pt), rest), (ans, susp)) -> 
                loop p depth down rest (ans, (c, pt * p) :: susp)
             
        let (ans, susp) = loop one 0 true choices (Dict(), [])
          
        [ yield! susp
          for (KeyValue (v, p)) in ans -> Value v, p ] 
               
     
    let inline first_success maxdepth ch = 
        let rec loop maxdepth = function
            | LazyList.Nil -> None
            | _ when maxdepth = 0 -> None
            | LazyList.Cons((Value _, _), _) as l -> 
                let choices =
                    [|for (v, p) in l do 
                        match v with
                        | Value x -> yield (x, p)
                        | _ -> () |] 
                if choices.Length = 0 then None else Some(Array.sampleOne choices)
            | LazyList.Cons((Continued (Lazy t), pt), rest) -> (* Unclear: expand and do BFS *)
                loop (maxdepth - 1) (LazyList.choice rest (LazyList.map (fun (v, p) -> (v, pt * p)) t))
        loop maxdepth ch
    
         
    module ProbabilitySpace =
        let inline expectedValue r f ps = 
            ps
            |> List.mapi (fun u a ->
                match a with
                | (Value x, p) -> f x * p
                | (_, p) -> r u * p)
            |> List.sum

        let map f l =
            [ for (v, p) in l do
                match v with
                | Value x -> yield (Value(f x), p)
                | _ -> yield (v, p) ]

        let mapValues f l =
            [ for (v, p) in l do
                match v with
                | Value x -> yield (f x, p)
                | _ -> () ]

        let mapValuesProb pf f l =
            [ for (v, p) in l do
                match v with
                | Value x -> yield (f x, pf p)
                | _ -> () ]

    let inline normalizeGeneric sumWith (choices: list<'a * 'w>) =
        let sum = sumWith snd choices
        List.map (fun (v, p) -> (v, p / sum)) choices

    let inline normalize distr = normalizeGeneric List.sumBy distr 

    module Distributions =
        let inline bernoulli one p =
            distribution one [ (true, p); (false, one - p) ]

        let inline bernoulliChoice one p (a, b) =
            distribution one [ (a, p); (b, one - p) ]

        let inline uniform one f (items: 'a list) =
            let len = f items.Length
            distribution one (List.map (fun item -> item, one / len) items)

        let inline categorical one distr =
            distribution one (List.normalizeWeights distr)

        let rec geometric one bernoulli n p : GenericProbabilitySpace<_, 'w> =
            dist one {
                let! a = bernoulli p

                if a then return n
                else return! (geometric one bernoulli (n + 1) p)
            }

        let inline discretizedSampler one toNumberType coarsener sampler (n: int) : GenericProbabilitySpace<_, 'w> =
            dist one {
                return!
                    [ for _ in 1..n -> sampler () ] 
                    |> coarsenWithGeneric toNumberType coarsener
                    |> categorical one 
            }

        let inline beta one draws a b =
            let rec loop draws a b = dist one {
                if draws <= 0 then
                    return a / (a + b)
                else
                    let! ball = categorical one [ 1, a / (a + b); 2, b / (a + b) ]

                    if ball = 1 then
                        return! loop (draws - 1) (a + one) b
                    else
                        return! loop (draws - 1) a (b + one)
                }

            loop draws a b: GenericProbabilitySpace<_, 'w>

        let inline dirichlet one draws d =
            let rec loop draws d = dist one {
                let t = List.sum d

                if draws <= 0 then
                    return (List.map (fun a -> (a / t)) d)
                else
                    let ps = List.mapi (fun i a -> i, (a / t)) d
                    let! ball = categorical one ps
                    let d' = List.mapi (fun i a -> if i = ball then a + one else a) d
                    return! loop (draws - 1) d'
            }

            loop draws d: GenericProbabilitySpace<_, 'w>

        
                
     
