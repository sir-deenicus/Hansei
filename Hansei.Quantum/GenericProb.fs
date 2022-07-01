namespace Hansei.Generic

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
    let distribution_of_lazy (one:'w) ch = 
        LazyList.map (fun (v, p) -> 
            Continued(lazy(LazyList.ofList [Value v, one])), p) ch 
        : GenericProbabilitySpace<_, _> 

    let distribution one ch = distribution_of_lazy one (LazyList.ofList ch) : GenericProbabilitySpace<_, 'w>  
  
    let always one x = distribution_of_lazy one (LazyList.singleton (x,one)) : GenericProbabilitySpace<_, _> 
    
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
        member d.Zero () = always one ()  
        member __.Combine(x,y) = LazyList.choice x y
        member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
        member l.Yield x = l.Return x  
     
    let dist one = GenericProbabilitySpaceBuilder(one)

    let observe one test = dist one { if not test then return! fail () }  : GenericProbabilitySpace<_,'w>

    let constrain one test = observe one test : GenericProbabilitySpace<_,'w>
    
    let softConstrainOn one r = dist one { if random.NextDouble() > r then return! fail () }  : GenericProbabilitySpace<_,'w>

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
    
         

    ////This sampler takes after beam search or even breadth first search
    ////when the width is high enough.
    //let inline best_first_sample_dist (maxtime : _ option) prevtabu 
    //    zero one tofloat maxwidth maxdepthval width lookaheadWidth 
    //    maxTemperature niters space =

    //    let t0 = System.DateTime.Now
    //    let paths = defaultArg prevtabu (Dict<int32 list, float>())
    //    let maxbeamdepth = int (log maxwidth / log (float width))
    //    let gainT = maxTemperature ** (1./200.)
    //    let gainLiteT = maxTemperature ** (1./300.)
    //    let attenT = maxTemperature ** (-1./10.)
    //    let useTemperature = maxTemperature > 1. 
    //    let mutable T = 1.
    //    let maxdepth = if maxdepthval % 2 = 0 then maxdepthval + 1 else maxdepthval  
    //    let discreteSampler ps =  
    //        Stats.discreteSample (Array.normalize [|for (p,w,_) in ps -> (tofloat p * w) ** (1./T)|])
    //    let empty = Dict()
    //    let fch curpath ch =
    //        ch
    //        |> List.mapi (fun i (p, t) ->
    //                let pass, w = testPath paths (i :: curpath)
    //                if pass then zero, 0., t
    //                else p, w, t)
    //    let rec loop (curpath : int32 list) maxd depth lookahead pcontrib ans =
    //        function
    //        | [] -> //FIX PRP{AGATE
    //            propagateUp 1. false paths 0.5 -0.1 curpath
    //            paths.ExpandElseAdd curpath (fun _ -> -1.) -1.
    //            if useTemperature then T <- min maxTemperature (T * gainT)
    //            empty  
    //        | ([ p, Value v ] : GenericProbabilitySpace<'W, 'T>) ->
    //            propagateUp  1. true paths 0.5 0.1 curpath
    //            paths.ExpandElseAdd curpath (fun _ -> -1.) -1.
    //            if useTemperature then T <- max 1. (T * attenT)
    //            Utils.insertWithx (+) v (p * pcontrib) ans
    //        | _ when depth > maxdepth
    //                    || (maxtime.IsSome
    //                        && (DateTime.Now - t0).TotalSeconds > maxtime.Value) -> 
    //            if lookahead then
    //                propagateUp 1. false paths 0.5 -0.01 curpath
    //            if useTemperature then T <- min maxTemperature (T * gainLiteT)
    //            empty
    //        | [ p, Continued(Lazy th) ] ->
    //            if lookahead then
    //                propagateUp 1. true paths 0.5 -0.01 curpath
    //                empty
    //            else loop curpath maxd (depth + 1) false (p * pcontrib) ans th
    //        | ch when not lookahead ->              
    //            let bwidth =
    //                if depth > maxbeamdepth then 1
    //                else width 
              
    //            let widths, lookahead, steps = 
    //                if lookaheadWidth = 0 then [|bwidth|], [|false|], 0
    //                else
    //                    [|lookaheadWidth;bwidth|], [|true; false|], 1

    //            let selected =
    //                [ for i in 0..steps do 
    //                    let choices =
    //                        sampleN_No_ReplacementsX discreteSampler widths.[i] (fch curpath ch)
    //                    yield! [ for (b, (p, _, t)) in choices do
    //                                if p <> zero then
    //                                    yield loop (b :: curpath) maxd (depth + 1)
    //                                            lookahead.[i] (pcontrib) ans [ p, t ] ] ]

    //            for samples in selected do
    //                ans.MergeWith (fun _ t -> t) (Seq.toArray samples) 
    //            ans
    //        | _ -> empty

    //    let rec sampler (ch : GenericProbabilitySpace<'W, 'T>) ans =
    //        function
    //        | 0 ->
    //            let t1 = System.DateTime.Now
    //            printfn "done %d worlds\nTime taken: %A seconds" niters
    //                (round 3 ((t1 - t0).TotalSeconds))
    //            { Values = 
    //                [ for (KeyValue(v, p:'W)) in ans -> p, Value v ]; 
    //              Continuation = ch; 
    //              Paths = paths}
    //        | n ->
    //            let ans = loop [] maxdepth 0 false one ans ch
    //            sampler ch ans (n - 1)  

    //    sampler (reify0 one space) (Dict()) niters 
    
    (* ------------------------------------------------------------------------ *)
    (*	Approximate inference strategies:				    *)
    (*  Trace a few paths from the root to a leaf of the search tree            *)
    (* The following procedures are non-deterministic; they use a given selector*)
    (* procedure, of the type 'selector', to chose among the alternatives.      *)
    (* For top-level inference, the selector uses system random generator.      *)

    (* Naive, rejection sampling: the baseline *)        
    (* Random selection from a list of choices, using system randomness *)
    //let inline random_selector tofloat zero =  
    //    let rec selection r ptotal pcum = function
    //        | [] -> failwith "Choice selection: can't happen"
    //        | ((p,th)::rest) -> 
    //        let pcum = pcum + p  
    //        if r < tofloat pcum then (ptotal,th)
    //        else selection r ptotal pcum rest  
    //    fun choices ->
    //        let ptotal = List.sumBy fst choices  
    //        let r = random.NextDouble (0., tofloat ptotal)      (* 0<=r<ptotal *)
    //        selection r ptotal zero choices 
    
    //let inline rejection_sample_dist one fromInt selector nsamples ch : GenericProbabilitySpace<_, _> =
    //    let t0 = System.DateTime.Now
    //    let rec loop pcontrib ans = function
    //        | [(p,Value v)]  -> insertWithx (+) v (p * pcontrib) ans
    //        | []         -> ans
    //        | [(p,Continued (Lazy th))] -> loop (p * pcontrib) ans (th)
    //        | ch ->
    //            let (ptotal,th) = selector ch
    //            loop (pcontrib * ptotal) ans [(one,th)]
    //    let rec driver ch ans = function
    //        | 0 -> let ns = fromInt nsamples
    //               let t1 = System.DateTime.Now
    //               printfn "rejection_sample: done %d worlds\nTime taken: %A seconds"
    //                        nsamples (round 3 ((t1 - t0).TotalSeconds))
    //               [for KeyValue(v,p) in ans -> p / ns, Value v] 
    //        | n -> driver ch (loop one ans ch) (n-1)
    //    driver (reify0 one ch) (Dict()) nsamples 

         
    //type ModelFrom<'w, 'a, 'b>
    //    (reify0, explore : int option -> GenericProbabilitySpace<'w,'a> -> GenericProbabilitySpace<_,_>, 
    //        thunk : ('a -> GenericProbabilitySpace<'w, 'a>) -> GenericProbabilitySpace<'w,'b>, 
    //        ?rejection_sampler) =
    //    let exact_reify model = explore None (reify0 model)
    //    let limit_reify n model = explore (Some n) (reify0 model)
    //    member __.model = thunk 
    //    member __.RejectionSampler(nsamples:int) =
    //        match rejection_sampler with
    //        | None -> failwith "No sampler"
    //        | Some sampler ->
    //            sampler nsamples thunk: GenericProbabilitySpace<'w,'b> 

    //    member __.Reify(?limit:int) =
    //        match limit with
    //        | None -> exact_reify thunk : GenericProbabilitySpace<'w,'b>
    //        | Some n -> limit_reify n thunk  
        
    module ProbabilitySpace =
        let expectedValue f ps =
            ps
            |> List.map (function
                | (Value x, p) -> f x * p
                | _ -> 0.)
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

        
                
     
