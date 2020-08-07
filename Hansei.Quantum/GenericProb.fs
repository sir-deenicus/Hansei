namespace Hansei

module GenericProb =
    open System
    open Hansei.Utils
    open Hansei.Continuation    
    open MathNet.Symbolics
    open Prelude.Common
    open MathNet.Numerics 
    open Prelude.Math
     
    type GenericProbabilitySpace<'a, 'T> = list<'a * GenericWeightedTree<'a, 'T>>
    and GenericWeightedTree<'a, 'T> = 
        | Value of 'T 
        | Continued of Lazy<GenericProbabilitySpace<'a, 'T>>    

    
    type ProbSearchResult<'W,'T> =
        {Values : GenericProbabilitySpace<'W,'T>
         Continuation : GenericProbabilitySpace<'W,'T>
         Paths : Dict<int32 list, float>}

    let valueExtract = function Value x -> x

    let valueExtract2 = function Value x -> Some x | _ -> None

    let reflect tree k =  
        let rec make_choices pv = 
            List.map (function 
              | (p, Value x) -> (p, Continued(lazy(k x)))
              | (p, Continued(Lazy x)) -> (p, Continued(lazy(make_choices x)))) pv
        
        make_choices tree 

    (* Variable elimination optimization: transform a stochastic function
       a -> b to a generally faster function
    *)
    let variable_elim reify f arg = reflect (reify (f arg))  

    let distribution ch k = List.map (fun (p:'a,v) -> (p, Continued(lazy(k v)))) ch

    let fail () = distribution [] 
    
    let observe test = cont { if not test then return! fail() }

    let constrain test = observe test

    let softConstrainOn r = cont { if random.NextDouble() > r then return! fail() }

    let filterDistribution f p = cont {
        let! x = p
        do! observe (f x)
        return x
    }   

    let inline reify0 one m = m (fun x -> [(one , Value x)])

    let exactly one x = distribution [one , x] 

    let inline explore probMap one (maxdepth : int option)
           (choices : GenericProbabilitySpace<'a, 'T>) =
        let rec loop p depth down susp answers =
            match (down, susp, answers) with
            | (_, [], answers) -> answers
            | (_, (pt, Value v) :: rest, (ans, susp)) ->
                loop p depth down rest (insertWithx (+) v (pt * p) ans, susp)
            | (true, (pt, Continued(Lazy t)) :: rest, answers) ->
                let down' =
                    match maxdepth with
                    | Some x -> depth < x
                    | None -> true
                loop p depth true rest (loop (pt * p) (depth + 1) down' t answers)
            | (down, (pt, c) :: rest, (ans, susp)) ->
                loop p depth down rest (ans, (pt * p, c) :: susp)

        let (ans, susp) = loop one 0 true choices (Dict(), [])
        [ yield! susp
          for (KeyValue(v, p)) in ans -> probMap p, Value v ] : GenericProbabilitySpace<_, _> 
    
    let inline exact_reify probMap one model = explore probMap one None (reify0 one model)

    let inline limit_reify probMap one n model = explore probMap one (Some n) (reify0 one model)
     
    let inline first_success maxdepth ch = 
        let rec loop maxdepth = function
            | [] -> None
            | _ when maxdepth = 0 -> None
            | ((_,Value _) :: _) as l  -> 
                let choices =
                    [|for (p, v) in l do 
                        match v with
                        | Value x -> yield (p,x)
                        | _ -> () |] 
                if choices.Length = 0 then None else Some(Array.sampleOne choices)
            | (pt,Continued (Lazy t)) :: rest -> (* Unclear: expand and do BFS *)
                loop (maxdepth - 1) (rest @ List.map (fun (p,v) -> (pt * p,v)) t) 
        loop maxdepth ch

    //This sampler takes after beam search or even breadth first search
    //when the width is high enough.
    let inline best_first_sample_dist (maxtime : _ option) prevtabu 
        zero one tofloat maxwidth maxdepthval width lookaheadWidth 
        maxTemperature niters space =

        let t0 = System.DateTime.Now
        let paths = defaultArg prevtabu (Dict<int32 list, float>())
        let maxbeamdepth = int (log maxwidth / log (float width))
        let gainT = maxTemperature ** (1./200.)
        let gainLiteT = maxTemperature ** (1./300.)
        let attenT = maxTemperature ** (-1./10.)
        let useTemperature = maxTemperature > 1. 
        let mutable T = 1.
        let maxdepth = if maxdepthval % 2 = 0 then maxdepthval + 1 else maxdepthval  
        let discreteSampler ps =  
            Stats.discreteSample (Array.normalize [|for (p,w,_) in ps -> (tofloat p * w) ** (1./T)|])
        let empty = Dict()
        let fch curpath ch =
            ch
            |> List.mapi (fun i (p, t) ->
                    let pass, w = testPath paths (i :: curpath)
                    if pass then zero, 0., t
                    else p, w, t)
        let rec loop (curpath : int32 list) maxd depth lookahead pcontrib ans =
            function
            | [] -> //FIX PRP{AGATE
                propagateUp 1. false paths 0.5 -0.1 curpath
                paths.ExpandElseAdd curpath (fun _ -> -1.) -1.
                if useTemperature then T <- min maxTemperature (T * gainT)
                empty  
            | ([ p, Value v ] : GenericProbabilitySpace<'W, 'T>) ->
                propagateUp  1. true paths 0.5 0.1 curpath
                paths.ExpandElseAdd curpath (fun _ -> -1.) -1.
                if useTemperature then T <- max 1. (T * attenT)
                Utils.insertWithx (+) v (p * pcontrib) ans
            | _ when depth > maxdepth
                        || (maxtime.IsSome
                            && (DateTime.Now - t0).TotalSeconds > maxtime.Value) -> 
                if lookahead then
                    propagateUp 1. false paths 0.5 -0.01 curpath
                if useTemperature then T <- min maxTemperature (T * gainLiteT)
                empty
            | [ p, Continued(Lazy th) ] ->
                if lookahead then
                    propagateUp 1. true paths 0.5 -0.01 curpath
                    empty
                else loop curpath maxd (depth + 1) false (p * pcontrib) ans th
            | ch when not lookahead ->              
                let bwidth =
                    if depth > maxbeamdepth then 1
                    else width 
              
                let widths, lookahead, steps = 
                    if lookaheadWidth = 0 then [|bwidth|], [|false|], 0
                    else
                        [|lookaheadWidth;bwidth|], [|true; false|], 1

                let selected =
                    [ for i in 0..steps do 
                        let choices =
                            sampleN_No_Replacements discreteSampler widths.[i] (fch curpath ch)
                        yield! [ for (b, (p, _, t)) in choices do
                                    if p <> zero then
                                        yield loop (b :: curpath) maxd (depth + 1)
                                                lookahead.[i] (pcontrib) ans [ p, t ] ] ]

                for samples in selected do
                    ans.MergeWith (fun _ t -> t) (Seq.toArray samples) 
                ans
            | _ -> empty

        let rec sampler (ch : GenericProbabilitySpace<'W, 'T>) ans =
            function
            | 0 ->
                let t1 = System.DateTime.Now
                printfn "done %d worlds\nTime taken: %A seconds" niters
                    (round 3 ((t1 - t0).TotalSeconds))
                { Values = 
                    [ for (KeyValue(v, p:'W)) in ans -> p, Value v ]; 
                  Continuation = ch; 
                  Paths = paths}
            | n ->
                let ans = loop [] maxdepth 0 false one ans ch
                sampler ch ans (n - 1)  

        sampler (reify0 one space) (Dict()) niters 
    
    (* ------------------------------------------------------------------------ *)
    (*	Approximate inference strategies:				    *)
    (*  Trace a few paths from the root to a leaf of the search tree            *)
    (* The following procedures are non-deterministic; they use a given selector*)
    (* procedure, of the type 'selector', to chose among the alternatives.      *)
    (* For top-level inference, the selector uses system random generator.      *)

    (* Naive, rejection sampling: the baseline *)        
    (* Random selection from a list of choices, using system randomness *)
    let inline random_selector tofloat zero =  
        let rec selection r ptotal pcum = function
            | [] -> failwith "Choice selection: can't happen"
            | ((p,th)::rest) -> 
            let pcum = pcum + p  
            if r < tofloat pcum then (ptotal,th)
            else selection r ptotal pcum rest  
        fun choices ->
            let ptotal = List.sumBy fst choices  
            let r = random.NextDouble (0., tofloat ptotal)      (* 0<=r<ptotal *)
            selection r ptotal zero choices 
    
    let inline rejection_sample_dist one fromInt selector nsamples ch : GenericProbabilitySpace<_, _> =
        let t0 = System.DateTime.Now
        let rec loop pcontrib ans = function
            | [(p,Value v)]  -> insertWithx (+) v (p * pcontrib) ans
            | []         -> ans
            | [(p,Continued (Lazy th))] -> loop (p * pcontrib) ans (th)
            | ch ->
                let (ptotal,th) = selector ch
                loop (pcontrib * ptotal) ans [(one,th)]
        let rec driver ch ans = function
            | 0 -> let ns = fromInt nsamples
                   let t1 = System.DateTime.Now
                   printfn "rejection_sample: done %d worlds\nTime taken: %A seconds"
                            nsamples (round 3 ((t1 - t0).TotalSeconds))
                   [for KeyValue(v,p) in ans -> p / ns, Value v] 
            | n -> driver ch (loop one ans ch) (n-1)
        driver (reify0 one ch) (Dict()) nsamples 

         
    type ModelFrom<'w, 'a, 'b>
        (reify0, explore : int option -> GenericProbabilitySpace<'w,'a> -> GenericProbabilitySpace<_,_>, 
            thunk : ('a -> GenericProbabilitySpace<'w, 'a>) -> GenericProbabilitySpace<'w,'b>, 
            ?rejection_sampler) =
        let exact_reify model = explore None (reify0 model)
        let limit_reify n model = explore (Some n) (reify0 model)
        member __.model = thunk 
        member __.RejectionSampler(nsamples:int) =
            match rejection_sampler with
            | None -> failwith "No sampler"
            | Some sampler ->
                sampler nsamples thunk: GenericProbabilitySpace<'w,'b> 

        member __.Reify(?limit:int) =
            match limit with
            | None -> exact_reify thunk : GenericProbabilitySpace<'w,'b>
            | Some n -> limit_reify n thunk  
        
    module ProbabilitySpace =
        let map f l = 
            [for (p,v) in l do
                match v with 
                | Value x -> yield (p, Value(f x))
                | _ -> yield (p,v)]

        let mapValues f l = 
            [for (p,v) in l do
                match v with 
                | Value x -> yield (p, (f x))
                | _ -> ()] 
            
        let mapItemsProb fp f l = List.map (fun (p,x) -> fp p, f x) l

        let mapValuesProb fp f l = 
            [for (p,v) in l do
                match v with 
                | Value x -> yield (fp p, (f x))
                | _ -> ()]

    let inline normalize sumwith (choices:list<'w * 'a>) =
        let sum = sumwith choices
        List.map (fun (p, v) -> (p/sum, v)) choices

    module Distributions =    
        let inline bernoulli one p = distribution [(p, true); (one - p, false)]

        let inline bernoulliChoice one p (a,b) = distribution [(p, a); (one - p, b)]
                                                          
        let inline uniform one f (items:'a list) = 
            let len = f items.Length
            distribution (List.map (fun item -> one/len, item) items)

        let categorical distr = distribution distr 

        let rec geometric bernoulli n p = cont {
            let! a = bernoulli p
            if a then return n else return! (geometric bernoulli (n+1) p)
        } 

        let inline discretizedSampler numbertype coarsener sampler (n: int) =
            cont {
                return! categorical ([| for _ in 1 .. n -> sampler() |]   |> coarsenWithGeneric numbertype coarsener)
            }

        let inline beta one draws a b = 
            let rec loop draws a b = cont {
                if draws <= 0 then return a/(a+b)
                else let! ball = categorical [a/(a+b),1;b/(a+b),2]
                     if ball = 1 then return! loop (draws - 1) (a+one) b
                     else return! loop (draws-1) a (b+one) } 
            loop draws a b

        let inline dirichlet one draws d = 
            let rec loop draws d = cont {
                let z = List.sum d
                if draws <= 0 then 
                    return (List.map (fun a -> (a/z)) d)
                else          
                    let ps = List.mapi (fun i a -> (a/z), i) d
                    let! ball = categorical ps
                    let d' = List.mapi (fun i a -> if i = ball then a + one else a) d
                    return! loop (draws - 1) d' }
            loop draws d

module Visualization = 
    open MathNet.Symbolics
    open MathNet.Symbolics.Core

    //let createGraphSymb joint = createGraph2 joint Expression.toFormattedString 1Q (fun s -> Expression.fromFloat (float s))
    //let createGraphSymbQ joint = createGraph2 joint (fun c ->  c.ToString()) (Complex 1Q) (fun s -> Complex (Expression.fromFloat (float s)))
 
    module Symbolic =
        let bernoulliChoice p (a,b) = [a, p; b, 1Q - p]

        let bernoulli p = ["true", p; "false", 1Q - p]

        let uniformGen l = 
            let len = List.length l
            List.map (fun x -> x, 1Q/len) l

        let uniformp l = 
            let len = List.length l
            List.map (fun (x,_) -> x, 1Q/len) l

    module Interval = 
        open MathNet.Symbolics.Utils

        let bernoulliChoice (l,r) (a,b) = 
            let p = interval l r
            [a, p ; b, 1. - p]

        let bernoulli (l,r) =
            let p = interval l r
            ["true", p; "false", 1. - p]

        let uniformGen l = 
            let len = List.length l |> float |> IntSharp.Types.Interval.FromDoublePrecise

            List.map (fun x -> x, 1./len) l

        let uniformp l = 
            let len = List.length l |> float |> IntSharp.Types.Interval.FromDoublePrecise
            List.map (fun (x,_) -> x, 1./len) l
