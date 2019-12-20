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

    let intToExpression = BigRational.FromInt >> Expression.FromRational

    let intToRational = BigRational.FromInt 

    let observe test = cont { if not test then return! fail() }

    let constrain test = observe test
    
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
            | [(p,Value v)]  -> insertWith (+) v (p * pcontrib) ans
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
                   Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans  
            | n -> driver ch (loop one ans ch) (n-1) 
        driver (reify0 one ch) Map.empty nsamples 
         
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
                return! categorical
                            ([| for _ in 1 .. n -> sampler() |]
                             |> coarsenWithGeneric numbertype coarsener)
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
    open Hansei.Visualization
    open MathNet.Symbolics
    open MathNet.Symbolics.Core

    let createGraphSymb joint = createGraph2 joint Expression.toFormattedString 1Q (fun s -> Expression.fromFloat (float s))
    let createGraphSymbQ joint = createGraph2 joint (fun c ->  c.ToString()) (Complex 1Q) (fun s -> Complex (Expression.fromFloat (float s)))
 
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
