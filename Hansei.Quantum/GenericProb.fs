namespace Hansei

module GenericProb =

    open System
    open Hansei.Utils
    open Hansei.Continuation    
    open MathNet.Symbolics
    open Prelude.Common
    open MathNet.Numerics
   

    type GenericProbabilitySpace<'a, 'T> = list<'a * GenericWeightedTree<'a, 'T>>
    and GenericWeightedTree<'a, 'T> = 
        | Value of 'T 
        | Continued of Lazy<GenericProbabilitySpace<'a, 'T>>    

    let valueExtract = function Value x -> x

    let valueExtract2 = function Value x -> Some x | _ -> None

    let distribution ch k = List.map (fun (p:'a,v) -> (p, Continued(lazy(k v)))) ch

    let fail () = distribution []

    let inline reify0 one m = m (fun x -> [(one , Value x)])

    let exactly one x = distribution [one , x] 

    let inline explore one (maxdepth : int option) (choices : GenericProbabilitySpace<'a , 'T>) =
      let rec loop p depth down susp answers =
        match (down, susp, answers) with
        | (_, [], answers) -> answers 
    
        | (_, (pt, Value v) :: rest, (ans, susp)) ->
          loop p depth down rest (insertWithx (+) v (pt*p) ans, susp)
 
        | (true, (pt,Continued (Lazy t))::rest, answers) ->
          let down' = match maxdepth with Some x -> depth < x | None -> true
          loop p depth true rest <| loop (pt*p) (depth+1) down' (t) answers
 
        | (down, (pt,c)::rest, (ans,susp)) ->
          loop p depth down rest (ans, (pt*p,c)::susp)
       
      let (ans,susp) = loop one 0 true choices (Dict(), [])   
      [ yield! susp
        for (KeyValue(v,p)) in ans -> p, Value v] : GenericProbabilitySpace<_, _>

    let inline exact_reify one model = explore one None (reify0 one model)
    let inline limit_reify one n model = explore one (Some n) (reify0 one model)

    let intToExpression = BigRational.FromInt >> Expression.FromRational
    let intToRational = BigRational.FromInt 

    let observe test = cont { if not test then return! fail() }

    type Model() =  
        static member inline Reify(one, thunk, ?limit) = 
            match limit with
            | None -> exact_reify one thunk
            | Some n -> limit_reify one n thunk

    type ModelFrom<'w, 'a, 'b>
        (reify0, explore : int option -> GenericProbabilitySpace<'w,'a> -> GenericProbabilitySpace<_,_>, 
            thunk : ('a -> GenericProbabilitySpace<'w, 'a>) -> GenericProbabilitySpace<'w,'b>) =
        let exact_reify model = explore None (reify0 model)
        let limit_reify n model = explore (Some n) (reify0 model)
        member __.model = thunk 
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
            
        let mapValuesProb fp f l = 
            [for (p,v) in l do
                match v with 
                | Value x -> yield (fp p, (f x))
                | _ -> ()]

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

        let discretizedSampler coarsener sampler (n:int) = cont {
            return! categorical ([|for _ in 1..n -> sampler ()|] |> coarsenWith coarsener)   
        }

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
