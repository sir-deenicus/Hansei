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

        
                
     
