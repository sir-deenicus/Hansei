//#r @".\bin\Release\net50\Hansei.Core.dll"
#r @"C:\Users\cybernetic\source\repos\Hansei\Hansei.Continuation\bin\Release\net50\Hansei.Core.dll"
#r @".\bin\Release\net50\Hansei.dll"

#I @"C:\Users\cybernetic\source\repos\"
#I @"C:\Users\cybernetic\.nuget\packages\"
#r "netstandard" 
#r @"dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"

open Prelude.Math
open Prelude.Common
open System
open Hansei.Core
open Hansei.Utils
open Hansei.Continuation
open Hansei.Core.Distributions
open Hansei.FSharpx.Collections  
 
open Hansei.Backtracking
  
let lazy_explore (choices: ProbabilitySpace<'T>) =  
    let rec loop p choices = LazyList.lazyList {
        match choices with
        | LazyList.Nil -> () 
        | LazyList.Cons((Value v, pt), rest) -> 
            yield (v, log pt + p)
            yield! loop p rest
        | LazyList.Cons((Continued (Lazy t), pt), rest) -> 
            yield! loop (log pt + p) (LazyList.choice t rest) 
    }
        
    loop 0. choices 
      

open Hansei.TreeSearch.Backtracking

let rec infd ls = search {
    let! a = choices [0..16]
    let ls' = a :: ls
    do! guard (ls'.Length <= 3)
    yield ls'
    return! infd ls'
}   


let infbits bits = 
    let rec build d xs = search {
        if d >= bits then return xs
        else
            let! a = choices [0;1]  
            return! build (d + 1) (a::xs)
    }    
    
    build 0 []

let intArrayToBytes (i) =
    let s = i |> Array.map string |> String.concat "" 
    Convert.ToByte(s, 2)
      
infbits 9
|> run None 
|> LazyList.takeOrMaxArray 20
|> Array.map (List.toArray >> Array.split_TakeN_atATime 3 >> Array.map intArrayToBytes)

#time 

infd [] 
|> FairStream.filter (fun (l) -> l.Length = 3)
|> run None 
|> LazyList.takeList 40



type ProbabilitySpace<'T> = LazyList<WeightedTree<'T> * float>
and WeightedTree<'T> = 
    | Value of 'T 
    | Continued of Lazy<ProbabilitySpace<'T>>       
        
//if use value instead of Continued, infinite computations will fail to return/terminate
let distribution_of_lazy ch = 
    LazyList.map (fun (v, p) -> Continued(lazy(LazyList.ofList [Value v, 1.])), p) ch : ProbabilitySpace<_> 
     
let distribution ch = distribution_of_lazy (LazyList.ofList ch) : ProbabilitySpace<_>  
  
let always x = distribution_of_lazy (LazyList.singleton (x,1.)) : ProbabilitySpace<_> 

let exactly x = distribution [x, 1.] 

let fail () = LazyList.empty : ProbabilitySpace<_>  
  
let reflect tree k =  
    let rec make_choices pv = 
        LazyList.map (function 
        | (Value x, p) -> Continued(lazy(k x)), p
        | (Continued(Lazy t), p) -> Continued(lazy(make_choices t)), p) pv 
    make_choices tree : ProbabilitySpace<_>  
    
type ProbabilitySpaceBuilder() =
    member inline d.Bind(space, k) = reflect space k
    member d.Return v = always v
    member d.ReturnFrom vs = vs : ProbabilitySpace<_> 
    member d.BindReturn (p:ProbabilitySpace<'a>, f:'a->'b) = 
        reflect p (f >> always) : ProbabilitySpace<_> 
    member d.Zero () = always ()  
    member __.Combine(x,y) = LazyList.choice x y
    member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
    member l.Yield x = l.Return x  
    member __.MergeSources(p1, p2) = LazyList.zip p1 p2
     
let dist = ProbabilitySpaceBuilder()
  
let observe test = dist { if not test then return! fail() } : ProbabilitySpace<_> 

let nearly_one = 1.0 - 1e-7
 

let sample_dist subsample nsamples maxdepth (selector:((ProbabilitySpace<_>*float) list) -> _) (ch: ProbabilitySpace<_>) =  
    let rec unravelSingleton d ps = function 
        | _ when d > 5 -> None
        | LazyList.Singleton (Continued (Lazy v), p) ->  
            unravelSingleton (d+1) (p*ps) v
        | LazyList.Singleton (Value v, p) -> Some (v, p * ps)
        | _ -> None
                    
    let look_ahead pcontrib (ans, acc) =
        function (* explore the branch a bit *)
        | (Value v, p) -> 
            printfn "In Lookahead. Item:%A" v
            insertWithx (+) v (p * pcontrib) ans, acc
        | (Continued (Lazy t), p) ->
            match t with
            | LazyList.Nil -> 
                printfn "In Lookahead. Empty"
                (ans, acc)
            | LazyList.Singleton (Value v, p1) -> 
                printfn "In Continued singleton"
                insertWithx (+) v (p * p1 * pcontrib) ans, acc
            | _ch_ ->  
                match unravelSingleton 0 1. _ch_ with 
                | Some (v, p1) ->  
                    insertWithx (+) v (p * p1 * pcontrib) ans, acc
                | _ ->
                    let ch = subsample _ch_
                    let ptotal = LazyList.fold (fun pa (_, p) -> pa + p) 0.0 ch

                    (ans,
                     if ptotal < nearly_one then
                         (LazyList.map (fun (x, p) -> (x, p / ptotal)) ch, p * ptotal)
                         :: acc
                     else (ch, p) :: acc)

    let rec loop depth pcontrib (ans: Dict<_, _>) =
        function
        | LazyList.Singleton (Value v, p) -> insertWithx (+) v (p * pcontrib) ans
        | LazyList.Nil -> ans
        | LazyList.Singleton (Continued (Lazy th), p) -> loop (depth + 1) (p * pcontrib) ans th
        | ch when depth < maxdepth -> (* choosing one thread randomly *)
            match LazyList.fold (look_ahead pcontrib) (ans, []) (subsample ch) with
            | (ans, []) -> ans
            | (ans, cch) ->
                let (th: ProbabilitySpace<_>, ptotal) = selector cch
                loop (depth + 1) (pcontrib * ptotal) ans th
        | _ -> ans
        
    let toploop pcontrib cch ans =
        (* cch are already pre-explored *)
        let (th, ptotal) = selector cch
        loop 0 (pcontrib * ptotal) ans th 
    
    let rec sample_runner samples th =
        function 
        | 0 -> samples
        | n -> sample_runner (th samples) th (n - 1)

    let driver pcontrib vals cch =
        let ans = sample_runner (Dict()) (toploop pcontrib cch) nsamples
        let ns = float nsamples
        //let ans = Map.fold (fun ans v p  -> insertWith (+) v (ns * p) ans) ans vals
        for (KeyValue (v, p)) in vals do
            insertWithx (+) v (ns * p) ans |> ignore

        printfn "sample_importance: done %d worlds\n" nsamples
        //Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans
        [ for (KeyValue (v, p)) in ans -> Value v, p / ns ]

    let rec pre_explore depth pcontrib ans ch =
        (* pre-explore initial threads *)
        printfn "going in look_ahead"
        let res = LazyList.fold (look_ahead pcontrib) (ans, []) (subsample ch)
        printfn "res = %A" res
        match res with
        | (ans, []) -> (* pre-exploration solved the problem *) 
            printfn "done"
            [ for (KeyValue (v, p)) in ans -> Value v, p ]
        | (ans, [ch, p]) when depth < maxdepth -> (* only one choice, make more *)
            printfn "make_threads"
            pre_explore (depth + 1) (pcontrib * p) ans ch 
        | (ans, cch) -> 
            printfn "going to driver"
            driver pcontrib ans cch

    pre_explore 0 1.0 (Dict()) ch

let random_selector_list dosort =  
    let rec selection r ptotal pcum = function
        | [] -> failwith "Choice selection: can't happen"
        | (th, p) :: rest -> 
            let pcum = pcum + p  
            if r < pcum then (th, ptotal)
            else selection r ptotal pcum rest

    fun choices ->
        let ptotal = List.sumBy snd choices  
        let r = random.NextDouble (0., ptotal)      (* 0<=r<ptotal *)
        
        if dosort then List.sortBy snd choices
        else choices
        |> selection r ptotal 0.0 
        
let bernoulli p = distribution [(true, p); (false, 1.0-p)]

let l =
    dist {
        let! a = bernoulli 0.999999
        match a with 
        | true -> return (a,1)
        | false -> 
            let! c = bernoulli 0.999999
            let! b = bernoulli 0.8
            match b with 
            | true -> return (b,3)
            | _ ->return (c,0)
    } 

l|> sample_dist id 1000 100 (random_selector_list false)
  