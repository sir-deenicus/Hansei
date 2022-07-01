#r @"..\bin\Debug\net47\Prelude.dll"
#r @"..\bin\Debug\net47\Hansei.Core.dll"
#r @"..\bin\Debug\net47\Hansei.dll"
open Prelude.Math
open Prelude.Common
open System
open Hansei.Core
open Hansei.Utils
open Hansei.Continuation
open Hansei.Core.Distributions

//This document is tests of modifications to allow Hansei to avoid unproductive paths by learning to weight certain paths, as apart from the underlying distribution.
//The below is a modified version of random selector, it should allow sampling of unnormalized distributions.

let random_selector2 =
    let rec selection r wtotal ptotal pcum =
        function
        | [] -> failwith "Choice selection: can't happen"
        | ((p, i, th) :: rest) ->
            let pcum = pcum + p
            if r < pcum then (ptotal, wtotal, i, th) 
            else selection r wtotal ptotal pcum rest

    fun (ws:_[]) (pass:_[]) T choices ->
        let mutable ptotal, wtotal, i = 0., 0., -1

        let wch =
            [ for (p, t) in choices do
                ptotal <- ptotal + p
                i <- i + 1
                
                if pass.[i] then 
                    let pw = (p * ws.[i]) ** (1. / T)
                    wtotal <- wtotal + pw
                    yield pw, i, t ]

        let r = random.NextDouble(0., wtotal) (* 0<=r<ptotal *)
        selection r wtotal ptotal 0.0 wch

let sampler T = random_selector2 [|1.;1.|] [|true;true|] T [8.,"A"; 2., "B"]
let countAndnormalize ps =
    ps
    |> Array.countBy id
    |> Array.map (keepLeft float)
    |> Array.normalizeWeights

//This should have a ratio of 0.8 for A, 0.2 for B
 
[|for _ in 1..100 -> sampler 1.|] |> countAndnormalize
 
// Increase the temperature 

// Demonstrate that even if ps do not sum to 1, we can still sample from them in a sensible manner.

let mutable T = 5.62

[|"A", 8. ** (1./T); "B", 2. ** (1./T)|] |> Array.normalizeWeights

[|"A", 0.8 ** (1./T); "B", 0.2 ** (1./T)|] |> Array.normalizeWeights

[|for _ in 1..10000 -> sampler T|] |> countAndnormalize

[|for _ in 1..10000 -> sampler 100.|] |> countAndnormalize

// Let's try weights. The below modification changes things to 4., 2.

let sampler2 T = random_selector2 [|0.5;1.|] [|true;true|] T [8.,"A"; 2., "B"]

Array.normalize [|4.; 2.|] 
//val it : float [] = [|0.6666666667; 0.3333333333|]

[|for _ in 1..10000 -> sampler2 1.|] |> countAndnormalize

T <- 2.

[|"A", 4. ** (1./T); "B", 2. ** (1./T)|] |> Array.normalizeWeights

[|for _ in 1..10000 -> sampler2 T|] |> countAndnormalize

// Again but the below modifications yield 4., 6.

let sampler3 T = random_selector2 [|0.5;3.|] [|true;true|] T [8.,"A"; 2., "B"]
 
[|for _ in 1..1000 -> sampler3 1.|] |> countAndnormalize

[|"A", 4. ** (1./T); "B", 6. ** (1./T)|] |> Array.normalizeWeights

[|for _ in 1..10000 -> sampler3 T|] |> countAndnormalize

//Finally, test leaving out an option.

let sampler4 T = random_selector2 [|1.;1.;1.|] [|true;false;true|] T [7.,"A"; 2., "B"; 1., "C"]
 
[|for _ in 1..1000 -> sampler4 1.|] |> countAndnormalize

[|for _ in 1..10000 -> sampler3 T|] |> countAndnormalize

Array.normalize [|7.; 1.|] 

// In the actual code, ptotal will always be less than 1. wtotal on the other hand can be any positive number. ptotal needs to be separate to preserve the propagation of probability.

// # Sampling with addresses

let random_selector3 =
    let rec selection r ptotal pcum =
        function
        | [] -> failwith "Choice selection: can't happen"
        | ((p, i, th) :: rest) ->
            let pcum = pcum + p
            if r < pcum then (ptotal, i, th) 
            else selection r ptotal pcum rest

    fun paths curpath T choices ->
        let mutable ptotal, wtotal, i = 0., 0., -1

        let wch =
            [ for (p, t) in choices do
                ptotal <- ptotal + p
                i <- i + 1
                let skip, w = testPath paths (i :: curpath)
                if not skip then 
                    let pw = (p * w) ** (1. / T)
                    wtotal <- wtotal + pw
                    yield pw, i, t ]

        let r = random.NextDouble(0., wtotal) (* 0<=r<ptotal *)
        selection r ptotal 0.0 wch

let sample_distb prevtabu maxTemperature maxdepth (selector) (sample_runner) (ch:ProbabilitySpace<_>) =
    let paths = defaultArg prevtabu (Dict<int32 list, float>())
    let useTemperature = maxTemperature > 1. 
    let gainT = maxTemperature ** (1./200.)
    let attenT = maxTemperature ** (-1./10.)
    let mutable T = 1.
    printfn "%A" useTemperature

    let look_ahead curpath pcontrib (ans,j,acc) = function (* explore the branch a bit *)
    | (p,Value v) -> 
        propagateUp paths 0.5 0.01 (j::curpath)
        printfn "In Lookahead. Item:%A, path: %A" v (j::curpath)
        if useTemperature then T <- min maxTemperature (T * attenT)
        insertWithx (+) v (p * pcontrib) ans, j + 1, acc
    | (p,Continued (Lazy t)) -> 
        match (t)  with
        | [] -> 
            propagateUp paths 0.5 -0.01 (j::curpath)
            paths.ExpandElseAdd (j::curpath) (fun _ -> -1.0) -1.0
            printfn "In Lookahead. Fail , path: %A" (j::curpath)
            if useTemperature then T <- min maxTemperature (T * gainT)
            (ans, j + 1, (acc:(float * ProbabilitySpace<_>) list))
        | [(p1,Value v)] ->
            propagateUp paths 0.5 0.01 (j::curpath)
            //printfn "In Lookahead, p1. Item:%A, path: %A" v (j::curpath)
            if useTemperature then T <- min maxTemperature (T * attenT)
            (insertWithx (+) v (p * p1 * pcontrib) ans, j + 1, acc)
        | ch ->
            let ptotal = List.fold (fun pa (p,_) -> pa + p) 0.0 ch 
            (ans, j + 1,
              if ptotal < nearly_one then
                (p * ptotal, List.map (fun (p,x) -> (p / ptotal,x)) ch)::acc 
              else (p, ch)::acc)        
    let rec loop (curpath : int32 list) depth pcontrib (ans:Dict<_,_>) = function
    | [(p,Value v)]  -> 
        propagateUp paths 0.5 0.2 curpath
        printfn "Item:%A, path: %A" v (curpath)
        if useTemperature then T <- min maxTemperature (T * attenT)
        insertWithx (+) v (p * pcontrib) ans
    | []         -> 
        propagateUp paths 0.5 -0.2 curpath
        printfn "Fail, path: %A" (curpath)
        paths.ExpandElseAdd curpath (fun _ -> -1.0) -1.0
        if useTemperature then T <- min maxTemperature (T * gainT)
        ans
    | [(p,Continued (Lazy th))] -> loop curpath (depth+1) (p * pcontrib) ans (th)
    | ch when depth < maxdepth -> (* choosing one thread randomly *)    
        match List.fold (look_ahead curpath pcontrib) (ans,0,[]) ch with
        | (ans,_,[]) -> ans
        | (ans,_,cch) -> 
           let (ptotal,i,th:ProbabilitySpace<_>) = selector paths curpath T cch
           printfn "Selected %d" i
           loop (i::curpath) (depth+1) (pcontrib * ptotal) ans th  
    | _ -> ans    
    let toploop pcontrib ans cch = (* cch are already pre-explored *)
        let (ptotal,i,th) = selector paths [] T cch 
        loop [i] 0 (pcontrib * ptotal) ans th 
    let driver pcontrib vals cch =
        let (ans,nsamples) = 
             sample_runner (Dict()) (fun ans -> toploop pcontrib ans cch)  
        let ns = float nsamples   
        for (KeyValue(v,p)) in vals do  
            insertWithx (+) v (ns * p) ans |> ignore
        printfn "sample_importance: done %d worlds\n" nsamples;     
        [for (KeyValue(v,p)) in ans -> p/ns, Value v], paths
    let rec make_threads depth pcontrib ans ch =  (* pre-explore initial threads *)
        match List.fold (look_ahead [] pcontrib) (ans,0,[]) ch with
        | (ans,_,[]) -> (* pre-exploration solved the problem *) 
          [for (KeyValue(v,p)) in ans -> p, Value v], paths
        | (ans,_,[(p,ch)]) when depth < maxdepth -> (* only one choice, make more *)
           make_threads (depth+1) (pcontrib * p) ans ch
          (* List.rev is for literal compatibility with an earlier version *)
        | (ans,_,cch) -> driver pcontrib ans (List.rev cch) 
    make_threads 0 1.0 (Dict()) ch : ProbabilitySpace<_> * Dict<_,_>

let sample_importanceN nsamples thunk = 
    let rec loop th z =
        function
        | 0 -> (z, nsamples) 
        | n -> loop th (th z) (n - 1)
    sample_distb None 0. 100 random_selector3 (fun z th -> loop th z nsamples)
        (reify0 thunk) 

let sample_dist maxdepth (selector) (sample_runner) (ch:ProbabilitySpace<_>) =
    let look_ahead pcontrib (ans,acc) = function (* explore the branch a bit *)
    | (p,Value v) -> (insertWithx (+) v (p * pcontrib) ans, acc)
    | (p,Continued (Lazy t)) -> 
        match (t)  with
        | [] -> (ans,(acc:(float * ProbabilitySpace<_>) list))
        | [(p1,Value v)] -> 
           (insertWithx (+) v (p * p1 * pcontrib) ans, acc)
        | ch ->
            let ptotal = List.fold (fun pa (p,_) -> pa + p) 0.0 ch 
            (ans,
              if ptotal < nearly_one then
                (p * ptotal, List.map (fun (p,x) -> (p / ptotal,x)) ch)::acc 
              else (p, ch)::acc)        
    let rec loop depth pcontrib (ans:Dict<_,_>) = function
    | [(p,Value v)]  -> insertWithx (+) v (p * pcontrib) ans
    | []         -> ans
    | [(p,Continued (Lazy th))] -> loop (depth+1) (p * pcontrib) ans (th)
    | ch when depth < maxdepth -> (* choosing one thread randomly *)    
        match List.fold (look_ahead pcontrib) (ans,[]) ch with
        | (ans,[]) -> ans
        | (ans,cch) ->
           let (ptotal,th:ProbabilitySpace<_>) = selector cch 
           loop (depth+1) (pcontrib * ptotal) ans th  
    | _ -> ans    
    let toploop pcontrib ans cch = (* cch are already pre-explored *)
        let (ptotal,th) = selector cch 
        loop 0 (pcontrib * ptotal) ans th 
    let driver pcontrib vals cch =
        let (ans,nsamples) = 
             sample_runner (Dict()) (fun ans -> toploop pcontrib ans cch)  
        let ns = float nsamples  
        //let ans = Map.fold (fun ans v p  -> insertWith (+) v (ns * p) ans) ans vals 
        for (KeyValue(v,p)) in vals do  
            insertWithx (+) v (ns * p) ans |> ignore
        printfn "sample_importance: done %d worlds\n" nsamples;
        //Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans       
        [for (KeyValue(v,p)) in ans -> p/ns, Value v]
    let rec make_threads depth pcontrib ans ch =  (* pre-explore initial threads *)
        match List.fold (look_ahead pcontrib) (ans,[]) ch with
        | (ans,[]) -> (* pre-exploration solved the problem *) 
          [for (KeyValue(v,p)) in ans -> p, Value v]
        | (ans,[(p,ch)]) when depth < maxdepth -> (* only one choice, make more *)
           make_threads (depth+1) (pcontrib * p) ans ch
          (* List.rev is for literal compatibility with an earlier version *)
        | (ans,cch) -> driver pcontrib ans (List.rev cch) 
    make_threads 0 1.0 (Dict()) ch : ProbabilitySpace<_> 

let sample_importanceNb nsamples (thunk) =
    let rec loop th z =
        function 
        | 0 -> (z, nsamples)
        | n -> loop th (th z) (n - 1)
    sample_dist 100 random_selector (fun z th -> loop th z nsamples) 
        (reify0 thunk)

cont {
    let! b = bernoulli 0.5 
    return b 
} |> sample_importanceN 1000

cont {
    let! b = bernoulli 0.5
    let! b2 = bernoulli 0.5
    return b,b2
} |> sample_importanceN 1000

cont {
    let! b = bernoulli 0.5
    let! b2 = bernoulli 0.5
    return b,b2
} |> sample_importanceNb 1000

(** 

Dec 25, 2019

Propagating rewards naively is problematic! If there is a reward every time a leaf node is hit, this can potentially harm inference by begining to overweighting on the most rewarding paths. Let me look at the reward function concept.
*)