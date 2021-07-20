module Hansei.Core

open Hansei.Continuation
open Hansei.Utils
open Prelude.Common
open Prelude.Math 
open System
open Prelude.Math
open Utils.Sampling

//open FSharp.Collections.ParallelSeq

//Core of Hansei modified from base:
//https://gist.github.com/einblicker/3245547#file-hansei
//Remaining and majority of code (till line 250) ported and modified to work with continuation monad from
//http://okmij.org/ftp/kakuritu/Hansei.html
//Ocaml style comments in code below are Oleg's

//This framework is much more flexible than the system described in Expert F#. 
//A continuation monad is used to describe distributions as lazy continuation trees (lazily generated nested lists).
//Better base for http://dippl.org 
 
//==========

type ProbabilitySpace<'T> = list<float * WeightedTree<'T>>
and WeightedTree<'T> = 
    | Value of 'T 
    | Continued of Lazy<ProbabilitySpace<'T>>    

type ProbabilityContinuation<'a,'b> = 'a -> ProbabilitySpace<'b>

type ProbSearchResult<'T> =
    {Values : list<float * 'T>
     Continuation : ProbabilitySpace<'T>
     Paths : Dict<int32 list, float>}

let reflect tree k =  
    let rec make_choices pv = 
        List.map (function 
          | (p, Value x) -> (p, Continued(lazy(k x)))
          | (p, Continued(Lazy x)) -> (p, Continued(lazy(make_choices x)))) pv
        
    make_choices tree  : ProbabilitySpace<_> 

let variable_elim reify f arg zz = reflect (reify (f arg)) zz    
    
let distribution ch (k:ProbabilityContinuation<_,_>) = List.map (fun (p,v) -> (p, Continued(lazy(k v)))) ch : ProbabilitySpace<_>

let fail () = distribution []

let observe test = cont { if not test then return! fail() }

let constrain test = cont { if not test then return! fail() }

let softConstrainOn r = cont { if random.NextDouble() > r then return! fail() }

let filterDistribution f p = cont {
    let! x = p
    do! observe (f x)
    return x
}   

let inline reify0 (m:ProbabilityContinuation<_,_> -> 'b) = m (fun x -> [(1.0, Value x)] : ProbabilitySpace<_>)

let exactly x = distribution [1., x] 

let explore (maxdepth: int option) (choices: ProbabilitySpace<'T>) =
    let rec loop p depth down susp answers =
        match (down, susp, answers) with
        | (_, [], answers) -> answers
        | (_, (pt, Value v) :: rest, (ans, susp)) -> 
            loop p depth down rest (insertWithx (+) v (pt * p) ans, susp) 
        | (true, (pt, Continued (Lazy t)) :: rest, answers) ->
            let down' =
                match maxdepth with
                | Some x -> depth < x
                | None -> true
            loop p depth true rest
            <| loop (pt * p) (depth + 1) down' (t) answers

        | (down, (pt, c) :: rest, (ans, susp)) -> loop p depth down rest (ans, (pt * p, c) :: susp)

    //let (ans, susp) = loop 1.0 0 true choices (Map.empty, [])
    let (ans, susp) = loop 1.0 0 true choices (Dict(), [])

    //Map.fold (fun a v p -> (p, Value v)::a) susp ans : ProbabilitySpace<'T>
    [ yield! susp
      for (KeyValue (v, p)) in ans -> p, Value v ]: ProbabilitySpace<_>   
      
let exploreb (maxdepth : int option) (choices : ProbabilitySpace<'T>) =
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

  //let (ans, susp) = loop 1.0 0 true choices (Map.empty, [])
  let (ans,susp) = loop 1.0 0 true choices (Dict(), [])  
  //Map.fold (fun a v p -> (p, Value v)::a) susp ans : ProbabilitySpace<'T>
  [ yield! susp
    for (KeyValue(v,p)) in ans -> p, Value v] : ProbabilitySpace<_>
     
let nearly_one = 1.0 - 1e-7;

(* Explore but do not flatten the tree: 
   perform exact inference to the given depth
   We still pick out all the produced answers and note the failures. *)
     
let shallow_explore maxdepth (choices : ProbabilitySpace<_>) =
    let add_answer pcontrib v mp = insertWithx (+) v pcontrib mp

    let rec loop pc depth ans acc =
        function
        | [] -> (ans, acc)
        | _ when maxdepth = -1 -> (ans, acc)
        | (p, Value v) :: rest ->
            loop pc depth (add_answer (p * pc) v ans) acc rest
        | c :: rest when depth >= maxdepth -> loop pc depth ans (c :: acc) rest
        | (p, Continued(Lazy t)) :: rest ->
            let (ans, ch) = loop (pc * p) (depth + 1) ans [] t
            let ptotal = List.fold (fun pa (p, _) -> pa + p) 0.0 ch

            let acc =
                if ptotal = 0.0 then acc
                else if ptotal < nearly_one then
                    (p * ptotal,
                     let ch = List.map (fun (p, x) -> (p / ptotal, x)) ch
                     Continued(lazy ch))
                    :: acc
                else (p, Continued(lazy ch)) :: acc
            loop pc depth ans acc rest

    let (ans, susp) = loop 1.0 0 (Dict()) [] choices
    [ yield! susp
      for (KeyValue(v, p)) in ans -> p, Value v ] : ProbabilitySpace<_>


///(* Explore the tree till we find the first success -- the first leaf
///   (V v) -- and return the resulting tree. If the tree turns out to
///   have no leaves, return the empty tree. *) 
let rec first_success maxdepth = function
    | [] -> [] : ProbabilitySpace<_>
    | _ when maxdepth = 0 -> [] 
    | ((_,Value _) :: _) as l  -> 
    l |> List.groupBy snd 
        |> List.map (fun (v, ps) -> List.sumBy fst ps, v)
    | (pt,Continued (Lazy t)) :: rest -> (* Unclear: expand and do BFS *)
        first_success (maxdepth - 1) (rest @ List.map (fun (p,v) -> (pt * p,v)) t)
       
//This sampler takes after beam search or even breadth first search
//when the width is high enough.
let best_first_sample_dist (maxtime : _ option) prevtabu maxdepth maxTemperature
    niters space = 
    let t0 = System.DateTime.Now
    let paths = defaultArg prevtabu (Dict<int32 list, float>())
    let useTemperature = maxTemperature > 1. 
    let gainT = maxTemperature ** (1./200.)
    let gainLiteT = maxTemperature ** (1./300.)
    let attenT = maxTemperature ** (-1./10.)
    let mutable T = 1. 
    let discreteSampler ps =
        Stats.discreteSample
            (Array.normalize [| for (p, w, _) in ps -> (p * w)**(1./T) |])  

    let fch curpath ch =
        List.mapi (fun i (p, t) ->
            let pass, w = testPath paths (i :: curpath)
            if pass then 0., 0., t else p, w, t) ch
    let update curpath ans (pcontrib : float) = function 
        | p, Value v ->
            if not (paths.ContainsKey curpath) then
                //propagateUp paths 0.5 0.1 curpath
                paths.ExpandElseAdd curpath (fun _ -> -1.0) -1.0
                if useTemperature then T <- max 1. (T * attenT)
                Utils.insertWithx (+) v (p * pcontrib) ans |> ignore
            true 
        | _ -> false  
    let rec loop (curpath : int32 list) maxd depth pcontrib ans =
        function
        | []  -> 
            //propagateUp paths 0.5 -0.1 curpath
            paths.ExpandElseAdd curpath (fun _ -> -1.0) -1.0
            if useTemperature then T <- min maxTemperature (T * gainT) 
        | [ x ] -> update (0::curpath) ans pcontrib x |> ignore
        | _ when depth > maxdepth
                 || (maxtime.IsSome
                     && (DateTime.Now - t0).TotalSeconds > maxtime.Value) ->
            if useTemperature then T <- min maxTemperature (T * gainLiteT) 
        | [ p, Continued(Lazy th) ] -> loop curpath maxd (depth + 1) (p * pcontrib) ans th
        | ch -> 
            let branches =
                let mutable i = -1
                [ for c in ch do    
                    i <- i + 1
                    if not (update (i::curpath) ans pcontrib c) then yield c] 

            let choices =
                sampleN_No_ReplacementsX discreteSampler 1 (fch curpath branches) 
            for (b, (p, _, t)) in choices do
                if p <> 0. then
                    loop (b :: curpath) maxd (depth + 1) (pcontrib) ans [ p, t ] 
        | _ -> ()

    let rec sampler (ch : ProbabilitySpace<'T>) ans =
        function
        | 0 ->
            let t1 = System.DateTime.Now
            printfn "done %d worlds\nTime taken: %A seconds" niters
                (round 3 ((t1 - t0).TotalSeconds))
            { Values =
                  [ for (KeyValue(v, p)) in ans -> p, v ]
              Continuation = ch
              Paths = paths }
        | n ->
            loop [] maxdepth 0 1. ans ch
            sampler ch ans (n - 1)

    sampler (reify0 space) (Dict()) niters
     
(* ------------------------------------------------------------------------ *)
(*	Approximate inference strategies:				                        *)
(*  Trace a few paths from the root to a leaf of the search tree            *)
(* The following procedures are non-deterministic; they use a given selector*)
(* procedure, of the type 'selector', to chose among the alternatives.      *)
(* For top-level inference, the selector uses system random generator.      *)

(* Naive, rejection sampling: the baseline *)        
(* Random selection from a list of choices, using system randomness *)

let max_selector choices = List.maxBy fst choices  

let random_selector =  
  let rec selection r ptotal pcum = function
      | [] -> failwith "Choice selection: can't happen"
      | ((p,th)::rest) -> 
        let pcum = pcum + p  
        if r < pcum then (ptotal,th)
        else selection r ptotal pcum rest

  fun choices ->
      let ptotal = List.sumBy fst choices  
      let r = random.NextDouble (0., ptotal)      (* 0<=r<ptotal *)
      selection r ptotal 0.0 choices
  
let random_selector2 =
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
  

let rejection_sample_dist selector nsamples ch =    
    let t0 = System.DateTime.Now
    
    let rec loop pcontrib ans = function
        | ([(p,Value v)] :ProbabilitySpace<_>) -> insertWith (+) v (p * pcontrib) ans
        | []         -> ans
        | [(p,Continued (Lazy th))] -> loop (p * pcontrib) ans (th)
        | ch ->
            let (ptotal,th) = selector (ch)
            loop (pcontrib * ptotal) ans [(1.0,th)] 

    let rec driver (ch:ProbabilitySpace<_>) ans = function
        | 0 -> let ns = float nsamples 
               let t1 = System.DateTime.Now
               printfn "rejection_sample: done %d worlds\nTime taken: %A seconds" nsamples (round 3 ((t1 - t0).TotalSeconds))
               Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans : ProbabilitySpace<_>
              
        | n -> driver ch (loop 1.0 ans ch) (n-1) 
    driver (reify0 ch) Map.empty nsamples  : 'a ProbabilitySpace
                          
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

//let sample_distb prevtabu maxTemperature maxdepth (selector) (sample_runner) (ch:ProbabilitySpace<_>) =
//    let paths = defaultArg prevtabu (Dict<int32 list, float>())
//    let useTemperature = maxTemperature > 1. 
//    let gainT = maxTemperature ** (1./200.)
//    let attenT = maxTemperature ** (-1./10.)
//    let mutable T = 1.

//    let look_ahead curpath pcontrib (ans,j,acc) = function (* explore the branch a bit *)
//    | (p,Value v) -> 
//        propagateUp paths 0.5 0.2 (j::curpath)
//        if useTemperature then T <- min maxTemperature (T * attenT)
//        insertWithx (+) v (p * pcontrib) ans, j + 1, acc
//    | (p,Continued (Lazy t)) -> 
//        match (t)  with
//        | [] -> 
//            propagateUp paths 0.5 -0.2 (j::curpath)
//            paths.ExpandElseAdd (j::curpath) (fun _ -> -1.0) -1.0
//            if useTemperature then T <- min maxTemperature (T * gainT)
//            (ans, j + 1, (acc:(float * ProbabilitySpace<_>) list))
//        | [(p1,Value v)] ->
//            propagateUp paths 0.5 0.2 (j::curpath)
//            if useTemperature then T <- min maxTemperature (T * attenT)
//            (insertWithx (+) v (p * p1 * pcontrib) ans, j + 1, acc)
//        | ch ->
//            let ptotal = List.fold (fun pa (p,_) -> pa + p) 0.0 ch 
//            (ans, j + 1,
//              if ptotal < nearly_one then
//                (p * ptotal, List.map (fun (p,x) -> (p / ptotal,x)) ch)::acc 
//              else (p, ch)::acc)        
//    let rec loop (curpath : int32 list) depth pcontrib (ans:Dict<_,_>) = function
//    | [(p,Value v)]  -> 
//        propagateUp paths 0.5 0.2 curpath
//        if useTemperature then T <- min maxTemperature (T * attenT)
//        insertWithx (+) v (p * pcontrib) ans
//    | []         -> 
//        propagateUp paths 0.5 -0.2 curpath
//        paths.ExpandElseAdd curpath (fun _ -> -1.0) -1.0
//        if useTemperature then T <- min maxTemperature (T * gainT)
//        ans
//    | [(p,Continued (Lazy th))] -> loop curpath (depth+1) (p * pcontrib) ans (th)
//    | ch when depth < maxdepth -> (* choosing one thread randomly *)    
//        match List.fold (look_ahead curpath pcontrib) (ans,0,[]) ch with
//        | (ans,_,[]) -> ans
//        | (ans,_,cch) -> 
//           let (ptotal,i,th:ProbabilitySpace<_>) = selector paths curpath T cch
//           loop (i::curpath) (depth+1) (pcontrib * ptotal) ans th  
//    | _ -> ans    
//    let toploop pcontrib ans cch = (* cch are already pre-explored *)
//        let (ptotal,i,th) = selector paths [] T cch 
//        loop [i] 0 (pcontrib * ptotal) ans th 
//    let driver pcontrib vals cch =
//        let (ans,nsamples) = 
//             sample_runner (Dict()) (fun ans -> toploop pcontrib ans cch)  
//        let ns = float nsamples   
//        for (KeyValue(v,p)) in vals do  
//            insertWithx (+) v (ns * p) ans |> ignore
//        printfn "sample_importance: done %d worlds\n" nsamples;     
//        [for (KeyValue(v,p)) in ans -> p/ns, Value v]
//    let rec make_threads depth pcontrib ans ch =  (* pre-explore initial threads *)
//        match List.fold (look_ahead [] pcontrib) (ans,0,[]) ch with
//        | (ans,_,[]) -> (* pre-exploration solved the problem *) 
//          [for (KeyValue(v,p)) in ans -> p, Value v]
//        | (ans,_,[(p,ch)]) when depth < maxdepth -> (* only one choice, make more *)
//           make_threads (depth+1) (pcontrib * p) ans ch
//          (* List.rev is for literal compatibility with an earlier version *)
//        | (ans,_,cch) -> driver pcontrib ans (List.rev cch) 
//    make_threads 0 1.0 (Dict()) ch : ProbabilitySpace<_> 

//=================
///////////////////
(* A selector from a list of choices relying on the non-determinism
   supported by the parent reifier.
*)
let dist_selector ch =
    let ptotal = List.fold (fun pa (p, _) -> pa + p) 0.0 ch
    (ptotal, distribution (List.map (fun (p, v) -> (p / ptotal, v)) ch))

let sample_importanceN3 selector d maxdpeth nsamples (thunk) =
    let rec loop th samples =
        function 
        | 0 -> (samples, nsamples)
        | n -> loop th (th samples) (n - 1)
    sample_dist maxdpeth selector (fun samples th -> loop th samples nsamples) 
        (shallow_explore d (reify0 thunk))

//let sample_importanceN4 (maxtime : _ option) selector lowp d maxdpeth nsamples
//    thunk =
//    let t0 = DateTime.Now 
//    let rec loop th z =
//        function
//        | 0 -> (z, nsamples)
//        | _ when maxtime.IsSome
//                 && (DateTime.Now - t0).TotalSeconds > maxtime.Value ->
//            (z, nsamples)
//        | n -> loop th (th z) (n - 1)
//    sample_distb lowp maxdpeth selector (fun z th -> loop th z nsamples)
//        (shallow_explore d (reify0 thunk))

let sample_importance maxdpeth nsamples (thunk) =
    sample_importanceN3 random_selector 3 maxdpeth nsamples (thunk)
             
let inline exact_reify model = explore None (reify0 model)
let inline limit_reify n model = explore (Some n) (reify0 model)

type Model() =
    static member ImportanceSample(thunk, nsamples, maxdepth, ?maxtime,
                                   ?shallowExploreDepth, ?selector) =
        sample_importanceN3 (defaultArg selector random_selector)
            (defaultArg shallowExploreDepth 3) maxdepth nsamples (thunk)
    //static member ImportanceSampleExplore(thunk, nsamples, maxdepth,
    //                                      ?lowprobBranchExploreProbability,
    //                                      ?maxtime, ?shallowExploreDepth,
    //                                      ?selector) =
    //    sample_importanceN4 maxtime (defaultArg selector random_selector)
    //        (defaultArg lowprobBranchExploreProbability 0.1)
    //        (defaultArg shallowExploreDepth 3) maxdepth nsamples (thunk)
    //static member SampleBreadth(space, niters, maxdepth,?maxwidth, ?lookaheadWidth,  
    //                            ?beamwidth, ?maxTemperature,
    //                            ?maxtime, ?state) =
    //    best_first_sample_dist maxtime state (defaultArg maxwidth 16.) maxdepth 
    //        (defaultArg beamwidth 8) (defaultArg lookaheadWidth 32)
    //        (defaultArg maxTemperature 0.) niters space
    static member Reify(thunk, ?limit) =
        match limit with
        | None -> exact_reify thunk
        | Some n -> limit_reify n thunk

type ModelFrom<'a, 'b when 'b : comparison>(thunk : ('a -> ProbabilitySpace<'a>) -> ProbabilitySpace<'b>) =
    member __.model = thunk

    member __.ImportanceSample(nsamples, maxdepth, ?maxtime,
                                   ?shallowExploreDepth, ?selector) =
        sample_importanceN3 (defaultArg selector random_selector) 
            (defaultArg shallowExploreDepth 3) maxdepth nsamples (thunk)
    
    member __.Reify(?limit) =
        match limit with
        | None -> exact_reify thunk
        | Some n -> limit_reify n thunk
    
    member __.RejectionSample nsamples =
        rejection_sample_dist random_selector nsamples thunk

//=-=-=-=-=-=-=-=-=-=

module ProbabilitySpace =   
    let inline expectedValue (ps) =
        ps |> List.map (function (p, Value x) -> x * p | _ -> 0.) |> List.sum

    let valueExtract = function Value x -> x | _ -> failwith "Not a value"

    let valueExtract2 = function Value x -> Some x | _ -> None

    let printWith fp f x =  
        List.map (function (p, Value x) -> fp p, f x | (p, Continued _) -> fp p, "...") x
         
    let mapValue f (p,v) = 
        p, match v with Value x -> Value(f x) | _ -> failwith "error"
    
    let inline top l =
        l
        |> List.sortByDescending fst 
        |> List.head

    let best l =
        l |> List.maxBy fst |> snd

    let map f l = 
        [for (p,v) in l do
            match v with 
            | Value x -> yield (p, Value(f x))
            | _ -> yield (p,v)]

    let mapValuesProb fp f l = 
        [for (p,v) in l do
            match v with 
            | Value x -> yield (fp p, (f x))
            | _ -> ()]

    let mapValues f l = 
        [for (p,v) in l do
            match v with 
            | Value x -> yield (p, (f x))
            | _ -> ()]

    let inline mkProbabilityMap t =
        Map [for (p, v) in (normalize t) do
              match v with
               | Value x -> yield (x,p)
               | _ -> ()]     

module Distributions =                

    let bernoulli p = distribution [(p, true); (1.0-p, false)]

    let bernoulliChoice p (a,b) = distribution [(p, a); (1.0-p, b)]
                                                          
    let uniform (items:'a list) = 
        let num = float items.Length
        distribution (List.map (fun item -> 1./num, item) items)

    let categorical distr = distribution distr 

    //=-=-=-=-=-=-=-=-=-=

    let rec geometric n p = cont {
        let! a = bernoulli p
        if a then return n else return! (geometric(n+1) p)
    }
   
    ///polya's urn
    let rec beta roundto draws a b = cont {
        if draws <= 0 then return (round roundto (a/(a+b)))
        else let! ball = categorical [a/(a+b),1;b/(a+b),2]
             if ball = 1 then return! beta roundto (draws - 1) (a+1.) b
             else return! beta roundto (draws-1) a (b+1.)
    } 

    let rec dirichlet3 roundto draws a b c = cont {
        if draws <= 0 then return (Array.map (round roundto) [|a/(a+b+c);b/(a+b+c);c/(a+b+c)|])
        else let! ball = categorical [a/(a+b+c),1;b/(a+b+c),2;c/(a+b+c),3]
             if ball = 1 then return! dirichlet3 roundto (draws - 1) (a+1.) b c
             elif ball = 2 then return! dirichlet3 roundto (draws - 1) a (b+1.) c
             else return! dirichlet3 roundto (draws-1) a b (c+1.)
    }

    let rec dirichlet roundto draws d = cont {
        let z = List.sum d
        if draws <= 0 then 
            return (List.map (fun a -> round roundto (a/z)) d)
        else          
            let ps = List.mapi (fun i a -> (a/z), i) d
            let! ball = categorical ps
            let d' = List.mapi (fun i a -> if i = ball then a + 1. else a) d
            return! dirichlet roundto (draws - 1) d'         
    }
  
    let discretizedSampler coarsener sampler (n:int) = cont {
        return! categorical ([for _ in 1..n -> sampler ()] |> coarsenWith coarsener)   
    }

