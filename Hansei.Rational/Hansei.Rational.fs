module Hansei.Core

open Hansei.Continuation
open Hansei.Continuation.Rational
open Hansei.Utils
open MathNet.Numerics
open Prelude.Common
open Prelude.Math

//Core of Hansei modified from base:
//https://gist.github.com/einblicker/3245547#file-hansei
//Remaining and majority of code (till line 250) ported/lightly modified from
//http://okmij.org/ftp/kakuritu/Hansei.html
//Ocaml style comments in code below are Oleg's

//This framework is much more flexible than the system described in Expert F#. 
//A continuation monad is used to describe distributions as lazy continuation trees.
//Better base for http://dippl.org 
 
//==========

let distribution ch k = List.map (fun (p,v) -> (p, Continued(lazy(k v)))) ch

let fail () = distribution []

let inline reify0 m = m (fun x -> [(1N, Value x)])

let exactly x = distribution [1N, x]


let explore (maxdepth : int option) (choices : ProbabilitySpace<'T>) =
  let rec loop (p:BigRational) depth down susp answers =
    match (down, susp, answers) with
    | (_, [], answers) -> answers 
 
    | (_, (pt, Value v) :: rest, (ans, susp)) ->
      loop p depth down rest (insertWith (+) v (pt*p) ans, susp)
 
    | (true, (pt,Continued (Lazy t))::rest, answers) ->
      let down' = match maxdepth with Some x -> depth < x | None -> true
      loop p depth true rest <| loop (pt*p) (depth+1) down' (t) answers
 
    | (down, (pt,c)::rest, (ans,susp)) ->
      loop p depth down rest (ans, (pt*p,c)::susp)

  let (ans, susp) = loop 1N 0 true choices (Map.empty, [])
  Map.fold (fun a v p -> (p, Value v)::a) susp ans : ContinuationTree<'T>

let nearly_one = 9007198354021067N/9007199254740992N;

(* Explore but do not flatten the tree: 
   perform exact inference to the given depth
   We still pick out all the produced answers and note the failures. *)
let shallow_explore maxdepth (choices ) =
    let add_answer pcontrib v mp = insertWith (+) v pcontrib mp 
    let rec loop pc depth ans acc = function
    | [] -> (ans,acc)
    | (p,Value v)::rest -> loop pc depth (add_answer (p * pc) v ans) acc rest
    | c::rest when depth >= maxdepth -> loop pc depth ans (c::acc) rest
    | (p,Continued (Lazy t))::rest -> 
      let (ans,ch) = loop (pc * p) (depth + 1) ans [] (t) 
      let ptotal = List.fold (fun pa (p,_) -> pa + p) 0N ch 
      let acc =
        if ptotal = 0N then acc
        else if ptotal < nearly_one then
             (p * ptotal, 
              let ch = List.map (fun (p,x) -> (p / ptotal,x)) ch          
              Continued (lazy ch))::acc
            else (p, Continued (lazy ch))::acc 
      loop pc depth ans acc rest
    
    let (ans,susp) = loop 1N 0 Map.empty [] choices
    Map.fold (fun a v p -> (p,Value v)::a) susp ans 

(* ------------------------------------------------------------------------ *)
(*	Approximate inference strategies:				    *)
(*  Trace a few paths from the root to a leaf of the search tree            *)
(* The following procedures are non-deterministic; they use a given selector*)
(* procedure, of the type 'selector', to chose among the alternatives.      *)
(* For top-level inference, the selector uses system random generator.      *)

(* Naive, rejection sampling: the baseline *)        
(* Random selection from a list of choices, using system randomness *)
let random_selector  =  
  let rec selection r ptotal pcum = function
      | [] -> failwith "Choice selection: can't happen"
      | ((p,th)::rest) -> 
        let pcum = pcum + p  
        if r < float pcum then (ptotal,th)
        else selection r ptotal pcum rest

  fun choices ->
      let ptotal = List.sumBy fst choices  
      let r = random.NextDouble (0., float ptotal)      (* 0<=r<ptotal *)
      selection r ptotal 0N choices

let rejection_sample_dist selector nsamples ch : 'a ProbabilitySpace =    
    let t0 = System.DateTime.Now
    
    let rec loop pcontrib ans = function
        | [(p,Value v)]  -> insertWith (+) v (p * pcontrib) ans
        | []         -> ans
        | [(p,Continued (Lazy th))] -> loop (p * pcontrib) ans (th)
        | ch ->
            let (ptotal,th) = selector ch 
            loop (pcontrib * ptotal) ans [(1N,th)] 

    let rec driver ch ans = function
        | 0 -> let ns = BigRational.FromInt nsamples 
               let t1 = System.DateTime.Now
               printfn "rejection_sample: done %d worlds\nTime taken: %A seconds" nsamples (round 3 ((t1 - t0).TotalSeconds))
               Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans 
              
        | n -> driver ch (loop 1N ans ch) (n-1) 
    driver (reify0 ch) Map.empty nsamples


let sample_dist maxdepth (selector) (sample_runner) ch  =
    let look_ahead pcontrib (ans,acc) = function (* explore the branch a bit *)
    | (p,Value v) -> (insertWith(+) v (p * pcontrib) ans, acc)
    | (p,Continued (Lazy t)) -> 
        match (t)  with
        | [] -> (ans,acc)
        | [(p1,Value v)] -> 
           (insertWith (+) v (p * p1 * pcontrib) ans, acc)
        | ch ->
            let ptotal = List.fold (fun pa (p,_) -> pa + p) 0N ch 
            (ans,
              if ptotal < nearly_one then
                (p * ptotal, List.map (fun (p,x) -> (p / ptotal,x)) ch)::acc 
              else (p, ch)::acc)
        
    let rec loop depth pcontrib ans = function
    | [(p,Value v)]  -> insertWith (+) v (p * pcontrib) ans
    | []         -> ans
    | [(p,Continued (Lazy th))] -> loop (depth+1) (p * pcontrib) ans (th)
    | ch when depth < maxdepth -> (* choosing one thread randomly *)
    
        match List.fold (look_ahead pcontrib) (ans,[]) ch with
        | (ans,[]) -> ans
        | (ans,cch) ->
           let (ptotal,th) = selector cch 
           loop (depth+1) (pcontrib * ptotal) ans th  
     | _ -> ans
    
    let toploop pcontrib ans cch = (* cch are already pre-explored *)
        let (ptotal,th) = selector cch 
        loop 0 (pcontrib * ptotal) ans th 

    let driver pcontrib vals cch =
        let (ans,nsamples) = 
             sample_runner Map.empty (fun ans -> toploop pcontrib ans cch)  

        let ns = BigRational.FromInt nsamples  
        let ans = Map.fold (fun ans v p  -> insertWith (+) v (ns * p) ans) ans vals 

        printfn "sample_importance: done %d worlds\n" nsamples;
        Map.fold (fun a v p -> (p / ns,Value v)::a) [] ans   
    
    let rec make_threads depth pcontrib ans ch =  (* pre-explore initial threads *)
        match List.fold (look_ahead pcontrib) (ans,[]) ch with
        | (ans,[]) -> (* pre-exploration solved the problem *)
          Map.fold (fun a v p -> (p,Value v)::a) [] ans
        | (ans,[(p,ch)]) when depth < maxdepth -> (* only one choice, make more *)
           make_threads (depth+1) (pcontrib * p) ans ch
          (* List.rev is for literal compatibility with an earlier version *)
        | (ans,cch) -> driver pcontrib ans (List.rev cch) 

    make_threads 0 1N Map.empty ch 

(* A selector from a list of choices relying on the non-determinism
   supported by the parent reifier.
*)
let dist_selector ch =
  let ptotal = List.fold (fun pa (p,_) -> pa + p) 0.0 ch in
  (ptotal, distribution (List.map (fun (p,v) -> (p / ptotal, v)) ch))
                                        
let sample_importance selector maxdpeth nsamples (thunk)  =
  let rec loop th z = function 0 -> (z,nsamples) | n -> loop th (th z) (n-1)
  sample_dist maxdpeth 
    selector 
    (fun z th -> loop th z nsamples)
    (shallow_explore 3 (reify0 thunk))
                                 

let observe test = cont { if not test then return! fail() }

let inline exact_reify model   =  explore None     (reify0 model)  
let inline limit_reify n model =  explore (Some n) (reify0 model)  
                 

//=-=-=-=-=-=-=-=-=-=
module Distributions =                

  let bernoulli p = distribution [(p, true); (1N-p, false)]

  let bernoulliChoice p (a,b) = distribution [(p, a); (1N-p, b)]
                                                          
  let uniform (items:'a list) = 
      let num = BigRational.FromInt items.Length
      distribution (List.map (fun item -> 1N/num, item) items)

  let categorical distr = distribution distr 

  //=-=-=-=-=-=-=-=-=-=

  (* Uniform choice from [0..(n-1)] *)
  let uniform_int = function
   | 1 -> exactly 0
   | n when n > 1 -> uniform [0..(n-1)]
   | _ -> failwith "uniform: non-positive count n"

  let uniform_int_range low high = cont {
      let! i = uniform_int (high - low + 1)
      return low + i } 

  ///stepsizing says how precise you want your discrete approximation to be. The larger it is,
  ///the larger the space that is being dealt with. And the slower sampling will be. And more memory will be used
  ///Ex: 10 = [0, 0.1..0.1..1] | 20 = [0, 0.05..0.05..1.0] | 100 = [0, 0.01..0.01..1.0]

  let uniform_float stepsizing = cont {
      let! u_ = uniform_int_range 0 stepsizing 
      return (float u_ / float stepsizing)
      }
 
  ///low high works this way 10...90 => 0.1..0.9 | 10..900 => 0.1..0.01..0.9
  ///1..950 => 0.01..0.01..0.95
  let uniform_float_range low high = cont {
      let! x = uniform_int_range low high
      let r = ceil(log10 (float high))
      return (float x / 10. ** r) } 
    

  ///stepsizing says how precise you want your discrete approximation to be. The larger it is,
  ///the larger the space that is being dealt with. And the slower sampling will be. And more memory will be used
  ///Ex: 10 = [0, 0.1..0.1..1] | 20 = [0, 0.05..0.05..1.0] | 100 = [0, 0.01..0.01..1.0]
  let normal mean scale stepsizing roundto = cont {
      let! u = uniform_float_range 1 stepsizing 
      let! v = uniform_float_range 1 stepsizing 
      let z = sqrt (-2. * log u)  * cos(2. * pi * v)
      return round roundto (mean + z * scale)
  }

  ///stepsizing says how precise you want your discrete approximation to be. The larger it is,
  ///the larger the space that is being dealt with. And the slower sampling will be. And more memory will be used
  ///Ex: 10 = [0, 0.1..0.1..1] | 20 = [0, 0.05..0.05..1.0] | 100 = [0, 0.01..0.01..1.0]
  ///mu scale correspond to what would be the mean , stdev of normal distribution that you get by taking the log of this functions output
  let lognormal mu scale stepsizing roundto = cont {
      let! u = uniform_float_range 1 stepsizing 
      let! v = uniform_float_range 1 stepsizing 
      let z = sqrt (-2. * log u)  * cos(2. * pi * v)
      return round roundto (exp(mu + z * scale))
  }

  ///upper suggsted < 9999 to avoid a sudden jump in the distribution at 1,
  ///see uniform_float_range for how upper is transformed.
  let poisson lambda upper = 
      let p = exp -lambda
      let rec loop u s p x =
          if u <= s then x
          else let x' = x + 1.
               let p' = p * lambda/x'
               loop u (s+p') p' x'
      cont {
         let! u = uniform_float_range 0 upper 
         return (loop u p p 0.)
      }  

  let rec geometric n p = cont {
    let! a = bernoulli p
    if a then return n else return! (geometric(n+1) p)
  }

  ///polya's urn
  let rec beta draws a b = cont {
      if draws <= 0 then return a/(a+b)
      else let! ball = categorical [a/(a+b),1;b/(a+b),2]
           if ball = 1 then return! beta (draws - 1) (a+1N) b
           else return! beta (draws-1) a (b+1N)
  }


  let rec dirichlet3 draws a b c = cont {
      if draws <= 0 then return [|a/(a+b+c);b/(a+b+c);c/(a+b+c)|]
      else let! ball = categorical [a/(a+b+c),1;b/(a+b+c),2;c/(a+b+c),3]
           if ball = 1 then return! dirichlet3 (draws - 1) (a+1N) b c
           elif ball = 2 then return! dirichlet3 (draws - 1) a (b+1N) c
           else return! dirichlet3 (draws-1) a b (c+1N)
  }

  let rec dirichlet draws d = cont {
      let z = List.sum d
      if draws <= 0 then 
         return (List.map (fun a -> (a/z)) d)
      else          
           let ps = List.mapi (fun i a -> (a/z), i) d
           let! ball = categorical ps
           let d' = List.mapi (fun i a -> if i = ball then a + 1N else a) d
           return! dirichlet (draws - 1) d'         
  }


  let rec drawrandom draws n pd = cont {
      if n = 0 then return draws
      else let! fresh = pd
           return! drawrandom (fresh::draws) (n-1) pd
  }     
