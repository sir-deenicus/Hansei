module Hansei.LogSpace

open Prelude.Common
open Prelude.Math
open System             
open Hansei.Continuation
open Hansei.Utils

//===========
let log_nearly_one = log(1.0 - 1e-7);

let log_add x y = let x',y' = max x y, min x y in x' + log (1. + exp(y'-x'))

let log_sum = function 
     | [] -> log 0.
     | [p,_] -> p
     | (p0,_)::ptail -> List.fold (fun acc (logp,_) -> log_add acc logp) p0 ptail


let inline log_normalize (choices) =
  let sum = log_sum choices
  List.map (fun (p, v) -> (p - sum, v)) choices


let inline exponentiate (choices) = List.map (keepRight exp) choices


// compute log(1+x) without losing precision for small values of x https://www.johndcook.com/blog/csharp_log_one_plus_x/
let logOnePlusX x = 
    if x <= -1.0 then failwith ("Invalid input argument: " + (string x))

    if abs x > 1e-4 then log(1.0 + x) // x is large enough that the obvious evaluation is OK       
    // Use Taylor approx. log(1 + x) = x - x^2/2 with error roughly x^3/3
    // Since |x| < 10^-4, |x|^3 < 10^-12, relative error less than 10^-8        
    else (-0.5*x + 1.0)*x   
                                          
//==========

let distribution ch k = List.map (fun (p,v) -> (log p, Continued(lazy(k v)))) ch

let fail () = distribution []

let observe test = cont { if not test then return! fail() }

let reify0 m = m (fun x -> [(0., Value x)])

let exactly x = distribution [0., x]
  

let explore (maxdepth : int option) (choices : ProbabilitySpace<'T>) =
  let rec loop p depth down susp answers =
    match (down, susp, answers) with
    | (_, [], answers) -> answers 
 
    | (_, (pt, Value v) :: rest, (ans, susp)) ->
      loop p depth down rest (insertWith log_add v (pt+p) ans, susp)
 
    | (true, (pt,Continued(Lazy t))::rest, answers) ->
      let down' = match maxdepth with Some x -> depth < x | None -> true
      loop p depth true rest <| loop (pt+p) (depth+1) down' t answers
 
    | (down, (pt,c)::rest, (ans,susp)) ->
      loop p depth down rest (ans, (pt+p,c)::susp)

  let (ans, susp) = loop 0. 0 true choices (Map.empty, [])
  Map.fold (fun a v p -> (p, Value v)::a) susp ans : ListofContinuationTrees<'T>


(* Explore but do not flatten the tree: 
   perform exact inference to the given depth
   We still pick out all the produced answers and note the failures. *)
let shallow_explore maxdepth (choices ) =
    let add_answer pcontrib v mp = insertWith (+) v pcontrib mp 
    let rec loop pc depth ans acc = function
    | [] -> (ans,acc)
    | (p,Value v)::rest -> loop pc depth (add_answer (p + pc) v ans) acc rest
    | c::rest when depth >= maxdepth -> loop pc depth ans (c::acc) rest
    | (p,Continued(Lazy t))::rest -> 
      let (ans,ch) = loop (pc + p) (depth + 1) ans [] (t) 
      let ptotal = log_sum ch 
      let acc =
        if exp ptotal = 0.0 then acc
        else if ptotal < log_nearly_one then
             (p + ptotal, 
              let ch = List.map (fun (p,x) -> (p - ptotal,x)) ch          
              Continued (lazy ch))::acc
            else (p, Continued (lazy ch))::acc 
      loop pc depth ans acc rest
    
    let (ans,susp) = loop 0. 0 Map.empty [] choices
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
        let pcum = log_add pcum p  
        if r < pcum then (ptotal,th)
        else selection r ptotal pcum rest

  fun choices ->
      let ptotal = log_sum choices  
      let r = random.NextDouble (0., exp ptotal)      (* 0<=r<ptotal *)
      selection (log r) ptotal (log 0.0) choices

let rejection_sample_dist selector nsamples ch : 'a ProbabilitySpace =
    let timer = Diagnostics.Stopwatch()
    timer.Start()
    let rec loop pcontrib ans = function
        | [(p,Value v)]  -> insertWith log_add v (p + pcontrib) ans
        | []         -> ans
        | [(p,Continued (Lazy th))] -> loop (p + pcontrib) ans th
        | ch ->
            let (ptotal,th) = selector ch 
            loop (pcontrib + ptotal) ans [(0.,th)] 

    let rec driver ch ans = function
        | 0 -> let ns = log(float nsamples)
               printf "rejection_sample: done %d worlds\nTime taken: %A" nsamples (timer.Elapsed)
               Map.fold (fun a v p -> (p - ns,Value v)::a) [] ans 
              
        | n -> driver ch (loop 0. ans ch) (n-1) 
    driver (reify0 ch) Map.empty nsamples


let sample_dist maxdepth (selector) (sample_runner) ch  =
    let look_ahead pcontrib (ans,acc) = function (* explore the branch a bit *)
    | (p,Value v) -> (insertWith log_add v (p + pcontrib) ans, acc)
    | (p,Continued (Lazy t)) -> 
        match t  with
        | [] -> (ans,acc)
        | [(p1,Value v)] -> 
           (insertWith log_add v (p + p1 + pcontrib) ans, acc)
        | ch ->
            let ptotal = log_sum ch 
            (ans,
              if ptotal < log_nearly_one then
                (p + ptotal, List.map (fun (p,x) -> (p - ptotal,x)) ch)::acc 
              else (p, ch)::acc)
        
    let rec loop depth pcontrib ans = function
    | [(p,Value v)]  -> insertWith log_add v (p + pcontrib) ans
    | []         -> ans
    | [(p,Continued (Lazy th))] -> loop (depth+1) (p + pcontrib) ans (th)
    | ch when depth < maxdepth -> (* choosing one thread randomly *)
    
        match List.fold (look_ahead pcontrib) (ans,[]) ch with
        | (ans,[]) -> ans
        | (ans,cch) ->
           let (ptotal,th) = selector cch 
           loop (depth+1) (pcontrib + ptotal) ans th  
     | _ -> ans
    
    let toploop pcontrib ans cch = (* cch are already pre-explored *)
        let (ptotal,th) = selector cch 
        loop 0 (pcontrib + ptotal) ans th 

    let driver pcontrib vals cch =
        let (ans,nsamples) = 
             sample_runner Map.empty (fun ans -> toploop pcontrib ans cch)  

        let ns = log (float nsamples)
        let ans = Map.fold (fun ans v p  -> insertWith (log_add) v (ns + p) ans) ans vals 

        printf "sample_importance: done %d worlds\n" nsamples;
        Map.fold (fun a v p -> (p - ns,Value v)::a) [] ans   
    
    let rec make_threads depth pcontrib ans ch =  (* pre-explore initial threads *)
        match List.fold (look_ahead pcontrib) (ans,[]) ch with
        | (ans,[]) -> (* pre-exploration solved the problem *)
          Map.fold (fun a v p -> (p,Value v)::a) [] ans
        | (ans,[(p,ch)]) when depth < maxdepth -> (* only one choice, make more *)
           make_threads (depth+1) (pcontrib + p) ans ch
          (* List.rev is for literal compatibility with an earlier version *)
        | (ans,cch) -> driver pcontrib ans (List.rev cch) 

    make_threads 0 0. Map.empty ch 

(* A selector from a list of choices relying on the non-determinism
   supported by the parent reifier.
*)
let dist_selector ch =
  let ptotal = log_sum ch in
  (ptotal, distribution (List.map (fun (p,v) -> (p - ptotal, v)) ch))
                                        
let sample_importance selector maxdpeth nsamples (thunk)  =
  let rec loop th z = function 0 -> (z,nsamples) | n -> loop th (th z) (n-1)
  sample_dist maxdpeth 
    selector 
    (fun z th -> loop th z nsamples)
    (shallow_explore 3 (reify0 thunk))

/////////Helpers

let inline exact_reify model   =  explore None     (reify0 model)  
let inline limit_reify n model =  explore (Some n) (reify0 model)  

//=-=-=-=-=-=-=-=-=-=
module Distributions = 
  let bernoulli p = distribution [(log p, true); (log (1.0-p), false)]

  let bernoulliC p (a,b) = distribution [(log p, a); (log (1.0-p), b)]
                                                          
  let uniform (items:'a list) = 
      let num = float items.Length
      distribution (List.map (fun item -> log (1./num), item) items)

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
  let rec beta roundto draws a b = cont {
      if draws <= 0 then return (round roundto (log(a/(a+b))))
      else let! ball = categorical [log (a/(a+b)),1;log(b/(a+b)),2]
           if ball = 1 then return! beta roundto (draws - 1) (a+1.) b
           else return! beta roundto (draws-1) a (b+1.)
  }


  let rec dirichlet3 roundto draws a b c = cont {
      if draws <= 0 then return (Array.map (log >> round roundto) [|a/(a+b+c);b/(a+b+c);c/(a+b+c)|])
      else let! ball = categorical [log(a/(a+b+c)) ,1;log(b/(a+b+c)),2;log(c/(a+b+c)),3]
           if ball = 1 then return! dirichlet3 roundto (draws - 1) (a+1.) b c
           elif ball = 2 then return! dirichlet3 roundto (draws - 1) a (b+1.) c
           else return! dirichlet3 roundto (draws-1) a b (c+1.)
  }

  let rec dirichlet roundto draws d = cont {
      let z = List.sum d
      if draws <= 0 then 
         return (List.map (fun a -> round roundto (log (a/z))) d)
      else          
           let ps = List.mapi (fun i a -> log (a/z), i) d
           let! ball = categorical ps
           let d' = List.mapi (fun i a -> if i = ball then a + 1. else a) d
           return! dirichlet roundto (draws - 1) d'         
  }


  let rec drawrandom draws n pd = cont {
      if n = 0 then return draws
      else let! fresh = pd
           return! drawrandom (fresh::draws) (n-1) pd
  }     

  let discretizedSampler coarsener sampler (n:int) = cont {
      return! categorical ([|for _ in 1..n -> sampler ()|] |> (coarsenWith coarsener >> List.map (keepRight log)))
  }
   

