module Hansei.SymbolicProb

open System
open Hansei.Utils
open Hansei.Continuation    
open MathNet.Symbolics
open MathNet.Symbolics.Extras 
open Prelude.Common
open MathNet.Numerics

type SymbolicProbabilitySpace<'T> = list<Expression * SymbolicWeightedTree<'T>>
and SymbolicWeightedTree<'T> = 
    | Value of 'T 
    | Continued of Lazy<SymbolicProbabilitySpace<'T>>    

let valueExtract = function Value x -> x

let valueExtract2 = function Value x -> Some x | _ -> None

let distribution ch k = List.map (fun (p:Expression,v) -> (p, Continued(lazy(k v)))) ch

let fail () = distribution []

let inline reify0 m = m (fun x -> [(1Q, Value x)])

let exactly x = distribution [1Q, x] 

let explore (maxdepth : int option) (choices : SymbolicProbabilitySpace<'T>) =
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
       
  let (ans,susp) = loop 1Q 0 true choices (Dict(), [])   
  [ yield! susp
    for (KeyValue(v,p)) in ans -> Algebraic.simplify true p, Value v] : SymbolicProbabilitySpace<_>

module Distributions =    
  let bernoulli p = distribution [(p, true); (1Q-p, false)]

  let bernoulliChoice p (a,b) = distribution [(p, a); (1Q-p, b)]
                                                          
  let uniform (items:'a list) = 
      let num = BigRational.FromInt items.Length |> Expression.FromRational
      distribution (List.map (fun item -> 1Q/num, item) items)

  let categorical distr = distribution distr 

  let rec geometric n p = cont {
    let! a = bernoulli p
    if a then return n else return! (geometric(n+1) p)
  }

  ///polya's urn
  let rec beta draws a b = cont {
      if draws <= 0 then return a/(a+b)
      else let! ball = categorical [a/(a+b),1;b/(a+b),2]
           if ball = 1 then return! beta (draws - 1) (a+1Q) b
           else return! beta (draws-1) a (b+1Q)
  }

let observe test = cont { if not test then return! fail() }

let inline exact_reify model   =  explore None     (reify0 model)  
let inline limit_reify n model =  explore (Some n) (reify0 model)   

type Model<'a,'b when 'b : comparison>(thunk:(('a -> SymbolicProbabilitySpace<'a>) -> SymbolicProbabilitySpace<'b>)) =   
     member __.model = thunk
     member __.Reify (?limit) = match limit with None -> exact_reify thunk | Some n -> limit_reify n thunk 

let inline normalize (choices:list<Expression * 'a>) =
  let sum = List.sumBy (fst) choices
  List.map (fun (p, v) -> (p/sum, v)) choices


 