module Hansei.Backtracking

//The backtracking monad or fairstream concept is also Oleg's, copied from here: http://fortysix-and-two.blogspot.com/2009/09/simple-and-fair-backtracking.html
//and shares the core essence of the probability monad of this library. The backtracking monad also implements
//the logic monad interface and so also captures the core of logic programming.

type LazyStream<'a> = 
  | Nil                               //empty
  | One of 'a                         //one element
  | Choice of 'a * LazyStream<'a>         //one element, and maybe more
  | Incomplete of Lazy<LazyStream<'a>>    //suspended stream

let rec choice r r' = 
  match r with
  | Nil -> r' //first empty-> try the second
  | One a -> Choice (a,r') //Put in front of the new stream
  | Choice (a,rs) -> Choice (a,choice r' rs) //interleave r and r' here
  | Incomplete i ->
      match r' with
      | Nil -> r
      | One b -> Choice (b,r)
      | Choice (b,rs') -> Choice (b,choice r rs') 
      | Incomplete j -> Incomplete (lazy (choice i.Value j.Value))

let rec bind m f =
  match m with
      | Nil -> Nil
      | One a -> (f a)
      | Choice (a,r) -> choice (f a) (Incomplete (lazy bind r f))
      | Incomplete i -> Incomplete (lazy bind i.Value f)

type FairStream() =
  member __.Return a = One a
  member __.ReturnFrom(x) = x
  member __.Yield a = One a
  member __.Bind (m, f) = bind m f
  member __.Zero() = Nil
  member __.Combine (r,r') = choice r r'
  member __.Delay (f:unit->LazyStream<_>) = Incomplete (Lazy.Create f)
   
let bt = FairStream()

let rec run d st = 
    match (d,st) with 
    | _,Nil -> Seq.empty
    | _,One a -> seq { yield a }
    | _,Choice (a,r) -> seq { yield a; yield! run d r }
    | Some 0,Incomplete _ -> Seq.empty //exhausted depth
    | d,Incomplete (Lazy r) -> let d' = Option.map ((-) 1) d in run d' r

let guard assertion = 
    bt { if assertion then return () }

