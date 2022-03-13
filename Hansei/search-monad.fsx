#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx"
#time "on"
open Prelude.Common
open Prelude.Math
open System
open MathNet.Symbolics.Utils 
open Hansei.FSharpx.Collections.LazyList.ComputationExpression 
open Hansei.FSharpx.Collections 
open Hansei.Backtracking

(** 
parent(sarah, john).
parent(arnold, john).
parent(john, anne).

grandparent(Person, Grandchild) :- parent(Person, X), parent(X, Grandchild).

grandparent(sarah, anne).
grandparent(arnold, anne).

 parents =  LazyList.ofList ["Sarah", "John"; "Arnold", "John"; "John", "Anne"]

lzlist {
    let! p, c = parents
    let! c', g = parents
    do! guard (c = c' && g = "Anne")
    return p
}
|> LazyList.take 2
|> LazyList.toArray 

[ for p, c in parents do
      for c', g in parents do
          if c = c' && g = "Anne" then yield p ]*)

let letters = List.removeDuplicates (List.ofSeq "SENDMOREMONEY")

//let digits = choices [0..9]
let digits = LazyList.ofList [0..9]
//let choices = LazyList.ofList 
        
//lzlist {
//    let m = 1
//    let! s = choices [ 2 .. 9 ]
//    let! e = digits
//    do! guard (Hashset([ s; e; m ]).Count = 3) 
//    let! n = digits
//    do! guard (Hashset([ s; e; n; m ]).Count = 4)
//    let! d = digits
//    do! guard (Hashset([ s; e; n; d; m ]).Count = 5)
//    let! o = digits
//    do! guard (Hashset([ s; e; n; d; m; o ]).Count = 6)
//    let! r = digits
//    do! guard (Hashset([ s; e; n; d; m; o; r ]).Count = 7)
//    let! y = digits
//    do! guard (Hashset([ s; e; n; d; m; o; r; y ]).Count = 8)

//    do!
//        guard (
//            s * 1000 + e * 100 + n * 10 + d +
//            m * 1000 + o * 100 + r * 10 + e =
//                m * 10_000 + o * 1000 + n * 100 + e * 10 + y
//        )

//    return [ s; e; n; d; m; o; r; y ]
//}  |> Seq.toArray 

//let rec assign l xs (seen:Set<_>) = lzlist {
//        match l with
//        | [] -> 
//            let l = dict xs 
//            do!
//                guard (
//                    l.['S'] * 1000 + l.['E'] * 100 + l.['N'] * 10 + l.['D'] +
//                    l.['M'] * 1000 + l.['O'] * 100 + l.['R'] * 10 + l.['E'] =
//                        l.['M'] * 10_000 + l.['O'] * 1000 + l.['N'] * 100 + l.['E'] * 10 + l.['Y']
//                )
//            return l
//        | h :: t ->
//            let! d = digits
//            do! guard (not (seen.Contains d))
//            do! guard (d > 0 || (h <> 'S' && h <> 'M')) 
//            return! assign t ((h, d) :: xs) (seen.Add d)
//    }
       
//let l = assign letters [] Set.empty |> Seq.head 

//l.['S'] * 1000 + l.['E'] * 100 + l.['N'] * 10 + l.['D']
//l.['M'] * 1000 + l.['O'] * 100 + l.['R'] * 10 + l.['E'] 
//l.['M'] * 10_000 + l.['O'] * 1000 + l.['N'] * 100 + l.['E'] * 10 + l.['Y']

//Seq.toArray l 


let rowColTest f pieces r =
    pieces |> List.exists (f >> (=) r) |> not

let rowTest pieces r = rowColTest fst pieces r 

let colTest pieces r = rowColTest snd pieces r 
 
let diagonalTest0 n dr dc r c (rows:Set<_>) (cols:Set<_>) =
    let rec loop r c =
        if r < 0 || r >= n then true 
        elif rows.Contains r && cols.Contains c then false 
        else loop (r+dr) (c+dc)
    loop r c

let diagonalTest n dr dc r c pieces =
    let rec loop r c =
        if r < 0 || r >= n || c < 0 || c >= n then true 
        elif List.exists ((=) (r,c)) pieces then false 
        else loop (r+dr) (c+dc)
    loop r c


module Seq = 
    let nth n s = s |> Seq.skip n |> Seq.head
 
let excluded pieces rs = 
    List.filter (flip Set.contains pieces >> not) rs

let diagonalTests n r c pieces = 
    diagonalTest n 1 -1 r c pieces 
    && diagonalTest n -1  1 r c pieces
    && diagonalTest n  1  1 r c pieces
    && diagonalTest n -1 -1 r c pieces

let locs = choices [0..7]

let nqueens cnt n = 
    let seen = Hashset() 
    //let mutable cnt = 0
    let rec loop pieces rows cols =
        bt {
            let! r = locs 
            do! guard (not (Set.contains r rows))
            let! c = locs
            do! guard (not (Set.contains c cols))
            do! guard (diagonalTests n r c pieces) 
            if List.length pieces + 1 = n then   
                seen.Add((r,c)::pieces) |> ignore 
                //cnt <- cnt + 1
                printfn "%A" seen.Count
                return List.sort ((r,c)::pieces)//, pieces, (r,c)
            else 
                if seen.Count < cnt then
                    return! loop ((r,c)::pieces) (Set.add r rows) (Set.add c cols)
        }
    loop [] Set.empty Set.empty, seen

let nqueens2 cnt n = 
    let seen = Hashset() 
    //let mutable cnt = 0
    let rec loop pieces rows cols =
        bt { 
            let! r = choices (excluded rows [0..n-1]) 
            let! c = choices (excluded cols [0..n-1])  
            do! guard (diagonalTests n r c pieces) 
            if List.length pieces + 1 = n then   
                seen.Add((r,c)::pieces) |> ignore 
                //cnt <- cnt + 1
                printfn "%A" seen.Count
                return List.sort ((r,c)::pieces)//, pieces, (r,c)
            else 
                if seen.Count < cnt then
                    return! loop ((r,c)::pieces) (Set.add r rows) (Set.add c cols) 

        }
    loop [] Set.empty Set.empty, seen

//Lazylist: 245 ms for nqueens2; 500 ms for nqueens

//let r1 = nqueens2 4 |> LazyList.removeDuplicates |> Seq.truncate 5
let f1, r1 = nqueens2 1 10
f1 |> run 50 |> Seq.truncate 1 |> Seq.toArray
//let f1 = f1 |> LazyList.removeDuplicates |> Seq.truncate 5 |> Seq.toArray

r1
r1.Count
f1
//Seq.length f1


let n = 10
let grid = Array2D.create n n ' '
 
for r in 0..n-1 do for c in 0..n-1 do grid.[r,c] <- ' '
for (r,c) in Seq.nth 0 r1  do
    grid.[r,c] <- '♕'
//grid
for r in 0..n-1 do    
    for c in 0..(n-1) do 
        if grid.[r,c] = ' ' then printf "|____|" 
        else printf "|%A|" (grid.[r,c])
    printfn ""


//////////////////////////////////////////

//Uses list
module ListTreeSearch = 
    type SearchSpace<'T> = list<WeightedTree<'T>>
    and WeightedTree<'T> = //Need a tree to do search right
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choices ch = List.map (fun v -> Continued(lazy([Value v]))) ch : SearchSpace<_> 
    let fail () = choices []

    let reflect tree k =  
        let rec make_choices pv = 
            List.map (function 
            | Value x -> Continued(lazy(k x))
            | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv
            
        make_choices tree  : SearchSpace<_> 
    
    let exactly x = choices [x]  

    type SearchBuilder() =
        member inline d.Bind(space, f) = reflect space f
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly () 

    let search = SearchBuilder()
    let explore (maxdepth: int option) (choices: SearchSpace<'T>) =
        let rec loop depth down susp answers =
            match (down, susp, answers) with
            | (_, [], answers) -> answers
            | (_, (Value v) :: rest, (ans:ResizeArray<_>, susp)) -> 
                loop depth down rest (ans.Add v; ans, susp) 
            | (true, (Continued (Lazy t)) :: rest, answers) ->
                let down' =
                    match maxdepth with
                    | Some x -> depth < x
                    | None -> true
                loop depth true rest
                <| loop (depth + 1) down' t answers
    
            | (down, c :: rest, (ans, susp)) -> loop depth down rest (ans, c :: susp)
     
        let (ans, susp) = loop 0 true choices (ResizeArray(), [])
     
        [ yield! susp
          for v in ans -> Value v ]: SearchSpace<_>   

    let rec simpleTest i = search { 
        let! a = choices [true;false]
        if a then return i
        else return! (simpleTest (i+1))
    } 
    
    simpleTest 0 
    |> explore (Some 14)  
    |> List.filter (function Value _ -> true | _ -> false)

open Hansei.FSharpx.Collections

//uses lazy list
module TTree2 = 
    type SearchSpace<'T> = LazyList<WeightedTree<'T>>
    and WeightedTree<'T> = 
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    //if use value instead of Continued, infinite computations will fail to return/terminate
    let choicesLzy ch = LazyList.map (fun v -> Continued(lazy(LazyList.ofList [Value v]))) ch : SearchSpace<_> 
    //let choicesLzy ch = LazyList.map Value ch : SearchSpace<_> 
    let choices ch = choicesLzy (LazyList.ofList ch) : SearchSpace<_> 
    let exactly x = choicesLzy (LazyList.singleton x) : SearchSpace<_> 
    let fail () = LazyList.empty : SearchSpace<_>  

//return! error
//    visualprobmonad.fsx(815,19): error FS0001: Type mismatch. Expecting a
//    'SearchSpace<'a>'    
//but given a
//    'LazyList<WeightedTree<SearchSpace<'a>>>'    
//The types ''a' and 'SearchSpace<'a>' cannot be unified.
    let reflect2 tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
              | Value x -> Value((k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 

    let reflect tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
            | Value x -> Continued(lazy(k x))
            | Continued(Lazy t) -> Continued(lazy(make_choices t))) pv 
        make_choices tree  : SearchSpace<_>  
    
    type SearchBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly ()  
        member __.Combine(x,y) = LazyList.choice x y
        member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
        member l.Yield x = l.Return x

    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, LazyList.Nil, answers) -> 
                answers
            | (_, LazyList.Cons(Value v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add v; ans, susp)  
            | (true, LazyList.Cons(Continued (Lazy t), rest), answers) ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true  
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc + 1) (depth + 1) down' (t) answers
    
            | (down, LazyList.Cons( c,rest), (ans, susp)) ->  
                loop (hloc+1) depth down rest (ans, ( c) :: susp)
               
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        LazyList.ofList
            [ yield! susp
              for v in ans -> Value v ]: SearchSpace<_> 

//Allow Recompute, is this really needed?        
module TTree5 = 
    type SearchSpace<'T> = LazyList<WeightedTree<'T>>
    and WeightedTree<'T> = 
        | Value of (unit -> 'T)
        | ComputedValue of 'T
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choicesLzy ch = LazyList.map (fun v -> Continued(lazy(LazyList.ofList [Value (fun () -> v)]))) ch : SearchSpace<_> 
    let choices ch = choicesLzy (LazyList.ofList ch) : SearchSpace<_> 
    let exactly x = choicesLzy (LazyList.singleton x) : SearchSpace<_>
    let fail () = LazyList.empty : SearchSpace<_>  
     
//return! error
//    visualprobmonad.fsx(815,19): error FS0001: Type mismatch. Expecting a
//    'SearchSpace<'a>'    
//but given a
//    'LazyList<WeightedTree<SearchSpace<'a>>>'    
//The types ''a' and 'SearchSpace<'a>' cannot be unified.
    let reflect2 tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
              | Value x -> Value((k x)) 
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 

    let reflect tree k =  
        let rec make_choices pv = 
            LazyList.map (function 
            | Value f -> Continued(lazy(k (f())))
            | ComputedValue x -> Continued(lazy(k x))
            | Continued(Lazy t) -> Continued(lazy(make_choices t))) pv 
        make_choices tree  : SearchSpace<_> 
    
    
    type SearchBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly (fun () -> ())
        member __.Combine(x,y) = LazyList.choice x y
        member __.Delay(f: unit -> LazyList<_>) = LazyList.delayed f 
        member l.Yield x = l.Return x

    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, LazyList.Nil, answers) -> 
                answers
            | (_, LazyList.Cons(ComputedValue v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add v; ans, susp)  
            | (_, LazyList.Cons(Value v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add (v()); ans, susp)  
            | (true, LazyList.Cons(Continued (Lazy t), rest), answers) ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true  
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc + 1) (depth + 1) down' (t) answers
    
            | (down, LazyList.Cons( c,rest), (ans, susp)) ->  
                loop (hloc+1) depth down rest (ans, ( c) :: susp)
               
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        LazyList.ofList
            [ yield! susp
              for v in ans -> ComputedValue v ]: SearchSpace<_>   

open TTree2 

//Using seq is too slow.
module TTree4 =
    //if take away continuation, lose flexibility in how easily can represent general
    type SearchSpace<'T> = seq<WeightedTree<'T>>
    and WeightedTree<'T> = 
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choices ch = Seq.map (fun v -> Continued(lazy(seq [Value v]))) ch : SearchSpace<_> 
    //let choices ch = Seq.map Value ch : SearchSpace<_> 
    let fail () = choices Seq.empty

//return! error
//    visualprobmonad.fsx(815,19): error FS0001: Type mismatch. Expecting a
//    'SearchSpace<'a>'    
//but given a
//    'LazyList<WeightedTree<SearchSpace<'a>>>'    
//The types ''a' and 'SearchSpace<'a>' cannot be unified.
    let reflect2 tree k =  
        let rec make_choices pv = 
            Seq.map (function 
              | Value x -> Value((k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 

    let reflect tree k =  
        let rec make_choices pv = 
            Seq.map (function 
            | Value x -> Continued(lazy(k x))
            | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv 
        make_choices tree  : SearchSpace<_> 
    
    let exactly x = choices (Seq.singleton x)
    
    type SearchBuilder() =
        member inline d.Bind(space, k) = reflect space k
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly ()  
        member __.Combine(x,y) = Seq.choice x y
        member __.Delay(f: unit -> seq<_>) = Seq.delay f 
        member l.Yield x = l.Return x

    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, Seq.Nil, answers) -> 
                answers
            | (_, Seq.Cons(Value v, rest), (ans:ResizeArray<_>, susp))   ->  
                loop (hloc+1) depth down rest (ans.Add v; ans, susp)  
            | (true, Seq.Cons(Continued (Lazy t), rest), answers) ->
                let down' =
                    Option.map (fun x -> depth < x) maxdepth 
                    |> Option.defaultValue true  
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc + 1) (depth + 1) down' (t) answers
    
            | (down, Seq.Cons( c,rest), (ans, susp)) ->  
                loop (hloc+1) depth down rest (ans, ( c) :: susp)
               
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        seq
            [ yield! susp
              for v in ans -> Value v ]: SearchSpace<_>   

//fiddleing Search algorithm
module TTree3 = 
    type SearchSpace<'T> = list<WeightedTree<'T>>
    and WeightedTree<'T> = //Need a tree to do search right
        | Value of 'T 
        | Continued of Lazy<SearchSpace<'T>>       
        
    let choices ch = List.map (fun v -> Continued(lazy([Value v]))) ch : SearchSpace<_> 
    let fail () = choices []

    let reflect tree k =  
        let rec make_choices pv = 
            List.map (function 
              | Value x -> Continued(lazy(k x))
              | Continued(Lazy x) -> Continued(lazy(make_choices x))) pv
            
        make_choices tree  : SearchSpace<_> 
    
    let exactly x = choices [x]  

    type SearchBuilder() =
        member inline d.Bind(space, f) = reflect space f
        member d.Return v = exactly v
        member d.ReturnFrom vs = vs
        member d.Zero () = exactly () 
        member __.Combine(x,y) = List.append x y
        member __.Delay(f: unit -> List<_>) =  f ()
        member l.Yield x = l.Return x
        
    let search = SearchBuilder()
    let explore (maxwidth : int option) (maxdepth: int option) (choices: SearchSpace<'T>) =
        let maxw = defaultArg maxwidth Int32.MaxValue
        let rec loop hloc depth down susp answers =
            match (down, susp, answers) with
            | (_, [], answers) -> answers
            | (_, (Value v) :: rest, (ans:ResizeArray<_>, susp)) -> 
                loop (hloc+1) depth down rest (ans.Add v; ans, susp) 
            | (true, (Continued (Lazy t)) :: rest, answers) ->
                let down' =
                    match maxdepth with
                    | Some x -> depth < x
                    | None -> true
                
                if hloc > maxw then answers 
                else 
                    loop (hloc+1) depth true rest
                    <| loop (hloc+1) (depth + 1) down' (t) answers
                //loop depth true rest
                //<| loop (depth + 1) down' (t) answers
    
            | (down, ( c) :: rest, (ans, susp)) -> loop (hloc+1) depth down rest (ans, ( c) :: susp)
    
        //let (ans, susp) = loop 1.0 0 true choices (Map.empty, [])
        let (ans, susp) = loop 0 0 true choices (ResizeArray(), [])
    
        //Map.fold (fun a v p -> (p, Value v)::a) susp ans : ProbabilitySpace<'T>
        [ yield! susp
          for v in ans -> Value v ]: SearchSpace<_>   


open TTree2

let rec ss i = search { 
    let! a = choices ([true;false])
    if a then return i
    else return! (ss (i+1))
} 

ss 0 |> explore None (Some 10) 
|> LazyList.takeOrMax 10  |> LazyList.toList

open TTree2
//Using list yields to Stack overflow here
let rec ss2 i = search { 
    yield i
    yield -i
    return! ss2 (i+1)
} 

let qz = ss2 0 

qz |> explore (Some 500) (Some 10)  
|> LazyList.takeOrMax 100
|> LazyList.toList

|> List.filter (function Value _ -> true | _ -> false)