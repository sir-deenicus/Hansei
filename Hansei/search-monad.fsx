#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx"
#time "on"
open Prelude.Math
open Prelude.Common
open System
open MathNet.Symbolics.Utils  
open Hansei.FSharpx.Collections 
open Hansei.Backtracking
open Hansei.TreeSearch

open Hansei.TreeSearch.LazyList

(** 
parent(sarah, john).
parent(arnold, john).
parent(john, anne).

grandparent(Person, Grandchild) :- parent(Person, X), parent(X, Grandchild).

grandparent(sarah, anne).
grandparent(arnold, anne).*)

let parentChildPairs =  choices ["Sarah", "John"; "Arnold", "John"; "John", "Anne"]

search {
    let! parent, child = parentChildPairs
    let! parentOfGrandchild, grandchild = parentChildPairs
    do! guard (child = parentOfGrandchild && grandchild = "Anne")
    return parent
}
|> Hansei.Backtracking.run None 
|> LazyList.toArray 

[ for parent, child in [ "Sarah", "John"; "Arnold", "John"; "John", "Anne" ] do
      for parentOfGrandchild, grandchild in [ "Sarah", "John"; "Arnold", "John"; "John", "Anne" ] do
          if child = parentOfGrandchild && grandchild = "Anne" then
              yield parent ]

let letters = List.removeDuplicates (List.ofSeq "SENDMOREMONEY")

let digits = choices [0..9]

let eachunique l = List.length l = List.length (List.distinct l)

search {
    let! m = choices [1..9] 
    let! s = digits
    do! guard (eachunique [ s; m ])
    let! e = digits
    do! guard (eachunique [ s; e; m]) 
    let! n = digits
    do! guard (eachunique [ s; e; n; m ])
    let! d = digits
    do! guard (eachunique [ s; e; n; d; m])
    let! o = digits
    do! guard (eachunique [ s; e; n; d; m; o ])
    let! r = digits
    do! guard (eachunique [ s; e; n; d; m; o; r ])
    let! y = digits
    do! guard (eachunique [ s; e; n; d; m; o; r; y ])

    do!
        guard ( 
            s * 1000 + e * 100 + n * 10 + d +
            m * 1000 + o * 100 + r * 10 + e =
                m * 10_000 + o * 1000 + n * 100 + e * 10 + y
        )

    return [ s; e; n; d; m; o; r; y ]
}  
|> Hansei.Backtracking.run None
|> LazyList.toList 
|> List.map (List.zip letters)


search {
    let m = 1
    let! s = choices [ 2 .. 9 ]
    let! e = digits
    do! guard (eachunique [ s; e; m ]) 
    let! d = digits
    do! guard (d + e >= 10)  
    do! guard (eachunique [ s; e; d; m ])     
    let! n = digits
    do! guard (eachunique [ s; e; n; d; m ]) 
    let! r = digits
    do! guard (n + r + 1 >= 10)  
    do! guard (eachunique [ s; e; n; d; m; r ]) 
    let! o = digits
    do! guard (eachunique [ s; e; n; d; m; o; r ]) 
    let! y = digits
    do! guard (eachunique [ s; e; n; d; m; o; r; y ]) 

    do!
        guard (
            s * 1000 + e * 100 + n * 10 + d +
            m * 1000 + o * 100 + r * 10 + e =
                m * 10_000 + o * 1000 + n * 100 + e * 10 + y
        )

    return [ s; e; n; d; m; o; r; y ]
}  
//|> Hansei.Backtracking.run None
|> LazyList.toArray 

search {
    let m = 1
    let! s = choices [ 2 .. 9 ]
    let! e = digits
    do! guard (eachunique [ s; e; m ]) 
    let! d = choices [ 10 - e .. 9 ] // Constraint 3: D + E must be 10 or more
    do! guard (eachunique [ s; e; d; m ])   
    let! n = digits
    do! guard (eachunique [ s; e; n; d; m ])
    let! r =  choices [ 10 - (n + 1) .. 9 ] // Constraint 4: R must be such that N + R + 1 is 10 or more
    do! guard (eachunique [ s; e; n; d; m; r ]) 
    let! o = digits
    do! guard (eachunique [ s; e; n; d; m; o; r ]) 
    let! y = digits
    do! guard (eachunique [ s; e; n; d; m; o; r; y ]) 

    do!
        guard (
            s * 1000 + e * 100 + n * 10 + d +
            m * 1000 + o * 100 + r * 10 + e =
                m * 10_000 + o * 1000 + n * 100 + e * 10 + y
        )

    return [ s; e; n; d; m; o; r; y ]
}  
|> Hansei.Backtracking.run None
|> LazyList.toArray 

let solve() = 
    let lettersKey = [|'M'; 'S'; 'E'; 'D'; 'N'; 'R'; 'O'; 'Y'|]
    let ls = dict (Array.map swap (Array.indexed lettersKey))
    let rec iter depth (state:_ list) = search {
        match depth with
        | 8 -> 
            let vs = List.rev state
            do! guard (
                vs[ls['S']] * 1000 + vs[ls['E']] * 100 + vs[ls['N']] * 10 + vs[ls['D']] +
                vs[ls['M']] * 1000 + vs[ls['O']] * 100 + vs[ls['R']] * 10 + vs[ls['E']] =
                    vs[ls['M']] * 10_000 + vs[ls['O']] * 1000 + vs[ls['N']] * 100 + vs[ls['E']] * 10 + vs[ls['Y']]
            )
            return (Seq.zip lettersKey vs |> Seq.toList)
        | _ -> 
            let! nextDigit = 
                match depth with
                | 1 -> choices [ 2 .. 9 ]
                | 3 -> choices [ 10 - state[0] .. 9 ]
                | 5 -> choices [ 10 - (state[0] + 1) .. 9 ]
                | _ -> digits
            let state' = nextDigit :: state
            do! guard (eachunique state')
            return! iter (depth + 1) state'
    }
    iter 1 [1]

solve()
|> Hansei.Backtracking.run None
|> LazyList.toArray 

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


let excluded pieces rs = 
    List.filter (flip Set.contains pieces >> not) rs

let diagonalTest n dr dc r c pieces =
    let rec loop r c =
        if r < 0 || r >= n || c < 0 || c >= n then true 
        elif List.exists ((=) (r,c)) pieces then false 
        else loop (r+dr) (c+dc)
    loop r c

let diagonalTests n r c pieces = 
    diagonalTest n 1 -1 r c pieces 
    && diagonalTest n -1  1 r c pieces
    && diagonalTest n  1  1 r c pieces
    && diagonalTest n -1 -1 r c pieces

let locs = choices [0..10]

let nqueens cnt n = 
    let seen = Hashset() 
    //let mutable cnt = 0
    let rec loop pieces rows cols =
        search {
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

let drawBoard positions n =
    //let board = Array.init n (fun i -> Array.init n (fun j -> if (i + j) % 2 = 0 then " □ " else " ■ "))
    let board =
        [| for i in 0..n-1 do
            [| for j in 0..n-1 do
                if (i + j) % 2 = 0 then " □ " else " ■ " |] |]
    for (r, c) in positions do
        board.[r].[c] <- " ♛"
    [|for row in board do
        String.Concat row|]
    |> String.concat "\n"

let drawBoard positions n =
    let board = Array.init n (fun _ -> Array.create n " . ")
    for (r, c) in positions do
        board.[r].[c] <- " ♛"
    for row in board do
        printfn "%s" (String.Concat row)


open Hansei
    
let positions, _ = nqueens 1 11
//let positions = Backtracking.run (Some 128) positions  
drawBoard (Seq.head positions) 11
|> printfn "%s"

let nqueens2 cnt n = 
    let seen = Hashset() 
    //let mutable cnt = 0
    let rec loop pieces rows cols =
        search { 
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
let f1, r1 = nqueens2 1 7
f1 |> Seq.take 1
r1
f1 |> run 50 |> Seq.truncate 1 |> Seq.toArray
//let f1 = f1 |> LazyList.removeDuplicates |> Seq.truncate 5 |> Seq.toArray

r1
r1.Count
f1
//Seq.length f1



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