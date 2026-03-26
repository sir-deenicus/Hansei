#load @"C:\users\cybernetic\jupyter-notebooks\maths-repl.fsx"
#time "on"
open Prelude.Math
open Prelude.Common
open System
open MathNet.Symbolics.Utils  
open Hansei.FSharpx.Collections 
open Hansei.Backtracking
open Hansei.TreeSearch

open Hansei.TreeSearch.Backtracking3

(*
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

//|> FairStream.take 1

//|> Hansei.Backtracking.run None
|> LazyList.toArray 

open Hansei.TreeSearch.Backtracking3
 
/// SEND + MORE = MONEY (naive search improved with incremental column constraints, no pre-fixed digits)
let sendMoreMoney_naiveBetter() =
    let inline isFree mask d = (mask &&& (1 <<< d)) = 0
    let inline add mask d = mask ||| (1 <<< d)
    search {
        // Leading letters (cannot be 0)
        let! m = choices [1..9]
        let used1 = add 0 m
        let! s = choices [1..9]      // allow s=1 too; constraint later may reject
        do! guard (isFree used1 s)
        let used2 = add used1 s

        // Pick E then D -> derive Y and carry C1 from units column: D + E = Y + 10*C1
        let! e = choices [0..9]
        do! guard (isFree used2 e)
        let used3 = add used2 e

        let! d = choices [0..9]
        do! guard (isFree used3 d)
        let sumDE = d + e
        let y = sumDE % 10
        let c1 = sumDE / 10
        do! guard (isFree used3 y && y <> d)         // y must be new (y ≠ d guaranteed if free)
        let used4 = add (add used3 d) y

        // Pick N and R -> tens column: N + R + C1 = E + 10*C2
        let! n = choices [0..9]
        do! guard (isFree used4 n)
        let used5 = add used4 n

        let! r = choices [0..9]
        do! guard (isFree used5 r)
        let used6 = add used5 r

        let tensDiff = n + r + c1 - e
        do! guard (tensDiff = 0 || tensDiff = 10)
        let c2 = if tensDiff = 10 then 1 else 0

        // Pick O -> hundreds: E + O + C2 = N + 10*C3
        let! o = choices [0..9]
        do! guard (isFree used6 o)
        let used7 = add used6 o

        let hundDiff = e + o + c2 - n
        do! guard (hundDiff = 0 || hundDiff = 10)
        let c3 = if hundDiff = 10 then 1 else 0

        // Thousands: S + M + C3 = O + 10*C4  and Ten-thousands: C4 = M
        // => S + M + C3 = O + 10*M  -> rearrange: S + C3 = O + 9*M
        // Instead of algebraically solving, enforce directly:
        let lhs = s + m + c3
        let c4 = (lhs - o) / 10
        do! guard ((lhs - o) % 10 = 0)
        do! guard (c4 = m)          // ensures MONEY’s first digit = M

        // Final full equation check (safety)
        do! guard (
            (s*1000 + e*100 + n*10 + d) +
            (m*1000 + o*100 + r*10 + e) =
              m*10000 + o*1000 + n*100 + e*10 + y
        )

        return [ ('S',s); ('E',e); ('N',n); ('D',d)
                 ('M',m); ('O',o); ('R',r); ('Y',y) ]
    }

sendMoreMoney_naiveBetter() |> FairStream.take 1

sendMoreMoney_naiveBetter()
|> LazyList.toArray

/// Highly pruned SEND+MORE=MONEY using carry reasoning (still expressed via choices/guards)
let sendMoreMoney_pruned() =
    search {
        // Fixed by reasoning
        let m = 1
        let s = 9
        let o = 0
        let r = 8
        // Domain for E (2..7) but only E=5 survives; keep it general-ish
        let! e = choices [2..7]
        let n = e + 1
        do! guard (n < 10)
        // Exclude already fixed digits
        do! guard (List.forall (fun d -> d <> e && d <> n) [s; m; o; r])
        // R already fixed → implies carry C1=1, so D+E >= 10
        let! d = choices [max 0 (10 - e) .. 9]
        do! guard ((List.distinct [d; e; n]).Length = 3)
        do! guard (not (List.contains d [s; m; o; r]))
        let y = d + e - 10
        do! guard (y >= 0 && y < 10)
        do! guard (List.forall ((<>) y) [s; m; o; r; e; n; d])
        // Final arithmetic check (cheap now)
        do! guard (
            (s*1000 + e*100 + n*10 + d) +
            (m*1000 + o*100 + r*10 + e) =
             m*10000 + o*1000 + n*100 + e*10 + y
        )
        return [ ('S',s); ('E',e); ('N',n); ('D',d)
                 ('M',m); ('O',o); ('R',r); ('Y',y) ]
    }

/// Bitmask uniqueness version (no List.distinct allocations)
let sendMoreMoney_bitmask() =
    let inline isFree mask d = (mask &&& (1 <<< d)) = 0
    let inline add mask d = mask ||| (1 <<< d)
    search {
        // Fixed digits
        let m, s, o, r = 1, 9, 0, 8
        let used0 = add (add (add (add 0 m) s) o) r
        // Choose E (still keep a domain; only 5 works)
        let! e = choices [2..7]
        do! guard (isFree used0 e)
        let n = e + 1
        do! guard (n < 10 && n <> e && isFree used0 n)
        let used1 = add (add used0 e) n
        // D must give carry (D+E >=10)
        let! d = choices [max 0 (10 - e) .. 9]
        do! guard (d <> e && d <> n && isFree used1 d)
        let y = d + e - 10
        do! guard (y >= 0 && y < 10 && y <> d && y <> e && y <> n && isFree used1 y)
        // Arithmetic confirmation
        do! guard (
            (s*1000 + e*100 + n*10 + d) +
            (m*1000 + o*100 + r*10 + e) =
             m*10000 + o*1000 + n*100 + e*10 + y
        )
        return [ ('S',s); ('E',e); ('N',n); ('D',d)
                 ('M',m); ('O',o); ('R',r); ('Y',y) ]
    }

// Quick check (both yield the single solution)

sendMoreMoney_pruned() |> FairStream.take 1000 

let sendMoreMoney_solution2 =
    sendMoreMoney_bitmask()  |> FairStream.take 1000 

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
 


open Hansei
    
let positions, _ = nqueens 1 11
let positions = Backtracking.run (Some 128) positions  

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

let nqueens3 n =  
    //let mutable cnt = 0
    let rec loop pieces rows cols =
        search { 
            let! r = choices (excluded rows [0..n-1]) 
            let! c = choices (excluded cols [0..n-1])  
            do! guard (diagonalTests n r c pieces) 
            if List.length pieces + 1 = n then     
                return List.sort ((r,c)::pieces)//, pieces, (r,c)
            return! loop ((r,c)::pieces) (Set.add r rows) (Set.add c cols) 
        }
    loop [] Set.empty Set.empty

//let r1 = nqueens2 4 |> LazyList.removeDuplicates |> Seq.truncate 5
let f1, r1 = nqueens2 1 8
f1 |> Seq.take 1
r1
f1 |> run (Some 50) |> Seq.truncate 1 |> Seq.toArray
f1 |> FairStream.take 1
//let f1 = f1 |> LazyList.removeDuplicates |> Seq.truncate 5 |> Seq.toArray

nqueens3 8 |> FairStream.take 1

r1
r1.Count
f1
//Seq.length f1

/// Original-style nqueens3 but without factorial duplication:
/// - Still chooses row then column (like your nqueens3)
/// - Adds lastRow to enforce strictly increasing row order
/// - Tracks diagonals with two sets (r-c and r+c) instead of rescanning pieces
/// - Removes final sort (rows already increasing)
let nqueens3_canonical n =
    let rec loop pieces lastRow usedRows usedCols usedD1 usedD2 =
        search {
            if List.length pieces = n then
                return List.rev pieces
            else
                // Rows not yet used AND greater than lastRow (canonical order)
                let! r =
                    [ for r in 0 .. n-1 do
                        if not (Set.contains r usedRows) &&
                           (match lastRow with None -> true | Some lr -> r > lr)
                        then yield r ] |> choices
                // Columns that are free and pass diagonal constraints
                let! c =
                    [ for c in 0 .. n-1 do
                        if not (Set.contains c usedCols) then
                            let d1 = r - c
                            let d2 = r + c
                            if not (Set.contains d1 usedD1) &&
                               not (Set.contains d2 usedD2) then
                                yield c ] |> choices

                // Extend partial solution
                return!
                    loop ((r,c)::pieces)
                         (Some r)
                         (Set.add r usedRows)
                         (Set.add c usedCols)
                         (Set.add (r - c) usedD1)
                         (Set.add (r + c) usedD2)
        }
    loop [] None Set.empty Set.empty Set.empty Set.empty

nqueens3_canonical 8 |> FairStream.take 2
 
open Hansei.TreeSearch.LazyList

let nqueens3_ordered n =
    let rec loop pieces lastRow rows cols =
        search {
            if List.length pieces = n then
                return List.rev pieces
            else
                let! r = 
                    [ for r in 0..n-1 do
                        if not (Set.contains r rows) &&
                           (match lastRow with None -> true | Some lr -> r > lr) then
                            yield r ] |> choices
                let! c =
                    [ for c in 0..n-1 do
                        if not (Set.contains c cols) &&
                           diagonalTests n r c pieces then
                            yield c ] |> choices
                return! loop ((r,c)::pieces) (Some r) (Set.add r rows) (Set.add c cols)
        }
    loop [] None Set.empty Set.empty

nqueens3_ordered 8 |> LazyList.takeOrMaxList 1
nqueens3_ordered 8 |> FairStream.take 3

// Fast N-Queens with bit masks (row = depth), still uses the backtracking monad.
let nqueensFast n =
    // columns: bit i means column i used
    // diag1 (r - c) shifted by (n-1) to make index non-negative; size 2n-1
    // diag2 (r + c); size 2n-1
    // We pack diag1 & diag2 also into bit masks (need up to 2n-1 bits)
    let inline bit i = 1 <<< i
    let inline test mask i = (mask &&& (bit i)) <> 0
    let inline setBit mask i = mask ||| (bit i)

    // For diag1 index: (r - c) + (n-1)
    // For diag2 index: (r + c)
    let rec place row cols d1 d2 acc =
        search {
            if row = n then
                yield List.rev acc
            else
                // Enumerate available columns by scanning bits (fast enough for n <= 20).
                // Optionally you can precompute remaining columns list once per level.
                let! c =
                    // Build choices lazily only for free columns
                    [ for c in 0 .. n-1 do
                        if not (test cols c)
                           then
                               let diag1Idx = row - c + (n - 1)
                               let diag2Idx = row + c
                               if not (test d1 diag1Idx) && not (test d2 diag2Idx) then
                                   yield c ]
                    |> choices
                let diag1Idx = row - c + (n - 1)
                let diag2Idx = row + c
                yield!
                    place (row + 1)
                          (setBit cols c)
                          (setBit d1 diag1Idx)
                          (setBit d2 diag2Idx)
                          ((row, c) :: acc)
        }
    // Start with all masks zero
    place 0 0 0 0 []

// All solutions, fully lazy, bit‑mask optimized (supports n <= 32 with uint64)
let nqueensFastBits n =
    if n < 1 || n > 32 then invalidArg "n" "n must be 1..32"
    let fullMask = (1UL <<< n) - 1UL
    // Enumerate set bits of a mask as a list (LSB order)
    let bitChoices mask =
        let rec loop m acc =
            if m = 0UL then List.rev acc
            else
                let lsb = m &&& (~~~m + 1UL) // m & -m (unsigned)
                loop (m ^^^ lsb) (lsb :: acc)
        loop mask []
    let rec place row (cols:uint64) diagLeft diagRight acc =
        search {
            if row = n then
                return List.rev acc
            else
                // available columns (bits) for this row
                let avail = fullMask &&& ~~~(cols ||| diagLeft ||| diagRight)
                do! guard (avail <> 0UL) // fail early if dead end
                let! bit = bitChoices avail |> choices
                let col = System.Numerics.BitOperations.TrailingZeroCount bit |> int
                // Advance; note diagonal shifts & mask to width
                return!
                    place (row + 1)
                          (cols ||| bit)
                          (((diagLeft ||| bit) <<< 1) &&& fullMask)
                          (((diagRight ||| bit) >>> 1) &&& fullMask)
                          ((row, col) :: acc)
        }
    place 0 0UL 0UL 0UL []


let first8_old =
    nqueens3 8
    |> Hansei.Backtracking.run (Some 128)
    |> LazyList.tryHead

let first8_new =
    nqueensFast 8
    |> Hansei.Backtracking.run (Some 128)
    |> LazyList.tryHead
 
nqueensFast  6 |> FairStream.take 40 //|> LazyList.toList
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

let rec searchStream i = search { 
    let! a = choices ([true;false])
    if a then return i
    else return! (searchStream (i+1))
} 

searchStream 0 |> explore None (Some 10) 
|> LazyList.takeOrMax 10  |> LazyList.toList

open TTree2
//Using list yields to Stack overflow here
let rec searchStream2 i = search { 
    yield i
    yield -i
    return! searchStream2 (i+1)
} 

let searchStreamOutput = searchStream2 0 

searchStreamOutput |> explore (Some 500) (Some 10)  
|> LazyList.takeOrMax 100
|> LazyList.toList

|> List.filter (function Value _ -> true | _ -> false)