module Hansei.Combinatorics

open Hansei.FSharpx.Collections
open Prelude.Common

//https://www.programminglogic.com/integer-partition-algorithm/
let numberOfPartitions n =
    let table =
        Array.init (n + 1) (fun i ->
            Array.init (n + 1) (fun _ ->
                if i = 0 then 1I else 0I))
    for i in 0 .. n do
        for j in 1 .. n do
            if (i - j < 0)
            then table.[i].[j] <- table.[i].[j - 1]
            else table.[i].[j] <- table.[i].[j - 1] + table.[i - j].[j]
    table.[n].[n]


//https://www.geeksforgeeks.org/generate-unique-partitions-of-an-integer/
let integerPartitionsDesc n =
    // An array to store a partition
    let p = Array.create n 0
    // Index of last element in a partition
    let mutable k = 0
    // Initialize first partition as number itself
    p.[0] <- n
    // This loop first prints current partition, then generates next partition.
    // The loop stops when the current partition has all 1s
    let rec loop() = seq {
        yield p.[..k]
        // Generate next partition
        // Find the rightmost non-one value in p[]. Also, update
        // the rem_val so that we know how much value can be accommodated
        let mutable rem_val = 0
        while k >= 0 && p.[k] = 1 do
            rem_val <- rem_val + p.[k]
            k <- k - 1
        // if k < 0, all the values are 1 so there are no more partitions
        if (k >= 0) then
            // Decrease the p[k] found above and adjust the rem_val
            p.[k] <- p.[k] - 1
            rem_val <- rem_val + 1
            // If rem_val is more, then the sorted order is violated. Divide rem_val in
            // different values of size p[k] and copy these values at different positions after p[k]
            while rem_val > p.[k] do
                p.[k + 1] <- p.[k]
                rem_val <- rem_val - p.[k]
                k <- k + 1
            // Copy rem_val to next position and increment position
            p.[k + 1] <- rem_val
            k <- k + 1
            yield! loop()
        }
    loop()

//http://jeromekelleher.net/generating-integer-partitions.html
let integerPartitionsAsc n =
    let a = [| for _ in 0 .. n -> 0 |]
    let mutable k = 1
    a.[1] <- n
    seq {
        while k <> 0 do
            let mutable x = a.[k - 1] + 1
            let mutable y = a.[k] - 1
            k <- k - 1
            while x <= y do
                a.[k] <- x
                y <- y - x
                k <- k + 1
            a.[k] <- x + y
            yield a.[..k]
    }


let generatePossibilities alphabet n =
    let rec iterate symbols i = seq {
        if i = 0 then yield List.rev symbols
        else
            for symbol in alphabet do
                yield! iterate (symbol::symbols) (i-1)
        }
    iterate [] n

let powerset (items: _ []) =
    let n = items.Length

    seq {
        for bitpattern in generatePossibilities [ false; true ] n do
            yield
                [| for i in 0 .. n - 1 do
                    if bitpattern.[i] then yield items.[i] |]
    }

let powersetSeq n (items : _ seq) =
    let itemsList = LazyList.ofSeq items
    seq {
        for bitpattern in generatePossibilities [ false; true ] n do
            yield [| for i in 0..n - 1 do
                        if bitpattern.[i] then yield LazyList.head (LazyList.skip i itemsList) |]
    }


let permutationsMaxLen comparer takeN items =
    generatePossibilities items takeN
    |> Seq.map List.removeDuplicates
    |> Seq.filter (fun l -> comparer l.Length takeN)

let permutations takeN items = permutationsMaxLen (=) takeN items 

let combinations takeN items =
    permutations takeN items
    |> Seq.map List.sort
    |> Seq.removeDuplicates

let combinationsMaxLen takeN items = 
    permutationsMaxLen (<=) takeN items
    |> Seq.map List.sort
    |> Seq.removeDuplicates
     

module List =
    //https://stackoverflow.com/questions/1526046/f-permutations
    let rec internal distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
    
    let rec permutations = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permutations xs) 
        
    let combinations l =
        permutations l 
        |> List.map List.sort
        |> List.removeDuplicates   

module Array =
    let permutations a = 
        List.permutations (List.ofArray a) 
        |> List.map List.toArray 
        |> List.toArray

    let combinations a =
        List.combinations (List.ofArray a)
        |> List.map List.toArray
        |> List.toArray
        
    let permutedIndices (a:_[]) =
        let perms = List.permutations [0..a.Length - 1]
        [|for p in perms -> [|for i in p -> a[i]|]|] 
        |> Array.removeDuplicates
     

module LazyList =
    open LazyList.ComputationExpressions
    
    let rec distribute e =
        function
        | LazyList.Nil -> LazyList.singleton (LazyList.singleton e)

        | LazyList.Cons (x, xs') as xs ->
            LazyList.cons
                (LazyList.cons e xs)
                (lzlist {
                    let! xs = distribute e xs'
                    return LazyList.cons x xs
                }) 
 
    let rec permutations =
        function
        | LazyList.Nil -> LazyList.singleton LazyList.empty
        | LazyList.Cons (e, xs) ->
            LazyList.map (distribute e) (permutations xs)
            |> LazyList.concat 
        
    let combinations l =
        permutations l
        |> LazyList.map LazyList.sort
        |> LazyList.removeDuplicates

        
         