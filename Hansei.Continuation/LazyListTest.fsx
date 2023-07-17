open System.Collections.Generic


#r @"bin\Release\netstandard2.1\Hansei.Core.dll"

open Hansei.FSharpx.Collections
open Hansei.FSharpx.Collections.LazyList.ComputationExpressions

#time "on"

lazyList { 
    let! p = LazyList.ofList [ 1..1000 ]
    let! q = LazyList.ofList [ 1..1000 ]
    return (p, q) } 
|> LazyList.reduce (fun (c,d) (a,b) -> (c+a, d+b))
 
lazyList {
  yield LazyList.ofList [1..10]
  yield LazyList.ofList [1..10]
  yield LazyList.ofList [1..10]
}
|> LazyList.map (LazyList.map ((*) 2))
|> LazyList.concat
|> LazyList.toArray

lazyList {
    let! x = LazyList.ofList [1..10]
    let! y = LazyList.ofList [1..10]
    do! guard (x > y && x % 2 = 0) 
    printfn "%A" (x,y)
    return (x,y) 
} |> LazyList.toArray 

let rec naturals n =
    lazyList {
        do! guard (n >= 0I)
        yield n
        return! (naturals (n+1I))
    }

let rec integers z =
    lazyList {
        yield z
        if z <> 0I then yield -z
        return! integers (z + 1I)
    } 

integers 0I
|> LazyList.take 15 
|> LazyList.toArray  

LazyList.zip (naturals 0I) (integers 0I) 
|> LazyList.take 10 
|> LazyList.toArray
 
LazyList.choice (integers 0I) (integers 10I)
|> LazyList.take 100_000
|> LazyList.toArray

LazyList.append (integers 0I) (integers 10I)
|> LazyList.take 100_000
|> LazyList.toArray 

LazyList.ofList [1..1000]
|> LazyList.map ((*) 5) 
|> LazyList.take 2 
|> LazyList.toArray


//module FairStreamTests =
open Hansei.Backtracking

let rec naturals n =
    bt {
        do! guard (n >= 0I)
        yield n
        return! (naturals (n + 1I))
    }

let rec integers z =
    bt {
        yield z
        if z <> 0I then yield -z
        return! integers (z + 1I)
    } 

let rationalsIntPair (n,d) =
    bt {
        do! guard (d <> 0I)
        let! p = integers n
        let! q = integers d
        return (p,q)
    }

let r1 =
    integers 0I
    |> FairStream.map ((*) 2I)
    |> run (Some 100)
    |> LazyList.take 10 
    |> LazyList.toArray

let r2 =
    FairStream.zip (naturals 0I) (integers 0I)
    |> run (Some 100)
    |> LazyList.take 10
    |> LazyList.toArray

    
integers 0I 
|> run (Some 250000)
|> LazyList.skip 5000 
|> LazyList.take 150000 
|> LazyList.map string 
|> LazyList.toArray 
|> Array.length

bt {
    let! x = choices [1..10]
    let! y = choices [1..10]
    do! guard (x > y && x % 2 = 0) 
    printfn "%A" (x,y)
    return (x,y) 
} 
|> run (Some 50)
|> LazyList.toArray 

let r3 =
    rationalsIntPair (0I,1I)
    |> run (Some 12)
    |> LazyList.toArray  

//with no guard check, 30 yields 306; 42 = 1500; 50 = 4174; 90 = 644511
let rintpair5 = [|(0, 1); (0, -1)|]
let rintpair10 = [|(0, 1); (0, -1); (1, 1); (0, 2); (-1, 1); (1, -1); (0, -2); (-1, -1); (2, 1); (1, 2); (0, 3)|]

Array.map (fun (x,y) -> int x, int y) r3 = rintpair10

let xs,ys = choices [0..3], choices [0..3]
    
FairStream.combine xs ys 
|> run (Some 3)
|> LazyList.toArray
    
FairStream.map (fun x -> FairStream.map (fun y -> (x,y)) ys) xs 
|> FairStream.fold FairStream.combine Nil
|> run (Some 6)
|> LazyList.toArray
|> Array.sort
|> fun a -> a = [|for x in [0..3] do for y in [0..3] -> x,y|]

let incrementalPrimes() =
    let seenPrimes = HashSet()
    let test x = 
        seenPrimes |> Seq.exists (fun p -> x % p = 0)
    
    let rec primes i =  seq {
        if not (test i) then 
            yield i
            seenPrimes.Add i |> ignore
            yield! primes (i + 1)
        else yield! primes (i + 1)
    }
    
    primes 2
