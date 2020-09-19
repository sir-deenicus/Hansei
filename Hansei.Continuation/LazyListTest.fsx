#r @"bin\Debug\netcoreapp2.1\Hansei.Core.dll"

open Hansei.FSharpx.Collections
#time "on"

LazyList.monad { let! p = LazyList.ofList [ 1..1000 ]
                 let! q = LazyList.ofList [ 1..1000 ]
                 return (p, q) }
|> LazyList.take 200000
|> LazyList.toArray

LazyList.monad {
    let! x = LazyList.ofList [1..10]
    let! y = LazyList.ofList [1..10]
    do! LazyList.guard (x > y && x % 2 = 0) 
    printfn "%A" (x,y)
    return (x,y) 
} |> LazyList.toArray 

let rec naturals n =
    LazyList.monad {
        do! LazyList.guard (n >= 0I)
        yield n
        return! (naturals (n+1I))
    }

let rec integers z =
    LazyList.monad {
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

module FairStreamTests =
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

    let zz =
        integers 0I
        |> FairStream.mapAlt ((*) 2I)
        |> run 100
        |> Seq.take 10
        |> Seq.toArray

    let zz2 =
        integers 0I
        |> FairStream.map ((*) 2I)
        |> run 100
        |> Seq.take 10
        |> Seq.toArray

    let qq =
        FairStream.zip (naturals 0I) (integers 0I)
        |> run 100
        |> Seq.take 10
        |> Seq.toArray

    
    integers 0I 
    |> run 250000 
    |> Seq.skip 5000 
    |> Seq.take 150000 
    |> Seq.toArray 
    |> Array.length
