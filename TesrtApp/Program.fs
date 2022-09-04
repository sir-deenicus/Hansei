open Hansei.Utils
open Hansei.Continuation
open Hansei.FSharpx.Collections  
 
open Hansei.Backtracking

let rec ints i = bt {yield i; yield! ints (i+1)}
   

ints 1 |> run None |> LazyList.takeList 10 |> printfn "%A"

let p2 = Choice(0, One 1)

let rec infb d xs = bt {
    if d > 15 then return xs
    else
        let! a = p2  
        return! infb (d + 1) (a::xs)
    }  


//infb 0 []
//|> run None
//|> LazyList.takeOrMaxArray 4 
//|> Seq.concat
//|> Seq.map string
//|> String.concat ""
//|> printfn "%A"