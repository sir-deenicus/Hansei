module Hansei.TextUtils 
            
open System  
open Prelude.Common
open Strings
let stringr100 n x = string (round n (x * 100.))

//////////////
let buildTableRow (collens:_[]) (row:string[]) =
       row |> Array.mapi (fun i s ->  Strings.pad collens.[i] s) |> joinToStringWith " | "


let makeTable newline headers title (table:string[][]) =
       let hlen = Array.map String.length headers

       let lens = table |> Array.map (Array.map (String.length))

       let longest = [|for c in 0..headers.Length - 1 -> max hlen.[c] (Array.selectColumn c lens |> Array.map Seq.head |> Array.max)|]

       let t0 = table |> Array.map (buildTableRow longest) |> joinToStringWith newline

       let hrow = [|headers; [|for i in 0..headers.Length - 1 -> String.replicate longest.[i] "-"|]|] 
                  |> Array.map (buildTableRow longest) 
                  |> joinToStringWith newline
       String.Format("{0}{1}{1}{2}{1}{3}", (toupper title), newline, hrow, t0)

//////////////////////////

///////////////////////

////=-=-=-=-=-=-=-=-=-=

