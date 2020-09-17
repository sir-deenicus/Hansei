#r @"C:\Users\cybernetic\source\repos\Prelude\Prelude\bin\Release\net47\Prelude.dll"
// #I @"C:\Users\cybernetic\.nuget\packages\"
// #r @"mathnet.numerics\4.9.0\lib\net40\MathNet.Numerics.dll"
//#I @"C:\Users\cybernetic\Documents\Papers"
//#load @"disputil.fsx"
//#load @"literate.fsx"
//#r @"LiterateUtils\LiterateUtils\bin\Release\net47\LiterateUtils.dll"

#load @"C:\users\cybernetic\jupyter-notebooks\maths.fsx"
//open Parseutils
open Prelude.Common
open Prelude.Math
open System
open XPlot.Plotly
open MathNet.Symbolics.Core.Vars
open MathNet.Symbolics.Utils
open MathNet.Symbolics

setLatex()

Maths.adjustGrapher()

type SimpleGraph<'a>() =
    let edges = Dict<string, Dict<string, 'a ResizeArray>>()

    let numberOfChildren n =
        match edges.tryFind n with
        | Some a -> a.Count
        | None -> 0

    member g.Vertices =
        g.EdgeData
        |> Seq.map keyValueToKey
        |> Seq.toArray

    member g.Edges =
        [ for (KeyValue(e, e2s)) in edges do
            for (KeyValue(e2, ws)) in e2s do
                for w in ws do
                    yield (e, e2, w) ]

    member g.InsertVertex(s: string) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained

    member g.Remove(v) =
        match (edges.tryFind v) with
        | None -> false
        | Some elist ->
            for KeyValue(_, es) in edges do
                if es.ContainsKey v then
                    es.[v].Clear()
                    es.Remove v |> ignore
            elist.Clear()
            edges.Remove v

    member g.EdgeData = edges

    member g.InsertEdge(v0, v1, w) =
        maybe {
            let! edge = edges.tryFind v0
            match edge.tryFind v1 with
            | None ->
                g.InsertVertex v1 |> ignore
                edge.Add(v1, ResizeArray([ w ]))
                return true
            | Some n ->
                n.Add w
                return true
        }

    ///f informs whether the node is penultimate or not.
    member g.ModifyNodeEdges f n =
        match edges.tryFind n with
        | Some nodes ->
            let keys = Seq.toArray nodes.Keys
            let ispenultimate =
                keys
                |> Array.sumBy numberOfChildren = 0
            for k in keys do
                let currnodes = nodes.[k].ToArray()
                nodes.[k].Clear()
                nodes.[k] <- f ispenultimate currnodes
        | None -> ()

    member g.ContainsVertex v = edges.ContainsKey v

    member g.ContainsEdge v1 v2 = maybe {
                                      let! edges = edges.tryFind v1
                                      return (edges.ContainsKey v2) }
    member g.GetEdges v = maybe {
                              let! elist = edges.tryFind v
                              return elist |> keyValueSeqtoPairArray }

let g = SimpleGraph<string>()


g.InsertVertex "A"
g.InsertVertex "B"
g.InsertVertex "C"

g.InsertEdge ("A","B","3")
g.InsertEdge ("B","C","")

g.Vertices

g.Remove "B"

g.Edges

type VisualDistribution<'item, 'number when 'item: comparison> =
    { Categorical : ('item * 'number list) list
      Depth : int}

let inline bindx maxdepth (dist : VisualDistribution<'item,'number>) (k : 'item -> VisualDistribution<'changeditem list,'number>) =
    //let g = dist.G
    let maxdepthval = Option.defaultValue Int32.MaxValue maxdepth
    let mutable depth = -1
    //g.InsertVertex "root"
    let l =
        [

        for (x,p) in dist.Categorical do
            //printfn "in0: %A" (depth)

            if depth < maxdepthval then

                depth <- depth + 1//dn.Depth
                let dn = k(x)

                depth <- max depth dn.Depth
                //depth <- max depth dn.Depth
                //printfn "in: %A" (depth)
                //g.InsertEdge("root", string x, (List.reduce (*) p,List.head p))
                for (y,p2) in dn.Categorical do
                    //printfn "%A" (depth, y)
                    //let zz = List.fold (fun (i,s) b ->
                    //            let n = s + string b
                    //            if s <> "" then
                    //                g.InsertVertex s
                    //                g.InsertVertex n
                    //                let ps = p @ p2
                    //                //printfn "%A" ps
                    //                let jp = (ps |> List.take (min i ps.Length) |> List.reduce (*)), ps.[(min i ps.Length) - 1]
                    //                g.InsertEdge(s,n, jp)
                    //                |> ignore
                    //            i+1, n) (1,"") y
                    //g.InsertVertex (string y) |> ignore
                    //g.InsertEdge (string x,string y, string(p @ p2 |> List.reduce (*))) |> ignore
                    yield (y, p @ p2) ]

    //printfn "%A" ps
    {
          Categorical = l
          Depth = depth}

//%%markdown
//# Non-determism, Combinatorics, Learning, Probability and Quantum Mechanics
//[MARGIN]The target audience is meant to be fairly broad--anyone with an understanding of programming--but I might occasionally slip up/become lazy and assume more than the average target level intended. I apologize for this.[/MARGIN]

//In this work I explain how combinatorics can be seen as a key generator underlying much of what we understand as inference, which includes subjects such as statistical mechanics, probability, logic and even quantum mechanics. Non-deterministic computations in turn serves as a natural tool to explore these combinatoric possiblities.

//# Combinatorics

//## PINs
//Imagine attempting to guess a 4-digit PIN. How many possibilities must we work through? Well, for the first digit there are 10 possiblities: 0 to 9. For the second there are also ten possibilities. Since each of the first digit can go with 10 other digits, the possibile pairs will have the pattern: `[(0,1); (0,2); (0,3)...; (1,0); (1,1); (1,2)...]`. The pattern is `10 possiblities for 0` + `10 possiblities for 1` + and so on. The total then will be 10 * 10 = 100. Simple so far. We can enumerate this with F# code.

//%%code
[ for d1 in 0..9 do
    for d2 in 0..9 -> d1, d2]

//%%markdown
//It should be easy to see then that the total possibilities for a 4 digit pin is $10*10*10*10$ or $10^4$. In general, if there are digits or symbols or an alphabet with $a$ possibilities from which we wish to randomly select from $n$ times, the number of possiblities will be $a^n$.
//
//[MARGIN]This construction is also the same process followed for tensor products, which can be found in quantum mechanics, linear algebra and differential geometry. Even though it sounds fancy, the machinery is basically that of a humble nested for-loop.[/MARGIN]

//For the PIN example above, the full code to enumerate all possibilities will be:
//```
//[ for d1 in 0 .. 9 do
//    for d2 in 0 .. 9 do
//        for d3 in 0 .. 9 do
//            for d4 in 0 .. 9 -> (d1, d2, d3, d4) ]
//```

//We can enumerate the possibilities of two coin flips:

//%%code
[for c1 in ["H";"T"] do
    for c2 in ["H";"T"] -> c1+c2]

//%%markdown
//[MARGIN]Lazy means generated as needed. Useful for very big or even infinite lists[/MARGIN]
//But what if we wanted to enumerate 3, 4 or 10 flips? Writing this out as nested for loops would be too tiresome. In F# we can do this generally using a recusrively generated (lazy) list. We recurse $n$ times, starting a new for loop at each depth and using the `yield!` command to manage the book-keeping of concataneting all the levels of the list for us. At the same time, we're keeping and appending to the top of a list which tracks our current path through possibility space.

//%%code
let generatePossibilities alphabet n =
    let rec iterate symbols i = seq {
        if i = 0 then yield List.rev symbols
        else
            for symbol in alphabet do
                yield! iterate (symbol::symbols) (i-1)
        }
    iterate [] n

//Two digit example from before.
generatePossibilities [0..9] 2
|> Seq.toList

//%%code
//Three coin flips:
generatePossibilities ["H";"T"] 3
|> Seq.map (String.concat "") //join list into string
|> Seq.toList

//%%code
//The sum of 2 dice flips
generatePossibilities [1..6] 2
|> Seq.map List.sum //sum flips result
|> Seq.toList

//%%markdown
//## Discrete Probability as Counting and Filtering
//We can already start to do probability with this. Specifically, we need to be able to aggregate all items that are identical and then count them.
//%%code
let groupAndCountWith f s =
    s |> Seq.groupBy f
      |> Seq.map (fun (x, xs) -> x, Seq.length xs)

generatePossibilities [1..6] 2
|> Seq.map List.sum //sum flips result
|> groupAndCountWith id
|> Seq.toList
|> Chart.Column

//%%markdown
//Since probabilities have to sum to 1, we need to be able to normalize counts.

let normalize (xs: _ seq) =
    let total = float (Seq.sumBy snd xs)
    Seq.map (fun (x,c) -> x, float c/total) xs

//%%markdown
//Finally, to ask probability questions, we use filter to enter the desired subspace (getting rid of all not matching the query). We then sum the probabilities of all the members that are left in the subspace. For example to answer "What is the probability of seeing a number greater than 5 in two flips of the dice?", we do:
//%%code
generatePossibilities [1..6] 2
|> Seq.map List.sum //sum flips result
|> groupAndCountWith id
|> normalize
|> Seq.filter (fun (x, _) -> x > 5)
|> Seq.sumBy snd

//%%markdown
//This is enough to handle many types of discrete probability problems but it is still too manual. Being able to write more declarative specifications would be better as that allows us more flexibility in how we tackle problems while reducing the risk of making a errors. We will return to that, but next is combinatorics.
//## Permutations
//The counting rule of $a^n$ possibilities is the case for permutations *with repetition*. For permutations *without repetition* (which is what is commonly meant by permutation), the situation is different.
//Suppose we have 3 letters, "a,b,c". How many ways can we arrange them? Well, for the first letter there are 3 possibilities. After we have chosen the first letter, there are now only 2 letters left to choose from. So for the second letter there will be 2 possibilities and each of which will be paired with one of the 3 initial possibilities. After that there is only 1 possibility. Therefore there are $3*2*1=6$ possible arrangements. In general, the number of ways to arrange $n$ items is $n!$. We can write the code for this:
//%%code
[ for a in [ 'A' .. 'C' ] do
    for b in List.filter ((<>) a) [ 'A' .. 'C' ] do
        for c in [ for l in 'A' .. 'C' do
                       if l <> a && l <> b then yield l ] -> a, b, c ]
//%%markdown
//To generalize to arbitrary permutations, I'll simply reuse the generatePossibilities function from before but remove all lists with duplicated letters, so that eg [A,B,B] becomes [A,B], and then filter to just those items whose size are equal to the number of items that are being taken.
//%%code
let permutations takeN items =
    generatePossibilities items takeN
    |> Seq.map List.removeDuplicates
    |> Seq.filter (fun l -> l.Length = takeN)

permutations 3 ['A'..'C'] |> Seq.toList

//%%markdown
//It is also possible to calculate permutations where you only take some number of items that is less than the total number of items. For example: "how many ways to pick 2 items from 7 items?" That is 7 \* 6. Notice that this is the same as $$\frac{7*6*5*4*3*2*1}{5*4*3*2*1}$$ This motivates the mathematical formula for a permutation, which is (with $k$ the number of items to be taken): $$\frac{n!}{(n-k)!}$$ If n = 7 and k = 2, that is: $\frac{7!}{(7-2)!} = \frac{7!}{5!}=42$

//%%code
permutations 2 ['A'..'G'] |> Seq.length

//%%markdown
//[MARGIN]In general, how difficult or simple things seem are best thought of how far away you are to be able to solve them. Since these spaces are usually combinatorial, failure to solve a problem has more to do with proximity and persistance than to any lack in individual ability for a healthy human being.[/MARGIN]

//Here is a "simple" puzzle. How many ways are there to arrange the letters `ABCD` so that B and C are never together? We can do this by brute-force. In general, a brute-force solution reduces the need to be clever.
//%%code
let puzzleAnswer =
    [| for permutation in permutations 4 [ "A"; "B"; "C"; "D" ] do
        let pstring = String.concat "" permutation
        if not (pstring.Contains "BC" || pstring.Contains "CB")
        then yield pstring |]

puzzleAnswer.Length
let integerComposition n =
    let rec loop l m = seq { 
        for i in [1..n] do
            if (i + m <= n) then
                let l' = i::l 
                if i+m = n then yield l'
                yield! loop l' (m+i) }
    loop [] 0
     
let einsteinSolid n =
    let rec loop d l m = seq { 
        for i in [0..n] do
            if (i + m <= n) then
                let l' = i::l 
                if i+m <= n && l'.Length = n then yield l'
                if d + 1 < n then yield! loop (d+1) l' (m+i) }
    loop 0 [] 0
//%%markdown
//[MARGIN]Computers allow us to write specifications rather than have to turn the problem inside out to understand and exploit its structure. But because the specifications are executed there are limits to this approach.[/MARGIN]
//But it's worth solving to get a start on combinatorial reasoning. This time, let's do a simpler version, with `ABC` but avoiding `AC` pairings. First, let's generate all possible permutations of the `ABC` string.
//%%code
//A helper function to flatten a list of strings inside a sequence to a string (making a list of strings)
let seqToStringList sq = sq |> Seq.map (String.concat "") |> Seq.toList

let abcperms =
    permutations 3 ["A";"B";"C"]
    |> seqToStringList

abcperms
//%%markdown
//A and C are always together so we can treat them as one. Hence there are only $2!$ possibilities.
//%%code
let acbperms =
    permutations 2 ["AC";"B"]
    |> seqToStringList

acbperms

//%%markdown
//But we can have not just `AC` pairs, but also `CA` pairs.
//%%code
let cabperms =
    permutations 2 ["CA";"B"]
    |> seqToStringList

cabperms
//%%code
set abcperms - (set acbperms + set cabperms)

//%%markdown
//Notice that subtraction here corresponds to the filtering in our code. If we look at the cardinalities (size) of the sets, we can rewrite the code above to the general pattern of $$n! - ((n-1)! + (n-1)!) = n! - 2(n-1)!$$ Something worth pointing out is that the details of which letter is next to what is actually not so important. Not getting too caught up on surface details when attempting a solution is something important to grasp when solving math problems. It is important to extract the core of the problem in order to build a proper model it and deploy appropriate tools. Not doing so leads to being sidetracked into fruitless directions and failing to solve the problem.
//
//[MARGIN]I suspect getting this down to a science would lead to genuine IQ boosts.[/MARGIN]
//With computers, as mentioned before, it is possible to straight describe the problem. This makes it more accessible and approachable than a general mathematical solution. But rather than stop at executable descriptions, using programs as a way to get at the general strcuture of a problem--beyond just numeric models--is important but not enough studied.
//
//As an example let's try extending the original puzzle so that 2 letter permutations of a given 3 letter string are not contained in permutations of the full string. For example, how many permutations of `[A..E]` do not contain any two letter permutations of `ACE`? . We can first write the description as code to solidify the problem in our head.

//%%code
let disallows = permutations 2 ["A";"C";"E"] |> seqToStringList
disallows

let puzzleAnswer2 =
    [| for permutation in permutations 7 [ "A"; "B"; "C"; "D";"E"; "F"; "G"] do
        let pstring = String.concat "" permutation
        if not(List.exists pstring.Contains disallows)
        then yield pstring |]

puzzleAnswer2.Length
//%%markdown
//To use math, a description no longer suffices, we need to get inside the problem. For a start, we can apply the same approach. Treat the strings, such as `AE`, as one and find their permutations.
//%%code
permutations 4 [ "AE"; "B";"C"; "D"]

//%%markdown
//There are 24 of these. Since there are 6 such pairs, we can try $5! - 6 * 4! = -24$. Suggesting double counting. Where do the double counts occur and how many are there? The trick is to use the computer to explore and expose the problem. There are many investigative programs that can be written, each dependent on how much you know. To be brief I'll start from a position which requires less elaboration.
//
//You should notice that pairs like `AC` and `CE` can be joined to form `ACE`. The permutations of `[ACE,B,D]` is then the intersection of permutations based on `AC` and those based on `CE`.
//%%code
let aceperms = permutations 3 [ "ACE"; "B"; "D" ] |> seqToStringList
aceperms

//%%code
let s1 = set (permutations 4 [ "AC"; "B"; "D";"E" ] |> seqToStringList)
let s2 = set (permutations 4 [ "A"; "B"; "CE";"D" ] |> seqToStringList)

Set.intersect s1 s2 = set aceperms

//%%markdown
//Also notice that `AC` and `CE` can be joined to form `ACE`. So the general pattern is joining starts and ends of the strings that aren't mirrors of each other. Next, we'll wrap up this section.
//
//Working through, there are 5! possibilities for a string of length 5. For 2 letter permutations of a 3 letter string, there are 6 pairs. Treating each letter pair as a single letter in the original string, there will be 4! possible strings. Again, there are 6 such pairs for a total of $4!\times6$. Recalling the previous paragraphs on remove double counting, the final total is: $5! - P(3,2) * 4! + 6 * 3!$. We can generalize this to strings of length $n$ (for 2 letter permutations of 3 letter substrings) as $n! - P(3,2)(n-1)! + 6(n-2)!$. This can in turn be generalized to $k$ letter permutations of substrings of length $m$, but that would take us too far afield.
//%%code

let bxx = Array.create 3 ""
for x in 0..2 do
    let i = random.Next(0,3)
    bxx.[i] <- bxx.[i] + "x"

String.concat "|" bxx


let powerset (items : _ []) =
    let n = items.Length
    seq {
        for bitpattern in generatePossibilities [ false; true ] n do
            yield [| for i in 0..n - 1 do
                        if bitpattern.[i] then yield items.[i] |]
    }

let powerset2 n (items : _ seq) =
    seq {
        for bitpattern in generatePossibilities [ false; true ] n do
            yield [| for i in 0..n - 1 do
                        if bitpattern.[i] then
                            yield Seq.head (Seq.skip i items) |]
    }
2Q ** (2Q**15Q)

powerset [|"A";"B";"C";"D";"E"|] |> powerset2 (int(2. ** 5.)) |> Seq.take 100 |> Seq.toArray
2. ** (2. ** 5.)
Seq.initInfinite id |> Seq.length

Seq.skip 1 [|1..10|]
powerset [|1..15|] |> Seq.take 100 |> Seq.toArray
generatePossibilities [false;true] 20 |> Seq.take 200 |> Seq.toArray

2. ** 30.

generatePossibilities ["H";"T"] 3 |> Seq.toArray// |> Array.length
generatePossibilities [1..6] 2 |> Seq.toArray |> Array.map List.sum
//let generatePossibilities alphabet n =
//    tensorProduct alphabet n
//    |> Seq.removeDuplicates


let permutations items takeN =
    generatePossibilities items takeN
    |> Seq.map List.removeDuplicates
    |> Seq.filter (fun l -> l.Length = takeN)

permutations ["A";"B"; "C";"D"] 2// |> Seq.length

let combinations items takeN =
    permutations items takeN
    |> Seq.map List.sort
    |> Seq.removeDuplicates

let combinationsWithRepeats items takeN =
    generatePossibilities items takeN
    |> Seq.map List.sort
    |> Seq.removeDuplicates


generatePossibilities ["A";"B";"B";"B"] 2 |> Seq.toArray = (generatePossibilities ["A";"A";"A";"B"] 2 |> Seq.toArray)

generatePossibilities ["A";"B";"B";"B"] 2


combinations ['A'..'Z'] 5 |> Seq.take 30 |> Seq.toArray


combinationsWithRepeats ["A";"B"] 2

combinationsWithRepeats ["b"; "c"; "l"; "s"; "v";] 3
permutations ["A";"B";"C"; "D"] 2 //|> List.length
combinations ["A";"B";"A";"B"] 2 //|> List.length

(**A simple wrapper around the list monad can do some book-keeping of probabilities for us by silently tracking/propagating joint probabilities*)
module SimplestWrapper =
    let inline bind (dist : list<'item * 'number>)
               (k : 'item -> list<'changeditem * 'number>) =
        [ for (x, p) in dist do
              for (y, p2) in k x do
                  yield (y, p * p2) ]

    let fail() = []
    let bernoulli p = [true, p; false, 1.- p]
    let uniform l = l |> List.map (fun x -> x, 1./float l.Length)
    let bernoulliChoice a b p = [a,p;b,1.- p]
    let always x = [x,1.]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open SimplestWrapper

dist {
    let! b = bernoulliChoice "H" "T" 0.5
    let! b2 = bernoulliChoice "H" "T" 0.5
    return (b,b2)
}

dist {
    let! b = bernoulliChoice "H" "T" 0.5
    let! b2 = bernoulliChoice "H" "T" 0.5
    let! b3 = if b2 = "H" then bernoulliChoice "H" "T" 0.25
                else bernoulliChoice "H" "T" 0.5
    return (b, b2, b3)
}
(**The relationship between control structures for nondetermiism and inference. Monads show up a lot when you want to manage complex control flow using higher order functionas and a simple pipelining design. Nondeterminism is about computations which have multiple possiblities and branching paths. Hence the relation with *)
(** But what if we want to sample from a very large space? Laziness will be helpful. *)
dist {
    let! a = uniform [1..10000]
    let! b = uniform [1..10000]
    return (a + b)
}


module LazySeqWrapper =
    let inline bind (dist : seq<'item * 'number>)
               (k : 'item -> seq<'changeditem * 'number>) =
        seq { for (x, p) in dist do
                for (y, p2) in k x do
                    yield (y, p * p2) }

    let fail() = Seq.empty
    let bernoulli p = seq [true, p; false, 1.- p]
    let uniform l = l |> Seq.map (fun x -> x, 1./float (Seq.length l))
    let bernoulliChoice a b p = seq [a,p;b,1.- p]
    let always x = seq [x,1.]

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = Seq.concat [a;b]
        member __.Delay(f: unit -> seq<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open LazySeqWrapper

let qn =
    dist {
        let! a = uniform (seq [1..10000])
        let! b = uniform (seq [1..10000])
        return (a + b) % 20
    } |> Seq.take 100 |> Seq.toArray |> Array.groupBy fst |> Array.map (fun (x,ps) -> x, Array.sumBy snd ps)

let rec geom c p =
    dist {
            let! b = bernoulli p
            if b then return c
            else return! geom (c+1) p
        }

dist {
    let! i = geom 0 0.5
    do! observe (i> 3)
    let! j = geom 1 0.6
    return (i+j)
} |> Seq.take 1

geom 0 0.6 |> Seq.take 10  |> Seq.toArray

let rec infiniteFlips p = dist {
        let! b = bernoulli p
        return b
        return! infiniteFlips p
    }

infiniteFlips 0.001 |> Seq.take 10

dist {
    let! b = infiniteFlips 0.5
    let! j = geom 1 0.6
    do! observe (not b)
    return (b, j)
} |> Seq.take 10

seq {   for i in 1..10 do
            for j in Seq.initInfinite id do
              if i > 5 then yield (i,j) }
|> Seq.take 1

#r @"bin\Debug\net47\Hansei.Core.dll"
open Hansei.Backtracking

module BtWrapper =
    let inline bind (dist : LazyStream<'item * 'number>)
               (k : 'item -> LazyStream<'changeditem * 'number>) =
        bt { let! (x, p) = dist
             let! (y, p2) = k x
             yield (y, p * p2) }

    let fail() = Nil
    let bernoulli p = bt {yield true, p; yield false, 1.- p}
    let uniform l = l |> Seq.map (fun x -> x, 1./float (Seq.length l))
    let bernoulliChoice a b p = bt {yield a,p; yield b,1.- p}
    let always x = bt {yield x,1.}

    type DistributionBuilder() =
        member inline d.Bind(dist, f) = bind dist f
        member d.Return v = always v
        member d.ReturnFrom vs = vs
        member d.Zero () = always ()
        member d.Combine(a,b) = choice a b
        member __.Delay(f: unit -> LazyStream<_>) = f()

    let dist = DistributionBuilder()

    let observe test = dist {if not test then return! fail()}

open BtWrapper

let rec infiniteFlips p = dist {
        let! b = bernoulli p
        return b
        return! infiniteFlips p
    }

infiniteFlips 0.001 |> run (Some 100) |> Seq.take 50 |> Seq.toArray

let bn =
    dist
        {   let! b = bernoulliChoice "H" "T" 0.5
            do! observe (b = "H")
            let! b2 = bernoulliChoice "H" "T" 0.5
            let! b3 = if b2 = "H" then bernoulliChoicev "H" "T" 0.25 else bernoulliChoicev "H" "T" 0.5
            let! b4 = bernoulliChoicev "H" "T" 0.75
            return [b; "(b=H)"; b2;"_";b4] }
let bn =
    distv
        {   let! b = bernoulliChoicev "H" "T" 0.5
            if b = "H" then
                let! b2 = bernoulliChoicev "H" "T" 0.5
                let! b3 = if b2 = "H" then bernoulliChoicev "H" "T" 0.25 else bernoulliChoicev "H" "T" 0.5
                let! b4 = bernoulliChoicev "H" "T" 0.75
                return [b; b2;"_";b4]
            else return []}

let bn =
    distv
        {   let! b = bernoulliChoicev "H" "T" 0.5
            let! b2 = bernoulliChoicev "H" "T" 0.5
            return [ b;b2;string( b=b2 && b = "H")] }
let bn =
    distv
        {   let! b = bernoulliChoicev "H" "T" 0.5
            let! b2 = bernoulliChoicev "H" "T" 0.5
            do! observe (not ( b=b2 && b = "H"))
            return [b;b2;"b=b2 && b = H"] }




let bn = distv {
    let! child1 = bernoulliChoicev "boy" "girl" 0.5
    let! child2 = bernoulliChoicev "boy" "girl" 0.5//ProbList.bernoulliChoice p ("boy2", "girl2")
    let! seefirst = uniformv [child1; child2]
    do! observe (seefirst.Contains "boy")
    return [child1;child2; "see " + seefirst]
}


let hasHeads count = List.filter ((=) "H") >> List.length >> ((<=) count)

let rec atleast_n_in_m_flips count flips n = distv {
    if n = 0 then
     if hasHeads count flips then return List.rev(string true :: flips)
     else return List.rev(string false :: flips)
    else let! flip = bernoulliChoicev "H" "T" 0.5
         return! atleast_n_in_m_flips count (flip::flips) (n-1) }

let bn = atleast_n_in_m_flips 2 [] 3

let rec runloop sep soleLast (s:string) (g:SimpleGraph<float * float>) = function
    | [a1], [b1] when soleLast ->
        if s <> "" then
            g.InsertVertex s
            g.InsertEdge(s, a1, b1) |> ignore
    |  a1::ats, b1::bts ->
        let n = if s = "" then a1 else (s + sep + a1)
        if s <> "" then
            g.InsertVertex s
            g.InsertEdge(s, n, b1) |> ignore
        //(((s + a1),b1)::l)
        runloop sep soleLast n g (ats,bts)
    | [], [] -> ()
    | _ -> failwith "unexpected error in runloop"
let fixlen maxlen s =
    if String.length s > maxlen then s.Replace(",", "\\n").Replace("/", "\\n")
    else s



let template = System.IO.File.ReadAllText("C:\Users\cybernetic\Documents\Papers\dagre-template.txt")


let bn =
    distv
        {   let! b = bernoulliChoicev "H" "T" 0.5
            let! b2 = bernoulliChoicev "H" "T" 0.5
            let! b3 = bernoulliChoicev "H" "T" 0.25
            return [b;b2;"_" ] }
let kz =
    [for (y,ps) in bn.Categorical do
                        let (_,zz,p) =
                            List.fold (fun (i,s,l) b ->
                                    let n = string b::s
                                    //if s <> "" then
                                        //g.InsertVertex s
                                        //g.InsertVertex n
                                        //printfn "%A" ps
                                    let jp = (ps |> List.take (min i ps.Length) |> List.reduce (*)), ps.[(min i ps.Length) - 1]
                                        //g.InsertEdge(s,n, jp)
                                        //|> ignore
                                    i+1, n,jp::l) (1,[],[]) y
                        if p <> [] then yield List.rev zz,p
                        //g.InsertVertex (string y) |> ignore
                        //g.InsertEdge (string x,string y, string(p @ p2 |> List.reduce (*))) |> ignore

    ]


let g = SimpleGraph<float * float>()
g.InsertVertex "root"
for (a,b) in kz do
    let n = List.head a
    let w = List.last b
    if not(g.ContainsEdge "root" n |> Option.defaultValue false) then
        g.InsertEdge("root", n, w) |> ignore
    runloop "," true "" g (a,List.rev b)


createDagreGraph2 string 90 50 g
|> disp false "nn1" 1200 600
|> writeStrRaw


let inline reducer isPenultimate (n, ws) =
    if isPenultimate then Seq.reduce (fun (x,y) (u,_) -> x + u, y) ws
    else n

let inline reduceNodeWeights isPenultimate (ws:seq<_>) =
    let parts = Seq.groupBy id ws
    Seq.map (reducer isPenultimate) parts |> ResizeArray

let inline mergeNodes (g:SimpleGraph<_>) =
    Array.iter (g.ModifyNodeEdges reduceNodeWeights) g.Vertices


mergeNodes g

g.Edges

kz  |> List.groupBy fst
    |> List.map (fun (x,ps) -> x, ps |> List.sumBy (fun (_,cc) -> fst (List.head cc) )  ) //
    |> List.normalizeWeights
    |> List.sumBy snd

bn.Categorical |> List.groupBy fst |> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.reduce addLists)|> List.map (keepLeft (List.reduce (*))) |> List.sumBy snd
bn.Categorical |> List.groupBy fst |> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.reduce addLists)|> List.map (keepLeft (List.reduce (*)))

bn.Categorical |> List.groupBy fst //|> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.head) |> List.map (keepLeft (List.reduce (*))) |> List.sumBy snd
bn2.Categorical |> List.groupBy fst// |> List.map (fun (x,ps) -> x, ps |> List.map snd |> List.head) |> List.map (keepLeft (List.reduce (*)))
bn.Categorical |> List.map (keepLeft (List.reduce (*)))// |> List.sumBy snd
