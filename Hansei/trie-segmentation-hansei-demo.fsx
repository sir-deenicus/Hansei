#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open System.Diagnostics
open Hansei
open Hansei.Probability
open Hansei.TrieSegmentation

type private ReadingPreference =
    | Compositional
    | Lexicalized

let private benchmark repeats action =
    let timer = Stopwatch.StartNew()

    for _ in 1 .. repeats do
        action () |> ignore

    timer.Stop()
    timer.Elapsed.TotalMilliseconds / float repeats

let private aggregateValues (dist: ProbabilitySpace<'T>) =
    dist
    |> Seq.fold (fun acc (tree, weight) ->
        match tree with
        | Value value ->
            Map.change value (fun existing -> Some (weight + Option.defaultValue 0.0 existing)) acc
        | ContinuedSubTree _ -> acc) Map.empty

let private normalizeMap (dist: Map<'T, float>) =
    let total = dist |> Seq.sumBy (fun kv -> kv.Value)

    dist
    |> Map.map (fun _ weight ->
        if total <= 0.0 then
            0.0
        else
            weight / total)

let private normalizedValues dist =
    dist |> aggregateValues |> normalizeMap

let private unionKeys (a: Map<'T, float>) (b: Map<'T, float>) =
    Set.union (a |> Map.keys |> Set.ofSeq) (b |> Map.keys |> Set.ofSeq)

let private l1Distance (expected: Map<'T, float>) (actual: Map<'T, float>) =
    unionKeys expected actual
    |> Seq.sumBy (fun key ->
        let expectedP = Map.tryFind key expected |> Option.defaultValue 0.0
        let actualP = Map.tryFind key actual |> Option.defaultValue 0.0
        abs (expectedP - actualP))

let private assertPosteriorClose label expected actual tolerance =
    let distance = l1Distance expected actual

    if distance > tolerance then
        failwithf "%s L1 distance %.12f exceeded tolerance %.12f" label distance tolerance

let private printTop title count (dist: Map<string, float>) =
    printfn "%s" title

    dist
    |> Map.toList
    |> List.sortByDescending snd
    |> List.truncate count
    |> List.iter (fun (label, probability) -> printfn "  %-28s %.6f" label probability)

let private readingLabel reading =
    match reading with
    | Compositional -> "reading=Compositional"
    | Lexicalized -> "reading=Lexicalized"

let private readingPrior =
    [ Compositional, 0.5
      Lexicalized, 0.5 ]

let private lexiconEntries =
    [ "the", 0.55
      "rain", 0.35
      "therain", 0.15
      "he", 0.05
      "ran", 0.02 ]

let private trieModel =
    buildTrieSegmentationModel lexiconEntries noBoundaryCost

let private repeatedText = String.replicate 10 "therain"

let private tokenizeSegmentationLabel (segmentationLabel: string) =
    if String.IsNullOrWhiteSpace segmentationLabel then
        [||]
    else
        segmentationLabel.Split([| " | " |], StringSplitOptions.None)

let private segmentationReadingLikelihood reading segmentationLabel =
    let tokens = tokenizeSegmentationLabel segmentationLabel
    let splitTokenCount = tokens |> Array.sumBy (fun token -> if token = "the" || token = "rain" then 1 else 0)
    let mergedTokenCount = tokens |> Array.sumBy (fun token -> if token = "therain" then 1 else 0)

    match reading with
    | Compositional ->
        Math.Pow(0.92, float splitTokenCount) * Math.Pow(0.18, float mergedTokenCount)
    | Lexicalized ->
        Math.Pow(0.18, float splitTokenCount) * Math.Pow(0.92, float mergedTokenCount)

let private matchesAt (text: string) startIndex =
    lexiconEntries
    |> List.choose (fun (word, weight) ->
        if startIndex + word.Length <= text.Length && text.Substring(startIndex, word.Length) = word then
            Some (word, weight)
        else
            None)

let rec private genericSegmentationSubmodel (text: string) startIndex =
    dist {
        if startIndex = text.Length then
            return []
        else
            let matches = matchesAt text startIndex

            match matches with
            | [] ->
                return! fail ()
            | _ ->
                let! (token: string) = distribution matches
                let! suffix = genericSegmentationSubmodel text (startIndex + token.Length)
                return token :: suffix
    }

let private genericSegmentationLabelSubmodel text =
    dist {
        let! tokens = genericSegmentationSubmodel text 0
        return String.concat " | " tokens
    }

let private genericRecursiveReadingModel text =
    dist {
        let! reading = distribution readingPrior
        let! segmentation = genericSegmentationLabelSubmodel text
        do! factor (segmentationReadingLikelihood reading segmentation)
        return readingLabel reading
    }

let private trieBoundReadingModel text =
    dist {
        let! reading = distribution readingPrior
        let! segmentation = Model.TrieSegmentationExactSubmodel(trieModel, text)
        do! factor (segmentationReadingLikelihood reading segmentation)
        return readingLabel reading
    }

let private trieCollapsedReadingModel text =
    dist {
        let! reading = distribution readingPrior
        do! exact_local_likelihood (segmentationReadingLikelihood reading) (Model.TrieSegmentationExactSubmodel(trieModel, text))
        return readingLabel reading
    }

let private printPhase3Case text =
    let genericPosterior = genericRecursiveReadingModel text |> Model.ExactInfer |> normalizedValues
    let trieBoundPosterior = trieBoundReadingModel text |> Model.ExactInfer |> normalizedValues
    let trieCollapsedPosterior = trieCollapsedReadingModel text |> Model.ExactInfer |> normalizedValues

    assertPosteriorClose "Generic vs trie-bound reading posterior" genericPosterior trieBoundPosterior 1e-12
    assertPosteriorClose "Generic vs trie-collapsed reading posterior" genericPosterior trieCollapsedPosterior 1e-12

    printfn "\n=== Phase 3 Hansei Embedding Demo ==="
    printfn "Observed text: %s" text
    printfn "Ambiguous chunk count: %d" (text.Length / "therain".Length)
    printfn "Outer latent variable: reading preference over the same observed string."
    printTop "Generic Hansei recursive segmentation posterior" 2 genericPosterior
    printTop "Hansei + trie exact submodel posterior" 2 trieBoundPosterior
    printTop "Hansei + trie exact_local_likelihood posterior" 2 trieCollapsedPosterior
    printfn "L1(generic, trie bound) = %.6f" (l1Distance genericPosterior trieBoundPosterior)
    printfn "L1(generic, trie collapsed) = %.6f" (l1Distance genericPosterior trieCollapsedPosterior)

    let genericMs = benchmark 8 (fun () -> genericRecursiveReadingModel text |> Model.ExactInfer |> ignore)
    let trieBoundMs = benchmark 16 (fun () -> trieBoundReadingModel text |> Model.ExactInfer |> ignore)
    let trieCollapsedMs = benchmark 32 (fun () -> trieCollapsedReadingModel text |> Model.ExactInfer |> ignore)

    printfn "\nExact inference ms/run"
    printfn "generic recursive segmentation      %.3f" genericMs
    printfn "trie exact submodel bind           %.3f" trieBoundMs
    printfn "trie exact_local_likelihood        %.3f" trieCollapsedMs
    printfn "\nThe last model keeps the outer Hansei program intact while collapsing the segmentation subproblem into one exact local factor."

printfn "=== Trie Segmentation Phase 3 Demo ==="
printfn "This demo shows a Hansei model embedding the specialized trie engine as an exact local submodel instead of expanding segmentation structure generically."
printPhase3Case repeatedText
printfn "\nPhase 3 demo checks passed."