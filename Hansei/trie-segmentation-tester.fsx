#I @"C:\Users\cybernetic\.nuget\packages"
#r "netstandard"
#r @"..\..\Prelude\Prelude\bin\Release\netstandard2.1\Prelude.dll"
#r @"..\Hansei.Continuation\bin\Debug\net50\Hansei.Core.dll"
#r @".\bin\Debug\net50\Hansei.dll"

open System
open Hansei
open Hansei.Probability
open Hansei.TrieSegmentation

let private unionKeys (a: Map<'T, float>) (b: Map<'T, float>) =
    Set.union (a |> Map.keys |> Set.ofSeq) (b |> Map.keys |> Set.ofSeq)

let private l1Distance (expected: Map<'T, float>) (actual: Map<'T, float>) =
    unionKeys expected actual
    |> Seq.sumBy (fun key ->
        let expectedP = Map.tryFind key expected |> Option.defaultValue 0.0
        let actualP = Map.tryFind key actual |> Option.defaultValue 0.0
        abs (expectedP - actualP))

let private normalizedValues dist =
    let aggregated =
        dist
        |> Seq.fold (fun acc (tree, weight) ->
            match tree with
            | Value value ->
                Map.change value (fun existing -> Some (weight + Option.defaultValue 0.0 existing)) acc
            | ContinuedSubTree _ -> acc) Map.empty

    let total = aggregated |> Seq.sumBy (fun kv -> kv.Value)

    aggregated
    |> Map.map (fun _ weight -> if total = 0.0 then 0.0 else weight / total)

let private assertClose label expected actual tolerance =
    if Double.IsNaN actual || abs (expected - actual) > tolerance then
        failwithf "%s expected %.12f but got %.12f" label expected actual

let private assertEqual label expected actual =
    if actual <> expected then
        failwithf "%s expected %A but got %A" label expected actual

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
    |> List.iter (fun (label, probability) -> printfn "  %-30s %.6f" label probability)

let private printTopK title count (candidates: DynamicProgrammingDag.MaxPosteriorResult<string> list) =
    printfn "%s" title

    candidates
    |> List.truncate count
    |> List.iteri (fun index candidate ->
        printfn "  #%d %-26s log=%.6f score=%.6f" (index + 1) candidate.BestLabel candidate.LogScore candidate.Score)

let private exactViaHansei distribution =
    distribution |> Model.ExactInfer |> normalizedValues

let private lexiconEntries =
    [ "the", 0.55
      "rain", 0.35
      "therain", 0.15
      "he", 0.05
      "ran", 0.02 ]

let private exactModel =
    buildTrieSegmentationModel lexiconEntries noBoundaryCost

let private oovBoundaryLogWeight =
    combineBoundaryCostTemplates [ buildPerBreakLogWeight 0.98 ]

let private oovUnknownTokenGenerator =
    buildClassWeightedUnknownUnigramTokenGenerator
        4
        0.015
        0.28
        [ Letter, 0.48
          Digit, 0.09
          Punctuation, 0.12
          Other, 0.18 ]
        [ 'a', 0.60
          'e', 0.62
          'h', 0.44
          'i', 0.58
          'n', 0.55
          'r', 0.53
          't', 0.56
          'x', 0.18
          '1', 0.08 ]
        0.82
        [ Letter, 0.92
          Digit, 0.35
          Punctuation, 0.45
          Other, 0.55 ]
        []

let private oovModel =
    buildTrieSegmentationModelWithUnknown lexiconEntries oovBoundaryLogWeight oovUnknownTokenGenerator

let private printExactCase () =
    let text = "therain"
    let prepared = Model.PrepareTrieSegmentation(exactModel, text)
    let posterior = Model.EvaluateTrieSegmentation(exactModel, text)
    let exact = prepared.ExactPosterior()
    let beam = Model.InferTrieSegmentationBeamApprox(6, exactModel, text)
    let topK = Model.InferTrieSegmentationTopKMaxProduct(2, exactModel, text)
    let mapResult = Model.InferTrieSegmentationMaxProduct(exactModel, text)
    let adapterPosterior = Model.TrieSegmentationExactSubmodel(exactModel, text) |> exactViaHansei

    let expectedEvidence = 0.55 * 0.35 + 0.15
    let expectedPosterior =
        let total = expectedEvidence
        Map.ofList
            [ "the | rain", (0.55 * 0.35) / total
              "therain", 0.15 / total ]

    assertPosteriorClose "Exact trie posterior" expectedPosterior posterior 1e-12
    assertPosteriorClose "Exact vs beam posterior" exact.Posterior beam.Posterior 1e-12
    assertPosteriorClose "Exact vs Hansei adapter posterior" exact.Posterior adapterPosterior 1e-12
    assertEqual "Exact MAP label" "the | rain" mapResult.BestLabel
    assertEqual "Exact top-k winner" "the | rain" topK.Head.BestLabel
    assertClose "Exact evidence" expectedEvidence exact.Evidence 1e-12
    assertClose "Exact log evidence" (log expectedEvidence) exact.LogEvidence 1e-12

    printfn "\n=== Trie Segmentation Example ==="
    printfn "Observed text: %s" text
    printTop "Top exact segmentations" 4 posterior
    printTop "Top beam segmentations (beam=6)" 4 beam.Posterior
    printTopK "Top MAP paths" 2 topK
    printfn "Evidence (trie exact) = %.6f" exact.Evidence
    printfn "Log evidence (trie exact) = %.6f" exact.LogEvidence
    printfn "MAP segmentation = %s (score %.6f)" mapResult.BestLabel mapResult.Score
    printfn "L1(trie exact, Hansei adapter exact) = %.6f" (l1Distance exact.Posterior adapterPosterior)
    printfn "L1(trie exact, trie beam approx) = %.6f" (l1Distance exact.Posterior beam.Posterior)

let private printOovCase () =
    let text = "thera1n"
    let prepared = Model.PrepareTrieSegmentation(oovModel, text)
    let exact = prepared.ExactPosterior()
    let beam = prepared.BeamApproximation(8)
    let topK = prepared.TopKMaxPosterior(5)
    let mapResult = prepared.MaxPosterior()
    let adapterPosterior = prepared.ToProbabilitySpace() |> exactViaHansei

    assertEqual "OOV segmentation count" 8 exact.Posterior.Count
    assertPosteriorClose "OOV exact vs beam posterior" exact.Posterior beam.Posterior 1e-12
    assertPosteriorClose "OOV exact vs Hansei adapter posterior" exact.Posterior adapterPosterior 1e-12
    assertEqual "OOV MAP label" "the | <unk:ra1n>" mapResult.BestLabel
    assertEqual "OOV top-k winner" "the | <unk:ra1n>" topK.Head.BestLabel

    printfn "\n=== Trie Segmentation With OOV Fallback ==="
    printfn "Observed text: %s" text
    printfn "Total segmentations: %d" exact.Posterior.Count
    printTop "Top exact segmentations" 5 exact.Posterior
    printTop "Top beam segmentations (beam=8)" 5 beam.Posterior
    printTopK "Top MAP paths" 5 topK
    printfn "Evidence (trie exact) = %.6f" exact.Evidence
    printfn "Log evidence (trie exact) = %.6f" exact.LogEvidence
    printfn "MAP segmentation = %s (score %.6f)" mapResult.BestLabel mapResult.Score
    printfn "L1(trie exact, Hansei adapter exact) = %.6f" (l1Distance exact.Posterior adapterPosterior)
    printfn "L1(trie exact, trie beam approx) = %.6f" (l1Distance exact.Posterior beam.Posterior)

printfn "=== Trie Segmentation Phase 2 Tester ==="
printfn "Integrated Phase 2 covers specialized trie segmentation with exact, top-k max-product, beam approximation, and Hansei adapter paths."
printExactCase ()
printOovCase ()
printfn "\nAll Phase 2 trie checks passed."