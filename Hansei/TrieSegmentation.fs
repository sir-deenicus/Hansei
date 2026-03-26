module Hansei.TrieSegmentation

open System
open System.Collections.Generic
open Hansei.Distributions
open Hansei.DynamicProgrammingDag
open Hansei.Probability

type BoundaryLogWeightTemplate = string option -> string -> float

type UnknownCharacterClass =
    | Letter
    | Digit
    | WhiteSpace
    | Punctuation
    | Other

type internal TrieMatch =
    {
        EndExclusive: int
        WordId: int
        LogWeight: float
    }

type internal TrieWord =
    {
        Id: int
        Text: string
        LogWeight: float
    }

type internal TrieNode =
    {
        Id: int
        ChildCharacters: char[]
        ChildIds: int[]
        TerminalWordId: int option
    }

type internal TrieLexicon =
    {
        Nodes: TrieNode[]
        RootId: int
        Words: TrieWord[]
    }

type internal TrieTokenCatalog =
    {
        Lexicon: TrieLexicon
        DynamicWordIds: Dictionary<string, int>
        DynamicWords: ResizeArray<string>
    }

type TrieUnknownTokenGenerator internal (run: TrieTokenCatalog -> string -> int -> TrieMatch list) =
    member internal _.Run(catalog, text, startIndex) = run catalog text startIndex

type internal CompiledTrieBoundaryWeights =
    {
        StartLogWeights: float[]
        TransitionLogWeights: float[]
        FallbackLogWeight: BoundaryLogWeightTemplate
    }

type TrieSegmentationModel internal (lexicon: TrieLexicon, boundaryWeights: CompiledTrieBoundaryWeights, unknownTokenGenerator: TrieUnknownTokenGenerator) =
    member internal _.Lexicon = lexicon
    member internal _.BoundaryWeights = boundaryWeights
    member internal _.UnknownTokenGenerator = unknownTokenGenerator
    member _.KnownWordCount = lexicon.Words.Length
    member _.KnownWords = lexicon.Words |> Array.map (fun word -> word.Text)

type internal PreparedTrieSegmentationContext =
    {
        Model: TrieSegmentationModel
        Text: string
        Catalog: TrieTokenCatalog
        KnownMatchesByStart: TrieMatch[][]
        UnknownMatchesByStart: Dictionary<int, TrieMatch list>
        BoundaryMemo: Dictionary<struct (int option * int), float>
    }

type internal TriePartialCandidate =
    {
        Position: int
        PreviousWordId: int option
        WordIdsReversed: int list
        LogScore: float
    }

type internal TrieSegmentationCandidate =
    {
        WordIds: int list
        LogScore: float
    }

let noBoundaryCost : BoundaryLogWeightTemplate = fun _ _ -> 0.0

let internal noUnknownTokenGenerator = TrieUnknownTokenGenerator(fun _ _ _ -> [])

let internal buildTrieLexicon (entries: (string * float) list) =
    let childrenById = Dictionary<int, Dictionary<char, int>>()
    let terminalsById = Dictionary<int, int>()
    let mutable nextId = 1
    let rootId = 0
    childrenById.[rootId] <- Dictionary<char, int>()

    let words =
        entries
        |> List.mapi (fun wordId (word, weight) ->
            {
                Id = wordId
                Text = word
                LogWeight = logWeight weight
            })
        |> List.toArray

    for wordEntry in words do
        let mutable nodeId = rootId

        for character in wordEntry.Text do
            let children = childrenById.[nodeId]

            let childId =
                match children.TryGetValue character with
                | true, existing -> existing
                | _ ->
                    let created = nextId
                    nextId <- nextId + 1
                    children.[character] <- created
                    childrenById.[created] <- Dictionary<char, int>()
                    created

            nodeId <- childId

        terminalsById.[nodeId] <- wordEntry.Id

    let nodes =
        Array.init nextId (fun nodeId ->
            let children =
                childrenById.[nodeId]
                |> Seq.map (fun (KeyValue(character, childId)) -> character, childId)
                |> Seq.sortBy fst
                |> Seq.toArray

            let terminalWordId =
                match terminalsById.TryGetValue nodeId with
                | true, entry -> Some entry
                | _ -> None

            {
                Id = nodeId
                ChildCharacters = children |> Array.map fst
                ChildIds = children |> Array.map snd
                TerminalWordId = terminalWordId
            })

    {
        Nodes = nodes
        RootId = rootId
        Words = words
    }

let internal classifyUnknownCharacter (character: char) =
    if Char.IsLetter character then
        Letter
    elif Char.IsDigit character then
        Digit
    elif Char.IsWhiteSpace character then
        WhiteSpace
    elif Char.IsPunctuation character then
        Punctuation
    else
        Other

let combineBoundaryCostTemplates (templates: BoundaryLogWeightTemplate list) : BoundaryLogWeightTemplate =
    fun previousWord currentWord ->
        templates |> List.sumBy (fun template -> template previousWord currentWord)

let buildPerBreakLogWeight weight : BoundaryLogWeightTemplate =
    let breakLogWeight = logWeight weight
    fun _ _ -> breakLogWeight

let buildSimpleBigramWordModelLogWeight defaultWeight (entries: (((string option) * string) * float) seq) : BoundaryLogWeightTemplate =
    let table = entries |> Map.ofSeq
    let defaultLogWeight = logWeight defaultWeight

    fun previousWord currentWord ->
        table
        |> Map.tryFind (previousWord, currentWord)
        |> Option.map logWeight
        |> Option.defaultValue defaultLogWeight

let buildWordTransitionLogWeight defaultWeight (entries: (((string option) * string) * float) seq) =
    buildSimpleBigramWordModelLogWeight defaultWeight entries

let internal createTrieTokenCatalog lexicon =
    {
        Lexicon = lexicon
        DynamicWordIds = Dictionary<string, int>()
        DynamicWords = ResizeArray<string>()
    }

let internal knownTrieWordCount (catalog: TrieTokenCatalog) =
    catalog.Lexicon.Words.Length

let internal trieTokenText (catalog: TrieTokenCatalog) wordId =
    if wordId < knownTrieWordCount catalog then
        catalog.Lexicon.Words.[wordId].Text
    else
        catalog.DynamicWords.[wordId - knownTrieWordCount catalog]

let internal internDynamicTrieToken (catalog: TrieTokenCatalog) tokenText =
    match catalog.DynamicWordIds.TryGetValue tokenText with
    | true, cached -> cached
    | _ ->
        let wordId = knownTrieWordCount catalog + catalog.DynamicWords.Count
        catalog.DynamicWords.Add tokenText
        catalog.DynamicWordIds.[tokenText] <- wordId
        wordId

let internal compileTrieBoundaryWeights (lexicon: TrieLexicon) (boundaryLogWeight: BoundaryLogWeightTemplate) =
    let words = lexicon.Words
    let wordCount = words.Length

    {
        StartLogWeights = words |> Array.map (fun word -> boundaryLogWeight None word.Text)
        TransitionLogWeights =
            [|
                for previousWord in words do
                    for currentWord in words do
                        yield boundaryLogWeight (Some previousWord.Text) currentWord.Text
            |]
        FallbackLogWeight = boundaryLogWeight
    }

let internal trieBoundaryLogWeight (model: TrieSegmentationModel) (catalog: TrieTokenCatalog) previousWordId currentWordId =
    let knownWordCount = model.Lexicon.Words.Length

    if currentWordId < knownWordCount then
        match previousWordId with
        | None -> model.BoundaryWeights.StartLogWeights.[currentWordId]
        | Some previousKnownWordId when previousKnownWordId < knownWordCount ->
            model.BoundaryWeights.TransitionLogWeights.[previousKnownWordId * knownWordCount + currentWordId]
        | Some previousDynamicWordId ->
            model.BoundaryWeights.FallbackLogWeight (Some (trieTokenText catalog previousDynamicWordId)) model.Lexicon.Words.[currentWordId].Text
    else
        model.BoundaryWeights.FallbackLogWeight (previousWordId |> Option.map (trieTokenText catalog)) (trieTokenText catalog currentWordId)

let internal buildUnknownCharacterLogWeight defaultClassWeight (classEntries: (UnknownCharacterClass * float) seq) (characterEntries: (char * float) seq) =
    let classTable = classEntries |> Map.ofSeq
    let characterTable = characterEntries |> Map.ofSeq
    let defaultClassLogWeight = logWeight defaultClassWeight

    fun character ->
        characterTable
        |> Map.tryFind character
        |> Option.map logWeight
        |> Option.defaultWith (fun () ->
            classTable
            |> Map.tryFind (classifyUnknownCharacter character)
            |> Option.map logWeight
            |> Option.defaultValue defaultClassLogWeight)

let internal buildUnknownEndLogWeight defaultWeight (classEntries: (UnknownCharacterClass * float) seq) (characterEntries: (char * float) seq) =
    let classTable = classEntries |> Map.ofSeq
    let characterTable = characterEntries |> Map.ofSeq
    let defaultLogWeight = logWeight defaultWeight

    fun (terminalCharacter: char option) ->
        match terminalCharacter with
        | None -> defaultLogWeight
        | Some character ->
            characterTable
            |> Map.tryFind character
            |> Option.map logWeight
            |> Option.defaultWith (fun () ->
                classTable
                |> Map.tryFind (classifyUnknownCharacter character)
                |> Option.map logWeight
                |> Option.defaultValue defaultLogWeight)

let internal buildBoundedUnknownUnigramTokenGeneratorWithScorers maxLength tokenWeight characterLogWeight endLogWeight =
    let tokenLogWeight = logWeight tokenWeight

    TrieUnknownTokenGenerator(fun catalog text startIndex ->
        let maxEndExclusive = min text.Length (startIndex + maxLength)

        [ startIndex + 1 .. maxEndExclusive ]
        |> List.map (fun endExclusive ->
            let chunk = text.Substring(startIndex, endExclusive - startIndex)
            let charLogWeight = chunk |> Seq.sumBy characterLogWeight

            let endOfUnknownLogWeight =
                if String.IsNullOrEmpty chunk then endLogWeight None else endLogWeight (Some chunk.[chunk.Length - 1])

            {
                EndExclusive = endExclusive
                WordId = internDynamicTrieToken catalog ("<unk:" + chunk + ">")
                LogWeight = tokenLogWeight + charLogWeight + endOfUnknownLogWeight
            }))

let internal buildBoundedUnknownBigramTokenGeneratorWithScorers maxLength tokenWeight transitionLogWeight endLogWeight =
    let tokenLogWeight = logWeight tokenWeight

    TrieUnknownTokenGenerator(fun catalog text startIndex ->
        let maxEndExclusive = min text.Length (startIndex + maxLength)

        [ startIndex + 1 .. maxEndExclusive ]
        |> List.map (fun endExclusive ->
            let chunk = text.Substring(startIndex, endExclusive - startIndex)

            let previousCharacter, transitionScore =
                chunk
                |> Seq.fold
                    (fun (previousCharacter, accLogWeight) character ->
                        Some character, accLogWeight + transitionLogWeight previousCharacter character)
                    (None, tokenLogWeight)

            {
                EndExclusive = endExclusive
                WordId = internDynamicTrieToken catalog ("<unk:" + chunk + ">")
                LogWeight = transitionScore + endLogWeight previousCharacter
            }))

let buildClassWeightedUnknownUnigramTokenGenerator maxLength tokenWeight defaultCharacterClassWeight classEntries characterEntries defaultEndWeight endClassEntries endCharacterEntries =
    let characterLogWeight = buildUnknownCharacterLogWeight defaultCharacterClassWeight classEntries characterEntries
    let endLogWeight = buildUnknownEndLogWeight defaultEndWeight endClassEntries endCharacterEntries
    buildBoundedUnknownUnigramTokenGeneratorWithScorers maxLength tokenWeight characterLogWeight endLogWeight

let buildClassWeightedUnknownBigramTokenGenerator maxLength tokenWeight defaultTransitionWeight (entries: (((char option) * char) * float) seq) defaultEndWeight endClassEntries endCharacterEntries =
    let table = entries |> Map.ofSeq
    let defaultTransitionLogWeight = logWeight defaultTransitionWeight

    let transitionLogWeight previousCharacter character =
        table
        |> Map.tryFind (previousCharacter, character)
        |> Option.map logWeight
        |> Option.defaultValue defaultTransitionLogWeight

    let endLogWeight = buildUnknownEndLogWeight defaultEndWeight endClassEntries endCharacterEntries
    buildBoundedUnknownBigramTokenGeneratorWithScorers maxLength tokenWeight transitionLogWeight endLogWeight

let buildBoundedUnknownUnigramTokenGenerator maxLength tokenWeight defaultCharacterWeight (entries: (char * float) seq) =
    buildClassWeightedUnknownUnigramTokenGenerator
        maxLength
        tokenWeight
        defaultCharacterWeight
        []
        entries
        1.0
        []
        []

let buildBoundedUnknownBigramTokenGenerator maxLength tokenWeight defaultTransitionWeight (entries: (((char option) * char) * float) seq) =
    buildClassWeightedUnknownBigramTokenGenerator
        maxLength
        tokenWeight
        defaultTransitionWeight
        entries
        1.0
        []
        []

let buildTrieSegmentationModelWithUnknown lexiconEntries boundaryLogWeight unknownTokenGenerator =
    let lexicon = lexiconEntries |> Seq.toList |> buildTrieLexicon
    TrieSegmentationModel(lexicon, compileTrieBoundaryWeights lexicon boundaryLogWeight, unknownTokenGenerator)

let buildTrieSegmentationModel lexiconEntries boundaryLogWeight =
    buildTrieSegmentationModelWithUnknown lexiconEntries boundaryLogWeight noUnknownTokenGenerator

let internal tryFindTrieChildId (node: TrieNode) character =
    let index = Array.BinarySearch(node.ChildCharacters, character)

    if index >= 0 then
        Some node.ChildIds.[index]
    else
        None

let internal trieMatchesAt (model: TrieSegmentationModel) (text: string) startIndex =
    let mutable nodeId = model.Lexicon.RootId
    let matches = ResizeArray<TrieMatch>()
    let mutable index = startIndex
    let mutable keepScanning = true

    while keepScanning && index < text.Length do
        let node = model.Lexicon.Nodes.[nodeId]

        match tryFindTrieChildId node text.[index] with
        | Some childId ->
            nodeId <- childId
            index <- index + 1

            match model.Lexicon.Nodes.[nodeId].TerminalWordId with
            | Some wordId ->
                matches.Add
                    {
                        EndExclusive = index
                        WordId = wordId
                        LogWeight = model.Lexicon.Words.[wordId].LogWeight
                    }
            | None -> ()
        | None ->
            keepScanning <- false

    matches |> Seq.toList

let internal buildKnownTrieMatchesByStart (model: TrieSegmentationModel) (text: string) =
    Array.init (text.Length + 1) (fun startIndex ->
        if startIndex < text.Length then
            trieMatchesAt model text startIndex |> List.toArray
        else
            [||])

let internal prepareTrieSegmentationContext (model: TrieSegmentationModel) (text: string) =
    {
        Model = model
        Text = text
        Catalog = createTrieTokenCatalog model.Lexicon
        KnownMatchesByStart = buildKnownTrieMatchesByStart model text
        UnknownMatchesByStart = Dictionary<int, TrieMatch list>()
        BoundaryMemo = Dictionary<struct (int option * int), float>()
    }

let internal preparedTrieTokenMatches (context: PreparedTrieSegmentationContext) startIndex =
    let knownMatches = context.KnownMatchesByStart.[startIndex]

    if knownMatches.Length > 0 then
        knownMatches |> Array.toList
    else
        match context.UnknownMatchesByStart.TryGetValue startIndex with
        | true, cached -> cached
        | _ ->
            let matches = context.Model.UnknownTokenGenerator.Run(context.Catalog, context.Text, startIndex)
            context.UnknownMatchesByStart.[startIndex] <- matches
            matches

let internal preparedTrieBoundaryLogWeight (context: PreparedTrieSegmentationContext) previousWordId currentWordId =
    let knownWordCount = context.Model.Lexicon.Words.Length

    if currentWordId < knownWordCount then
        match previousWordId with
        | None -> context.Model.BoundaryWeights.StartLogWeights.[currentWordId]
        | Some previousKnownWordId when previousKnownWordId < knownWordCount ->
            context.Model.BoundaryWeights.TransitionLogWeights.[previousKnownWordId * knownWordCount + currentWordId]
        | _ ->
            let key = struct (previousWordId, currentWordId)

            match context.BoundaryMemo.TryGetValue key with
            | true, cached -> cached
            | _ ->
                let computed = trieBoundaryLogWeight context.Model context.Catalog previousWordId currentWordId
                context.BoundaryMemo.[key] <- computed
                computed
    else
        let key = struct (previousWordId, currentWordId)

        match context.BoundaryMemo.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let computed = trieBoundaryLogWeight context.Model context.Catalog previousWordId currentWordId
            context.BoundaryMemo.[key] <- computed
            computed

let internal trieLabelOfWordIds (catalog: TrieTokenCatalog) wordIds =
    wordIds
    |> List.map (trieTokenText catalog)
    |> String.concat " | "

let internal trieExactPosteriorResultFromLog (catalog: TrieTokenCatalog) entries =
    let rawLog =
        entries
        |> Seq.fold (fun acc (wordIds, logValue) -> addLogWeight wordIds logValue acc) Map.empty

    let logEvidence = rawLog |> Seq.map (fun kv -> kv.Value) |> Seq.toArray |> logSumExp

    let posterior =
        rawLog
        |> Seq.map (fun (KeyValue(wordIds, logValue)) ->
            let probability =
                if Double.IsNegativeInfinity logValue || Double.IsNegativeInfinity logEvidence then
                    0.0
                else
                    exp (logValue - logEvidence)

            trieLabelOfWordIds catalog wordIds, probability)
        |> Map.ofSeq

    let evidence =
        if Double.IsNegativeInfinity logEvidence then
            0.0
        else
            exp logEvidence

    {
        Posterior = posterior
        Evidence = evidence
        LogEvidence = logEvidence
    }

let internal inferPreparedTrieSegmentationExact (context: PreparedTrieSegmentationContext) =
    let memo = Dictionary<struct (int * int option), (int list * float) list>()

    let rec segmentationsFrom startIndex previousWord =
        let key = struct (startIndex, previousWord)

        match memo.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let segmentations =
                if startIndex = context.Text.Length then
                    [ [], 0.0 ]
                else
                    preparedTrieTokenMatches context startIndex
                    |> List.collect (fun trieMatch ->
                        let localLogWeight = preparedTrieBoundaryLogWeight context previousWord trieMatch.WordId + trieMatch.LogWeight

                        segmentationsFrom trieMatch.EndExclusive (Some trieMatch.WordId)
                        |> List.map (fun (suffixWordIds, suffixLogWeight) ->
                            trieMatch.WordId :: suffixWordIds, localLogWeight + suffixLogWeight))

            memo.[key] <- segmentations
            segmentations

    segmentationsFrom 0 None |> trieExactPosteriorResultFromLog context.Catalog

let internal inferPreparedTrieSegmentationTopKMaxProduct topK (context: PreparedTrieSegmentationContext) =
    let memo = Dictionary<struct (int * int option), TrieSegmentationCandidate list>()

    let rec topKFrom startIndex previousWord =
        let key = struct (startIndex, previousWord)

        match memo.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let candidates =
                if startIndex = context.Text.Length then
                    [ { WordIds = []; LogScore = 0.0 } ]
                else
                    preparedTrieTokenMatches context startIndex
                    |> List.collect (fun trieMatch ->
                        let localLogWeight = preparedTrieBoundaryLogWeight context previousWord trieMatch.WordId + trieMatch.LogWeight

                        topKFrom trieMatch.EndExclusive (Some trieMatch.WordId)
                        |> List.map (fun suffix ->
                            {
                                WordIds = trieMatch.WordId :: suffix.WordIds
                                LogScore = localLogWeight + suffix.LogScore
                            }))
                    |> List.distinctBy (fun candidate -> candidate.WordIds)
                    |> List.sortByDescending (fun candidate -> candidate.LogScore)
                    |> List.truncate topK

            memo.[key] <- candidates
            candidates

    topKFrom 0 None
    |> List.map (fun candidate ->
        let label = trieLabelOfWordIds context.Catalog candidate.WordIds
        let score = if Double.IsNegativeInfinity candidate.LogScore then 0.0 else exp candidate.LogScore

        {
            BestLabel = label
            Score = score
            LogScore = candidate.LogScore
        })

let internal inferTrieSegmentationMaxProductFromPrepared (context: PreparedTrieSegmentationContext) =
    match inferPreparedTrieSegmentationTopKMaxProduct 1 context with
    | best :: _ -> best
    | [] ->
        {
            BestLabel = "<no segmentation>"
            Score = 0.0
            LogScore = negativeInfinity
        }

let internal inferPreparedTrieSegmentationBeamApprox beamWidth (context: PreparedTrieSegmentationContext) =
    let rec loop frontier completed =
        match frontier with
        | [] -> completed |> trieExactPosteriorResultFromLog context.Catalog
        | _ ->
            let expandedBuffer = ResizeArray<TriePartialCandidate>()
            let completedBuffer = ResizeArray<int list * float>()

            for candidate in frontier do
                if candidate.Position = context.Text.Length then
                    completedBuffer.Add(List.rev candidate.WordIdsReversed, candidate.LogScore)
                else
                    for trieMatch in preparedTrieTokenMatches context candidate.Position do
                        let nextLogScore =
                            candidate.LogScore
                            + preparedTrieBoundaryLogWeight context candidate.PreviousWordId trieMatch.WordId
                            + trieMatch.LogWeight

                        expandedBuffer.Add
                            {
                                Position = trieMatch.EndExclusive
                                PreviousWordId = Some trieMatch.WordId
                                WordIdsReversed = trieMatch.WordId :: candidate.WordIdsReversed
                                LogScore = nextLogScore
                            }

            let nextFrontier =
                if expandedBuffer.Count = 0 then
                    []
                else
                    let expanded = expandedBuffer.ToArray()
                    Array.Sort(expanded, Comparison<TriePartialCandidate>(fun left right -> compare right.LogScore left.LogScore))

                    expanded
                    |> Seq.truncate beamWidth
                    |> Seq.toList

            let allCompleted =
                if completedBuffer.Count = 0 then
                    completed
                else
                    completedBuffer |> Seq.toList |> List.append completed

            loop nextFrontier allCompleted

    loop [ { Position = 0; PreviousWordId = None; WordIdsReversed = []; LogScore = 0.0 } ] []

type PreparedTrieSegmentation internal (context: PreparedTrieSegmentationContext) =
    member _.Text = context.Text

    member _.ExactPosterior() =
        inferPreparedTrieSegmentationExact context

    member _.Evaluate() =
        inferPreparedTrieSegmentationExact context |> fun result -> result.Posterior

    member _.MaxPosterior() =
        inferTrieSegmentationMaxProductFromPrepared context

    member _.TopKMaxPosterior(topK) =
        inferPreparedTrieSegmentationTopKMaxProduct (max 1 topK) context

    member _.BeamApproximation(beamWidth) =
        inferPreparedTrieSegmentationBeamApprox (max 1 beamWidth) context

    member _.ToProbabilitySpace() : ProbabilitySpace<string> =
        inferPreparedTrieSegmentationExact context
        |> fun result -> result.Posterior |> Map.toList |> categorical

let prepareTrieSegmentation model text =
    PreparedTrieSegmentation(prepareTrieSegmentationContext model text)

let inferTrieSegmentationExact model text =
    prepareTrieSegmentation model text |> fun prepared -> prepared.ExactPosterior()

let evaluateTrieSegmentation model text =
    prepareTrieSegmentation model text |> fun prepared -> prepared.Evaluate()

let inferTrieSegmentationMaxProduct model text =
    prepareTrieSegmentation model text |> fun prepared -> prepared.MaxPosterior()

let inferTrieSegmentationTopKMaxProduct topK model text =
    prepareTrieSegmentation model text |> fun prepared -> prepared.TopKMaxPosterior(topK)

let inferTrieSegmentationBeamApprox beamWidth model text =
    prepareTrieSegmentation model text |> fun prepared -> prepared.BeamApproximation(beamWidth)

let trieSegmentationExactSubmodelForModel (model: TrieSegmentationModel) (text: string) : ProbabilitySpace<string> =
    prepareTrieSegmentation model text |> fun prepared -> prepared.ToProbabilitySpace()

let trieSegmentationExactSubmodel lexiconEntries boundaryLogWeight (text: string) : ProbabilitySpace<string> =
    let model = buildTrieSegmentationModel lexiconEntries boundaryLogWeight
    trieSegmentationExactSubmodelForModel model text