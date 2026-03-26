namespace Hansei

open System
open Hansei.Probability
open Hansei.Exploration
open Hansei.Sampling
open Hansei.IncrementalSampling
open Hansei.StochasticBeam

type Model() =
    static member ImportanceSamples
        (
            distr,
            nsamples,
            maxdepth,
            ?subsample,
            ?preExplore,
            ?sortBeforeSampling,
            ?pre_exploreDepth,
            ?shallowExploreMaxNodes,
            ?preExploreMinMass,
            ?shallowExploreMaxFrontier,
            ?selector
        ) =
        let config =
            createImportanceSamplingConfig
                (defaultArg preExplore true)
                (defaultArg subsample id)
                (defaultArg pre_exploreDepth 3)
                (defaultArg preExploreMinMass 0.0)
                (Some (defaultArg shallowExploreMaxNodes 128))
                (Some (defaultArg shallowExploreMaxFrontier 128))

        runImportanceSamples config nsamples maxdepth (defaultArg sortBeforeSampling false) selector distr

    static member ImportanceSamplesState
        (
            distr,
            nsamples,
            maxdepth,
            ?subsample,
            ?preExplore,
            ?sortBeforeSampling,
            ?pre_exploreDepth,
            ?shallowExploreMaxNodes,
            ?preExploreMinMass,
            ?shallowExploreMaxFrontier,
            ?selector
        ) =
        let config =
            createImportanceSamplingConfig
                (defaultArg preExplore true)
                (defaultArg subsample id)
                (defaultArg pre_exploreDepth 3)
                (defaultArg preExploreMinMass 0.0)
                (Some (defaultArg shallowExploreMaxNodes 128))
                (Some (defaultArg shallowExploreMaxFrontier 128))

        createImportanceSamplingState config nsamples maxdepth (defaultArg sortBeforeSampling false) selector distr

    static member ExactInfer(distr, ?limit) = explore limit distr

    static member PrepareFiniteDag(dag) = DynamicProgrammingDag.prepareDag dag

    static member EvaluateFiniteDag(dag) = DynamicProgrammingDag.evaluateDag dag

    static member InferFiniteDag(engine, dag) = DynamicProgrammingDag.inferFiniteDag engine dag

    static member PrepareTrieSegmentation(model, text) = TrieSegmentation.prepareTrieSegmentation model text

    static member EvaluateTrieSegmentation(model, text) = TrieSegmentation.evaluateTrieSegmentation model text

    static member InferTrieSegmentationExact(model, text) = TrieSegmentation.inferTrieSegmentationExact model text

    static member InferTrieSegmentationMaxProduct(model, text) = TrieSegmentation.inferTrieSegmentationMaxProduct model text

    static member InferTrieSegmentationTopKMaxProduct(topK, model, text) = TrieSegmentation.inferTrieSegmentationTopKMaxProduct topK model text

    static member InferTrieSegmentationBeamApprox(beamWidth, model, text) = TrieSegmentation.inferTrieSegmentationBeamApprox beamWidth model text

    static member TrieSegmentationExactSubmodel(model, text) = TrieSegmentation.trieSegmentationExactSubmodelForModel model text

    static member PathSample(distr, nsamples, ?sortBeforeSampling, ?subsample) =
        sample_path (random_selector (defaultArg sortBeforeSampling true)) (defaultArg subsample id) nsamples distr

    static member PathSampleState(distr, nsamples, ?sortBeforeSampling, ?subsample) =
        createRandomPathSamplingState (defaultArg sortBeforeSampling true) (defaultArg subsample id) nsamples distr
 
    static member GreedySample(distr, nsamples, ?subsample) =
        sample_path (max_selector ()) (defaultArg subsample id) nsamples distr

    static member GreedySampleState(distr, nsamples, ?subsample) =
        createGreedyPathSamplingState (defaultArg subsample id) nsamples distr

    static member BeamSearch(distr, ?beamwidth, ?maxDepth, ?maxExpandedNodes) =
        beam_search
            (defaultArg beamwidth 1)
            maxDepth
            maxExpandedNodes
            distr

    static member StochasticBeam
        (
            distr,
            beamWidth,
            ?maxRounds,
            ?eliteCount,
            ?diversityBucketCount,
            ?lookaheadDepth,
            ?lookaheadStrength,
            ?minBeamWidth,
            ?seed
        ) =
        let beamWidth = max 1 beamWidth
        let effectiveSeed = defaultArg seed (max 1 (abs (Guid.NewGuid().GetHashCode())))
        let config =
            createStochasticBeamConfig
                beamWidth
                (defaultArg maxRounds 256)
                (defaultArg eliteCount 64)
                (defaultArg diversityBucketCount 32)
                (defaultArg lookaheadDepth 1)
                (defaultArg lookaheadStrength 0.5)
                (defaultArg minBeamWidth (min beamWidth (max 256 (beamWidth / 4))))
                effectiveSeed

        stochasticBeam config distr

    static member StochasticBeamState
        (
            distr,
            beamWidth,
            ?maxRounds,
            ?eliteCount,
            ?diversityBucketCount,
            ?lookaheadDepth,
            ?lookaheadStrength,
            ?minBeamWidth,
            ?seed
        ) =
        let beamWidth = max 1 beamWidth
        let effectiveSeed = defaultArg seed (max 1 (abs (Guid.NewGuid().GetHashCode())))
        let config =
            createStochasticBeamConfig
                beamWidth
                (defaultArg maxRounds 256)
                (defaultArg eliteCount 64)
                (defaultArg diversityBucketCount 32)
                (defaultArg lookaheadDepth 1)
                (defaultArg lookaheadStrength 0.5)
                (defaultArg minBeamWidth (min beamWidth (max 256 (beamWidth / 4))))
                effectiveSeed

        createStochasticBeamState config distr


type ModelFrom<'a, 'b when 'b: comparison>(distr: ProbabilitySpace<'b>, ?subsampler) =
    let subsample = defaultArg subsampler id

    member __.model = distr

    member __.ImportanceSample(nsamples, maxdepth, ?preExplore, ?preExploreDepth, ?shallowExploreMaxNodes, ?preExploreMinMass, ?shallowExploreMaxFrontier, ?subsampler, ?selector, ?sortBeforeSampling) =
        let config =
            createImportanceSamplingConfig
                (defaultArg preExplore true)
                (defaultArg subsampler subsample)
                (defaultArg preExploreDepth 3)
                (defaultArg preExploreMinMass 0.0)
                (Some (defaultArg shallowExploreMaxNodes 128))
                (Some (defaultArg shallowExploreMaxFrontier 128))

        runImportanceSamples config nsamples maxdepth (defaultArg sortBeforeSampling false) selector distr

    member __.ImportanceSampleState(nsamples, maxdepth, ?preExplore, ?preExploreDepth, ?shallowExploreMaxNodes, ?preExploreMinMass, ?shallowExploreMaxFrontier, ?subsampler, ?selector, ?sortBeforeSampling) =
        let config =
            createImportanceSamplingConfig
                (defaultArg preExplore true)
                (defaultArg subsampler subsample)
                (defaultArg preExploreDepth 3)
                (defaultArg preExploreMinMass 0.0)
                (Some (defaultArg shallowExploreMaxNodes 128))
                (Some (defaultArg shallowExploreMaxFrontier 128))

        createImportanceSamplingState config nsamples maxdepth (defaultArg sortBeforeSampling false) selector distr

    member __.ExactInfer(?limit) = explore limit distr

    member __.PathSample(nsamples, ?sortBeforeSampling, ?subsampler) =
        sample_path
            (random_selector (defaultArg sortBeforeSampling true))
            (defaultArg subsampler subsample)
            nsamples
            distr

    member __.PathSampleState(nsamples, ?sortBeforeSampling, ?subsampler) =
        createRandomPathSamplingState
            (defaultArg sortBeforeSampling true)
            (defaultArg subsampler subsample)
            nsamples
            distr

    member __.GreedySample(nsamples, ?subsampler) =
        sample_path (max_selector ()) (defaultArg subsampler subsample) nsamples distr

    member __.GreedySampleState(nsamples, ?subsampler) =
        createGreedyPathSamplingState (defaultArg subsampler subsample) nsamples distr

    member __.BeamSearch(?beamwidth, ?maxDepth, ?maxExpandedNodes) =
        beam_search
            (defaultArg beamwidth 1)
            maxDepth
            maxExpandedNodes
            distr

    member __.StochasticBeam(?beamWidth, ?maxRounds, ?eliteCount, ?diversityBucketCount, ?lookaheadDepth, ?lookaheadStrength, ?minBeamWidth, ?seed) =
        let effectiveBeamWidth = defaultArg beamWidth 1024
        let effectiveSeed = defaultArg seed (max 1 (abs (Guid.NewGuid().GetHashCode())))
        let config =
            createStochasticBeamConfig
                effectiveBeamWidth
                (defaultArg maxRounds 256)
                (defaultArg eliteCount 64)
                (defaultArg diversityBucketCount 32)
                (defaultArg lookaheadDepth 1)
                (defaultArg lookaheadStrength 0.5)
                (defaultArg minBeamWidth (min effectiveBeamWidth (max 256 (effectiveBeamWidth / 4))))
                effectiveSeed

        stochasticBeam config distr

    member __.StochasticBeamState(?beamWidth, ?maxRounds, ?eliteCount, ?diversityBucketCount, ?lookaheadDepth, ?lookaheadStrength, ?minBeamWidth, ?seed) =
        let effectiveBeamWidth = defaultArg beamWidth 1024
        let effectiveSeed = defaultArg seed (max 1 (abs (Guid.NewGuid().GetHashCode())))
        let config =
            createStochasticBeamConfig
                effectiveBeamWidth
                (defaultArg maxRounds 256)
                (defaultArg eliteCount 64)
                (defaultArg diversityBucketCount 32)
                (defaultArg lookaheadDepth 1)
                (defaultArg lookaheadStrength 0.5)
                (defaultArg minBeamWidth (min effectiveBeamWidth (max 256 (effectiveBeamWidth / 4))))
                effectiveSeed

        createStochasticBeamState config distr

