namespace Hansei.DiscreteProb

open MathNet.Numerics  
open MathNet.Symbolics
open MathNet.Symbolics.NumberProperties

open ListProb

module VisualProb =
    open VisualProb 

    module Symbolic =
        let dist = dist 1Q
        let uniform l = uniform Expression.FromInt32 1Q l
        let observe = observe 1Q
        let constrain = observe
        let bernoulli p = bernoulli 1Q p
        let bernoulliChoice a b p = bernoulliChoice 1Q a b p
        let always x = always 1Q x 
        let uniformPrior (n:int) baseps = uniformPrior (float >> Expression.fromFloat64) 1Q baseps

module ListProb = 

    module Rational =
        let dist = dist 1N
        let uniform l = uniform BigRational.FromInt 1N l 
        let observe = observe 1N
        let constrain = observe
        let bernoulli p = bernoulli 1N p
        let bernoulliChoice a b p = bernoulliChoice 1N a b p
        let always x = always 1N x
        let uniformPrior (n:int) baseps = uniformPrior (decimal >> BigRational.FromDecimal) 1N baseps
 
    module Symbolic =
        let dist = dist 1Q
        let uniform l = uniform Expression.FromInt32 1Q l
        let uniformN x (N:Expression) = [x, 1/N]
        let normal m s x = [x, ProbabilityDistributions.Normal(m,s).Prob x]
        let binomial n p k = [k, ProbabilityDistributions.Binomial(n,p).Prob k]
        let observe = observe 1Q
        let constrain = observe
        let bernoulli p = bernoulli 1Q p
        let bernoulliChoice a b p = bernoulliChoice 1Q a b p
        let always x = always 1Q x
        let uniformPrior (n:int) baseps = uniformPrior (float >> Expression.fromFloat64) 1Q baseps