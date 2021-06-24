{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UnicodeSyntax #-}

module DiceExample where

import Algebra.Classes
import ProbLang
import Prelude hiding (Num(..),(/),not,recip,sum,Ord(..))
import Boolean
import Logits
import Exact
import Histograms

die :: ProbLang m => m (Probabilistic Integer)
die = sample (DiscreteUniform (pure [1..6]))

twoDieAbove8 :: ProbLang m => m (Probabilistic Integer, Probabilistic Integer)
twoDieAbove8 = do
  d₁ ← die
  d₂ ← die
  observe (d₁ + d₂ > 8)
  return (d₁,d₂)

problem1 :: ProbLang m => m (Probabilistic Logit)
problem1 = do
  (x,y) ← twoDieAbove8
  return (indicator <$> (x*y > 20))


doit :: DistributionApproximation Logit
doit = evalExact problem1


-- >>> doit
-- fromList [(0.0,0.4000000000000001),(1.0,0.6000000000000001)]
