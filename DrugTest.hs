{-# LANGUAGE LambdaCase #-}
module DrugTest where

import Exact
import ProbLang
import Histograms

percent :: Probabilistic R
percent = 0.01

exampleDrug :: ProbLang m => m (Probabilistic Bool)
exampleDrug = do
  -- prior
  isUser <- sample (Bernoulli (0.5 * percent))
  -- evidence
  testedPositive <- if_ isUser $ \case
    True -> sample (Bernoulli (99 * percent))
    False -> sample (Bernoulli (1 * percent))
  observe testedPositive
  -- posterior
  return isUser

main :: DistributionApproximation Bool
main = evalExact exampleDrug

-- >>> main
-- fromList [(False,0.6677852348993287),(True,0.33221476510067105)]
