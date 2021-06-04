{-# LANGUAGE LambdaCase #-}
import Prelude hiding (not)
import ProbLang
import Histograms
import Exact

data Kid = Boy | Girl deriving (Eq,Show,Read,Ord)

randomPair :: ProbLang m => m (Probabilistic (Kid, Kid))
randomPair = do
  x <- sample (DiscreteUniform (pure [Boy,Girl]))
  y <- sample (DiscreteUniform (pure [Boy,Girl]))
  return ((,) <$> x <*> y)

atLeastOneBoy :: (Kid, Kid) -> Bool
atLeastOneBoy (x,y) = x == Boy || y == Boy

twoBoys :: (Kid, Kid) -> Bool
twoBoys (x,y) = x == Boy && y == Boy


problemModel :: ProbLang m => m (Probabilistic Bool)
problemModel = do
  p <- randomPair
  observe (atLeastOneBoy <$> p)
  return (twoBoys <$> p)


doit :: DistributionApproximation Bool
doit = evalExact problemModel


-- >>> doit
-- fromList [(False,0.6666666666666667),(True,0.33333333333333337)]
