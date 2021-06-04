{-# LANGUAGE ScopedTypeVariables #-}
module Histograms where

import Algebra.Classes hiding (normalize)
import Logits
import Data.Map as M
import Prelude hiding (Num(..),(/),not,recip,sum)
import PreciseProb
import Boolean
import Data.List (find)

type Histogram r = M.Map r Logit

-- | With the additional normalized property (sum of keys = 1)
type DistributionApproximation r = Histogram r

normalize :: (Foldable f, Functor f, Division b, Additive b) => f b -> f b
normalize hs = (/ sum hs) <$> hs

listToHist :: Ord r => [(r,Logit)] -> Histogram r
listToHist = M.fromListWith (+) 

listToDist :: Ord r => [(r,Logit)] -> DistributionApproximation r
listToDist = normalize . listToHist

showHistogram :: Int -> Double -> Double -> Histogram Double -> String
showHistogram n lo hi history = unlines [show (lo + fromIntegral i*delta, p) | (i,p) <- M.toList histogram]
  where getBin :: Double -> Int
        getBin x = floor (fromIntegral n * (x-lo) / (hi-lo))
        delta = (hi-lo)  / fromIntegral n
        histogram = M.fromListWith (+) [(getBin x,p) | (x,p) <- M.toList history]

quantile :: forall a. Logit -> DistributionApproximation a -> a
quantile θ m = case find ((>= θ) . snd) as of
  Just (a,_) -> a
  Nothing -> error "quantile: not found"
  where as :: [(a,Logit)]
        as = scanl (\(_,x) (a,x') -> (a,x + x')) (error "quantile:panic",zero) (M.assocs m)


expectedTruthValue :: DistributionApproximation Bool -> Double
expectedTruthValue c = fromLogit $ sum $ [ w * indicator x | (x,w) <- M.assocs c]
-- attn! do no use expectedValue here to avoid numerical errors.


-- trueProb :: Categorical Truth -> Prob
-- trueProb r = logitToProb $ M.findWithDefault zero True r


expectedValue :: DistributionApproximation Double -> Double
expectedValue c = sum [fromLogit w * x | (x,w) <- M.assocs c]

