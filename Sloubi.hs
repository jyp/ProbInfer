{-# LANGUAGE LambdaCase #-}

import MCMC
import Prelude hiding (not, Ord(..))
import ProbLang

player :: P (Probabilistic Double)
player = sample (Gaussian 1000 500)

match :: P (Probabilistic Double)
match = sample (Gaussian 0 100)

-- win :: (Applicative f, Ord a, Num a) => a -> a -> a -> f Bool
win m p q = ((m+p) >  q)

win' :: Probabilistic Double -> Probabilistic Double -> P ()
win' p q = do
  m <- match
  observe (win m p q)

sloubi :: P (Probabilistic Bool)
sloubi = do
  alice <- player
  bob <- player
  charles <- player
  david <- player
  win' alice bob
  win' bob charles
  win' bob david
  m <- match
  return (win m alice david)


-- >>> mcmc 100000 sloubi
-- Prob {fromProb = fromList [(False,1.4389999999999408e-2),(True,0.9856100000000002)]}




