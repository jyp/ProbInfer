{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exact where

-- exact evaluation for probabilistic programs
import Logits
import ProbLang
import Algebra.Classes
import Prelude hiding (Num(..),(/),fromRational)
import Control.Monad
import Histograms (listToDist, DistributionApproximation)

newtype FiniteSpace a = FiniteSpace {fromFiniteSpace :: [(a,Logit)]} deriving (Functor,Show)

instance Applicative FiniteSpace where
  pure x = FiniteSpace [(x,one)]
  (<*>) = ap

instance Monad FiniteSpace where
  FiniteSpace l >>= f = FiniteSpace $ do
    (x,p) <- l
    (y,q) <- fromFiniteSpace (f x)
    return (y,p*q)
    
instance ProbLang FiniteSpace where
  sample (Bernoulli (Prob p)) = FiniteSpace [(pure True ,toLogit p),
                                             (pure False,toLogit (1-p))]
  sample (DiscreteUniform (Prob xs)) = FiniteSpace [(pure x,one) | x <- xs]
  sample _ = error "exact computations not supported for continuous spaces"
  factor (Prob l) = FiniteSpace [((),l) | l Prelude.> zero]
  if_ (Prob t) k = k t


evalExact :: Ord r => FiniteSpace (Probabilistic r) -> DistributionApproximation r
evalExact = listToDist . fromFiniteSpace . fmap fromProb
