{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ProbLang where

import Algebra.Classes
import Prelude hiding (Num(..),(/),not,recip,sum,Ord(..))
import qualified Prelude
import Logits
import Boolean
import Control.Applicative

-- | This is an abstract type, so that we know it can be observed only
-- via the Choice/Infer combinator.  This means that we can trace the
-- dependencies on Probabilistic values explicitly; and thus we can
-- reuse bigger parts of traces.
newtype Probabilistic a = Prob {fromProb :: a}
  deriving (Functor,Additive,Multiplicative,Group,AbelianAdditive,Ring,Show,Eq,Prelude.Num,Division,Field,Fractional,Floating)


instance Ring a => Module (Probabilistic a) (Probabilistic a) where
  Prob a *^ Prob b = Prob (a * b)
instance Applicative Probabilistic where
  pure = Prob
  Prob f <*> Prob a = Prob (f a)

instance Boolean a => Boolean (Probabilistic a) where
  indicator (Prob x) = indicator x
  (∧) = liftA2 (∧)
  (∨) = liftA2 (∨)
  not = fmap (not)
  (<->) =  liftA2 (<->)
  fromBool = pure . fromBool

type R = Double

-- | Beta distribution with mean (μ) and sample size (ν)
betaμν :: Probabilistic R -> Probabilistic R -> Distribution R
betaμν μ ν = Beta (μ*ν) ((1-μ)*ν)

-- | Beta distribution with mode (ω) and concentration (κ).  (High concentration means low variance)
betaωκ :: Probabilistic R -> Probabilistic R -> Distribution R
betaωκ ω κ = Beta (ω*(κ-2)+1) ((1-ω)*(κ-2)+1)


data Distribution a where
  Bernoulli :: !(Probabilistic R) -> Distribution Bool
  DiscreteUniform :: Probabilistic [a] -> Distribution a
  Uniform :: !(Probabilistic R) -> !(Probabilistic R) -> Distribution R
  Gaussian :: !(Probabilistic R) -> !(Probabilistic R) -> Distribution R
  Beta :: !(Probabilistic R) -> !(Probabilistic R) -> Distribution R

deriving instance Show a => (Show (Distribution a))

class Monad m => ProbLang m where
  sample :: (Show a, Read a) => Distribution a -> m (Probabilistic a)
  factor :: Probabilistic Logit -> m ()
  if_ :: Probabilistic Bool -> (Bool -> m a) -> m a

observe :: ProbLang m => Probabilistic Bool -> m ()
observe x = factor (fmap indicator x) 



(<),(>) :: (Applicative f, Prelude.Ord a) => f a -> f a -> f Bool
(<) = liftA2 (Prelude.<)
(>) = liftA2 (Prelude.>)

infix 1 <
infix 1 >

-- class Boolean (Comparator a) => Ord' a where
--   type Comparator a 
--   (<) :: a -> a -> Comparator a
-- instance Ord a => Ord' (Probabilistic a) where
--   type Comparator (Probabilistic a) = Probabilistic Bool
--   (<)
  
