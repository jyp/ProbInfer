{-# LANGUAGE RebindableSyntax #-}
module PreciseProb where

-- The problem we solve here is to represent numbers very close to 1
-- or very close to 0 with sufficient precision.

import Algebra.Classes
import Prelude hiding (Num(..),(/),fromRational,(&&),(||),not)
-- import Boolean

import Logits

data Prob = Almost0 Logit | OneMinus Logit deriving (Eq)


oneMinus :: Logit -> Logit
oneMinus (ExpNeg x) = ExpNeg (- log (1-exp(-x)))

-- >>> toProb 1 * toProb 1
-- 1.0( large )


instance Multiplicative Prob where
  one = OneMinus zero
  Almost0 x * Almost0 y = Almost0 (x * y)
  Almost0 x * OneMinus y = Almost0 (x * oneMinus y)
  OneMinus x * Almost0 y = Almost0 (oneMinus x * y)
  OneMinus x * OneMinus y = logitToProb (oneMinus x * oneMinus y) 

-- instance Additive Prob where
--   zero = Almost0 zero
--   Almost0 x + Almost0 y = logitToProb (x + y)
--   OneMinus x + OneMinus y = 

instance Ord Prob where
  compare (Almost0 x) (Almost0 y) = compare x y
  compare (OneMinus x) (OneMinus y) = compare y x -- note the inversion (not derivable class)
  compare (Almost0 _x) (OneMinus _y) = LT
  compare (OneMinus _x) (Almost0 _y) = GT

oneMinus' :: Logit -> Prob
oneMinus' x@(ExpNeg e) = if e > log 2 then OneMinus x else Almost0 (oneMinus x) 

logitToProb :: Logit -> Prob
logitToProb x@(ExpNeg e) = if e > log 2 then Almost0 x else OneMinus (oneMinus x) 

probToLogit :: Prob -> Logit
probToLogit (Almost0 x) = x
probToLogit (OneMinus x) = oneMinus x

probToDouble :: Prob -> Double
probToDouble = fromLogit . probToLogit

-- | An approximation of the sigmoid function: exp(-x)/2 if x > 0, antisymmetric otherwise.
approxSigmoid :: Double -> Prob
approxSigmoid x | x < 0 = Almost0 (ExpNeg (log 2 - x))
                | otherwise = OneMinus (ExpNeg (log 2 + x))

approxEq :: Double -> Double -> Prob
approxEq x y = logitToProb (ExpNeg (square (x - y)))

approxGT :: Double -> Double -> Prob
approxGT x y = approxSigmoid (x-y)

square :: Multiplicative a => a -> a
square x = x*x

-- >>> approxSigmoid (0.001)
-- 0.5004997500833125( large )

-- >>> approxSigmoid 1
-- 0.8160602794142788( large )

-- >>> approxSigmoid (-1)
-- 0.18393972058572114( small )

-- >>> approxSigmoid 10
-- 0.9999773000351188( large )

-- >>> not (approxSigmoid 100)
-- 1.8600379880104146e-44( small )

instance Show Prob where
  show (Almost0 x) = show x ++ "( small )"
  show (OneMinus x) = show (oneMinus x) ++ "( large )"

-- instance Boolean Prob where
--   indicator = probToLogit
--   fromBool True = one
--   fromBool False = not one
--   not (OneMinus x) = Almost0 x
--   not (Almost0 x) = OneMinus x
--   (∧) = (*)

toProb :: Double -> Prob
toProb x = if x < 0.5 then Almost0 (toLogit x) else OneMinus (toLogit (1-x))


-- instance Fractional Prob where
--   fromRational x = if x >= 0.5 then OneMinus (toLogit (fromRational x)) else Almost0 (toLogit (1 - x))

half :: Prob
half = toProb 0.5

-- >>> half
-- 0.5( large )

quarter :: Prob
quarter = half * half

-- >>> quarter
-- 0.25( small )

-- t1 = quarter ∧ quarter
-- t2 = t1 ∧ t1
-- t3 = t2 ∧ t2
-- t4 = t3 ∧ t3
-- t5 = t4 ∧ t4
-- t6 = (not t5 ∨ t2)

-- >>> t1
-- 6.25e-2( small )

-- >>> t5
-- 5.42101086242753e-20( small )

-- >>> not t2 
-- 0.99609375( large )

-- >>> not t6
-- 5.3998350387461863e-20( small )


-- >>> (0.7 :: Prob)
-- <interactive>:1745:3-5: error:
--     • No instance for (Fractional Prob) arising from the literal ‘0.7’
--     • In the expression: (0.7 :: Prob)
--       In an equation for ‘it’: it = (0.7 :: Prob)
