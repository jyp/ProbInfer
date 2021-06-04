{-# LANGUAGE LambdaCase #-}

import MCMC
import Prelude hiding (not)
import ProbLang
import Histograms

data Color = Blue | Red deriving (Eq,Show,Ord)

boolToColor :: Bool -> Color
boolToColor = \case
  True -> Blue
  False -> Red

testEq :: Eq x => Probabilistic x -> x -> Probabilistic Bool
testEq x y = fmap (== y) x

exampleBalls :: ProbLang m => m (Probabilistic Color)
exampleBalls = do
  -- a priori distribution of the proportion of blue balls.
  ρ <- sample (Uniform 0 1) -- ρ <- sample (Beta 0.5 0.5) -- alternative
  -- sample a ball in the bag:
  let ball = do
        x <- sample (Bernoulli ρ)
        return (boolToColor <$> x) 
  -- sample a red ball:
  let redBall = do
        b <- ball  -- take a ball
        observe (testEq b Red) -- if it is not red, forget this situation.
  -- sample a blue ball:
  let blueBall = do
        b <- ball
        observe (testEq b Blue)
  redBall
  redBall
  redBall
  blueBall
  x <- ball
  return x

doIt :: Probabilistic (DistributionApproximation Color)
doIt = mcmc 100000 exampleBalls

-- >>> doIt
-- Prob {fromProb = fromList [(Blue,0.27750999999998643),(Red,0.7224900000000142)]}
