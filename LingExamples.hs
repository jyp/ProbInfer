{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
import Control.Monad
import MCMC
import Algebra.Classes
import Prelude hiding (Num(..),(/),sum,Ord(..))
import ProbLang
import Histograms


numberOfDimensions :: Int
numberOfDimensions = 2

sampleVectorOf :: (ProbLang m, Show a, Read a) => Distribution a → m [Probabilistic a]
sampleVectorOf distr = sequence (replicate numberOfDimensions (sample distr))

norm2 :: (Additive a, Multiplicative a) => [a] → a
norm2 x = dotProd x x

norm :: Vec → Probabilistic Double
norm = sqrt . norm2

normalize' :: Vec → Vec
normalize' xs = ((/ norm xs) <$> xs)

dotProd :: (Additive a, Multiplicative a) => [a] → [a] → a
dotProd x y = sum (zipWith (*) x y)

(·) :: (Additive a, Multiplicative a) => [a] → [a] → a
(·) = dotProd

sampleNormedVector :: P Vec
sampleNormedVector = do
  xs ← sampleVectorOf (Gaussian 0 1)
  return (normalize' xs)


type Prop = Probabilistic Bool

type Ind = Vec
type Vec = [Probabilistic R]

type Pred = Ind → Prop

type Grade = Ind → Probabilistic R

is :: Grade → Vec → Prop
is g x = g x > 0

averageFor :: Grade → Pred → Probabilistic Double
averageFor g cn = expectedValue <$> mcmc 1000 sampleGForCn
  where sampleGForCn = do
          y ← sampleInd
          observe (cn y)
          return (g y)
 
-- -- | Subsected graded adj.
subsectiveIs :: Grade → Pred → Vec → Prop
subsectiveIs g cn x = g x > averageFor g cn

more :: Grade → Vec → Vec → Prop
more g x y = g x > g y

samplePredicate :: P Pred
samplePredicate = do
  g ← sampleGrade
  return (is g)


predicateSimple :: P Pred
predicateSimple = do
  v ← sampleNormedVector
  b ← sample (Gaussian 0 1)
  return (\x → (b + x · v) > 0)
  
   -- dot product > 0; ie. a predicates are perceptrons only


sampleGrade :: P Grade
sampleGrade = do
  v ← sampleNormedVector      -- reference vector for the grade
  b ← sample (Gaussian 0 1)   -- reference bias
  return (\x → (b + x · v))

op :: Grade → Grade
op g = negate <$> g

sampleInd :: P Ind
sampleInd = sampleVectorOf (Gaussian 0 1)

























sampleSome :: Pred → P Vec
sampleSome cn = do
  x ← sampleInd
  observe (cn x)
  return x


exampleSocrates2 :: P (Prop)
exampleSocrates2 = do
  θ ← sample (Beta 5 2) -- for example
  man ← samplePredicate
  mortal ← samplePredicate
  observe (atLeast θ man mortal)
  socrates ← sampleInd
  observe (man socrates)
  return (mortal socrates)



atLeast :: Probabilistic Double → Pred → Pred → Prop
atLeast θ cn vp = probability (do x ← sampleSome cn; return (vp x)) > θ

atMost :: Probabilistic Double → Pred → Pred → Prop
atMost θ cn vp = probability (do x ← sampleSome cn; return (vp x)) < θ


most :: Pred → Pred → Prop
most = atLeast 0.8

few :: Pred → Pred → Prop
few = atMost 0.1

every :: Pred → Pred → Prop
every = atLeast 1

forAll :: P t → (t → Prop) → Bool
forAll a φ = probability (do x ← a; return (φ x)) == 1


exampleSocrates :: P (Prop)
exampleSocrates = do
  man ← samplePredicate
  mortal ← samplePredicate
  observe (most man mortal)
  socrates ← sampleInd
  observe (man socrates)
  return (mortal socrates)

-- >>> mcmc 1000 exampleSocrates
-- Prob {fromProb = fromList [(False,0.11499999999999963),(True,0.8850000000000008)]}


exampleSocrates0 :: P (Prop)
exampleSocrates0 = do
  man ← samplePredicate       -- declare predicate
  mortal ← samplePredicate    -- declare predicate
  observe (every man mortal)  -- premiss, interpreted using Montegovian semantics
  socrates ← sampleInd        -- declare individual
  observe (man socrates)      -- premiss, interpreted using Montegovian semantics
  return (mortal socrates)    -- conclusion, interpreted using Montegovian semantics




















exampleTall :: P Prop
exampleTall = do
  tall ← sampleGrade
  john ← sampleInd
  mary ← sampleInd
  observe (more tall john mary)
  return (is tall john)

-- >>> mcmc 1000 exampleTall >>= print
-- fromList [(False,0.3109999999999997),(True,0.6890000000000003)]

















exampleDumbo :: P Prop
exampleDumbo = do
  elephant ← samplePredicate
  mouse ← samplePredicate
  dumbo ← sampleSome elephant
  mickey ← sampleSome elephant
  large ← sampleGrade
  observe (not <$> (subsectiveIs large elephant dumbo)) 
  observe (subsectiveIs large mouse mickey)
  observe (most mouse (\x → most elephant (\y → more large y x)))
  return (more large dumbo mickey)



exampleBirds :: P Prop
exampleBirds = do
   fly ← samplePredicate
   bird ← samplePredicate
   animal ← samplePredicate
   observe (most bird fly)
   observe (few animal fly)
   observe (every bird animal)
   return (most animal (\x -> not <$> (bird x)))

