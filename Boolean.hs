module Boolean where

import qualified Prelude
import Algebra.Classes
import Prelude hiding (Num(..),(/),fromRational,not)

import Logits

class Boolean a where
  (∧) :: a -> a -> a
  (∨) :: a -> a -> a
  a ∨ b = not (not a ∧ not b)
  not :: a -> a
  (-->) :: a -> a -> a
  a --> b = not a ∨ b
  (<->) :: a -> a -> a
  a <-> b = (a --> b) ∧ (b --> a)
  fromBool :: Bool -> a
  indicator :: a -> Logit

(&&),(||) :: Boolean a => a -> a -> a
(&&) = (∧)
(||) = (∨)



instance Boolean Bool where
  fromBool = id
  (∧) = (Prelude.&&)
  (∨) = (Prelude.||)
  not = Prelude.not
  (<->) = (==)
  indicator True = one
  indicator False = zero

