module CT.Category.Morphism where

import Prelude hiding (id, (.))
import Control.Category

import FCI

import CT.Category.Entailment

data (p ∋ c) a b = Morph { run :: c a => p a b, preserve :: c a ⇒ c b }

instance Category p => Category (p ∋ c) where
  Morph r1 p1 . Morph r2 p2 = Morph (p2 <$= inst ==> r1 . r2) (p1 . p2)
  id = Morph id id
