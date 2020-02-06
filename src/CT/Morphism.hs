module CT.Morphism where

import Prelude hiding (id, (.))
import Control.Category

import FCI

import CT.Entailment

data Morphism c p a b = Morph { run :: c a => p a b, preserve :: c a ⇒ c b }

instance Category p => Category (Morphism c p) where
  Morph r1 p1 . Morph r2 p2 = Morph (p2 <$= inst ==> r1 . r2) (p1 . p2)
  id = Morph id id
