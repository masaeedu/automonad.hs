module CT.Category.Structure.Semigroupal where

import Control.Category

import FCI

import CT.Functor.Bifunctor
import CT.Category.Iso

class (Category p, Bifunctor p t) => Semigroupal t p
  where
  assoc :: Iso p (t '(a, (t '(b, c)))) (t '(t '(a, b), c))

mkInst ''Semigroupal
