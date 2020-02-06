module CT.Category.Structure.Semigroupal where

import Control.Category

import FCI

import CT.Category.Iso

class Category p => Semigroupal t p
  where
  assoc :: Iso p (t a (t b c)) (t (t a b) c)

mkInst ''Semigroupal
