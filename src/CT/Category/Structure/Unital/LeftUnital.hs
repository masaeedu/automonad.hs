module CT.Category.Structure.Unital.LeftUnital where

import Control.Category

import FCI

import CT.Functor.Bifunctor
import CT.Category.Iso

class (Category p, Bifunctor p t) => LeftUnital t i p
  where
  lunit :: Iso p (t '(i, a)) a

mkInst ''LeftUnital
