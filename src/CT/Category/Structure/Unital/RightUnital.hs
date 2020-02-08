module CT.Category.Structure.Unital.RightUnital where

import Control.Category

import FCI

import CT.Functor.Bifunctor
import CT.Category.Iso

class (Category p, Bifunctor p t) => RightUnital t i p
  where
  lunit :: Iso p (t '(a, i)) a

mkInst ''RightUnital
