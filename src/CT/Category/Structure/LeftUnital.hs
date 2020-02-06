module CT.Category.Structure.LeftUnital where

import Control.Category

import FCI

import CT.Category.Iso

class Category p => LeftUnital i p
  where
  lunit :: Iso p (t i a) a

mkInst ''LeftUnital
