module CT.Category.Structure.RightUnital where

import Control.Category

import FCI

import CT.Category.Iso

class Category p => RightUnital i p
  where
  lunit :: Iso p (t a i) a

mkInst ''RightUnital
