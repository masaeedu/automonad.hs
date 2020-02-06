module CT.Category.Structure.Unital where

import CT.Category.Structure.LeftUnital
import CT.Category.Structure.RightUnital

import FCI

class (LeftUnital i p, RightUnital i p) => Unital i p

mkInst ''Unital
