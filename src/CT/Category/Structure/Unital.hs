module CT.Category.Structure.Unital where

import CT.Category.Structure.Unital.LeftUnital
import CT.Category.Structure.Unital.RightUnital

import FCI

class (LeftUnital t i p, RightUnital t i p) => Unital t i p

mkInst ''Unital
