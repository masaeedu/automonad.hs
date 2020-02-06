module CT.Category.Structure.Monoidal where

import FCI

import CT.Category.Structure.Semigroupal
import CT.Category.Structure.Unital

class (Semigroupal t p, Unital i p) => Monoidal t i p

mkInst ''Monoidal
