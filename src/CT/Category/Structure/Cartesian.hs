module CT.Category.Structure.Cartesian where

import FCI

import CT.Category.Structure.Unital
import CT.Category.Structure.Semigroupal
import CT.Category.Structure.Monoidal

class Unital i p => Terminal i p where
  discard :: p x i

mkInst ''Terminal

class Semigroupal t p => Duplicative t p where
  duplicate :: p x (t x x)

mkInst ''Duplicative

class (Monoidal t i p, Terminal i p, Duplicative t p) => Cartesian t i p

mkInst ''Cartesian
