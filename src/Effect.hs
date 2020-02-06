module Effect where

import FCI

import CT.Category.Entailment
import CT.Category.NTrans
import CT.Category.Morphism
import CT.Functor

-- An effect class is a functor from the category of monads to a category of constraints
class GFunctor (Morphism Monad (↝)) (⇒) f => Effect f

mkInst ''Effect
