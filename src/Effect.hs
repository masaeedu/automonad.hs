module Effect where

import Prelude hiding (map)
import Data.Bifunctor

import FCI

import CT.Category.Entailment
import CT.Category.NTrans
import CT.Category.Morphism
import CT.Functor

-- An effect class is a functor from the category of monads to a category of constraints
type Effect = GFunctor ((↝) ∋ Monad) (⇒)

class (x f, y f) => Product x y f

mkInst ''Product

instance (Effect x, Effect y) => GFunctor ((↝) ∋ Monad) (⇒) (Product x y) where
  map f = Entail $ \(Product x y) -> Product (map f <$= x) (map f <$= y)

class Coproduct x y f where
  it :: Either (Inst (x f)) (Inst (y f))

mkInst ''Coproduct

instance (Effect x, Effect y) => GFunctor ((↝) ∋ Monad) (⇒) (Coproduct x y) where
  map f = Entail $ \(Coproduct x) -> Coproduct $ bimap (map f <$=) (map f <$=) x
