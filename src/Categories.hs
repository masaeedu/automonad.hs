{-# LANGUAGE InstanceSigs #-}
module Categories where

import Prelude hiding (id, (.))
import Control.Category

import FCI

newtype c ⇒ d = Entail { getEntailment :: Inst c -> Inst d }

instance Category (⇒) where
  Entail f . Entail g = Entail (f . g)
  id = Entail id

(<$=) :: a ⇒ b -> Inst a -> Inst b
(<$=) = getEntailment

newtype f ↝ g = Nat { runNat :: forall x. f x -> g x }

instance Category (↝) where
  Nat f . Nat g = Nat (f . g)
  id = Nat id

data Morphism c p a b = Morph { run :: c a => p a b, preserve :: c a ⇒ c b }

instance Category p => Category (Morphism c p) where
  Morph r1 p1 . Morph r2 p2 = Morph (p2 <$= inst ==> r1 . r2) (p1 . p2)
  id = Morph id id

type f ⤜ g = Morphism Monad (↝) f g

class (Category p, Category q) => GFunctor p q f where
  map :: p a b -> q (f a) (f b)

mkInst ''GFunctor
