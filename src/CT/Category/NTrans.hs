module CT.Category.NTrans where

import Prelude hiding (id, (.))
import Control.Category

import CT.Category.Morphism

newtype f ↝ g = Nat { runNat :: forall x. f x -> g x }

instance Category (↝) where
  Nat f . Nat g = Nat (f . g)
  id = Nat id

(<$~) :: a ↝ b -> a x -> b x
(<$~) = runNat

type (⤜) = (↝) ∋ Monad
