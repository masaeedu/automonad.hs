module CT.Category.Entailment where

import Prelude hiding (id, (.))
import Control.Category

import FCI

newtype c ⇒ d = Entail { getEntailment :: Inst c -> Inst d }

instance Category (⇒) where
  Entail f . Entail g = Entail (f . g)
  id = Entail id

(<$=) :: a ⇒ b -> Inst a -> Inst b
(<$=) = getEntailment
