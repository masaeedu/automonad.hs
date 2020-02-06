module CT.Category.Iso where

import Prelude hiding (id, (.))
import Control.Category

data Iso p a b = Iso { fwd :: p a b, bwd :: p b a }

instance Category p => Category (Iso p)
  where
  Iso f1 b1 . Iso f2 b2 = Iso (f1 . f2) (b2 . b1)
  id = Iso id id
