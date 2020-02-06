module CT.Functor where

import Control.Category

import FCI

class (Category p, Category q) => GFunctor p q f where
  map :: p a b -> q (f a) (f b)

-- mkInst ''GFunctor
