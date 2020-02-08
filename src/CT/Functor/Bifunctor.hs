module CT.Functor.Bifunctor where

import CT.Functor
import CT.Category.Product

import FCI

class GFunctor (p ∧ p) p f => Bifunctor p f

mkInst ''Bifunctor
