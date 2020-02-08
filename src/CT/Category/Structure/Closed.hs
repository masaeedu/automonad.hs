module CT.Category.Structure.Closed where

import FCI

import CT.Category.Iso

class Closed t p

class Apply p where
  compose :: p (p y z) (p (p x y) (p x z))
  -- ^ imagine the instantiation at (->)
  -- @(y -> z) -> ((x -> y) -> (x -> z))
