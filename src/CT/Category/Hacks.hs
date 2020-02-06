module CT.Category.Hacks where

import Unsafe.Coerce

data IsTup (xy :: (i, j)) where
  IsTup :: forall (x :: i) (y :: j). IsTup '(x, y)

isTup :: forall xy. IsTup xy
isTup = unsafeCoerce IsTup
