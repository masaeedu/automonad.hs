module CT.Category.Op where

import FCI

class Op (p :: k -> k -> *) (q :: k -> k -> *) where

mkInst ''Op
