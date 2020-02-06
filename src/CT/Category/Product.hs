{-# LANGUAGE InstanceSigs #-}
module CT.Category.Product where

import Prelude hiding ((.), id)

import Control.Category

import CT.Category.Hacks

data (p ∧ q) (a :: (k, k)) (b :: (k, k)) where
  (:×:) :: p a b -> q c d -> (∧) p q '(a, c) '(b, d)

instance (Category p, Category q) => Category (p ∧ q) where
  (p1 :×: q1) . (p2 :×: q2) = (p1 . p2) :×: (q1 . q2)

  id :: forall a. (p ∧ q) a a
  id | IsTup <- isTup @a  = id :×: id
