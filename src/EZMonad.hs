module EZMonad where

import FCI

type (::>>=) m = forall a b. m a -> (a -> m b) -> m b
type Pure m = forall a. a -> m a
type (::<$>) m = forall a b. (a -> b) -> m a -> m b
type (::$>) m = forall a b. b -> m a -> m b
type (::<*>) m = forall a b. m (a -> b) -> m a -> m b
type LiftA2 m = forall a b c. (a -> b -> c) -> m a -> m b -> m c
type (::*>) m = forall a b. m a -> m b -> m b
type (::<*) m = forall a b. m a -> m b -> m a

ezmonad :: forall m. (::>>=) m -> Pure m -> Inst (Monad m)
ezmonad (>>=) pure = Monad applicative (>>=) (*>) pure error
  where
  (<$>) :: (::<$>) m
  (<$>) f ma = ma >>= (pure . f)

  ($>) :: (::$>) m
  ($>) = (<$>) . const

  functor :: Inst (Functor m)
  functor = Functor (<$>) ($>)

  (<*>) :: (::<*>) m
  (<*>) mab ma = mab >>= \ab -> ma >>= \a -> pure $ ab a

  liftA2 :: LiftA2 m
  liftA2 abc fa fb = abc <$> fa <*> fb

  (*>) :: (::*>) m
  (*>) ma mb = (flip const) <$> ma <*> mb

  (<*) :: (::<*) m
  (<*) ma mb = const <$> ma <*> mb

  applicative :: Inst (Applicative m)
  applicative = Applicative functor pure (<*>) liftA2 (*>) (<*)

