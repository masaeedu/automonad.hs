{-# LANGUAGE TemplateHaskell, TypeFamilies, NoQuantifiedConstraints, NamedFieldPuns, ViewPatterns, TupleSections #-}
module Main where

import Prelude hiding (read, (.), id)
import Control.Category

import FCI

import EZMonad

type f ~> g = forall x. f x -> g x

data f ~⋅~> g = MMorph
  { convert :: Inst (Monad f) -> Inst (Monad g)
  , nat :: Monad f => f ~> g
  }

instance Category (~⋅~>)
  where
  (MMorph c n) . (MMorph d m) = MMorph (c . d) (\(ma :: m a) -> d (inst @(Monad m)) ==> n $ m ma)
  id = MMorph id id

class Monad m => Teletype m where
  read  :: m String
  write :: String -> m ()

mkInst ''Teletype

mapTeletype :: (f ~⋅~> g) -> Inst (Teletype f) -> Inst (Teletype g)
mapTeletype (MMorph { convert, nat }) (Teletype m r w) = m ==> Teletype (convert m) (nat r) (nat . w)

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

monadReaderT :: forall r m. Inst (Monad m) -> Inst (Monad (ReaderT r m))
monadReaderT m = m ==> ezmonad (>>=′) pure'
  where
  (>>=′) :: Monad m => (::>>=) (ReaderT r m)
  (ReaderT ma) >>=′ ((runReaderT .) -> amb) = ReaderT $ \r -> ma r >>= ($ r) . amb

  pure' :: Monad m => Pure (ReaderT r m)
  pure' = ReaderT . pure . pure

liftReaderT :: m ~⋅~> ReaderT r m
liftReaderT = MMorph monadReaderT (ReaderT . pure)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

monadStateT :: forall s m. Inst (Monad m) -> Inst (Monad (StateT s m))
monadStateT m = m ==> ezmonad (>>=′) pure'
  where
  (>>=′) :: Monad m => (::>>=) (StateT s m)
  (StateT ma) >>=′ ((runStateT .) -> amb) = StateT $ \s -> ma s >>= \(a, s') -> amb a s'

  pure' :: Monad m => Pure (StateT s m)
  pure' a = StateT $ \s -> pure (a, s)

liftStateT :: m ~⋅~> StateT s m
liftStateT = MMorph monadStateT (\ma -> StateT $ \s -> (, s) <$> ma)

test :: forall r s f. Inst (Teletype f) -> Inst (Teletype (ReaderT r (StateT s f)))
test = mapTeletype (liftReaderT . liftStateT)

main :: IO ()
main = print "Hello, world!"
