{-# LANGUAGE LiberalTypeSynonyms #-}
module Main where

import Prelude hiding (read, (.), id, map)
import Control.Category

import FCI

import CT.Category.NTrans
import CT.Category.Entailment
import CT.Category.Morphism
import CT.Functor

import Derivation.Monad

import Effect.Teletype

-- Transformers

-- ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

monadReaderT :: Monad m ⇒ Monad (ReaderT r m)
monadReaderT = Entail $ \m -> m ==> ezmonad (>>=′) (ReaderT . pure . pure)
  where
  (ReaderT ma) >>=′ ((runReaderT .) -> amb) = ReaderT $ \r -> ma r >>= ($ r) . amb

liftReaderT :: m ⤜ ReaderT r m
liftReaderT = Morph (Nat $ ReaderT . pure) monadReaderT

-- StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

monadStateT :: Monad m ⇒ Monad (StateT s m)
monadStateT = Entail $ \m -> m ==> ezmonad (>>=′) pure'
  where
  StateT ma >>=′ ((runStateT .) -> amb) = StateT $ \s -> ma s >>= \(a, s') -> amb a s'
  pure' a = StateT $ \s -> pure (a, s)

liftStateT :: m ⤜ StateT s m
liftStateT = Morph (Nat $ \ma -> StateT $ \s -> (, s) <$> ma) monadStateT

-- Up to here, no interaction between transformers and classes. Now we put them together
test :: Teletype f ⇒ Teletype (ReaderT r (StateT s f))
test = map (liftReaderT . liftStateT)

main :: IO ()
main = print "Hello, world!"
