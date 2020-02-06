module Main where

import Prelude hiding (read, (.), id, map)
import Control.Category

import FCI

import Categories
import EZMonad

class GFunctor (Morphism Monad (↝)) (⇒) f => Effect f

mkInst ''Effect

-- Classes
class Monad m => Teletype m where
  read  :: m String
  write :: String -> m ()

mkInst ''Teletype

effectTeletype :: Inst (Effect Teletype)
effectTeletype = Effect $ GFunctor inst inst f
  where
  f (Morph { run, preserve }) = Entail $
    \(Teletype m r w) ->
      m ==>
      Teletype (preserve <$= m) (runNat run r) (runNat run . w)

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
test = effectTeletype ==> map (liftReaderT . liftStateT)

main :: IO ()
main = print "Hello, world!"
