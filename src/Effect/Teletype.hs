module Effect.Teletype where

import FCI

import CT.Category.Morphism
import CT.Category.NTrans
import CT.Category.Entailment
import CT.Functor

import Effect

class Monad m => Teletype m where
  read  :: m String
  write :: String -> m ()

mkInst ''Teletype

instance GFunctor (Morphism Monad (↝)) (⇒) Teletype where
  map (Morph { run, preserve }) =
    Entail $ \(Teletype m r w) -> m ==>
      Teletype (preserve <$= m) (run <$~ r) ((run <$~) . w)

instance Effect Teletype
