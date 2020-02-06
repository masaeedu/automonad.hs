module Effect.Log where

import FCI

import CT.Category.Morphism
import CT.Category.NTrans
import CT.Category.Entailment
import CT.Functor

class Monad m => Log m where
  log :: String -> m ()

mkInst ''Log

instance GFunctor ((↝) ∋ Monad) (⇒) Log where
  map (Morph { run, preserve }) =
    Entail $ \(Log m l) -> m ==>
      Log (preserve <$= m) ((run <$~) . l)
