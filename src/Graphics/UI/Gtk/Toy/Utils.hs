{-# LANGUAGE  TypeOperators #-}
module Graphics.UI.Gtk.Toy.Utils where

import Control.Newtype (overF)
import Data.Label
import Debug.Trace (trace)

overM x f = (x `overF` (>>= f)) . return

modifyM :: Monad m => (b :-> a) -> (a -> m a) -> b -> m b
modifyM l f x = do
  a <- f $ get l x
  return $ set l a x

debug x = trace (show x) x
debug' s x = trace (s ++ show x) x