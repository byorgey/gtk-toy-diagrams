{-# LANGUAGE FlexibleInstances
           , FlexibleContexts 
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TupleSections
           , TypeSynonymInstances
           , UndecidableInstances
           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Diagrams
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Diagrams
  ( CairoDiagram, displayDiagram
  , Diagrammable(..), ToyTraversable(..)
  )
 where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk

import Control.Arrow ((***))
import qualified Data.Traversable as T
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Toy

type CairoDiagram = Diagram Cairo R2

-- | Convenience function for implementing the display function of
--   Interactive.
displayDiagram :: (a -> CairoDiagram)
                -> G.DrawWindow -> InputState -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x

class Diagrammable a b v where
  toDiagram :: a -> Diagram b v

instance Diagrammable (Diagram b v) b v where
  toDiagram = id

diagramFoldable ::
         ( T.Traversable t
         , Diagrammable a b v
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v) )
         => t a -> Diagram b v
diagramFoldable = T.foldMapDefault toDiagram

-- | Wrapper for making traversable things interactive.
data ToyTraversable t a = ToyTraversable (t a)

withTT :: (T.Traversable t, Diagrammable a Cairo R2, Interactive a)
       => (t a -> IO (t a)) -> ToyTraversable t a -> IO (ToyTraversable t a)
withTT f (ToyTraversable x) = ToyTraversable <$> f x

instance ( T.Traversable t, Diagrammable a Cairo R2, Interactive a )
      => Interactive (ToyTraversable t a) where

  -- TODO: or together the boolean results
  tick i x = (,True) <$> withTT ttick x
   where ttick = T.traverse $ \y -> fst <$> tick i y

  display dw i (ToyTraversable x)
    = ToyTraversable <$> displayDiagram diagramFoldable dw i x

  mouse   m i  = withTT $ T.traverse (mouse m i)
  keyboard k i = withTT $ T.traverse (keyboard k i)
