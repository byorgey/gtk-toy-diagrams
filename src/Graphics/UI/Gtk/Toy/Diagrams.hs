{-# LANGUAGE MultiParamTypeClasses
           , ScopedTypeVariables
           , TupleSections
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts 
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
data (T.Traversable t, Diagrammable a Cairo R2, Interactive a)
  => ToyTraversable t a = ToyTraversable (t a)

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
 
{-
-- Re-expors Graphics.UI.Gtk.Toy's functionality under the guise of a
-- new typeclass, 

type StateId = Int

class HasStateId a where
  getStateId :: a -> StateId

-- TODO: what about IO? sure it's evil, but what if this is the root of the app?

class HasStateId a => InteractiveDiagram a where
  -- | Tick is (ideally) called every 30ms.
  tick     :: a -> a

  -- | Display is called when the rendering needs to be refreshed.
  display  :: a -> CDiagram

  -- | Mouse is called when the mouse moves or presses occur.
  mouse    :: (Double, Double) -> Maybe (Bool, Int) -> a -> a

  -- | Keyboard is called when 
  keyboard :: Bool -> Either String Char -> a -> a

instance InteractiveDiagram a => T.Interactive a where
  T.tick     = T.simpleTick tick
  T.display  = T.simpleDisplay (\dw -> renderToGtk
  T.keyboard = T.simpleKeyboard keyboard
  T.mouse c inp = return . mouse (T.mousePos inp) c

-}
