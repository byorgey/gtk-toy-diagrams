{-# LANGUAGE MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts #-}
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
  , Diagrammable(..), displayDiagrammable
  )
 where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Toy

type CairoDiagram = Diagram Cairo R2

-- I realize this seems silly, but it saves the user from ever even needing to
-- import any other Gtk modules.

displayDiagram :: (a -> CairoDiagram)
                -> G.DrawWindow -> InputState -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x

class Diagrammable a b v where
  toDiagram :: a -> Diagram b v

instance Diagrammable (Diagram b v) b v where
  toDiagram = id

displayDiagrammable :: Diagrammable a Cairo R2
                    => G.DrawWindow -> InputState -> a -> IO a
displayDiagrammable = displayDiagram toDiagram


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
