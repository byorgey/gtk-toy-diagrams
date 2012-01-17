{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Draggable
-- Copyright   :  (c) 2011 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Functions for things that can be clicked and dragged around.
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Draggable
  ( Clickable(..)
  , Draggable(..)

  -- * Lenses
  , dragState, dragOffsetAcc, dragContent

  -- * Interaction with mouse
  -- | Starts drag when mouse 1 (left) is pressed, and ends when released.
  , mouseDrag

  -- * Update
  , mkDraggable, startDrag, updateDrag, endDrag

  -- * Query
  , isDragging, dragOffset) where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams

import Data.Label
import Data.Maybe (isJust)

import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Diagrams.TwoD.Text

-- | Clickable things have some concept of which positions are clickable.
class Clickable a where
  clickInside :: a -> R2 -> Bool

instance Clickable CairoDiagram where
  clickInside d = getAny . runQuery (query d) . P

 
-- | Draggable things are translatable, and store state during the drag
--   process.
data Draggable a = Draggable 
  { _dragState :: (Maybe (R2, R2))
  , _dragOffsetAcc :: R2
  , _dragContent :: a
  }

$(mkLabels [''Draggable])

instance Diagrammable a Cairo R2
      => Diagrammable (Draggable a) Cairo R2 where
  toDiagram d@(Draggable _ _ a)
    = translate (dragOffset d) $ toDiagram a

instance (Diagrammable a Cairo R2, Clickable a)
      => Interactive (Draggable a) where
  display = displayDiagram toDiagram
  mouse = simpleMouse mouseDrag

instance Clickable a
      => Clickable (Draggable a) where
  clickInside d p = clickInside (_dragContent d) $ p ^-^ dragOffset d

-- | Creates dragging state for some object, with an initial offset.
mkDraggable :: R2 -> a -> Draggable a
mkDraggable = Draggable Nothing

-- | Pure mouse handler, compatible with the type expected by "simpleMouse".
mouseDrag (Just (True,  0)) p d | clickInside d p = startDrag p d
mouseDrag Nothing           p d                   = updateDrag p d
mouseDrag (Just (False, 0)) p d                   = endDrag d
mouseDrag _ _ d = d

-- | Switches into dragging mode at the given position.
startDrag :: R2 -> Draggable a -> Draggable a
startDrag p = set dragState (Just (p, p))

-- | Updates the drag with a new mouse position, if the object is being
--   dragged.  TODO: consider having a check for the input state to
--   check if the mouse is being held down?
updateDrag :: R2 -> Draggable a -> Draggable a
updateDrag p (Draggable (Just (_, s)) o c) = Draggable (Just (p, s)) o c
updateDrag _ d = d

-- | Switches out of dragging mode.
endDrag :: Draggable a -> Draggable a
endDrag d = Draggable Nothing (dragOffset d) $ _dragContent d

-- | Queries whether we're currently in dragging-modk
isDragging :: Draggable a -> Bool
isDragging = isJust . get dragState

-- | Gets the current amount of drag-induced offset for the diagram.
dragOffset (Draggable c o _) =  o ^+^ (maybe (0, 0) (uncurry (^-^)) c)