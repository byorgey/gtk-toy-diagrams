{-# LANGUAGE MultiParamTypeClasses
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
-- Functions for things that can be clicked and dragged in order to move them.
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Draggable where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams

import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Diagrams.TwoD.Text

data Draggable a = Draggable (Maybe (R2, R2)) R2 a

class Clickable a where
  clickInside :: a -> R2 -> Bool

instance Clickable CairoDiagram where
  clickInside d = getAny . runQuery (query d) . P
 
instance (Diagrammable a Cairo R2)
 => Diagrammable (Draggable a) Cairo R2 where
  toDiagram d@(Draggable _ _ a)
    = translate (dragOffset d) $ toDiagram a

instance (Diagrammable a Cairo R2, Clickable a)
 => Interactive (Draggable a) where
  display = displayDiagrammable
  mouse = simpleMouse handleMouse

handleMouse :: Clickable a
            => R2 -> (Maybe (Bool, Int)) -> Draggable a -> Draggable a
handleMouse pos (Just (True, 0)) d@(Draggable _ o a)
  | clickInside a pos = Draggable (Just (pos, pos)) o a
handleMouse pos (Just (False, 0)) d
  = unDrag d
handleMouse pos _ (Draggable (Just (_, s)) o a)
  = Draggable (Just (pos, s)) o a

-- | Switches the diagram out of dragging mode.
unDrag :: Draggable a -> Draggable a
unDrag d@(Draggable _ _ a) = Draggable Nothing (dragOffset d) a

-- | Gets the current amount of drag-induced offset for the diagram.
dragOffset (Draggable c o _) = o ^+^ (maybe (0, 0) (uncurry (^-^)) c)