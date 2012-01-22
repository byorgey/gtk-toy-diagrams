{-# LANGUAGE FlexibleInstances
           , FlexibleContexts 
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TupleSections
           , TypeOperators
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
  ( Diagrammable(..), Clickable(..)
  , CairoDiagram, TToy(..) --, TProxy(..)
  , displayDiagram, diagramTraversable
  ) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk

import Control.Applicative ((<$>), pure)
import Control.Arrow ((***))
import Data.Label
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

diagramTraversable ::
         ( T.Traversable t
         , Diagrammable a b v
         , InnerSpace v, HasLinearMap v, OrderedField (Scalar v) )
         => t a -> Diagram b v
diagramTraversable = T.foldMapDefault toDiagram


-- | Clickable things have some concept of which positions are clickable.
class Clickable a where
  clickInside :: a -> V a -> Bool

instance HasLinearMap v => Clickable (Diagram b v) where
  clickInside d = getAny . runQuery (query d) . P

 
-- | Wrapper for making traversable things interactive.
data TToy t a = TToy (t a)

withTT :: (T.Traversable t, Interactive a)
       => (t a -> IO (t a)) -> TToy t a -> IO (TToy t a)
withTT f (TToy x) = TToy <$> f x

instance ( T.Traversable t, Interactive a )
      => Interactive (TToy t a) where

  -- TODO: or together the boolean results
  tick i x = (,True) <$> withTT ttick x
   where ttick = T.traverse $ \y -> fst <$> tick i y

  mouse   m i  = withTT $ T.traverse (mouse m i)
  keyboard k i = withTT $ T.traverse (keyboard k i)

instance ( T.Traversable t, Diagrammable a Cairo R2, Interactive a )
      => GtkInteractive (TToy t a) where
  display dw i (TToy x)
    = TToy <$> displayDiagram diagramTraversable dw i x


-- | Wrapper for making a single-item interactive instance
{-
data TProxy a b = TProxy (a :-> b) a

instance T.Traversable (TProxy a) where
  traverse f (TProxy l x) = TProxy . modify l f <$> pure x
-}