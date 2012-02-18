{-# LANGUAGE ExistentialQuantification
           , FlexibleInstances
           , FlexibleContexts 
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , StandaloneDeriving
           , TemplateHaskell
           , TupleSections
           , TypeFamilies
           , TypeOperators
           , TypeSynonymInstances
           , UndecidableInstances
           , KindSignatures
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
  , CairoDiagram, TToy(..), TDia(..)
  , displayDiagram
  ) where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Utils

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Gtk

import Control.Applicative ((<$>), pure)
import Control.Arrow ((***))
import Control.Newtype (Newtype, pack, unpack)
import Control.Newtype.TH
import Data.Label
import qualified Data.Traversable as T
import qualified Graphics.UI.Gtk as G

type CairoDiagram = Diagram Cairo R2

class Diagrammable a b v where
  toDiagram :: a -> Diagram b v

instance Diagrammable (Diagram b v) b v where
  toDiagram = id

-- | Clickable things have some concept of which positions are clickable.
class Clickable a where
  clickInside :: a -> Point (V a) -> Bool

instance HasLinearMap v => Clickable (Diagram b v) where
  clickInside d = getAny . runQuery (query d)

-- | Wrapper for making traversable things interactive.
newtype TToy t a = TToy (t a)

-- | Wrapper to make @GtkInteractive@ instances for Diagrammables.
newtype TDia a = TDia a
  deriving (Clickable, Juxtaposable)

type instance V (TToy t a) = V a
type instance V (TDia a)   = V a

deriving instance ( InnerSpace (V a), HasLinearMap (V a), OrderedField (Scalar (V a))
                  , Enveloped a)
                 => Enveloped     (TDia a)
deriving instance ( Transformable a, HasLinearMap (V a))
                 => Transformable (TDia a)
deriving instance ( HasOrigin a, VectorSpace (V a) )
                 => HasOrigin     (TDia a)

instance Diagrammable a b v => Diagrammable (TDia a) b v where
  toDiagram = toDiagram . unpack

$(mkNewTypes [''TToy, ''TDia])

-- | Convenience function for implementing the display function of
--   Interactive.
displayDiagram :: (a -> CairoDiagram)
               -> G.DrawWindow -> InputState -> a -> IO a
displayDiagram f dw _ x = (renderToGtk dw $ f x) >> return x

-- Traversable Toy instances

instance ( T.Traversable t, Diagrammable a b v
         , v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v) )
      => Diagrammable (TToy t a) b v where
  toDiagram = T.foldMapDefault toDiagram . unpack

instance ( T.Traversable t, Interactive a )
      => Interactive (TToy t a) where
  -- TODO: or together the boolean results
  tick       i = liftA (, True)
               . (TToy `overM` T.traverse (liftA fst . tick i))
  mouse    m i =  TToy `overM` T.traverse (mouse m i)
  keyboard k i =  TToy `overM` T.traverse (keyboard k i)

instance ( T.Traversable t, Diagrammable a Cairo R2, Interactive a, R2 ~ V a)
      => GtkInteractive (TToy t a) where
  display dw i x = displayDiagram toDiagram dw i x


-- Diagrammable Toy instances

instance Interactive (TDia a) where {}

instance ( Diagrammable a Cairo R2 )
      => GtkInteractive (TDia a) where
  display dw i = TDia `overM` displayDiagram toDiagram dw i


-- | Wrapper for making a single-item interactive instance
{-
data TProxy a b = TProxy (a :-> b) a

instance T.Traversable (TProxy a) where
  traverse f (TProxy l x) = TProxy . modify l f <$> pure x
-}