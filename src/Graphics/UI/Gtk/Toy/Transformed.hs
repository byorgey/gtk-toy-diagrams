{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , RankNTypes
           , TemplateHaskell
           , TupleSections
           , TypeFamilies
           , UndecidableInstances
           , ImpredicativeTypes
           , ScopedTypeVariables
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Transformed
-- Copyright   :  (c) 2011 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.Toy.Transformed
  ( Transformed(..), mkTransformed ) where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Utils

import Control.Arrow (first, second)
import Control.Newtype (Newtype, pack, unpack, under, over, overF)
import Control.Newtype.TH
import Data.Foldable (fold, foldMap)
import Data.Label
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (under)
import Graphics.Rendering.Diagrams.Points
import Debug.Trace

newtype Transformed a = Transformed [(Transformation (V a), a)]
  deriving (Monoid, Semigroup)

{-
data TThing 
  = TThing
  ( forall a. ( R2 ~ V a, GtkInteractive a
              , Boundable a, Juxtaposable a
              ) => a )

-- | Existential wrapper for GtkInteractive, layout-able stuff
type ToyThing = Transformed TThing
-}

$(mkNewTypes [''Transformed])

mkTransformed :: HasLinearMap (V a) => a -> Transformed a
mkTransformed = Transformed . (:[]) . (mempty, )

type instance V (Transformed a) = V a

type instance V InputState = R2
instance Transformable InputState where
--  transform t is = is { mousePos = trace (show $ transl $ inv t) $ debug $ transform t $ mousePos is }
  transform t is = is { mousePos = under P (transform $ inv t) $ mousePos is }

instance HasLinearMap (V a) => HasOrigin     (Transformed a) where
  moveOriginTo p = translate (origin .-. p)
  
instance HasLinearMap (V a) => Transformable (Transformed a) where
  transform a = Transformed `over` map (first (a <>))

instance ( Boundable a, HasLinearMap (V a) )
      => Boundable (Transformed a) where
  getBounds = foldMap (\(t, x) -> transform t $ getBounds x) . unpack

instance HasStyle a => HasStyle (Transformed a) where
  applyStyle s = Transformed `over` map (second $ applyStyle s)

instance ( v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Diagrammable a Cairo v)
        => Diagrammable (Transformed a) Cairo v where
  toDiagram = foldMap (\(t, x) -> transform t $ toDiagram x) . unpack

instance ( Boundable a, HasLinearMap (V a) )
      => Juxtaposable (Transformed a) where
  juxtapose = juxtaposeDefault

overInpT f i = Transformed `overM` mapM (\(t, x) -> (t,) <$> f (transform t i) x)

instance ( Interactive a, V a ~ R2 )
      => Interactive (Transformed a) where
  -- TODO: or together the boolean results
  tick     i = liftA (, True)
             . overInpT (\i' -> liftA fst . tick i') i
  mouse    m = overInpT (mouse m)
  keyboard k = overInpT (keyboard k)

instance ( Interactive a, Diagrammable a Cairo R2, V a ~ R2 )
      => GtkInteractive (Transformed a) where
  display dw i = displayDiagram toDiagram dw i

instance ( Clickable a, HasLinearMap (V a) )
        => Clickable (Transformed a) where
  clickInside d p = any (\(t, x) -> clickInside x $ transform t p) $ unpack d

{-
instance Interactive TThing where
  tick       i = liftA (, True)
               $ TThing `overM` (liftA fst . tick i)
  mouse    m i = TThing `overM` mouse m i
  keyboard k i = TThing `overM` keyboard k i

instance GtkInteractive TThing where
  display = display
-}
