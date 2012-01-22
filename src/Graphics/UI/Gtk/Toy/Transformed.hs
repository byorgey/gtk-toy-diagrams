{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
           , UndecidableInstances
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
  ( Transformed(..) ) where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams

import Control.Arrow (first, second)
import Control.Newtype (Newtype, pack, unpack, over)
import Control.Newtype.TH
import Data.Foldable (foldMap)
import Data.Label
import Diagrams.Backend.Cairo
import Diagrams.Prelude

newtype Transformed a = Transformed [(Transformation (V a), a)]
  deriving (Monoid)

$(mkNewTypes [''Transformed])

type instance V (Transformed a) = V a

type instance V InputState = R2
instance Transformable InputState where
  transform t is = is { mousePos = transform t $ mousePos is }

instance HasLinearMap (V a)
      => HasOrigin      (Transformed a) where
  moveOriginTo p = translate (origin .-. p)
  
instance HasLinearMap (V a) 
      => Transformable  (Transformed a) where
  transform a = Transformed `over` map (first (a <>))

instance ( HasLinearMap (V a) 
         , Boundable                   a)
        => Boundable      (Transformed a) where
  getBounds = foldMap (\(t, x) -> transform t $ getBounds x) . unpack

instance HasStyle                    a
      => HasStyle       (Transformed a) where
  applyStyle s = Transformed `over` map (second $ applyStyle s)

{-
instance Interactive                 a
      => Interactive    (Transformed a) where
  tick       i (Transformed t xs) 
             = (Transformed t `first`) <$> mapM (tick       (transform t i)) xs
  mouse    m i (Transformed t xs) 
             = (Transformed t  )       <$> mapM (mouse m    (transform t i)) xs
  keyboard k i (Transformed t xs) 
             = (Transformed t  )       <$> mapM (keyboard k (transform t i)) xs

instance ( Diagrammable a Cairo R2, Interactive a )
      => GtkInteractive (Transformed a) where
  display dw i (Transformed t x)
    = Transformed t <$> displayDiagram (mapM toDiagram) dw (transform t i) x

instance ( v ~ V a, HasLinearMap v, InnerSpace v, OrderedField (Scalar v)
         , Diagrammable a Cairo v)
        => Diagrammable   (Transformed a) Cairo v where
  toDiagram (Transformed t x) = transform t $ foldMap toDiagram x

instance ( Clickable a, AdditiveGroup (V a) )
        => Clickable    (Transformed a) where
  clickInside (Transformed t x) p = clickInside x $ transform t p

-}