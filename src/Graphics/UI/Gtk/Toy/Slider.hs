{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
           , TypeOperators
           , TypeSynonymInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Slider
-- Copyright   :  (c) 2012 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Slider 
  ( Slider
  , sliderHandle, sliderLine, sliderMetric
  , mkToggle, mkSlider
  ) where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable
import Graphics.UI.Gtk.Toy.Utils

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Newtype (unpack)
import Data.Label
import Data.Maybe (fromJust)
import Diagrams.Backend.Cairo
import Diagrams.Prelude

-- TODO: once decent math stuff is in place, make sliders on arbitrary paths.

data Slider a = Slider
  { _sliderMetric :: Bijection (->) Double a
  , sliderHandle_ :: Draggable CairoDiagram
  , _sliderLine :: R2
  }

$(mkLabels [''Slider])

roundBij :: Bijection (->) a b -> a -> a
roundBij f = bw f . fw f

               
paramBij :: Slider a -> Bijection (->) (Double, Double) Double
paramBij s = Bij (clamp (0, 1) . (/ magnitude l) . (normalized l <.>)) (lerp zeroV l)
 where l = get sliderLine s
       clamp (f, t) x
         | x < f = f
         | x > t = t
         | otherwise = x

sliderHandle = lens sliderHandle_
  $ \x s -> let offset' = roundBij $ get sliderMetric s . paramBij s
             in s { sliderHandle_ = modify dragOffset offset' x }
 
type instance V (Slider a) = R2

instance Interactive (Slider a) where
  mouse m i = modifyM sliderHandle (mouse m i)

instance (Renderable (Path R2) Cairo)
      => Diagrammable (Slider a) Cairo R2 where
  toDiagram s = stroke (fromOffsets [get sliderLine s])
              <> toDiagram (get sliderHandle s)

instance Boundable (Slider a) where
  getBounds s = getBounds [origin, P $ get sliderLine s]
             <> getBounds (get sliderHandle s)

mkSlider ivl d = Slider (ivlBij ivl) (mkDraggable (0, 0) d)
 where
  -- Creates a bijection between (0, 1) and some other interval
  ivlBij (f, t) = Bij (\x -> x * delta + f)
                      (\x -> x / delta - f)
   where delta = t - f

mkToggle = Slider boolBij (mkDraggable (0, 0) $ circle 5) (0, 10)
 where
  -- Not a real bijection...
  boolBij = Bij (> 0.5) (fromIntegral . fromEnum)