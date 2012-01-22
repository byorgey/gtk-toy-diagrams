{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , TemplateHaskell
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

import Data.Label
import Diagrams.Backend.Cairo
import Diagrams.Prelude

import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable
-- TODO: once decent math stuff is in place, make sliders on arbitrary paths.

data Slider a = Slider
  { _sliderMetric :: Bijection (->) Double a
  , _sliderHandle :: Draggable CairoDiagram
  , _sliderLine :: R2
  }

$(mkLabels [''Slider])

{-
proxy = TToy . TProxy sliderHandle

instance Interactive (Slider a) where
  mouse = mouse m i . proxy
-}

instance (Renderable (Path R2) Cairo)
      => Diagrammable (Slider a) Cairo R2 where
  toDiagram s = stroke (fromOffsets [get sliderLine s])
              <> toDiagram (get sliderHandle s)

mkSlider ivl d = Slider (ivlBij ivl) (mkDraggable (0, 0) d)
 where
  -- Creates a bijection between (0, 1) and some other interval
  ivlBij (f, t) = Bij (\x -> x * delta + f)
                      (\x -> x / delta - f)
   where delta = t - f

mkToggle     d = Slider boolBij      (mkDraggable (0, 0) d)
 where
  -- Not a real bijection...
  boolBij = Bij (> 0.5) (fromIntegral . fromEnum)

