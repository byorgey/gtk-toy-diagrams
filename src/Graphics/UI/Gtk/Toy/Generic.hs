{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Generic
-- Copyright   :  (c) 2011 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Generic where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Text

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Data.Data
import Data.Generics.Aliases (mkM, mkQ)


-- TODO: Good way to do this ?

{-
newtype GenericInteractive a = GenericInteractive a
  deriving (Data, Typeable)

instance Data a => Interactive (GenericInteractive a) where
  tick = sybTick
  mouse = sybMouse
  keyboard = sybKeyboard

sybTick i x = (, True) <$> gmapM (mkM (\x' -> fst <$> tick i x')) x

sybMouse      m i = gmapM $ mkM $ mouse      m i

sybKeyboard p k i = gmapM $ mkM $ keyboard p k i

sybIsCursor :: (Data a, Typeable a) => a -> Bool
sybIsCursor = any id . gmapQ (mkQ False isCursor)

sybDrawMark x s d = foldr ($) d $ gmapQ (mkQ id $ (`drawMark` s)) x

sybDiagrams :: Data a => a -> [CairoDiagram]
sybDiagrams = gmapQ (mkQ mempty toDiagram)
 -}

