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

import Data.Data
import Data.Generics.Aliases (mkQ)

-- TODO: This sorta does OOP for cheap. Template-haskellify?

sybTick i = gmapM $ mkM $ tick i

sybDiagrams = gmapQ $ mkQ mempty toDiagram

sybMouse    p m i = gmapM $ mkM $ mouse    p m i

sybKeyboard p k i = gmapM $ mkM $ keyboard p k i

sybDrawMark x s d = foldr ($) d $ gmapQ (`drawMark` s) x

sybIsCursor = any . gmapQ $ mkQ False isCursor