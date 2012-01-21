{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Wrap
-- Copyright   :  (c) 2012 Michael Sloan 
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Michael Sloan <mgsloan@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Wrap where

import Control.Arrow (first, (&&&))
import Data.List (find, tails, inits)
import Diagrams.Prelude

-- TODO: consider returning a different representation of the results
--   of this algorithm.  For example, a list of translational offsets,
--   or a nested list of participants (and have the dimensionality as
--   part of the type)

-- TODO: Take into account the negative bounds, and iteratively refine
--   the list selection.

-- TODO: Search for a region before / after the target pick.

fillOutside f = fillInside (not . f)

-- | fillInside greedily wraps content to fill a space defined by a
--   predicate.  It is also passed a list of vectors which express the
--   order of dimensions to be filled.  In other words, wrapping RTL text
--   is done by passing in [unitX, unitY], in order to first exhaust
--   horizontally, and then vertically.
fillInside :: forall v b. (HasLinearMap v, Boundable (Diagram b v))
           => (Point v -> Bool) -> [v] -> Point v 
           -> [Diagram b v] -> (Diagram b v, [Diagram b v])
fillInside f axis start = rec zeros
 where
  zeros = map snd . zip axis $ repeat (0, 0)
  norms = map normalized axis
  getVector = sumV . zipWith (^*) norms

-- [[min bound, max bound]] of each axis.
  boundsScalars :: Diagram b v -> [[v]]
  boundsScalars d
    = flip map norms
    $ \v -> map (.-. origin) [boundary (negateV v) d, boundary v d]

-- Recurses on a current set of coefficients for the different axii,
-- each paired with the max boundary seen, in that direction, from the
  rec :: [(Scalar v, Scalar v)]
      -> [Diagram b v] -> (Diagram b v, [Diagram b v])
  rec _ [] = (mempty, [])
  rec scs (d:ds) 
-- Recurse a satisfactory position can be found, otherwise yields the
-- list of the remaining diagrams to be laid out.
    = maybe (mempty, d:ds)
            (\(v, scs') -> first (translate v d <>) $ rec scs' ds)
    $ find (check . fst) potential
   where
    curB = boundsScalars d

-- Yields whether a given vector offset avoids collision.
    check v = all (f . (start .+^) . sumV . (v:)) $ sequence curB

-- Updates the max bounds of an axis.
    maxB [_, b] (x, m) = (x, max m $ x + magnitude b)

-- List of potential offsets to try, each paired with an updated list
-- of current / maxbound scalar coefficients for the axis.
    potential = map (getVector . map fst &&& zipWith maxB curB)
-- Try setting an axis to its max-seen bound, zeroing all preceding.
              . zipWith (++) (inits $ repeat (0, 0))
              . map (\((_,x):xs) -> (x,x):xs)
              . init $ tails scs
