{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.WrapLayout
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
import Debug.Trace

-- TODO: consider returning a different representation of the results
--   of this algorithm.  For example, a list of translational offsets,
--   or a nested list of participants (and have the dimensionality as
--   part of the type)

-- TODO: Take into account the negative bounds, and iteratively refine
--   the list selection.

-- TODO: Search for a region before / after the target pick.

-- | fillInside greedily wraps content to fill a space defined by a
--   predicate.  It is also passed a list of vectors which express the
--   order of dimensions to be filled.  In other words, wrapping RTL text
--   is done by passing in [unitX, unitY], in order to first exhaust
--   horizontally, and then vertically.
fillInside :: forall v b. (Show v, Show (Scalar v), HasLinearMap v, Boundable (Diagram b v))
           => (Point v -> Bool) -> [v] -> Point v 
           -> [Diagram b v] -> (Diagram b v, [Diagram b v])
fillInside f axis start = rec zeros
 where
  zeros = map snd . zip axis $ repeat (0, 0)
  norms = map normalized axis

-- rec recurses on the current set of coefficients
  rec :: [(Scalar v, Scalar v)]
      -> [Diagram b v] -> (Diagram b v, [Diagram b v])
  rec _ [] = (mempty, [])
  rec cur (d:ds) =
    case find (not . bcol . fst) ps of
      Just (off, cur') -> first (translate off d <>) $ rec cur' ds
      Nothing -> (mempty, d:ds)
   where
-- Get a vector given a set of scalars for each axis
    getVector = sumV . zipWith (^*) norms

-- Yields whether a given vector offset works for the given diagram
-- chunk.
    bcol v = any (f . (start .+^) . sumV . (v:)) $ sequence bounds

-- Extract the offset induced by each potential selection.
    ps = map (getVector . map fst &&& id)

-- Update the max bounds.
       . map (zipWith (\b (cx, cm) -> (cx, max cm $ cx + magnitude (b !! 1))) bounds)

-- Zero the axii which precede the incremented axis.
       . zipWith (++) (inits $ repeat (0, 0))

-- Try setting each axis to its max-seen bound.
       . map (\((_,x):xs) -> (x,x):xs)
       $ tails cur


    -- [[min bound, max bound]] of each axis.
    bounds :: [[v]]
    bounds = map minmax norms
     where
      minmax :: v -> [v]
      minmax v = map (.-. P zeroV) [boundary (negateV v) d, boundary v d]


{-
-- TODO: consider yielding a representation of the remainder

projectTo :: (Floating (Scalar v), InnerSpace v)
          => v -> [v] -> [Scalar v]
projectTo v [] = []
projectTo v (x:xs) = magnitude prj : projectTo (v ^-^ prj) xs
 where
  prj = project x v
-}