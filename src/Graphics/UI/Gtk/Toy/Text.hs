{-# LANGUAGE RankNTypes
           , ScopedTypeVariables
           , TemplateHaskell
           , TupleSections
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Text
-- Copyright   :  (c) 2011 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Provides a (currently inefficient) representation of text with styling and
-- metadata annotations applied to particular intervals.
--
-----------------------------------------------------------------------------
module Graphics.UI.Gtk.Toy.Text where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.Backend.Cairo.Text (StyleParam, textLineBounded)

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative ((<$>))
import Control.Monad (msum)

import Data.Colour.Names (black, white)
import Data.Either (partitionEithers)
import Data.Label
import Data.List (partition, findIndices, sortBy, sort, delete, group, (\\))
import Data.Maybe (fromJust, catMaybes, maybeToList)
import Data.Ord (comparing)

import Debug.Trace (trace)

type Ivl = (Int, Int)

data MarkedText m = MarkedText
  { _mText  :: String
  , _mMarks :: [(Ivl, m)]
  } deriving (Eq, Show)

$(mkLabels [''MarkedText])

class CanBeCursor a where
  mkCursor :: a
  isCursor :: a -> Bool

-- TODO: function to handle mark splitting

-- TODO: rename all m type variables to something else..

class Mark a where
  -- TODO: use
  styleMark :: a -> StyleParam
  drawMark :: a -> String -> CairoDiagram -> CairoDiagram
  mergeMark :: a -> a -> Maybe a
  styleMark _ = id
  drawMark _ _ = id
  mergeMark _ _ = Nothing

emptyText :: MarkedText m
emptyText = MarkedText "" []

-- | Extract an interval of the text.  First parameter is True if inclusive.
substrText :: Bool -> MarkedText m -> Ivl -> MarkedText m
substrText inc (MarkedText str ms) ivl@(f, t)
  = MarkedText (take (t - max 0 f) $ drop f str)
  . catMaybes $ map (firstA $ local) ms
 where
-- Transform intervals into the result, yielding Nothing if they aren't
-- inside the substring interval.
  local x | x == ivl = Just (0, t - f)
          | inc && fst x ==  0 && fst ivl ==  0 = Just (0, min (snd x) (snd ivl))
          | inc && snd x == tl && snd ivl == tl = Just (max (fst x) (fst ivl), tl)
          | otherwise = mapT (subtract f) <$> ivlIntersect ivl x
  tl = length str

-- | Concatenates two marked texts together, attempting to merge the marks
--   incident on the join.
addText :: forall m. (Mark m, Eq m) 
      => MarkedText m -> MarkedText m -> MarkedText m
addText (MarkedText at ams) (MarkedText bt bms)
  = MarkedText (at ++ bt) rms
 where
  al = length at

-- Separate off marks that cross the border between the texts.
  (ap, an) = partition ((>=al-1) . snd . fst) ams

  (bp, bn) = mapT (map $ first $ mapT (+al))
           $ partition ((<=0)  . fst . fst) bms

-- Collect all of the resulting marks.
  rms = sortBy (comparing fst) $ an ++ performMerges ap bp ++ bn

-- Merge overlapping marks from the two texts.
performMerges []     ys = ys
performMerges (x:xs) ys = case msum . map doMerge $ ys of
  Just (y, x') -> x' : performMerges xs (delete y ys)
  Nothing      -> x  : performMerges xs ys
 where
  doMerge y = do
    i <- ivlMaybeUnion (fst x) (fst y)
    m <- mergeMark     (snd x) (snd y)
    return (y, (i, m))

--TODO: consider a mark for lines / line #s?
--TODO: figure out how style will be applied to the text by marks

-- | Draws the marked text, given an initial style to apply.
{-
drawText :: (Show a, Mark a) => StyleParma -> MarkedText a -> CairoDiagram
drawText style mt =
  . 
  . map (drawLine . (substrText True mt) . (second (+1)))
  . ivlsFromSlices (textLength mt)
  . findIndices (=='\n')
  $ _text mt
 where
  drawLine l = map (hcat
             . mergeCompatible containment
             . map (, [])
             . sortBy (comparing $ uncurry (-) . fst)
             $ _marks l
  
  text (_, MarkedText s1 _) (_, MarkedText s2 _) t
    = --strutX (kerningCorrection style (last $ s1 ++ s2) $ head t) |||
      textLineBounded style t
  containment ((i1, m1), xs) (x@(i2, m2), ys)
    | i1 `ivlContainsIvl` i2 = Just ((i1, m1), ys ++ x:xs)
    | otherwise = Nothing


-- List utility used in drawText

mergeCompatible _ [] = []
mergeCompatible _ [x] = [x]
mergeCompatible f (x:xs)
  = case extractJust (f x) xs of
      Just (r, rs) -> mergeCompatible f (r:rs)
      Nothing -> x : mergeCompatible f xs

-- Removes the first element that yields a "Just".
extractJust :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
extractJust f [] = Nothing
extractJust f (x:xs) = case f x of
  Just y  -> Just (y, xs)
  Nothing -> second (x:) <$> extractJust f xs
-}

-- | Draws the marked text, given a style to apply by default to the unmarked text.
drawText :: (Mark a) => StyleParam -> MarkedText a -> CairoDiagram
drawText style mt
  = vcat
  . map (hcat . window3 chunk . dummyPad)
  . map (textChunks . (substrText True mt) . (second (+1)))
  . ivlsFromSlices (textLength mt)
  . findIndices (=='\n')
  $ get mText mt
 where
  dummyPad = (replicate 2 ((0,0), emptyText) ++)
--TODO: sweepline-style context accumulation would be more efficient.
  chunk l1 l2 (_, MarkedText t ms) = foldr ((`drawMark` t) . snd) (text l1 l2 t) ms
  text (_, MarkedText s1 _) (_, MarkedText s2 _) t
    = --strutX (kerningCorrection style (last $ s1 ++ s2) $ head t) |||
      textLineBounded style t
  window3 f (x:y:z:xs) = f x y z : window3 f (y:z:xs)
  window3 f _ = []

-- | A chunk specifies a located marked text.
type Chunk m = (Ivl, MarkedText m)

-- | Breaks the marked text into chunks such that the marks stay consistent
--   throughout.
textChunks :: MarkedText m -> [Chunk m]
textChunks mt
  = map (id &&& (substrText False mt))
  . ivlsFromSlices (textLength mt)
  . concatMap (ivlSlices . fst)
  $ get mMarks mt

-- | Given a list of slice points, and an overall length, yields a list of
--   intervals broken at those points.
ivlsFromSlices l xs = map head . group $ zip (0 : ys) (ys ++ [l])
  where ys = sort xs

textLength = length . get mText

applyEdit :: (Eq m, Mark m) => Chunk m -> MarkedText m -> MarkedText m
applyEdit ((f, t), sub) mt
  = addText (substrText False mt (-1, f))
  $ addText sub
  $ substrText False mt (t, textLength mt + 1)

-- | Applies a set of edits in such a way that the indexing is consistent.
--   If the edits overlap, then strange things happen.
applyEdits :: (Eq m, Mark m) => [Chunk m] -> MarkedText m -> MarkedText m
applyEdits edits mt
  = foldr applyEdit mt
  $ sortBy (\l -> flipOrd . comparing fst l) edits

-- | Enumerates all of the chunks of the text. The function provided as the
--   first argument is applied to each, yielding a set of edits.
edit :: (Eq m, Mark m) => (Chunk m -> [Chunk m])
     -> MarkedText m -> MarkedText m
edit f mt = (`applyEdits` mt) . concatMap f $ textChunks mt

--   the second function when the first one matches a particular mark.
whenMarked :: (a -> Bool) -> (Chunk a -> b) -> Chunk a -> [b]
whenMarked f g x@(_, MarkedText _ ms)
  | any (f . snd) ms = [g x]
  | otherwise = []

-- | Mutates a given interval of the text.
mutateSlice :: (Eq m, Mark m)
            => (MarkedText m -> MarkedText m) -> Ivl
            -> MarkedText m -> MarkedText m
mutateSlice f i mt = applyEdit (i, f $ substrText True mt i) mt

-- TODO: make more efficient - should be able to avoid slicing text.
-- | Applies a mark to the given interval.
addMark :: (Eq m, Mark m)
        => (Ivl, m) -> MarkedText m -> MarkedText m
addMark (ivl, m) = mutateSlice 
  (\(MarkedText txt ms) -> MarkedText txt $ (ivl, m) : ms) ivl

-- | Removes marks that match the given predicate.
removeMark :: ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
removeMark f (MarkedText txt xs) = MarkedText txt $ filter (not . f) xs

mutateMarks :: (Eq m, Mark m)
               => ((Ivl, m) -> Maybe (Ivl, m)) -> MarkedText m -> MarkedText m
mutateMarks f (MarkedText t ms) = MarkedText t $ ms' ++ (ms \\ del)
 where
  (del, ms') = unzip . catMaybes $ map (raiseSndA . (id &&& f)) ms

moveCursor f (i, x) 
  | isCursor x = (f i, x)
  | otherwise = (i, x)

-- | Crops and merge marks that extend beyond the bounds of the MarkedText.
--TODO: merge behaviour
clipMarks :: (Eq m, Mark m) => MarkedText m -> MarkedText m
clipMarks mt = modify mMarks (uncurry performMerges . partitionEithers . map process) mt
 where
  l = textLength mt
  process ((f, t), m)
    | f < 0 || t < 0 || f > l || t > l = Left ((wrap f, wrap t), m)
    | otherwise = Right ((f, t), m)
  wrap = max 0 . min (textLength mt)

-- Builtin Marks

data CursorMark = CursorMark deriving (Eq, Show)

instance CanBeCursor CursorMark where
  mkCursor = CursorMark
  isCursor = const True

instance Mark CursorMark where
  drawMark _ str td
    | null str = lineWidth 1 . lineColor black
               . moveOriginBy (-1.5, 2)
               . setBounds mempty
               . stroke . pathFromTrail
               $ Trail [Linear (0, 18)] False
    | otherwise = td # fc white # highlight black
  mergeMark _ _ = Just CursorMark

highlight c d = setBounds (getBounds d)
              $ d <> boxFit (boundingBox d) (square 1) # fc c

--TODO: These don't quite work the way that they should yet, in the case that
--the user-provided style overrides them. Plus they won't effect font dimension

data SizeMark = SizeMark Double
instance Mark SizeMark where
  drawMark (SizeMark size) _ = fontSize size
  
data SlantMark = SlantMark FontSlant
instance Mark SlantMark where
  drawMark (SlantMark slant) _ = fontSlant slant

data WeightMark = WeightMark FontWeight
instance Mark WeightMark where
  drawMark (WeightMark weight) _ = fontWeight weight

-- Utils

debug x = trace (show x) x
debug' p x = trace (p ++ show x) x

mapT f = f *** f

firstA f  = raiseFstA . first f
secondA f = raiseSndA . second f

raiseFstA :: Applicative m => (m a, t) -> m (a, t)
raiseFstA (x, y) = (,y) <$> x

raiseSndA :: Applicative m => (t, m a) -> m (t, a)
raiseSndA (x, y) = (x,) <$> y

flipOrd LT = GT
flipOrd EQ = EQ
flipOrd GT = LT

-- Integer interval utilities

ivlIntersect :: Ivl -> Ivl -> Maybe Ivl
ivlIntersect (f1, t1) (f2, t2)
  | f2 >= t1 = Nothing
  | f1 >= t2 = Nothing
  | otherwise = Just (max f1 f2, min t1 t2)

ivlIntersectInc :: Ivl -> Ivl -> Maybe Ivl
ivlIntersectInc (f1, t1) (f2, t2)
  | f2 > t1 = Nothing
  | f1 > t2 = Nothing
  | otherwise = Just (max f1 f2, min t1 t2)

ivlOverlaps :: Ivl -> Ivl -> Bool
ivlOverlaps a@(f1, t1) b@(f2, t2)
 =  ivlContains a f2
 || ivlContains a t2
 || ivlContains b f1
 || ivlContains b t1

ivlContains :: Ivl -> Int -> Bool
ivlContains (f, t) x = f <= x && x <= t

ivlContainsIvl :: Ivl -> Ivl -> Bool
ivlContainsIvl i (f, t) = ivlContains i f && ivlContains i t

ivlUnion :: Ivl -> Ivl -> Ivl
ivlUnion (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)

ivlMaybeUnion :: Ivl -> Ivl -> Maybe Ivl
ivlMaybeUnion a@(f1, t1) b@(f2, t2)
  | f2 > t1 = Nothing
  | f1 > t2 = Nothing
  | otherwise = Just $ ivlUnion a b

ivlSlices :: Ivl -> [Int]
ivlSlices (a, b) = [a, b]

ivlOffset :: Int -> Ivl -> Ivl
ivlOffset x (a, b) = (a + x, b + x)
