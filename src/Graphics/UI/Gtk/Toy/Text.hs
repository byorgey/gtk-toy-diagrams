{-# LANGUAGE TupleSections
           , ScopedTypeVariables
           , RankNTypes
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
import Diagrams.Backend.Cairo.Unsafe (StyleParam, textLineBounded)

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative ((<$>))
import Control.Monad (msum)

import Data.Colour.Names (black, white)
import Data.Label
import Data.List (partition, findIndices, sortBy, sort, delete, group, (\\))
import Data.Maybe (fromJust, catMaybes, maybeToList)
import Data.Ord (comparing)

import Debug.Trace (trace)

data MarkedText m = MarkedText
  { _text  :: String
  , _marks :: [(Ivl, m)]
  } deriving (Eq, Show)

class CanBeCursor a where
  mkCursor :: a
  isCursor :: a -> Bool

-- TODO: function to handle mark splitting

-- TODO: rename all m type variables to something else..

class Mark a where
  drawMark :: a -> String -> CairoDiagram -> CairoDiagram
  mergeMark :: a -> a -> Maybe a
  drawMark _ _ = id
  mergeMark _ _ = Nothing

emptyText :: MarkedText m
emptyText = MarkedText "" []

-- | Extract an interval of the text.
substrText :: Ivl -> MarkedText m -> MarkedText m
substrText ivl@(f, t) (MarkedText str ms)
  = MarkedText (take (t - f) $ drop f str)
  . catMaybes $ map (firstA $ local) ms
 where
-- Transform intervals into the result, yielding Nothing if they aren't
-- inside the substring interval.
  local x | x == ivl = Just (0, t - f)
          | otherwise = mapT (subtract f) <$> ivlIntersect ivl x

-- | Concatenates two marked texts together, attempting to merge the marks
--   incident on the join.
addText :: forall m. (Mark m, Eq m, Show m) 
      => MarkedText m -> MarkedText m -> MarkedText m
addText (MarkedText at ams) (MarkedText bt bms)
  = MarkedText (at ++ bt) rms
 where
  al = length at

-- Separate off marks that cross the border between the texts.
  (ap, an) = partition ((>=al) . snd . fst) ams

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

-- | Draws the marked text, given a style to apply by default to the unmarked text.
drawText :: (Show a, Mark a) => StyleParam -> MarkedText a -> CairoDiagram
drawText style mt
  = vcat
  . map (hcat . window3 chunk . dummyPad . textChunks . (`substrText` mt))
  . ivlsFromSlices (textLength mt)
  . findIndices (=='\n')
  $ _text mt
 where
  dummyPad = ([((0,0), ("", [])), ((0,1), (" ", []))] ++)
--TODO: sweepline-style context accumulation would be more efficient.
  chunk l1 l2 (_,(t,ms)) = foldr (`drawMark` t) (text l1 l2 t) ms
  text (_,(s1,_)) (_,(s2,_)) t 
    = --strutX (kerningCorrection style (last $ s1 ++ s2) $ head t) |||
      textLineBounded style t
  window3 f (x:y:z:xs) = f x y z : window3 f (y:z:xs)
  window3 f _ = []

-- | Breaks the marked text into chunks such that the marks stay consistent
--   throughout.
textChunks mt
  = map (id &&& (sansIvls . (`substrText` mt)))
  . ivlsFromSlices (textLength mt)
  $ concatMap (ivlSlices . fst) $ _marks mt
 where
  sansIvls (MarkedText txt ms) = (txt, map snd ms)

-- | Given a list of slice points, and an overall length, yields a list of
--   intervals broken at those points.
ivlsFromSlices l xs = map head . group $ zip (0 : ys) (ys ++ [l])
  where ys = sort xs

textLength = length . _text

-- | A chunk is a located piece of text with the same marks across its content.
type Chunk m = (Ivl, (String, [m]))

-- | An edit specifies a deletion interval, with the marked text to insert.
type Edit m = (Ivl, MarkedText m)

-- | Gives chunk's marks intervals that span the entire text.
chunkToEdit :: Chunk m -> Edit m
chunkToEdit (ivl, (txt, ms)) = (ivl, MarkedText txt $ map ((0, length txt),) ms)

-- | Applies an edit to a marked text, substituting an interval for a
--   replacement.
applyEdit :: (Eq m, Mark m, Show m) => Edit m -> MarkedText m -> MarkedText m
applyEdit ((f, t), sub) mt
  = addText (substrText (0, f) mt)
  $ addText sub
  $ substrText (t, textLength mt) mt

-- | Applies a set of edits in such a way that the indexing is consistent.
--   If the edits overlap, then strange things happen.
applyEdits :: (Eq m, Mark m, Show m) => [Edit m] -> MarkedText m -> MarkedText m
applyEdits edits mt
  = foldr applyEdit mt
  $ sortBy (\l -> flipOrd . comparing fst l) edits

-- | Enumerates all of the chunks of the text. The function provided as the
--   first argument is applied to each, resulting in a set of edits.
edit :: (Eq m, Mark m, Show m) => (Chunk m -> [Edit m])
     -> MarkedText m -> MarkedText m
edit f mt = (`applyEdits` mt) . concatMap f $ textChunks mt

-- | Creates an editing function that replaces chunks.
--inplace :: (Chunk m -> Maybe (MarkedText m)) -> [Edit m]
inplace f = maybeToList . raiseSndA . (fst &&& f)

-- | This is useful for building functions to use with editLocal.  It applies
--   the second function when the first one matches a particular mark.
whenMarked :: (Monoid m) => (a -> Bool) -> (Chunk a -> b) -> Chunk a -> m b
whenMarked f g x@(_, (_, ms))
  | any f ms = Just $ g x
  | otherwise = Nothing

-- | Mutates a given interval of the text.
mutateSlice :: (Eq m, Show m, Mark m)
            => (MarkedText m -> MarkedText m) -> Ivl
            -> MarkedText m -> MarkedText m
mutateSlice f i mt = applyEdit (i, f $ substrText i mt) mt

-- TODO: make more efficient - should be able to avoid slicing text.
-- | Applies a mark to the given interval.
addMark :: (Eq m, Show m, Mark m)
        => (Ivl, m) -> MarkedText m -> MarkedText m
addMark (ivl, m) = mutateSlice 
  (\(MarkedText txt ms) -> MarkedText txt $ (ivl, m) : ms) ivl

-- | Removes marks that match the given predicate.
removeMark :: ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
removeMark f (MarkedText txt xs) = MarkedText txt $ filter (not . f) xs

mutateMarks :: (Eq m, Mark m, Show m)
            => ((Ivl, m) -> Maybe (Ivl, m)) -> MarkedText m -> MarkedText m
mutateMarks f (MarkedText t ms) = MarkedText t $ ms' ++ (ms \\ del)
 where
  (del, ms') = unzip . catMaybes $ map (raiseSndA . (id &&& f)) ms

moveCursor f (i, x) 
  | isCursor x = (f i, x)
  | otherwise = (i, x)

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
type Ivl = (Int, Int)

ivlIntersect :: Ivl -> Ivl -> Maybe Ivl
ivlIntersect (f1, t1) (f2, t2)
  | f2 >= t1 = Nothing
  | f1 >= t2 = Nothing
  | otherwise = Just (max f1 f2, min t1 t2)

ivlOverlaps :: Ivl -> Ivl -> Bool
ivlOverlaps a@(f1, t1) b@(f2, t2)
 =  ivlContains a f2
 || ivlContains a t2
 || ivlContains b f1
 || ivlContains b t1

ivlContains :: Ivl -> Int -> Bool
ivlContains (f, t) x = f <= x && x <= t

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