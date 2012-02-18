{-# LANGUAGE RankNTypes
           , FlexibleInstances
           , MultiParamTypeClasses
           , TemplateHaskell
           , TupleSections
           , TypeFamilies
           , TypeSynonymInstances
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
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)

import Debug.Trace (trace)

type Ivl = (Int, Int)

data MarkedText m = MarkedText
  { _mText  :: String
  , _mMarks :: [(Ivl, m)]
  } deriving (Eq, Show)

$(mkLabels [''MarkedText])

newtype StyleState = StyleState StyleParam

class CanBeCursor a where
  mkCursor :: a
  isCursor :: a -> Bool

-- TODO: function to handle mark splitting

-- TODO: rename all m type variables to something else..

class Mark a where
  type DrawState a
  initialDrawState :: MarkedText a -> DrawState a

  drawStateStyle :: MarkedText a -> DrawState a -> StyleParam
  drawStateStyle _ _ = monoStyle

  drawMark :: a -> DrawState a -> MarkedText a -> CairoDiagram
  drawMark _ s m = drawRec s m

  mergeMark :: a -> a -> Maybe a
  mergeMark _ _ = Nothing

monoStyle :: StyleParam
monoStyle = font "monospace" . fontSize 18

emptyText :: MarkedText m
emptyText = MarkedText "" []

plainText :: String -> MarkedText m
plainText t  = MarkedText t []

-- | Extract an interval of the text.  First parameter is True if inclusive.
substrText :: Bool -> MarkedText m -> Ivl -> MarkedText m
substrText inc (MarkedText str ms) ivl@(f, t)
  = MarkedText (take count $ drop f' str)
  . catMaybes $ map (firstA $ local) ms
 where
  f' = max 0 f
  count = t - f'
-- Transform intervals into the result, yielding Nothing if they aren't
-- inside the substring interval.
  local x | x == ivl = Just (0, count)
          | inc && fst x ==  0 && fst ivl ==  0 = Just (0, min (snd x) (snd ivl))
          | inc && snd x == tl && snd ivl == tl = Just (max (fst x) (fst ivl), tl)
          | otherwise = mapT (subtract f') <$> ivlIntersect ivl x
  tl = length str

-- Sort marks, big first.
sortMarks :: [(Ivl, m)] -> [(Ivl, m)]
sortMarks = sortBy (comparing (\((f, t), _) -> (f, f - t)))

-- | Concatenates two marked texts together, attempting to merge the marks
--   incident on the join.
addText :: forall m. (Mark m, Eq m) 
      => MarkedText m -> MarkedText m -> MarkedText m
addText (MarkedText at ams) (MarkedText bt bms)
  = MarkedText (at ++ bt) rms
 where
  al = length at

-- Separate off marks that cross the border between the texts.
  (ap, an) = partition ((>=al - 1) . snd . fst) ams

  (bp, bn) = mapT (map $ first $ mapT (+al))
           $ partition ((<=0)  . fst . fst) bms

-- Collect all of the resulting marks.
  rms = sortMarks $ an ++ performMerges ap bp ++ bn

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

drawText' :: Mark m => MarkedText m -> CairoDiagram
drawText' mt = drawText (initialDrawState mt) mt

drawText :: Mark m => DrawState m -> MarkedText m -> CairoDiagram
drawText initial mt
  = vcat
  . map (drawRec initial . substrText True sorted . second (+1))
  . ivlsFromSlices (textLength mt)
  . findIndices (=='\n')
  $ get mText mt
 where
  sorted = modify mMarks sortMarks mt

drawRec :: forall m. Mark m => DrawState m -> MarkedText m -> CairoDiagram
drawRec st mt@(MarkedText txt [])
  = textLineBounded (drawStateStyle mt st) $ filter (not . (`elem` "\r\n")) txt
drawRec st mt@(MarkedText txt (((fm, tm), m):xs))
  =   drawRec st (substrText False mt (-1, fm))
  ||| drawMark m st (substrText True mt (fm, tm))
  ||| drawRec st (substrText False mt (tm, length txt + 1))

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
addMark m (MarkedText txt ms) = MarkedText txt $ m : ms
{- TODO: fix
addMark (ivl, m) = mutateSlice 
  (\(MarkedText txt ms) -> MarkedText txt $ (ivl, m) : ms) ivl
-}

addMarks ms t = foldr addMark t ms

-- | Removes marks that match the given predicate.
removeMark :: ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
removeMark f (MarkedText txt xs) = MarkedText txt $ filter (not . f) xs

mutateMarks :: (Eq m, Mark m)
            => ((Ivl, m) -> Maybe (Ivl, m)) -> MarkedText m -> MarkedText m
mutateMarks f (MarkedText t ms) = MarkedText t $ mapMaybe f ms
{- Old defn - Nothing indicates noop)
mutateMarks f (MarkedText t ms) = MarkedText t $ ms' ++ (ms \\ del)
 where
  (del, ms') = unzip . catMaybes $ map (raiseSndA . (id &&& f)) ms
-}

filterMarks :: (Eq m, Mark m)
            => ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
filterMarks f = mutateMarks (\m -> if f m then Just m else Nothing)

moveCursor f (i, x) 
  | isCursor x = (f i, x)
  | otherwise = (i, x)

-- | Crops and merge marks that extend beyond the envelope of the MarkedText.
--TODO: merge behaviour
clipMarks :: (Eq m, Mark m) => MarkedText m -> MarkedText m
clipMarks mt = modify mMarks (uncurry performMerges . partitionEithers . map process) mt
 where
  l = textLength mt
  process ((f, t), m)
    | f < 0 || t < 0 || f > l || t > l = Left ((wrap f, wrap t), m)
    | otherwise = Right ((f, t), m)
  wrap = max 0 . min (textLength mt)

clearMarks :: MarkedText m -> MarkedText m
clearMarks (MarkedText t _) = MarkedText t []

-- Builtin Marks

data EmptyMark = EmptyMark deriving (Eq, Show)

instance Mark EmptyMark where
  type DrawState EmptyMark = ()
  initialDrawState _ = ()
  drawStateStyle _ _ = monoStyle
  drawMark m _ mt = drawRec () mt
  mergeMark _ _ = Just EmptyMark

data CursorMark = CursorMark deriving (Eq, Show)

instance CanBeCursor CursorMark where
  mkCursor = CursorMark
  isCursor = const True

instance Mark CursorMark where
  type DrawState CursorMark = StyleState
  initialDrawState _ = StyleState monoStyle
  drawStateStyle _ (StyleState s) = s
  drawMark m (StyleState s) mt@(MarkedText txt _)
    | null txt = lineWidth 1 . lineColor black
               . moveOriginBy (-1.5, 2)
               . setEnvelope mempty
               . stroke . pathFromTrail
               $ Trail [Linear (0, 18)] False
    | otherwise = highlight black $ drawRec (StyleState $ s . fc white) mt 
  mergeMark _ _ = Just CursorMark

highlight c d = setEnvelope (getEnvelope d)
              $ d <> boxFit (boundingBox d) (square 1) # fc c

--TODO: These don't quite work the way that they should yet, in the case that
--the user-provided style overrides them. Plus they won't effect font dimension

data SizeMark = SizeMark Double
instance Mark SizeMark where
  type DrawState SizeMark = StyleState
  initialDrawState _ = StyleState monoStyle
  drawStateStyle _ (StyleState s) = s
  drawMark (SizeMark size) (StyleState s) mt
    = drawRec (StyleState $ fontSize size . s) mt
  
data SlantMark = SlantMark FontSlant
instance Mark SlantMark where
  type DrawState SlantMark = StyleState
  initialDrawState _ = StyleState monoStyle
  drawStateStyle _ (StyleState s) = s
  drawMark (SlantMark slant) (StyleState s) mt
    = drawRec (StyleState $ fontSlant slant . s) mt

data WeightMark = WeightMark FontWeight
instance Mark WeightMark where
  type DrawState WeightMark = StyleState
  initialDrawState _ = StyleState monoStyle
  drawStateStyle _ (StyleState s) = s
  drawMark (WeightMark weight) (StyleState s) mt
    = drawRec (StyleState $ fontWeight weight . s) mt

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
