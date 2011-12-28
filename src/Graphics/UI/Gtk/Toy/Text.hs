{-# LANGUAGE TupleSections, ScopedTypeVariables, RankNTypes #-}
module Graphics.UI.Gtk.Toy.Text where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams

import Diagrams.Prelude hiding ((<.>))
import Diagrams.TwoD.Path.Turtle
import Diagrams.TwoD.Text
import Diagrams.Backend.Cairo.Unsafe (StyleParam, textLineBounded)

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative ((<$>))
import Control.Monad (msum)

import Data.Colour.Names (black, white)
import Data.Label
import Data.List (partition, findIndices, sortBy, sort, delete, group)
import Data.Maybe (fromJust, catMaybes, maybeToList)
import Data.Ord (comparing)
--import Data.Label.Maybe

import Debug.Trace (trace)

data MarkedText m = MarkedText
  { _text  :: String
  , _marks :: [(Ivl, m)]
  } deriving (Eq, Show)

-- $(mkLabels [''MarkedText])
class CanBeCursor a where
  mkCursor :: a
  isCursor :: a -> Bool

-- TODO: function to handle mark splitting

class Mark a where
  drawMark :: a -> String -> CairoDiagram -> CairoDiagram
  mergeMark :: a -> a -> Maybe a
  drawMark _ _ = id
  mergeMark _ _ = Nothing

(<.>) f g x = f <$> g x

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f = f *** f

plainText str = MarkedText str []

addText :: forall m. (Mark m, Eq m, Show m) 
      => MarkedText m -> MarkedText m -> MarkedText m
addText (MarkedText at ams) (MarkedText bt bms) = MarkedText (at ++ bt) rms
 where
  al = length at

-- Separate off marks that are right on the border between the texts.
  (ap, an) = debug' "a " $ partition ((==al) . snd . fst) ams
  (bp, bn) = debug' "b " $ partition ((==0)  . fst . fst) bms

-- Collect all of the resulting marks.
  rms = sortBy (comparing fst) $ an ++ mms ++ map (first (+(al, al))) (bp' ++ bn)

-- Attempt to merge all of the marks.
  (bp', mms) = scanFold findMerge bp ap

-- Try to merge every item of the list with the given item, and remove the item
-- that succeeds.
  findMerge xs unmerged@((f, t), m)
    = fromJust . msum . (++ [Just (xs, unmerged)])
    $ map (raiseSndA . ((`delete` xs) &&& process)) xs
   where
-- Try to merge.  When successful, extend the interval appropriately.
    process :: (Ivl, m) -> Maybe (Ivl, m)
    process = first ((f,) . (t+) . snd) <.> secondA (mergeMark m)

--TODO: test
scanFold :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
scanFold _ a [] = (a, [])
scanFold f a (b:bs) = second (c:) $ scanFold f a' bs
  where (a', c) = f a b

substrText :: Ivl -> MarkedText m -> MarkedText m
substrText ivl@(f, t) (MarkedText str ms)
  = MarkedText substr . catMaybes
  $ map (firstA $ local) ms
 where
  substr = take (t - f) $ drop f str
  local x | x == ivl = Just (0, uncurry (-) x)
          | otherwise = ivlLocal ivl x

ivlsFromSlices l xs = map head . group $ zip (0 : ys) (ys ++ [l])
  where ys = sort xs

inverseIvls l xs
  = zipWith (\(_, f) (t, _) -> (f, t)) ((0, 0) : xs) (xs ++ [(l, l)])

--TODO: consider a mark for lines / line #s?
--TODO: figure out how style will be applied to the text

debug x = trace (show x) x
debug' p x = trace (p ++ show x) x

textLength = length . _text

drawText :: (Show a, Mark a) => StyleParam -> MarkedText a -> CairoDiagram
drawText style mt = vcat $ map (hcat . chunks . markSliced . (`substrText` mt)) ns
 where
  ns = ivlsFromSlices (textLength mt) $ findIndices (=='\n') (_text mt)
  -- TODO: sweepline-style context accumulation would be more efficient.
  chunks = map (\(_, (txt, ms))
                 -> foldr (`drawMark` txt) (textLineBounded style txt) ms)

markSliced mt 
  = map (id &&& sansIvls . (`substrText` mt))
  . ivlsFromSlices (textLength mt)
  $ concatMap (ivlSlices . fst) $ _marks mt
 where
  sansIvls (MarkedText txt ms) = (txt, map snd ms)

getCursors :: (CanBeCursor m) => MarkedText m -> [Ivl]
getCursors = map fst . filter (isCursor . snd) . _marks

type Chunk m = (Ivl, (String, [m]))
type Edit m = (Ivl, MarkedText m)

chunkToEdit :: Chunk m -> Edit m
chunkToEdit (ivl, (txt, ms)) = (ivl, MarkedText txt $ map ((0, length txt),) ms)

applyEdit :: (Eq m, Mark m, Show m) => Edit m -> MarkedText m -> MarkedText m
applyEdit ((f, t), sub) mt
  = addText (substrText (0, f) mt)
  $ addText sub
  $ substrText (t, length $ _text mt) mt

applyEdits :: (Eq m, Mark m, Show m) => [Edit m] -> MarkedText m -> MarkedText m
applyEdits edits mt
  = foldr applyEdit mt
  $ sortBy (\l -> flipOrd . comparing fst l) edits

edit :: (Eq m, Mark m, Show m)
     => (Chunk m -> [Edit m])
     -> MarkedText m -> MarkedText m
edit f mt = (`applyEdits` mt) . concatMap f $ markSliced mt

editLocal :: (Eq m, Mark m, Show m)
     => (Chunk m -> Maybe (MarkedText m))
     -> MarkedText m -> MarkedText m
editLocal f = edit (maybeToList . raiseSndA . (fst &&& f))

whenMarked :: (m -> Bool)
           -> (Chunk m -> MarkedText m)
           -> Chunk m -> Maybe (MarkedText m)
whenMarked f g x@(_, (_, ms))
  | any f ms = Just $ g x
  | otherwise = Nothing

--TODO: merge logic..

addMark :: (Ivl, m) -> MarkedText m -> MarkedText m
addMark x (MarkedText txt xs) = MarkedText txt $ x : xs

removeMark :: ((Ivl, m) -> Bool) -> MarkedText m -> MarkedText m
removeMark f (MarkedText txt xs) = MarkedText txt $ filter (not . f) xs

cw  (x, y) = (negate y, x)
ccw (x, y) = (y, -x)


-- Builtin Marks

data CursorMark = CursorMark deriving (Eq, Show)

instance CanBeCursor CursorMark where
  mkCursor = CursorMark
  isCursor = const True

instance Mark CursorMark where
  drawMark _ str td
    | null str = lineWidth 1 . lineColor black . stroke . runTurtle
               $  penUp >> forward 1.5 >> penDown
               >> left 90 >> forward 18
    | otherwise = td # fc white # highlight black

highlight c d = setBounds (getBounds d)
              $ d <> boxFit (boundingBox d) (square 1) # fc c

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
ivlOverlaps a@(af, at) b@(bf, bt)
 =  ivlContains a bf
 || ivlContains a bt
 || ivlContains b af
 || ivlContains b at

ivlContains :: Ivl -> Int -> Bool
ivlContains (f, t) x = f <= x && x <= t

ivlLocal :: Ivl -> Ivl -> Maybe Ivl
ivlLocal a@(f, _) b = (subtract f *** subtract f) <$> ivlIntersect a b

ivlUnion :: Ivl -> Ivl -> Ivl
ivlUnion (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)

ivlSlices :: Ivl -> [Int]
ivlSlices (a, b) = [a, b]

ivlOffset :: Int -> Ivl -> Ivl
ivlOffset x (a, b) = (a + x, b + x)