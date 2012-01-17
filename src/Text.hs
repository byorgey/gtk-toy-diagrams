module Text where

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Text

import Control.Applicative ((<$>))
import Control.Arrow (first, second)

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Text (StyleParam, textLineBounded)

import Data.Colour.Names (black, yellow)
import Data.Maybe (maybeToList)

import System.IO.Unsafe

main = runToy $ MarkedText "Hello, world!!" [((5,5), CursorMark)]

instance (Eq m, Show m, Mark m, CanBeCursor m)
      => Interactive (MarkedText m) where
  keyboard = simpleKeyboard handleKey
  display  = displayDiagram handleDisplay

handleDisplay mt
  = scaleY (-1) . (strutX 100 |||) . (strutY 100 ===)
  $ drawText (font "monospace" . fontSize 18) mt

handleKey (True, e) mt = case e of
  Right k -> insert [k]
  Left  k -> case k of
    "Return"    -> insert "\n"
    "Left"      -> mutateCursors (subtract 1)
    "Right"     -> mutateCursors (+1)
    "Home"      -> mutateCursors (const (-1, -1))
    "End"       -> mutateCursors (const (maxIx, maxIx))
    "Delete"    -> editCursors (\(ivl, _) -> (second  (+1)  ivl, justCursor))
    "BackSpace" -> editCursors (\(ivl, _) -> (first (+(-1)) ivl, justCursor))
    "Escape"    -> unsafePerformIO $ (quitToy >> return mt)
    _           -> mt
 where
  justCursor = MarkedText "" [((0, 0), mkCursor)]
  editCursors f = edit (whenMarked isCursor f) mt
  insert s = editCursors $ second $ const (MarkedText s [((p, p), mkCursor)])
    where p = length s
  mutateCursors f = clipMarks . editCursors . second . mutateMarks
                  $ \m -> first f <$> toMaybe (isCursor . snd) m
  maxIx = textLength mt

handleKey _ ts = ts

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f x = if f x then Just x else Nothing
