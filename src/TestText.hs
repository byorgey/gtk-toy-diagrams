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

data TestState = TestState (MarkedText CursorMark)

main = runToy . TestState $ MarkedText "Hello, world!!" [((5,5), CursorMark)]

instance Interactive TestState where
--  keyboard = simpleKeyboard handleKey
  display = displayDiagram handleDisplay

handleDisplay (TestState mt)
  = scaleY (-1) . (strutX 100 |||) . (strutY 100 ===)
  $ drawText (font "monospace" . fontSize 18) mt

handleKey True e (TestState mt) = TestState $ case e of
--  Right k       -> insert [k]
  Left  k       -> case k of
--    "Return"    -> insert "\n"
--    "Left"      -> mutateCursors (subtract 1)
--    "Right"     -> mutateCursors (+1)
    "Delete"    -> editCursors mt (\(ivl, _) -> (second (+1) ivl, emptyText))
    "Backspace" -> editCursors mt (\(ivl, _) -> (first (+(-1)) ivl, emptyText))
    "Escape"    -> unsafePerformIO $ (quitToy >> return mt)
    _           -> mt
{-
  Left "End"       -> moveCursor (const 0)
  Left "BackSpace" -> backspace
  Left "Delete"    -> delete -}
 where
  --editCursorsInplace f = editCursors (inplace . f)
--  insert s = editCursors mt $ const $ MarkedText s [((1, 1), CursorMark)]
--  mutateCursors f = editCursors mt $ mutateMarks 
--                    (\m -> first f <$> toMaybe (isCursor.snd) m)

handleKey _ _ ts = ts

editCursors mt f = edit (maybeToList . whenMarked isCursor f) mt

toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f x = if f x then Just x else Nothing
