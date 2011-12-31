import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Text

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Unsafe (StyleParam, textLineBounded)

import Data.Colour.Names (black, yellow)

import System.IO.Unsafe

data TestState = TestState (MarkedText CursorMark)

main = runToy . TestState $ MarkedText "Hello, world!!" [((5,5), CursorMark)]

instance Interactive TestState where
  keyboard = simpleKeyboard handleKey
  display = diagramsDisplay handleDisplay

handleDisplay (TestState mt)
  = scaleY (-1) . (strutX 100 |||) . (strutY 100 ===)
  $ drawText (fontSize 18) mt

handleKey True e (TestState mt) = TestState $ case e of
  Right k       -> insert [k]
  Left  k       -> case k of
    "Return"    -> insert "\n"
    "Left"      -> moveCursor (-1)
    "Right"     -> moveCursor (1)
    "Escape"    -> unsafePerformIO $ (quitToy >> return mt)
--    "Delete"    -> 
--    "BackSpace" -> 
    _           -> mt
{-
  Left "End"       -> moveCursor (const 0)
  Left "BackSpace" -> backspace
  Left "Delete"    -> delete -}
 where
  editCursor f = editLocal (whenMarked isCursor $ f) mt
  insert s = editCursor $ const $ MarkedText s [((1, 1), CursorMark)]
  moveCursor p
    = editCursor
    $ addMark ((p, p), CursorMark) 
    . removeMark (isCursor . snd)
    . snd . chunkToEdit

handleKey _ _ ts = ts