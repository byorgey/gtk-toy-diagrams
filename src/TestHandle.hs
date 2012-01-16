import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable

import Diagrams.Prelude

import Debug.Trace

debug x = trace (show x) x

data TestState = TestState (Draggable CairoDiagram)

traceTS ts@(TestState (Draggable a b _)) = trace (show a ++ " " ++ show b) ts

instance Interactive TestState where
  mouse m i (TestState d) = (traceTS . TestState) <$> mouse m i d
  display dw i (TestState x) = (traceTS . TestState) <$> display dw i x
  keyboard = quitKeyboard

main = runToy $ mkDraggable (50, 50) handle

handle :: CairoDiagram
handle = circle 5