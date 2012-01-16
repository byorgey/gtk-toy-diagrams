import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable

import Diagrams.Prelude

data TestState = TestState (Draggable CairoDiagram)

instance Interactive TestState where
  mouse m i (TestState d) = TestState <$> mouse m i d
  display dw i (TestState x) = TestState <$> display dw i x
  keyboard = quitKeyboard

main = runToy $ mkDraggable (50, 50) (circle 5 :: CairoDiagram)