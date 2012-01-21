import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable

import Diagrams.Prelude

type HandlesState = ToyTraversable [] (Draggable CairoDiagram)

main = runToy $ ToyTraversable 
     [ mkDraggable (x, y) (circle 5 :: CairoDiagram)
     | x <- [50,60..100], y <- [50, 60..100] ]
