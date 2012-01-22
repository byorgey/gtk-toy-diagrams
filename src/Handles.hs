import Graphics.UI.Gtk.Toy.Prelude

type HandlesState = TToy [] (Draggable CairoDiagram)

main = runToy $ TToy
     [ mkDraggable (x, y) (circle 5 :: CairoDiagram)
     | x <- [50,60..100], y <- [50, 60..100] ]
