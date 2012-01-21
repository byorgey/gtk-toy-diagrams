import Graphics.UI.Gtk.Toy.Prelude

type HandlesState = ToyTraversable [] (Draggable CairoDiagram)

main = runToy $ ToyTraversable 
     [ mkDraggable (x, y) (circle 5 :: CairoDiagram)
     | x <- [50,60..100], y <- [50, 60..100] ]
