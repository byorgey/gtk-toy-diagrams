import Graphics.UI.Gtk.Toy.Prelude
import Graphics.UI.Gtk.Toy.Slider
import Graphics.UI.Gtk.Toy.Transformed
import Data.Dynamic
import Control.Newtype (pack)

type HandlesState = TToy [] (Slider Double)

sli :: Transformed (Slider Double)
sli = mkTransformed $ mkSlider (0, 1) (circle 5) (0, 100)
-- tog = mkTransformed $ mkToggle (circle 5) (0, 10)

main = runToy $ sli ||| sli ||| sli ||| sli ||| sli ||| sli
