{-# LANGUAGE TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , MultiParamTypeClasses 
           , FlexibleContexts
           , TypeOperators #-}

import Graphics.UI.Gtk.Toy.Prelude

import Control.Applicative ((<$>))
import Control.Category ((.))
import Prelude hiding ((.))
import Data.Label
import Data.List (intersperse)
import Diagrams.Prelude hiding (text)
import Diagrams.Backend.Cairo
import Diagrams.Layout.Wrap

type MTC = MarkedText CursorMark

data State = State { _txt :: MTC
                   , _bnds :: Draggable (Diagram Cairo R2) }

$(mkLabels [''State])

main = runToy $ State (addText cursorText (plainText "this is a series of words.") :: MTC) (mkDraggable (50, 50) $ circle 100)

instance Diagrammable State Cairo R2 where
  toDiagram s = r <> diaBnds
   where
    diaBnds = toDiagram (get bnds s)
    axis = [unitX, negateV $ unitY]
    r = wrapDiagram
      . wrapInside (getAny . sample diaBnds) axis (P (10, 10))
      . intersperse (strutX 10)	
      . map (alignTL . monoText . (plainText :: String -> MTC)) . words
      $ get (mText . txt) s

instance Interactive State where
  keyboard = simpleKeyboard (\k -> modify txt (textKeyHandler k))
  mouse m i = modF bnds (mouse m $ flipMouse i)

instance GtkInteractive State where
  display = displayDiagram (scaleY (-1) . toDiagram)

flipMouse (InputState (x, y) kt) = InputState (x, -y) kt

modF :: (Functor f) => (b :-> a) -> (a -> f a) -> b -> f b
modF l f s = flip (set l) s <$> f (get l s)
