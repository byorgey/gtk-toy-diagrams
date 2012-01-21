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

data State = State { _txt :: MarkedText CursorMark
                   , _bnds :: Draggable (Diagram Cairo R2) }

$(mkLabels [''State])

main = runToy $ State cursorText (mkDraggable (50, 50) $ circle 100)

instance Diagrammable State Cairo R2 where
  toDiagram s = (r <> diaBnds) === hcat rest
   where
    diaBnds = toDiagram (get bnds s)
    axis = [unitX, negateV $ unitY]
    (r, rest) = fillInside (getAny . sample diaBnds) axis (P (10, 10))
              . intersperse (strutX 10)	
  			      . map (alignTL . plainText) . words
	            $ get (mText . txt) s

plainText = monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

instance Interactive State where
  display = displayDiagram (scaleY (-1) . toDiagram)
  keyboard = simpleKeyboard (\k -> modify txt (textKey k))
  mouse m i = modF bnds (mouse m $ flipMouse i)

flipMouse (InputState (x, y) kt) = InputState (x, -y) kt

modF :: (Functor f) => (b :-> a) -> (a -> f a) -> b -> f b
modF l f s = flip (set l) s <$> f (get l s)