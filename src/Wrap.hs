{-# LANGUAGE TemplateHaskell
           , TypeSynonymInstances
           , FlexibleInstances
           , MultiParamTypeClasses 
           , FlexibleContexts #-}

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Text
import qualified Graphics.UI.Gtk.Toy.Text.Interactive as Text
import Graphics.UI.Gtk.Toy.Wrap

import Control.Category ((.))
import Prelude hiding ((.))
import Data.Label
import Data.List (intersperse)
import Diagrams.Prelude hiding (text)
import Diagrams.Backend.Cairo

data State = State { _txt :: MarkedText CursorMark }

$(mkLabels [''State])

main = runToy $ State $ MarkedText "Hello this is a longish string of text to wrap. We'll see!!" [((0,0),CursorMark)]

plainText = Text.monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

instance Diagrammable State Cairo R2 where
  toDiagram s = result <> mconcat rest <> bnds
   where
	(result, rest) = fillInside f [unitX, negateV $ unitY] (P (10, 10))
                 . intersperse (strutX 10)	
			           . map (alignTL . plainText) . words
			           $ get (text . txt) s
	f = not . getAny . sample bnds
	bnds :: Diagram Cairo R2
   	bnds = stroke $ circle 200

instance Interactive State where
  display = displayDiagram (scaleY (-1) . Text.margin . toDiagram)
  keyboard = simpleKeyboard (\k -> modify txt (Text.handleKey k))