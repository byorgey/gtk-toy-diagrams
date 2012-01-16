{-# LANGUAGE ViewPatterns #-}

import Graphics.UI.Gtk.Toy
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Draggable

import Diagrams.Prelude
import Physics.ForceLayout

import Data.Label
import qualified Data.Map as M

data State = State (Ensemble R2) (M.Map PID (Draggable CairoDiagram))

instance Interactive State where
  mouse   = simpleMouse
          $ \p m (State e hm) -> State e $ M.map (mouseDrag p m) hm

  tick    = simpleTick
          $ \(State e hm) -> updateHandles $ State (ensembleStep 0.1 e) hm

  display = displayDiagram 
          $ \(State _ hm) -> (mconcat . map toDiagram $ M.elems hm)
  
  keyboard = quitKeyboard

updateHandles :: State -> State
updateHandles (State e hm) = State (modify particles (M.intersectionWith constrain hm) e)
                                   (M.intersectionWith update hm (get particles e))
 where
-- Move particle to its handle if dragged.
  constrain d p 
    | isDragging d = set pos (P $ dragOffset d) p
    | otherwise    = p
-- Move non-dragging handles to their particles.
  update d (get pos -> P p)
    | isDragging d = d
    | otherwise    = set dragOffsetAcc p d

main = runToy 
     $ State e
     $ M.fromList $ [(k, mkDraggable p $ circle 5) | (k, get pos -> P p) <- M.toList particleMap]
 where
  e = Ensemble [ (edges,    hookeForce 1 4)
               , (allPairs, coulombForce 1)
               ]
               particleMap
  edges       = [(1,2), (2,3), (2,5), (3,5), (3,4), (4,5)]
  allPairs    = [(x,y) | x <- [1..4], y <- [x+1..5]]
  particleMap = M.fromList . zip [1..]
              . map (initParticle . P)
              $ [ (102.0, 103.1), (106.3, 107.2)
                , (100.3, 104.2), (101.6, 99.1)
                , (104.8, 102.9)
                ]