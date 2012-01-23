-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.Gtk.Toy.Demo
-- Copyright   :  (c) 2012 Michael Sloan (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  mgsloan@gmail.com
--
-- Convenient way of making manipulable things.
--
-----------------------------------------------------------------------------

import Control.Applicative ((<$>))
import Data.Dynamic
import Data.Label
import Data.Map as M

-- Used for passing optional options.
glookup :: (Typeable a, Ord k) => k -> a -> Map k Dynamic -> a
glookup k x m = maybe x id . fromDynamic <$> M.lookup k m

class Manipulable a where
  type Manipulator a
  mkManipulate  :: M.Map String Dynamic -> a -> Manipulator a
  manipulated   :: Manipulator a :-> a

manipulate = manipulateOpts M.empty

handleOpts opts = glookup "handle" (circle (glookup "size" 5 opts)) opts

instance Manipulable R2 where
  type Manipulator R2 = Draggable CairoDiagram
  manipulateOpts opts x = mkDraggable x $ handleOpts opts
  manipulated = dragOffset

instance Manipulable Double where
  type Manipulator Double = Slider Double
  manipulateOpts opts x
    = mkSlider (glookup "interval" (0, 1) opts) (handleOpts opts)
    $ glookup "delta" (0, 100) opts
  manipulated = sliderValue

instance Manipulable Bool where
  type Manipulator Double = Slider Bool
  manipulateOpts opts x
    = mkToggle (handleOpts opts)
    $ glookup "delta" (0, 5) opts
  manipulated = sliderValue

{-
instance Manipulable String where
  type Manipulator String = MarkedText CursorMark
  manipulateOpts opts x
    = 
-}

data RManip a = RManip a

class ManipFunc a where
  type ManipResult a
  getManips :: a -> [IManip]
  runManips :: a -> [IManip] -> ManipResult a

instance ManipFunc (ManipR a) where
  type ManipResult a = a
  getManips = const []
  runManips = const

instance (Default a, ManipFunc b)
      => ManipFunc (a -> b) where
  type ManipResult a = ManipResult b
  getManips _ = manipulate (def :: a) : getManips (undefined :: b)
  runManips [] = 
  runManips f (m:ms) = runManips (f (get manipulated $ fromDyn m)) ms

{-
instance (Interactive a, Interactive b)
      => Interactive (Demo a b) where
  tick i (Demo x f) = do
    x' <- tick i x
    tick i (f x')

--data Demo a b = Demo [IManip] ([IManip] -> b)

class Demoable a b where
  demo :: Demo a b

instance (Manipulable a, Demoable b)
      => Demoable (a -> b) where
  demo f = manipulate
-}

{-
data Demo a b = Demo a b

class Typeable a => Demo a b where
  demoNamed :: [String] -> a -> Demo b
  demo :: String -> a -> Demo b
  demo = show . typeOf
-}