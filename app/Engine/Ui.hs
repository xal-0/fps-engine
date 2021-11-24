{-# LANGUAGE TemplateHaskell #-}

module Engine.Ui where

import Control.Lens
import Control.Monad
import Engine.Logic
import FRP.Netwire hiding (id)
import Linear

data Picture
  = PRect !(V2 Int)
  | PString String
  | PTranslate !(V2 Int) Picture
  | PColour !(V4 Float) Picture
  | PPictures [Picture]
  | PNone

data WidgetInput = WidgetInput
  { _widgetMouse :: V2 Int,
    _widgetMouse1 :: Bool
  }

makeLenses ''WidgetInput

type Widget a b = W (WidgetInput, a) (Picture, b)

button :: Widget String (Event ())
button = proc (input@WidgetInput {..}, label) -> do
  let mouseOn = all (>= 0) _widgetMouse && and (liftA2 (<=) _widgetMouse rectSize)
      col =
        if
            | _widgetMouse1 && mouseOn -> V4 0.4 0.4 1 1
            | mouseOn -> V4 0.2 0.2 1 1
            | otherwise -> V4 0 0 1 1
      V2 w h = textSize label
      rectSize = V2 (w + 4) (h + 2)
      pict =
        PPictures
          [ PColour col (PRect rectSize),
            PTranslate (V2 2 1) (PString label)
          ]
  click <- clicked -< input
  ev <- void ^<< filterE id -< mouseOn <$ click
  returnA -< (pict, ev)

clicked :: W WidgetInput (Event ())
clicked = _widgetMouse1 ^>> edge id >>> filterE id >>^ void

textSize :: String -> V2 Int
textSize s = V2 (8 * length s) 15

translate :: Widget a b -> Widget (a, V2 Int) b
translate widget = proc (input, (a, pos)) -> do
  (p, b) <- widget -< (input & widgetMouse -~ pos, a)
  returnA -< (PTranslate pos p, b)

drawUi :: Widget a b -> W a (Picture, b)
drawUi widget = proc a -> do
  _widgetMouse <- fmap round ^<< getMouse -< ()
  _widgetMouse1 <- getMouse1 -< ()
  (p, b) <- widget -< (WidgetInput {..}, a)
  returnA -< (p, b)
