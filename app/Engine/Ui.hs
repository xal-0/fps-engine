module Engine.Ui where

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
  { _widgetMouse :: Maybe (V2 Float),
    _widgetMouse1 :: Bool
  }

type Widget a b = W (WidgetInput, a) (Picture, b)

button :: Widget String (Event ())
button = proc (WidgetInput {..}, label) -> do
  ev <- edge id -< _widgetMouse1
  let col = if _widgetMouse1 then V4 0.3 0.3 1 1 else V4 0 0 1 1
      V2 w h = textSize label
      pict = PPictures
        [ PColour col (PRect (V2 (w + 4) (h + 2)))
        , PTranslate (V2 2 1) (PString label)
        ]
  returnA -< (pict, void ev)

textSize :: String -> V2 Int
textSize s = V2 (9 * length s) 15

drawUi :: Widget a b -> W a (Picture, b)
drawUi widget = proc a -> do
  _widgetMouse <- fmap Just getMouse -< ()
  _widgetMouse1 <- getMouse1 -< ()
  (p, b) <- widget -< (WidgetInput {..}, a)
  returnA -< (p, b)
