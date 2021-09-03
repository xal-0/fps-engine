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

button :: String -> Widget a (Event ())
button label = proc (WidgetInput {..}, _) -> do
  ev <- edge id -< _widgetMouse1
  let col = if _widgetMouse1 then V4 0.3 0.3 1 1 else V4 0 0 1 1
      pict = PPictures
        [ PColour col (PRect (V2 100 20))
        , PTranslate (V2 2 1) (PString label)
        ]
  returnA -< (pict, void ev)

drawUi :: Widget () b -> W a Picture
drawUi widget = proc _ -> do
  _widgetMouse <- fmap Just getMouse -< ()
  _widgetMouse1 <- getMouse1 -< ()
  (p, _) <- widget -< (WidgetInput {..}, ())
  returnA -< p
