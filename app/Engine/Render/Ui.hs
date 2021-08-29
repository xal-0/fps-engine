module Engine.Render.Ui () where

import Graphics.GPipe

data Picture
  = PRect !(V2 Int) !(V2 Int) !(V3 Float)
  | PString !String
  | PTranslate !(V2 Int) !Picture
  | PPictures ![Picture]
