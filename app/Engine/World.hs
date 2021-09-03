{-# LANGUAGE TemplateHaskell #-}

module Engine.World where

import Control.Lens
import Data.Default
import Engine.Logic
import Engine.Ui
import FRP.Netwire

newtype World = World
  { _worldUi :: Picture
  }

instance Default World where
  def = World {_worldUi = PNone}

makeLenses ''World

worldWire :: W a World
worldWire = proc _ -> do
  _worldUi <- drawUi (button "hello") -< ()
  returnA -< World {..}
