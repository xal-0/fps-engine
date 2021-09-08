{-# LANGUAGE TemplateHaskell #-}

module Engine.World where

import Control.Lens
import Data.Default
import Engine.Logic
import Engine.Player
import Engine.Ui
import FRP.Netwire

data World = World
  { _worldUi :: Picture
  , _worldPlayer :: Player
  }

instance Default World where
  def = World {_worldUi = PNone, _worldPlayer = def}

makeLenses ''World

worldWire :: W a World
worldWire = proc _ -> do
  _worldUi <- pure PNone -< ()
  _worldPlayer <- playerWire -< ()
  returnA -< World {..}
