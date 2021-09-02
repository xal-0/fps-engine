{-# LANGUAGE TemplateHaskell #-}

module Engine.World where

import Control.Lens
import Data.Default
import Engine.Logic

data World = World

instance Default World where
  def = World

makeLenses ''World

worldWire :: W a World
worldWire = undefined
