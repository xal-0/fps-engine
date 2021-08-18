{-# LANGUAGE TemplateHaskell #-}

module Engine.Player () where

import Control.Lens
import Engine.Logic
import Linear hiding (identity)

data Player = Player
  { _playerPos :: !(V3 Float),
    _playerLook :: !(Quaternion Float)
  }

makeLenses ''Player
