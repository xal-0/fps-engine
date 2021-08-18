{-# LANGUAGE TemplateHaskell #-}

module Engine.Player () where

import Control.Lens
import Engine.Logic
import Linear hiding (identity)
import Control.Wire
import FRP.Netwire

data Player = Player
  { _playerPos :: !(V3 Float),
    _playerLook :: !(V3 Float)
  }

makeLenses ''Player

playerWire :: W a Player
playerWire = Player <$> pos <*> undefined
  where
    pos = pure 0

    look = proc _ -> do
      adj <- lookAdjs -< ()
      returnA -< 0
      -- integral (V3 0 0 (-1))

    lookAdjs = proc _ -> do
      V2 x y <- getCursor -< ()
      yaw' <- derivative <|> 0 -< -x
      pitch' <- derivative <|> 0 -< -y
      returnA -< axisAngle (V3 0 1 0) (yaw' * sensitivity)
        * axisAngle (V3 1 0 0) (pitch' * sensitivity)

sensitivity :: Float
sensitivity = 0.1
