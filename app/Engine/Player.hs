{-# LANGUAGE TemplateHaskell #-}

module Engine.Player (Player, playerPos, playerAng, playerWire, playerMat) where

import Control.Lens
import Control.Wire
import Data.Bool
import Data.Default
import Engine.Logic
import FRP.Netwire
import qualified Graphics.GPipe.Context.GLFW.Input as GLFW
import Graphics.GPipe.Expr (clamp)
import Linear hiding (identity, trace)

data Player = Player
  { _playerPos :: !(V3 Float),
    _playerAng :: !(V2 Float) -- Euler
  }
  deriving (Show)

instance Default Player where
  def = Player 0 (V2 0 0)

makeLenses ''Player

integralClamp :: Float -> Float -> Float -> W Float Float
integralClamp lo hi a = mkPure \_ da ->
  let a' = clamp lo (a + da) hi in a' `seq` (Right a', integralClamp lo hi a')

playerWire :: W a Player
playerWire = Player <$> pos <*> look
  where
    pos = proc _ -> do
      w <- bool 0 1 ^<< getKeyPress GLFW.Key'W -< ()
      a <- bool 0 1 ^<< getKeyPress GLFW.Key'A -< ()
      s <- bool 0 1 ^<< getKeyPress GLFW.Key'S -< ()
      d <- bool 0 1 ^<< getKeyPress GLFW.Key'D -< ()

      rot <- lookRot ^<< look -< ()
      let forwardv = rotate rot (V3 1 0 0)
          leftv = rotate rot (V3 0 1 0)

      integral 0
        -<
          forwardv ^* velocity * (w - s)
            + leftv ^* velocity * (a - d)

    look = proc _ -> do
      V2 cx cy <- getMouse -< ()
      cx' <- derivative <|> 0 -< - cx
      cy' <- derivative <|> 0 -< - cy
      yaw <- integral 0 -< sensitivity * cx'
      pitch <- integralClamp (- pi / 2) (pi / 2) 0 -< sensitivity * cy'
      returnA -< V2 yaw pitch

velocity :: Float
velocity = 5

sensitivity :: Float
sensitivity = 0.01

playerMat :: Player -> M44 Float
playerMat Player {..} = mkTransformation (recip (lookRot _playerAng)) 0 !*! mkTransformation 0 (- _playerPos)

lookRot :: V2 Float -> Quaternion Float
lookRot (V2 yaw pitch) = axisAngle (V3 0 0 1) yaw * axisAngle (V3 0 1 0) (- pitch)
