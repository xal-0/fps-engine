{-# LANGUAGE TemplateHaskell #-}

module Engine.Logic
  ( Input,
    newInputContext,
    keyCallback,
    getKey,
    getKeyPress,
    GLFW.Key (..),
    W,
  )
where

import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Control.Wire
import qualified Data.Vector.Mutable as MV
import qualified Graphics.GPipe.Context.GLFW as GLFW

type W = Wire (Timed NominalDiffTime ()) () (ReaderT Input IO)

type KeyStates = MV.IOVector GLFW.KeyState

newtype Input = Input {_inputKeys :: MVar KeyStates}

makeLenses ''Input

newInputContext :: IO Input
newInputContext = do
  _inputKeys <- MV.replicate totalKeys GLFW.KeyState'Released >>= newMVar
  pure Input {..}
  where
    totalKeys =
      fromEnum (maxBound :: GLFW.Key)
        - fromEnum (minBound :: GLFW.Key)

keyCallback :: Input -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback Input {..} k _ s _ = withMVar _inputKeys (\a -> MV.write a (fromEnum k) s)

getKey :: GLFW.Key -> W a GLFW.KeyState
getKey key = mkGen_ \_ -> do
  keys <- view inputKeys
  s <- liftIO (withMVar keys (\a -> MV.read a (fromEnum key)))
  pure (Right s)

getKeyPress key =
  getKey key
    >>> arr (liftA2 (||) (== GLFW.KeyState'Pressed) (== GLFW.KeyState'Repeating))
