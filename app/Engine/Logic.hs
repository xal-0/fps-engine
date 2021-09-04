{-# LANGUAGE TemplateHaskell #-}

module Engine.Logic
  ( Input,
    newInputContext,
    keyCallback,
    mouseCallback,
    cursorPosCallback,
    getKey,
    getKeyPress,
    getMouse1,
    getMouse,
    GLFW.Key (..),
    W,
  )
where

import Control.Concurrent
import Control.Lens
import Control.Monad.Reader
import Control.Wire
import Data.IORef
import qualified Data.Vector.Mutable as MV
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Linear.V2
import Prelude hiding ((.))

type W = Wire (Timed Int ()) () (ReaderT Input IO)

type KeyStates = MV.IOVector GLFW.KeyState

data Input = Input
  { _inputKeys :: MVar KeyStates,
    _inputMouse :: IORef (V2 Float),
    _inputMouse1 :: IORef Bool
  }

makeLenses ''Input

newInputContext :: IO Input
newInputContext = do
  _inputKeys <- MV.replicate totalKeys GLFW.KeyState'Released >>= newMVar
  _inputMouse <- newIORef 0
  _inputMouse1 <- newIORef False
  pure Input {..}
  where
    totalKeys =
      fromEnum (maxBound :: GLFW.Key)
        - fromEnum (minBound :: GLFW.Key)

keyCallback :: Input -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback Input {..} k _ s _ = withMVar _inputKeys (\a -> MV.write a (fromEnum k) s)

mouseCallback :: Input -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseCallback Input {..} GLFW.MouseButton'1 s _ = atomicWriteIORef _inputMouse1 (s == GLFW.MouseButtonState'Pressed)
mouseCallback _ _ _ _ = pure ()

cursorPosCallback :: Input -> Double -> Double -> IO ()
cursorPosCallback Input {..} x y = atomicWriteIORef _inputMouse (fmap realToFrac (V2 x y))

getKey :: GLFW.Key -> W a GLFW.KeyState
getKey key = mkGen_ \_ -> do
  keys <- view inputKeys
  s <- liftIO (withMVar keys (\a -> MV.read a (fromEnum key)))
  pure (Right s)

getKeyPress :: GLFW.Key -> W a Bool
getKeyPress key =
  getKey key
    >>^ liftA2 (||) (== GLFW.KeyState'Pressed) (== GLFW.KeyState'Repeating)

getMouse1 :: W a Bool
getMouse1 = mkGen_ \_ -> view inputMouse1 >>= fmap Right . liftIO . readIORef

getMouse :: W a (V2 Float)
getMouse = mkGen_ \_ -> view inputMouse >>= fmap Right . liftIO . readIORef
