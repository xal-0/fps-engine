{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Wire hiding (id, unless)
import Data.Bool
import Data.IORef
import Data.Time.Clock
import Engine.Logic
import Engine.World
import FRP.Netwire hiding (id, unless)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Prelude hiding ((.))

main :: IO ()
main = do
  world <- newIORef 0
  done <- newIORef False
  input <- newInputContext

  _ <-
    forkFinally
      (renderer input world)
      (const (atomicWriteIORef done True))
  logic input world done

logic :: Input -> IORef World -> IORef Bool -> IO ()
logic input world done = do
  let lloop s w = do
        (d, s') <- stepSession s
        (Right o, w') <- runReaderT (stepWire w d (Right ())) input
        atomicWriteIORef world o
        isdone <- readIORef done
        unless isdone (lloop s' w')

  lloop tickRateSession thewire

thewire :: W a World
thewire = proc _ -> do
  V2 x y <- getCursor -< ()
  x' <- derivative <|> pure 0 -< x
  y' <- derivative <|> pure 0 -< y
  returnA -< V2 x' y'

renderer :: Input -> IORef World -> IO ()
renderer input world = runContextT GLFW.defaultHandleConfig do
  win <-
    newWindow
      (WindowFormatColor RGB8)
      ( (GLFW.defaultWindowConfig "sector f lambda complex")
          { GLFW.configSwapInterval = Just 1
          }
      )

  Just () <- GLFW.setKeyCallback win (Just (keyCallback input))
  Just () <- GLFW.setMouseButtonCallback win (Just (mouseCallback input))
  Just () <- GLFW.setCursorPosCallback win (Just (cursorPosCallback input))

  buf :: Buffer _ (B3 Float) <- newBuffer 6
  writeBuffer buf 0 [V3 0.5 0.5 0,
                     V3 (-0.5) 0.5 0,
                     V3 (-0.5) (-0.5) 0,
                     V3 0.5 (-0.5) 0,
                     V3 0.5 0.5 0,
                     V3 (-0.5) (-0.5) 0]

  buf2 :: Buffer _  (B Float) <- newBuffer 6
  writeBuffer buf2 0 [1,1,1,1,1,1]

  let rloop = do
        w <- liftIO (readIORef world)
        render do
          clearWindowColor win (V3 0.1 0.1 0.1)

        swapWindowBuffers win
        close <- GLFW.windowShouldClose win
        unless (close == Just True) rloop

  rloop

tickRateSession :: Session IO (Timed Int ())
tickRateSession = Session do
  t <- liftIO getCurrentTime
  pure (Timed 1 (), session t)
  where
    session t = Session do
      t' <- liftIO getCurrentTime
      let dt = diffUTCTime t' t
      if dt < tickTime
        then do
          threadDelay (truncate (waitTime dt))
          stepSession (session t')
        else pure (Timed 1 (), session t')

    tickTime :: NominalDiffTime
    tickTime = 1 / 64

    waitTime dt = 1000000 * nominalDiffTimeToSeconds (tickTime - dt)
