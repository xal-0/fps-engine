{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Wire hiding (unless)
import Data.Bool
import Data.IORef
import Data.Time.Clock
import Engine.Logic
import Engine.World
import FRP.Netwire hiding (unless)
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
  w <- getKeyPress GLFW.Key'W -< ()
  s <- getKeyPress GLFW.Key'S -< ()
  rec pos' <-
        integral 0
          -<
            bool 0 1 (w && pos <= 1) + bool 0 (-1) (s && pos >= 0)
      pos <- delay 0 -< pos'
  returnA -< pos'

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

  let rloop = do
        w <- liftIO (readIORef world)
        render (clearWindowColor win (V3 w 0 0))
        swapWindowBuffers win
        close <- GLFW.windowShouldClose win
        unless (close == Just True) rloop

  rloop

tickRateSession :: Session IO (Timed NominalDiffTime ())
tickRateSession = Session do
  t <- liftIO getCurrentTime
  pure (Timed 0 (), session t)
  where
    session t = Session do
      t' <- liftIO getCurrentTime
      let dt = diffUTCTime t' t
      if dt < tickTime
        then do
          threadDelay
            (truncate (1000000 * nominalDiffTimeToSeconds (tickTime - dt)))
          stepSession (session t')
        else pure (Timed dt (), session t')

    tickTime :: NominalDiffTime
    tickTime = 1 / 64
