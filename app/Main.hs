{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Wire hiding (id, unless)
import Data.Bool
import Data.Default (def)
import Data.IORef
import Data.Time.Clock
import Engine.Logic
import Engine.Player
import Engine.Render.Bsp
import Engine.World
import FRP.Netwire hiding (id, unless)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Prelude hiding ((.))
import System.Mem (performMinorGC, performMajorGC)
import System.Environment (getArgs)

main :: IO ()
main = do
  world <- newIORef def
  done <- newIORef False
  input <- newInputContext

  ir <-
    forkFinally
      (renderer input world)
      (\r -> do
          print r
          atomicWriteIORef done True)

  i <- myThreadId
  putStrLn $ "logic: " <> show i
  putStrLn $ "render: " <> show ir
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
thewire = playerWire >>> force

renderer :: Input -> IORef World -> IO ()
renderer input world = runContextT GLFW.defaultHandleConfig do
  win <-
    newWindow
      (WindowFormatColor RGB8)
      ( (GLFW.defaultWindowConfig "sector f lambda complex")
          { GLFW.configSwapInterval = Just 1
          }
      )

  Just () <- GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  Just () <- GLFW.setKeyCallback win (Just (keyCallback input))
  Just () <- GLFW.setMouseButtonCallback win (Just (mouseCallback input))
  Just () <- GLFW.setCursorPosCallback win (Just (cursorPosCallback input))

  [mapname] <- liftIO getArgs
  bspGpu <- loadBsp mapname

  shader <- compileShader do
    prims <- toPrimitiveStream (view _2)
    lookmat <- getUniform (\s -> (s ^. _3, 0))
    frags <-
      rasterize
        (\e -> (FrontAndBack, PolygonLine 1, ViewPort 0 (e ^. _1), DepthRange 0 1))
        (fmap (\(V3 x y z) -> (lookmat !* V4 x y z 1, ())) prims)

    drawWindowColor
      (const (win, ContextColorOption NoBlending (V3 True True True)))
      (fmap (const (V3 1 1 1)) frags)

  matBuf :: Buffer _ (Uniform (M44 (B Float))) <- newBuffer 1

  let coordsm = V4 (V4 0 (-1) 0 0) (V4 0 0 1 0) (V4 (-1) 0 0 0) (V4 0 0 0 1)

  let rloop = do
        w <- liftIO (readIORef world)

        Just (width, height) <- GLFW.getWindowSize win
        let aspect = fromIntegral width / fromIntegral height

        writeBuffer matBuf 0 [perspective (pi / 2) aspect 1 10000 !*! coordsm !*! playerMat w]

        render do
          clearWindowColor win 0
          prims <- renderBsp bspGpu (w ^. playerPos)
          shader (V2 width height, prims, matBuf)

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
    tickTime = 1 / 128

    waitTime dt = 1000000 * nominalDiffTimeToSeconds (tickTime - dt)
