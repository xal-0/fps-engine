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
import Engine.Render.Text
import Engine.World
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import System.Environment (getArgs)
import Prelude hiding ((.))
import Text.Printf

main :: IO ()
main = do
  world <- newIORef def
  done <- newIORef False
  input <- newInputContext

  _ <-
    forkFinally
      (renderer input world)
      ( \r -> do
          print r
          atomicWriteIORef done True
      )

  logic input world done

logic :: Input -> IORef World -> IORef Bool -> IO ()
logic input world done = do
  let lloop t s w = do
        (d, s') <- stepSession s
        (Right o, w') <- runReaderT (stepWire w d (Right ())) input
        atomicWriteIORef world o
        isdone <- readIORef done

        t' <- getCurrentTime
        let dt = diffUTCTime t' t
        threadDelay (truncate (1000000 * (tickTime - dt)))
        unless isdone (lloop t' s' w')

  t <- getCurrentTime
  lloop t (countSession_ 1) thewire

tickTime = 1 / 64

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
    col <- getUniform (\s -> (s ^. _4, 0))
    frags <-
      rasterize
        (\e -> (FrontAndBack, PolygonLine 1, ViewPort 0 (e ^. _1), DepthRange 0 1))
        (fmap (\(V3 x y z) -> (lookmat !* V4 x y z 1, ())) prims)

    drawWindowColor
      (const (win, ContextColorOption NoBlending (V3 True True True)))
      (fmap (const col) frags)

  matBuf :: Buffer _ (Uniform (M44 (B Float))) <- newBuffer 1
  colBuf :: Buffer _ (Uniform (B3 Float)) <- newBuffer 1

  let coordsm = V4 (V4 0 (-1) 0 0) (V4 0 0 1 0) (V4 (-1) 0 0 0) (V4 0 0 0 1)

  textShader <- loadFont

  let rloop = do
        w <- liftIO (readIORef world)

        Just (width, height) <- GLFW.getWindowSize win
        let aspect = fromIntegral width / fromIntegral height

        writeBuffer matBuf 0 [perspective (pi / 2) aspect 1 10000 !*! coordsm !*! playerMat w]
        writeBuffer colBuf 0 [V3 1 1 1]

        render do
          clearWindowColor win 0
          prims <- renderBsp bspGpu (w ^. playerPos)
          shader (V2 width height, prims, matBuf, colBuf)

        drawString textShader win (V2 width height) (V3 1 0 0) (V2 10 10)
          (printf "pos: %5.0f %5.0f %5.0f" (w ^. playerPos . _x) (w ^. playerPos . _y) (w ^. playerPos . _z))
        drawString textShader win (V2 width height) (V3 0 1 0) (V2 10 25)
          (printf "ang: %3.2f %3.2f" (w ^. playerAng . _x) (w ^. playerAng . _y))

        swapWindowBuffers win
        close <- GLFW.windowShouldClose win
        unless (close == Just True) rloop

  rloop
