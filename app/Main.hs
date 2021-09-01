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
import Engine.Render.Ui
import Engine.World
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import System.Environment (getArgs)
import Text.Printf
import Prelude hiding ((.))

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
      (WindowFormatColor RGBA8)
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
    V3 r g b <- getUniform (\s -> (s ^. _4, 0))
    frags <-
      rasterize
        (\e -> (FrontAndBack, PolygonLine 1, ViewPort 0 (e ^. _1), DepthRange 0 1))
        (fmap (\(V3 x y z) -> (lookmat !* V4 x y z 1, ())) prims)

    drawWindowColor
      (const (win, ContextColorOption NoBlending (V4 True True True False)))
      (fmap (const (V4 r g b 0)) frags)

  matBuf :: Buffer _ (Uniform (M44 (B Float))) <- newBuffer 1
  colBuf :: Buffer _ (Uniform (B3 Float)) <- newBuffer 1

  let coordsm = V4 (V4 0 (-1) 0 0) (V4 0 0 1 0) (V4 (-1) 0 0 0) (V4 0 0 0 1)

  pictureShader <- compilePictureShader

  let rloop t = do
        w <- liftIO (readIORef world)

        Just (width, height) <- GLFW.getWindowSize win
        let aspect = fromIntegral width / fromIntegral height
            viewport = V2 width height

        writeBuffer matBuf 0 [perspective (pi / 2) aspect 1 10000 !*! coordsm !*! playerMat w]
        writeBuffer colBuf 0 [V3 1 1 1]

        render do
          clearWindowColor win 0
          prims <- renderBsp bspGpu (w ^. playerPos)
          shader (viewport, prims, matBuf, colBuf)

        t' <- liftIO getCurrentTime
        let dt = diffUTCTime t' t
            fps = realToFrac (1 / dt) :: Float

        drawPicture pictureShader (DrawEnv win viewport) $
          PTranslate
            (V2 10 10)
            ( PPictures
                [ PColour (V4 0 0 0.5 0.8) (PRect (V2 52 17)),
                  PTranslate (V2 1 1) (PString (printf "%2.0f fps" fps))
                ]
            )

        swapWindowBuffers win
        close <- GLFW.windowShouldClose win
        unless (close == Just True) (rloop t')

  t <- liftIO getCurrentTime
  rloop t
