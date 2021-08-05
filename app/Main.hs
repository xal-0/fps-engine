{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (unless, when)
import Graphics.GL.Core45 (glDisable)
import Graphics.GL.Ext.ARB.FramebufferSRGB as G
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Render

data Env os = Env
  { _viewportSize :: V2 Int,
    _timeBuf :: Buffer os (Uniform (B Float)),
    _modelPrim :: PrimitiveArray Triangles (B3 Float)
  }

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <-
      newWindow
        (WindowFormatColor RGB8)
        ((GLFW.defaultWindowConfig "gun") {GLFW.configHeight = 500, GLFW.configWidth = 500})

    when G.gl_ARB_framebuffer_sRGB $
      glDisable G.GL_FRAMEBUFFER_SRGB

    genModelPrim <- loadMdl "w_9mmar.mdl"

    rotBuf :: Buffer os (Uniform (Quaternion (B Float))) <- newBuffer 1

    shader <- compileShader do
      prim <- toPrimitiveStream _modelPrim
      let prim' = fmap ((\(V3 x y z) -> (V4 x y z 1, ())) . (/ 25)) prim
      frag <- rasterize (\s -> (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (_viewportSize s), DepthRange 0 1)) prim'
      let frag' = fmap (const (V3 1 1 1)) frag
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) frag'

    let loop time = do
          writeBuffer timeBuf 0 [_]

          Just (sizex, sizey) <- GLFW.getWindowSize win

          render do
            clearWindowColor win (V3 0 0 0)
            modelPrim <- genModelPrim
            shader (Env (V2 sizex sizey) timeBuf modelPrim)

          swapWindowBuffers win
          closeRequested <- GLFW.windowShouldClose win
          unless
            (closeRequested == Just True)
            (loop (time + 1))

    loop 0
