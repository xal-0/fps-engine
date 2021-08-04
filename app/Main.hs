{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (unless, when)
import Control.Monad.IO.Class
import GHC.Exts (toList)
import GHC.Word (Word8)
import Graphics.GL.Core45 (glDisable)
import Graphics.GL.Ext.ARB.FramebufferSRGB
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <-
      newWindow
        (WindowFormatColor RGB8)
        ( ( GLFW.defaultWindowConfig
              "Hello world!"
          )
            { GLFW.configWidth = 500,
              GLFW.configHeight = 500
            }
        )

    when gl_ARB_framebuffer_sRGB $
      glDisable GL_FRAMEBUFFER_SRGB

    vertexBuffer :: Buffer os (B4 Float) <- newBuffer 4
    writeBuffer
      vertexBuffer
      0
      [ V4 0.5 (-0.5) 0 1,
        V4 0.5 0.5 0 1,
        V4 (-0.5) (-0.5) 0 1,
        V4 (-0.5) 0.5 0 1
      ]

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream snd
      fragmentStream <-
        rasterize
          (\(size, _) -> (FrontAndBack, PolygonLine 1, ViewPort (V2 0 0) size, DepthRange 0 1))
          (fmap (,()) primitiveStream)
      let red = fmap (const (V3 1 0 0)) fragmentStream
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) red

    loop vertexBuffer shader win

loop ::
  Buffer os (B4 Float) ->
  CompiledShader os (V2 Int, PrimitiveArray Triangles (B4 Float)) ->
  Window os RGBFloat () ->
  ContextT GLFW.Handle os IO ()
loop vertexBuffer shader win = do
  Just (sizeW, sizeH) <- GLFW.getWindowSize win

  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
    shader (V2 sizeW sizeH, primitiveArray)
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win
