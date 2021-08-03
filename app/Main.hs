{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad (unless)
import Control.Monad.IO.Class
import GHC.Exts (toList)
import GHC.Word (Word8)
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

    terrainTexture <- readRGB8Texture "terrain.png"

    vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer 4
    writeBuffer
      vertexBuffer
      0
      [ (V4 1 (-1) 0 1, V2 1 1),
        (V4 1 1 0 1, V2 1 0),
        (V4 (-1) (-1) 0 1, V2 0 1),
        (V4 (-1) 1 0 1, V2 0 0)
      ]

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream snd
      fragmentStream <- rasterize (\(size, _) -> (Front, PolygonFill, ViewPort (V2 0 0) size, DepthRange 0 1)) primitiveStream
      sampler <- newSampler2D (const (terrainTexture, SamplerNearest, (V2 Repeat Repeat, V3 0 0 0)))
      let sample = sample2D sampler SampleAuto Nothing Nothing
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) (sample <$> fragmentStream)

    loop vertexBuffer shader win

readRGB8Texture file = do
  Right (ImageRGBA8 img) <- liftIO (readImage file)
  let size = V2 (imageWidth img) (imageHeight img)
  texture <- newTexture2D RGB8 size maxBound
  writeTexture2D texture 0 (V2 0 0) size (v3s (toList (imageData img)))
  generateTexture2DMipmap texture
  pure texture
  where
    v3s :: [Word8] -> [V3 Word8]
    v3s [] = []
    v3s (r : g : b : _ : rest) = V3 r g b : v3s rest

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
