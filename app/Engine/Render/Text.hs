module Engine.Render.Text (loadFont, drawString) where

import Codec.Picture
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Debug.Trace
import Graphics.GPipe

newtype TextureShader os
  = TextureShader
      (CompiledShader os (Window os RGBFloat (), PrimitiveArray Triangles (B2 Float, B2 Float),
                          V2 Int,
                          Buffer os (Uniform (B3 Float))))

loadFont :: ContextHandler ctx => ContextT ctx os IO (TextureShader os)
loadFont = do
  Right (ImageY8 img) <- liftIO (readPng textureFile)
  let imgSize = V2 (imageWidth img) (imageHeight img)
  texture <- newTexture2D R8 imgSize 1
  writeTexture2D texture 0 (V2 0 0) imgSize $
    toListOf (imagePixels . to (\x -> if x > 0 then 1 else 0 :: Float)) img

  TextureShader <$> compileShader do
    prims <- toPrimitiveStream (view _2)
    col <- getUniform (\s -> (s ^. _4, 0))
    sampler <-
      newSampler2D
        (const (texture, SamplerNearest, (V2 Repeat Repeat, undefined)))
    frags <-
      rasterize
        (\s -> (FrontAndBack, PolygonFill, ViewPort 0 (s ^. _3), DepthRange 0 1))
        (fmap (\(V2 x y, uv) -> (V4 x y 0 1, uv)) prims)
    let frags' =
          fmap
            ( \uv ->
                let x = sample2D sampler SampleAuto Nothing Nothing uv
                 in x *^ col
            )
            frags
    drawWindowColor
      (\s -> (s ^. _1, ContextColorOption NoBlending (V3 True True True)))
      frags'

drawString ::
  ContextHandler ctx =>
  TextureShader os ->
  Window os RGBFloat () ->
  V2 Int ->
  V3 Float ->
  V2 Int ->
  String ->
  ContextT ctx os IO ()
drawString (TextureShader shader) win viewport col (V2 posX posY) str = do
  let textureChar = 1 / fmap fromIntegral textureSize
      charUV c = fmap fromIntegral (V2 (ord c `mod` 16) (ord c `div` 16)) * textureChar

      V2 pixelX pixelY = 2 / fmap fromIntegral viewport
      V2 charX charY = charSize
      pixelToNDS (V2 x y) = V2 (fromIntegral x * pixelX - 1) (- fromIntegral y * pixelY + 1)

      charVerts :: V2 Int -> Char -> [(V2 Float, V2 Float)]
      charVerts p c =
        let V2 u v = charUV c
            V2 u' v' = V2 u v + textureChar
         in [ (V2 0 0, V2 u v),
              (V2 0 charY, V2 u v'),
              (V2 charX 0, V2 u' v),
              (V2 charX 0, V2 u' v),
              (V2 0 charY, V2 u v'),
              (V2 charX charY, V2 u' v')
            ]
            & traverse . _1 %~ (+ p)
            & traverse . _1 %~ pixelToNDS

      lineVerts = concat . zipWith charVerts [V2 (x * charX + posX) posY | x <- [0 ..]]

      line = lineVerts str

  buf :: Buffer _ (B2 Float, B2 Float) <- newBuffer (length line)
  writeBuffer buf 0 line

  cbuf :: Buffer _ (Uniform (B3 Float)) <- newBuffer 1
  writeBuffer cbuf 0 [col]

  render do
    va <- newVertexArray buf
    let prims = toPrimitiveArray TriangleList va
    shader (win, prims, viewport, cbuf)

charSize :: V2 Int
charSize = V2 8 15

textureSize :: V2 Int
textureSize = V2 16 16

textureFile = "textures/fixedsys.png"
