{-# LANGUAGE TemplateHaskell #-}

module Engine.Render.Ui
  ( compilePictureShader,
    drawPicture,
    Picture (..),
    DrawEnv (DrawEnv),
  )
where

import Codec.Picture
import Control.Lens
import Control.Monad.IO.Class
import Data.Char
import Graphics.GPipe

data DrawEnv os = DrawEnv
  { _envWindow :: Window os RGBAFloat (),
    _envSize :: V2 Int
  }

makeLenses ''DrawEnv

data Picture
  = PRect !(V2 Int)
  | PString String
  | PTranslate !(V2 Int) !Picture
  | PColour !(V4 Float) !Picture
  | PPictures ![Picture]

newtype PictureShader os
  = PictureShader (CompiledShader os (PictureEnv os))

data PictureEnv os = PictureEnv
  { _envDraw :: DrawEnv os,
    _envPrims :: PrimitiveArray Triangles (B2 Float, B2 Float, B4 Float)
  }

makeLenses ''PictureEnv

compilePictureShader :: ContextHandler ctx => ContextT ctx os IO (PictureShader os)
compilePictureShader = do
  Right (ImageY8 img) <- liftIO (readPng textureFile)
  let imgSize = V2 (imageWidth img) (imageHeight img)
  texture <- newTexture2D R8 imgSize 1
  writeTexture2D texture 0 (V2 0 0) imgSize $
    toListOf (imagePixels . to (\x -> if x > 0 then 1 else 0 :: Float)) img

  PictureShader <$> compileShader do
    prims <- toPrimitiveStream (view envPrims)
    sampler <-
      newSampler2D
        (const (texture, SamplerNearest, (V2 Repeat Repeat, undefined)))
    frags <-
      rasterize
        (\s -> (FrontAndBack, PolygonFill, ViewPort 0 (s ^. envDraw . envSize), DepthRange 0 1))
        (fmap (\(V2 x y, uv, c) -> (V4 x y 0 1, (uv, c))) prims)

    let frags' =
          fmap
            ( \(uv, V4 r g b a) ->
                let x = sample2D sampler SampleAuto Nothing Nothing uv
                 in V4 r g b (a * x)
            )
            frags
    drawWindowColor
      (\s -> (s ^. envDraw . envWindow, ContextColorOption blendingOptions (V4 True True True False)))
      frags'

drawPicture :: ContextHandler ctx => PictureShader os -> DrawEnv os -> Picture -> ContextT ctx os IO ()
drawPicture (PictureShader shader) env pic = do
  let verts = pictureVerts env pic
  buf <- newBuffer (length verts)
  writeBuffer buf 0 verts

  render do
    va <- newVertexArray buf
    let prims = toPrimitiveArray TriangleList va
    shader $ PictureEnv env prims

pictureVerts :: DrawEnv os -> Picture -> [(V2 Float, V2 Float, V4 Float)]
pictureVerts env (PColour col p) = pictureVerts env p & traverse . _3 .~ col
pictureVerts env (PRect (V2 w h)) =
  [ V2 0 0,
    V2 0 h,
    V2 w 0,
    V2 w 0,
    V2 0 h,
    V2 w h
  ]
    & fmap \v -> (pixelToNDS env v, V2 (135 / 136) 0, 1)
pictureVerts env (PString str) = stringVerts env str
pictureVerts env (PTranslate pos p) =
  fmap (_1 %~ (+ (fmap fromIntegral pos * (envPixelSize env & _2 %~ negate)))) (pictureVerts env p)
pictureVerts env (PPictures p) = foldr (\x -> (pictureVerts env x ++)) [] p

stringVerts env str =
  let textureChar = 1 / fmap fromIntegral textureSize
      charUV c = fmap fromIntegral (V2 (ord c `mod` 16) (ord c `div` 16)) * textureChar

      V2 charX charY = charSize

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
              & traverse . _1 %~ pixelToNDS env

      lineVerts y = concat . zipWith charVerts [V2 (x * charX) y | x <- [0 ..]]
   in lineVerts 0 str & fmap \(p, uv) -> (p, uv, V4 1 1 1 1)

envPixelSize env = 2 / fmap fromIntegral (env ^. envSize)

pixelToNDS env (V2 x y) =
  let V2 pixelX pixelY = envPixelSize env
   in V2 (fromIntegral x * pixelX - 1) (- fromIntegral y * pixelY + 1)

blendingOptions =
  BlendRgbAlpha
    (FuncAdd, FuncAdd)
    (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One OneMinusSrcAlpha)
    1

charSize :: V2 Int
charSize = V2 8 15

textureSize :: V2 Int
textureSize = V2 17 16

textureFile = "textures/fixedsys.png"
