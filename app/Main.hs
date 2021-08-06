{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import qualified Data.Atlas as AT
import Data.Foldable
import qualified Data.Vector as V
import Graphics.GL.Core45 (glDisable)
import Graphics.GL.Ext.ARB.FramebufferSRGB as G
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Studio
import System.Environment (getArgs)

data Env os = Env
  { _viewportSize :: V2 Int,
    _matBuf :: Buffer os (Uniform (M44 (B Float))),
    _modelPrim :: PrimitiveArray Triangles (B3 Float, B3 Float, B2 Float),
    _modelTexture :: Texture2D os (Format RGBFloat)
  }

main :: IO ()
main = do
  [file] <- getArgs

  runContextT GLFW.defaultHandleConfig $ do
    win <-
      newWindow
        (WindowFormatColorDepth RGB8 Depth16)
        ((GLFW.defaultWindowConfig "gun") {GLFW.configHeight = 500, GLFW.configWidth = 500})

    when G.gl_ARB_framebuffer_sRGB $
      glDisable G.GL_FRAMEBUFFER_SRGB

    (modelBuf, modelTexture) <- loadMdl file

    shader <- compileShader do
      prim <- toPrimitiveStream _modelPrim
      mat <- getUniform ((,0) . _matBuf)

      let shadeVertex (vert, _, uv) = (vert', (uv, (vert' ^. _z + 1) / 3))
            where
              vert' = mat !* point vert
          rastSettings s = (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (_viewportSize s), DepthRange 0 1)

      frag <- rasterize rastSettings (fmap shadeVertex prim)

      sampler <- newSampler2D \s ->
        (_modelTexture s, SamplerNearest, (V2 ClampToEdge ClampToEdge, undefined))
      let shadeFrag (uv, z) = (sample2D sampler SampleAuto Nothing Nothing uv, z)
          drawSettings _ =
            ( win,
              ContextColorOption NoBlending (V3 True True True),
              DepthOption Greater True
            )

      drawWindowColorDepth drawSettings (fmap shadeFrag frag)

    matBuf :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 1

    let extraR = axisAngle (V3 0 1 0) 0.015
        scale = m33_to_m44 (scaled (recip 30))
        loop rot = do
          Just (sizex, sizey) <- GLFW.getWindowSize win
          writeBuffer matBuf 0 [mkTransformation rot (V3 0 0 0) !*! scale]

          render do
            clearWindowColor win (V3 0.25 0.25 0.25)
            clearWindowDepth win (-1)
            modelVerts <- newVertexArray modelBuf
            let modelPrim = toPrimitiveArray TriangleList modelVerts
            shader (Env (V2 sizex sizey) matBuf modelPrim modelTexture)

          swapWindowBuffers win
          closeRequested <- GLFW.windowShouldClose win
          unless
            (closeRequested == Just True)
            (loop (extraR * rot))

    loop (Quaternion 1 (V3 0 0 0) :: Quaternion Float)

loadMdl ::
  ContextHandler ctx =>
  FilePath ->
  ContextT ctx os IO (Buffer os (B3 Float, B3 Float, B2 Float), Texture2D os (Format RGBFloat))
loadMdl file = do
  liftIO $ putStrLn $ "loading model " <> file
  studio <- liftIO (readStudio file)
  let Just meshes = studio ^? studioBodyparts . ix 2 . bodypartDefaultModel . modelMeshes
      Just textures = studio ^? studioTextures

      pixels = textures & sumOf (traverse . textureSize . to product)
      minsize = 2 ^ (ceiling (log2 (sqrt (fromIntegral pixels) :: Double)) :: Int)

  locs <- liftIO do
    atlas <- AT.create minsize minsize
    Right locs <- AT.pack atlas (view (textureSize . v2pt)) (\_ _ -> ()) const textures
    pure (fmap (review v2pt) locs)

  let meshesF = meshes
      meshes' = concatMap (\m -> fmap (updateMesh (m ^. meshSkin)) (m ^. meshTris)) meshesF
      updateMesh skin (Vertex v n uv) =
        ( v,
          n,
          fmap fromIntegral ((locs V.! skin) + fmap fromIntegral uv) / fromIntegral minsize
        )

  modelVerts <- newBuffer (sumOf (traverse . meshNumTris) meshes * 3)
  writeBuffer modelVerts 0 meshes'

  texture <- newTexture2D RGB8 (V2 minsize minsize) 1
  forM_ (zip (toList textures) (toList locs)) \(t, pos) ->
    writeTexture2D texture 0 pos (t ^. textureSize) (t ^. textureData)

  pure (modelVerts, texture)

v2pt :: Iso' (V2 Int) AT.Pt
v2pt = iso (\(V2 x y) -> AT.Pt x y) (\(AT.Pt x y) -> V2 x y)
