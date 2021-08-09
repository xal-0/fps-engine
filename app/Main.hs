{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import qualified Data.Atlas as AT
import Data.Bool (bool)
import Data.Foldable
import qualified Data.Vector as V
import Graphics.GL.Core45 (glDisable)
import Graphics.GL.Ext.ARB.FramebufferSRGB as G
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Studio
import Graphics.GPipe.Internal.Expr (normalize3)

data State = State
  { _statePos :: V3 Float,
    _stateLook :: V2 Float, -- yaw, pitch
    _statePrevMouse :: V2 Float
  }

makeLenses ''State

data Env os = Env
  { _envViewport :: V2 Int,
    _envMat :: Buffer os (Uniform (M44 (B Float))),
    _envModelPrim :: PrimitiveArray Triangles (B3 Float, B3 Float, B2 Float),
    _envModelTexture :: Texture2D os (Format RGBFloat)
  }

makeLenses ''Env

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <-
      newWindow
        (WindowFormatColorDepth RGB8 Depth16)
        ((GLFW.defaultWindowConfig "gun") {GLFW.configHeight = 720, GLFW.configWidth = 1080})

    -- Just () <- GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

    when G.gl_ARB_framebuffer_sRGB $
      glDisable G.GL_FRAMEBUFFER_SRGB

    (modelBuf, modelTexture) <- loadMdl "models/halflife/v_mp5.mdl"

    shader <- compileShader do
      prim <- toPrimitiveStream _envModelPrim
      mat <- getUniform ((,0) . _envMat)

      let shadeVertex (vert, n, uv) = (vert', (n, uv))
            where
              vert' = mat !* point vert
          rastSettings s = (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (s ^. envViewport), DepthRange 0 1)

      frag <- rasterize rastSettings (fmap shadeVertex prim)

      sampler <- newSampler2D \s ->
        (s ^. envModelTexture, SamplerNearest, (V2 ClampToEdge ClampToEdge, undefined))
      let shadeFrag (n, uv) info =
            ( sample2D sampler SampleAuto Nothing Nothing uv ^* clamp 0 (dot n (normalize3 (V3 0 40 30)) + 0.9) 1,
              rasterizedFragCoord info ^. _z
            )
          drawSettings _ =
            ( win,
              ContextColorOption NoBlending (V3 True True True),
              DepthOption Less True
            )

      drawWindowColorDepth drawSettings (withRasterizedInfo shadeFrag frag)

    matBuf :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 1

    let sensitivity = 0.005
        vel = 0.5
        loop state = do
          Just mouse' <- fmap (fmap (\(x, y) -> V2 (realToFrac x) (realToFrac y))) (GLFW.getCursorPos win)
          Just (sizeX, sizeY) <- GLFW.getWindowSize win

          [wkey, akey, skey, dkey] <-
            traverse (GLFW.getKey win) [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D]
              & fmap (fmap (== Just GLFW.KeyState'Pressed))

          let mdelta = mouse' - state ^. statePrevMouse
              look' = state ^. stateLook - (mdelta * sensitivity) & _2 %~ \pitch -> clamp (- pi / 2) pitch (pi / 2)
              rot = axisAngle (V3 0 1 0) (look' ^. _1) * axisAngle (V3 1 0 0) (look' ^. _2)
              forward = rotate rot (V3 0 0 (-1))
              right = rotate rot (V3 1 0 0)
              up = rotate rot (V3 0 1 0)
              pos' =
                state ^. statePos + forward * vel * (bool 0 1 wkey + bool 0 (-1) skey)
                  + right * vel * (bool 0 1 dkey + bool 0 (-1) akey)
              lookm = lookAt pos' (pos' + forward) (- up)
              aspect = fromIntegral sizeX / fromIntegral sizeY
              perspect = perspective 80 aspect 1 1000

          writeBuffer matBuf 0 [perspect !*! mkTransformation (axisAngle (V3 1 0 0) (pi/2)) (V3 (-4) (7) (-6)) !*! m33_to_m44 (scaled (V3 (-1) 1 1))]

          render do
            clearWindowColor win (V3 0.25 0.25 0.25)
            clearWindowDepth win 1
            modelVerts <- newVertexArray modelBuf
            let modelPrim = toPrimitiveArray TriangleList modelVerts
            shader (Env (V2 sizeX sizeY) matBuf modelPrim modelTexture)

          let state' =
                state
                  & statePrevMouse .~ mouse'
                  & stateLook .~ look'
                  & statePos .~ pos'

          swapWindowBuffers win
          closeRequested <- GLFW.windowShouldClose win
          unless
            (closeRequested == Just True)
            (loop state')

    loop (State (V3 0 0 20) (V2 0 0) 0)

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

  let meshes' = concatMap (\m -> fmap (updateMesh (m ^. meshSkin)) (m ^.. meshTris)) meshes
      updateMesh skin (Vertex v n uv _ _) =
        ( v,
          n,
          fmap fromIntegral ((locs V.! skin) + fmap fromIntegral uv) / fromIntegral minsize
        )

  modelVerts <- newBuffer (sumOf (traverse . meshNumTris) meshes * 3)
  writeBuffer modelVerts 0 meshes'

  texture <- newTexture2D RGB8 (V2 minsize minsize) 1
  forM_ (zip (toList textures) (toList locs)) \(t, pos) ->
    writeTexture2D texture 0 pos (t ^. textureSize) (t ^.. texturePixels)

  pure (modelVerts, texture)

v2pt :: Iso' (V2 Int) AT.Pt
v2pt = iso (\(V2 x y) -> AT.Pt x y) (\(AT.Pt x y) -> V2 x y)
