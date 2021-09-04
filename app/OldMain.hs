{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module OldMain where

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
import Pipes as P
import qualified Pipes.Prelude as P
import Studio
import Data.Int

data State = State
  { _statePos :: V3 Float,
    _stateLook :: V2 Float, -- yaw, pitch
    _statePrevMouse :: V2 Float,
    _stateFrame :: Int32
  }

makeLenses ''State

data Env os = Env
  { _envViewport :: V2 Int,
    _envUniform :: Buffer os (Uniform (M44 (B Float), B Int32)),
    _envModelPrim :: PrimitiveArray Triangles (B3 Float, B3 Float, B2 Float, B Int32),
    _envModelTexture :: Texture2D os (Format RGBFloat),
    _envAnimTexture :: Texture2D os (Format RGBAFloat)
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

    (modelBuf, modelTexture, skelTexture) <- loadMdl "models/halflife/v_mp5.mdl"

    shader <- compileShader do
      prim <- toPrimitiveStream _envModelPrim
      (mat, frame) <- getUniform ((,0) . _envUniform)

      samplerAnim <- newSampler2D \s ->
        (s ^. envAnimTexture, SamplerNearest, (V2 ClampToEdge ClampToEdge, undefined))

      let shadeVertex (vert, n, uv, b) = (vert', (n, uv))
            where
              -- vert' = mat !* (point ((rotate' skelquat vert) + skelpos))
              vert' = mat !* (mkTransformation skelquat skelpos !* point vert)
              skelquatv = texelFetch2D samplerAnim Nothing 0 (V2 (2 * b) frame)
              skelquat = Quaternion (skelquatv ^. _w) (skelquatv ^. _xyz)
              skelposv = texelFetch2D samplerAnim Nothing 0 (V2 (2 * b + 1) frame)
              skelpos = skelposv ^. _xyz

          rastSettings s = (FrontAndBack, PolygonFill, ViewPort (V2 0 0) (s ^. envViewport), DepthRange 0 1)

      frag <- rasterize rastSettings (fmap shadeVertex prim)

      sampler <- newSampler2D \s ->
        (s ^. envModelTexture, SamplerFilter Linear Linear Linear Nothing, (V2 ClampToEdge ClampToEdge, undefined))
      let shadeFrag (n, uv) info =
            ( sample2D sampler SampleAuto Nothing Nothing uv,
              rasterizedFragCoord info ^. _z
            )
          drawSettings _ =
            ( win,
              ContextColorOption NoBlending (V3 True True True),
              DepthOption Less True
            )

      drawWindowColorDepth drawSettings (withRasterizedInfo shadeFrag frag)

    uniBuf <- newBuffer 1

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
              perspect = mkTransformation 0 (V3 0 0 0) !*! perspective (pi / 2 - 0.2) aspect 1 1000
              gunmat = V4 (V4 0 1 0 0) (V4 0 0 1 0) (V4 (-1) 0 0 0) (V4 0 0 0 1)

          writeBuffer uniBuf 0 [(perspect !*! gunmat, (state ^. stateFrame `div` 2) `mod` 101)]

          render do
            clearWindowColor win (V3 0.25 0.25 0.25)
            clearWindowDepth win 1
            modelVerts <- newVertexArray modelBuf
            let modelPrim = toPrimitiveArray TriangleList modelVerts
            shader (Env (V2 sizeX sizeY) uniBuf modelPrim modelTexture skelTexture)

          let state' =
                state
                  & statePrevMouse .~ mouse'
                  & stateLook .~ look'
                  & statePos .~ pos'
                  & stateFrame +~ 1

          swapWindowBuffers win
          closeRequested <- GLFW.windowShouldClose win
          unless
            (closeRequested == Just True)
            (loop state')

    loop (State (V3 0 0 20) (V2 0 0) 0 0)

rotate' :: Conjugate a => Quaternion a -> V3 a -> V3 a
rotate' q v = ijk where
  Quaternion _ ijk = q $*$ Quaternion 0 v $*$ conjugate' q

conjugate' :: Conjugate a => Quaternion a -> Quaternion a
conjugate' (Quaternion e v) = Quaternion (conjugate e) (negate v)

($*$) :: Num a => Quaternion a -> Quaternion a -> Quaternion a
Quaternion s1 v1 $*$ Quaternion s2 v2 =
  Quaternion (s1 * s2 - (v1 `dot` v2)) $
    (v1 `cross` v2) + s1 *^ v2 + s2 *^ v1

loadMdl ::
  ContextHandler ctx =>
  FilePath ->
  ContextT
    ctx
    os
    IO
    ( Buffer os (B3 Float, B3 Float, B2 Float, B Int32),
      Texture2D os (Format RGBFloat),
      Texture2D os (Format RGBAFloat)
    )
loadMdl file = do
  liftIO $ putStrLn $ "loading model " <> file
  studio <- liftIO (readStudio file)
  let meshes = studio ^. studioBodyparts . traverse . bodypartDefaultModel . modelMeshes
      Just textures = studio ^? studioTextures

      pixels = textures & sumOf (traverse . textureSize . to product)
      minsize = 2 ^ (ceiling (log2 (sqrt (fromIntegral pixels) :: Double)) :: Int)

  locs <- liftIO do
    atlas <- AT.create minsize minsize
    Right locs <- AT.pack atlas (view (textureSize . v2pt)) (\_ _ -> ()) const textures
    pure (fmap (review v2pt) locs)

  let vertices = P.toList $ P.each meshes `for` updateMesh
      updateMesh m = m ^. meshTris >-> P.map (updateVertex m)
      updateVertex m (Vertex v n uv b _) = (v, n, fuv m uv, fromIntegral b)
      fuv m uv =
        fmap fromIntegral ((locs V.! (m ^. meshSkin)) + fmap fromIntegral uv)
          / fromIntegral minsize

  modelVerts <- newBuffer (sumOf (traverse . meshNumTris) meshes * 3)
  writeBuffer modelVerts 0 vertices

  texture <- newTexture2D RGB8 (V2 minsize minsize) 1
  forM_ (zip (toList textures) (toList locs)) \(t, pos) ->
    writeTexture2D texture 0 pos (t ^. textureSize) (t ^. texturePixels . to P.toList)

  let Just ss = studio ^? studioSeqs . ix 1
      skelTextureP = (ss ^. seqAnim) `for` skelTextureRow

      skelTextureSize = V2 (V.length (studio ^. studioBones) * 2) (ss ^. seqNumFrames)
      skelTextureRow skelTransforms = P.each skelTransforms `for` skelTextureTr
      skelTextureTr (rot, pos) = do
        yield $ rot ^. _xyzw
        yield $ vector pos

  skelTexture <- newTexture2D RGBA32F skelTextureSize 1
  writeTexture2D skelTexture 0 (V2 0 0) skelTextureSize (P.toList skelTextureP)

  -- skelVertices <- newBuffer (V.length (studio ^. studioBones) * 2)


  -- let Just firstConfig =  runIdentity (P.head (ss ^. seqAnim))
  --     boneVertices = V.izipWith boneVertex (studio ^. studioBones) firstConfig
  --     boneVertex n b (rot, pos) = undefined

  pure (modelVerts, texture, skelTexture)

v2pt :: Iso' (V2 Int) AT.Pt
v2pt = iso (\(V2 x y) -> AT.Pt x y) (\(AT.Pt x y) -> V2 x y)

{-


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


        writeBuffer matBuf 0 [perspective (pi / 2) aspect 1 10000 !*! coordsm !*! playerMat w]
        writeBuffer colBuf 0 [V3 1 1 1]

        --   prims <- renderBsp bspGpu (w ^. playerPos)
        --   shader (viewport, prims, matBuf, colBuf)

-}
