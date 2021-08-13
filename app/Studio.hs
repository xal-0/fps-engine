{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Studio
  ( readStudio,
    Studio,
    studioName,
    studioBodyparts,
    studioTextures,
    studioBones,
    studioSeqs,
    Texture,
    textureSize,
    texturePixels,
    Bodypart,
    bodypartModels,
    bodypartDefault,
    bodypartDefaultModel,
    Model,
    modelMeshes,
    Mesh,
    meshSkin,
    meshNumTris,
    meshTris,
    Vertex (..),
    Seq,
    seqName,
    seqFps,
    seqNumFrames,
    seqAnim,
    SkelTransform
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign (Storable (sizeOf), plusForeignPtr)
import Foreign.ForeignPtr (castForeignPtr)
import Linear hiding (trace)
import Pipes as P
import Pipes.Lift
import qualified Pipes.Prelude as P
import System.Directory
import System.FilePath.Lens
import System.IO.MMap
import Text.Printf (printf)

type SkelTransform = (Quaternion Float, V3 Float)

data Studio = Studio
  { _studioName :: T.Text,
    _studioBodyparts :: V.Vector Bodypart,
    -- TODO: figure out why skins are indexed differently
    _studioTextures :: V.Vector Texture,
    _studioBones :: V.Vector Bone,
    _studioSeqs :: V.Vector Seq
  }

data Texture = Texture
  { _textureSize :: V2 Int,
    _texturePixels :: Producer (V3 Word8) Identity ()
  }

data Bodypart = Bodypart
  { _bodypartModels :: V.Vector Model,
    _bodypartDefault :: Int
  }

newtype Model = Model
  { _modelMeshes :: V.Vector Mesh
  }

data Mesh = Mesh
  { _meshNumTris :: Int,
    _meshTris :: Producer Vertex Identity (),
    _meshSkin :: Int
  }

data Vertex = Vertex
  { _vertexPos :: !(V3 Float),
    _vertexNorm :: !(V3 Float),
    _vertexUV :: !(V2 Word16),
    _vertexBone :: !Int,
    _vertexNormBone :: !Int
  }
  deriving (Show)

data Bone = Bone
  { _boneName :: T.Text,
    _boneParent :: Int,
    _boneDefaultPos :: V3 Float,
    _boneDefaultRot :: V3 Float, -- Euler angles
    _boneScalePos :: V3 Float,
    _boneScaleRot :: V3 Float
  }
  deriving (Show)

data Seq = Seq
  { _seqName :: T.Text,
    _seqFps :: Float,
    _seqNumFrames :: Int,
    _seqAnim :: Producer (V.Vector SkelTransform) Identity ()
  }

data Keyframe a = Keyframe
  { _keyframePosData :: V3 a,
    _keyframeRotData :: V3 a
  }
  deriving (Functor, Foldable, Traversable)

makeLenses ''Studio
makeLenses ''Texture
makeLenses ''Bodypart
makeLenses ''Model
makeLenses ''Mesh
makeLenses ''Seq

readStudio :: FilePath -> IO Studio
readStudio file = do
  bs <- mmapFileByteString file Nothing

  let fileT = file & basename %~ (<> "t")
  textureFileEx <- doesFileExist fileT
  extraTextures <-
    if textureFileEx
      then _studioTextures <$> readStudio fileT
      else pure mempty

  let seekGetVec g = do
        num <- getInt32le
        off <- getInt32le
        seekGetVec' g num off

      seekGetVec' g num off =
        let g' = do
              skip (fromIntegral off)
              V.replicateM (fromIntegral num) g
         in either fail pure (runGet g' bs)

      seekGetVecS :: forall a n. (Storable a, Integral n) => n -> n -> VS.Vector a
      seekGetVecS num off =
        byteStringToVector
          ( B.take
              (fromIntegral num * sizeOf (undefined :: a))
              (B.drop (fromIntegral off) bs)
          )
          (fromIntegral num)

  let r = flip runGet bs do
        magic <- getBytes 4
        version <- getInt32le
        guard $ magic == "IDST" && version == 10
        _studioName <- getName 0x40

        skip 0x44
        _studioBones <- seekGetVec getBone

        skip 0x10
        _studioSeqs <- seekGetVec (getSeq _studioBones)
        skip 0x8

        _studioTextures <- seekGetVec getTexture
        skip 0x10
        _studioBodyparts <- seekGetVec getBodypart
        pure (Studio {..})

      getTexture = do
        skip 0x44
        width <- getInt32le
        height <- getInt32le
        off <- getInt32le
        let _textureSize = V2 (fromIntegral width) (fromIntegral height)
            texturePalette :: VS.Vector (V3 Word8) = seekGetVecS 0x100 (off + width * height)
            textureIndices :: VS.Vector Word8 = seekGetVecS (width * height) off
            _texturePixels =
              P.each (VS.toList textureIndices)
                >-> P.map ((texturePalette VS.!) . fromIntegral)

        pure (Texture {..})

      getBodypart = do
        -- Of course, this one array does not use the usual layout.
        skip 0x40
        num <- getInt32le
        _bodypartDefault <- fromIntegral <$> getInt32le
        off <- getInt32le
        _bodypartModels <- seekGetVec' getModel num off
        pure (Bodypart {..})

      getModel = do
        skip 0x48
        nummesh <- getInt32le
        meshindex <- getInt32le
        numverts <- getInt32le
        vertinfoindex <- getInt32le
        vertindex <- getInt32le

        let verts = seekGetVecS numverts vertindex
            vertInfo = seekGetVecS numverts vertinfoindex

        numnorms <- getInt32le
        norminfoindex <- getInt32le
        normindex <- getInt32le

        let norms = seekGetVecS numnorms normindex
            normInfo = seekGetVecS numnorms norminfoindex

        skip 0x8

        _modelMeshes <-
          seekGetVec'
            (getMesh verts vertInfo norms normInfo)
            nummesh
            meshindex

        pure (Model {..})

      getMesh verts vertInfo norms normInfo = do
        _meshNumTris <- fromIntegral <$> getInt32le
        triindex <- getInt32le
        _meshSkin <- fromIntegral <$> getInt32le
        skip 0x8

        let meshData = B.drop (fromIntegral triindex) bs
            _meshTris =
              runGetPipe
                (getTris verts vertInfo norms normInfo)
                meshData

        pure (Mesh {..})

      getBone = do
        _boneName <- getName 32
        _boneParent <- fromIntegral <$> getInt32le
        skip 0x1c
        _boneDefaultPos <- getV3
        _boneDefaultRot <- getV3
        _boneScalePos <- getV3
        _boneScaleRot <- getV3
        pure (Bone {..})

      getSeq bones = do
        _seqName <- getName 32
        _seqFps <- getFloat32le

        skip 0xc
        skip 0x8 -- events
        _seqNumFrames <- fromIntegral <$> getWord32le
        skip 0x3c

        numblends <- getWord32le
        guard $ numblends == 1

        animindex <- getWord32le
        keyframeData <- seekGetVec' getKeyframes (V.length bones) animindex
        let _seqAnim = pSeqAnim bones _seqNumFrames keyframeData

        skip 0x30

        pure (Seq {..})

      getKeyframes = do
        base <- bytesRead

        xs <- getAnimData base
        ys <- getAnimData base
        zs <- getAnimData base

        xrs <- getAnimData base
        yrs <- getAnimData base
        zrs <- getAnimData base

        let _keyframePosData = V3 xs ys zs
            _keyframeRotData = V3 xrs yrs zrs

        pure (Keyframe {..})

      getAnimData base = do
        off <- getWord16le
        if off == 0
          then pure Nothing
          else pure . Just $ B.drop (fromIntegral $ base + fromIntegral off) bs

  studio <- either fail pure r
  pure (studio & studioTextures %~ (<> extraTextures))

getV3 :: Get (V3 Float)
getV3 = V3 <$> getFloat32le <*> getFloat32le <*> getFloat32le

getName :: Int -> Get T.Text
getName len = T.decodeUtf8 . B.takeWhile (/= 0) <$> getBytes len

runGetPipe :: Monad m => Proxy a' a b' b Get r -> B.ByteString -> Proxy a' a b' b m r
runGetPipe p = evalStateT (distribute (hoist f p))
  where
    f g = do
      s <- get
      let Right (x, s') = runGetState g s 0
      put s'
      pure x

bodypartDefaultModel :: Traversal' Bodypart Model
bodypartDefaultModel f b = (bodypartModels . ix (_bodypartDefault b - 1)) f b

getTris ::
  VS.Vector (V3 Float) ->
  VS.Vector Word8 ->
  VS.Vector (V3 Float) ->
  VS.Vector Word8 ->
  Producer Vertex Get ()
getTris verts vertInfo norms normInfo = do
  len <- fromIntegral <$> lift getInt16le
  unless (len == 0) do
    let len' = abs len
        getVert = do
          vertindex <- fromIntegral <$> lift getWord16le
          normindex <- fromIntegral <$> lift getWord16le
          st <- lift $ liftA2 V2 getWord16le getWord16le
          yield $
            Vertex
              { _vertexPos = verts VS.! vertindex,
                _vertexNorm = norms VS.! normindex,
                _vertexUV = st,
                _vertexBone = fromIntegral (vertInfo VS.! vertindex),
                _vertexNormBone = fromIntegral (normInfo VS.! normindex)
              }

    replicateM_ len' getVert >-> if len > 0 then strip else fan
    getTris verts vertInfo norms normInfo

strip :: Monad m => Pipe a a m r
strip = do
  v0 <- await
  v1 <- await
  goA v0 v1
  where
    goA v0 v1 = do
      v2 <- await
      yield v0
      yield v1
      yield v2
      goB v1 v2

    goB v0 v1 = do
      v2 <- await
      yield v1
      yield v0
      yield v2
      goA v1 v2

fan :: Monad m => Pipe a a m r
fan = do
  centre <- await
  let go v1 = do
        v2 <- await
        yield centre
        yield v1
        yield v2
        go v2

  v1 <- await
  go v1

byteStringToVector :: forall a. (Storable a) => B.ByteString -> Int -> VS.Vector a
byteStringToVector bs len = vec
  where
    vec = VS.unsafeFromForeignPtr0 (castForeignPtr (plusForeignPtr fptr off)) len
    (fptr, off, _) = B.toForeignPtr bs

eulerToQuat :: V3 Float -> Quaternion Float
eulerToQuat (V3 roll pitch yaw) = Quaternion qw (V3 qx qy qz)
  where
    cy = cos (yaw * 0.5)
    sy = sin (yaw * 0.5)
    cp = cos (pitch * 0.5)
    sp = sin (pitch * 0.5)
    cr = cos (roll * 0.5)
    sr = sin (roll * 0.5)

    qw = cr * cp * cy + sr * sp * sy
    qx = sr * cp * cy - cr * sp * sy
    qy = cr * sp * cy + sr * cp * sy
    qz = cr * cp * sy - sr * sp * cy

type SkelAdjustment = Keyframe Int16

pSeqAnim ::
  Monad m =>
  V.Vector Bone ->
  Int ->
  V.Vector (Keyframe (Maybe B.ByteString)) ->
  Producer (V.Vector SkelTransform) m ()
pSeqAnim bones numFrames keyframeData = keyframes >-> P.map (skeletonConfig bones)
  where
    keyframes = zipP (fmap (boneKeyframes numFrames) keyframeData)

    boneKeyframes n kf = zipP (fmap (boneAdjs n) kf)

    boneAdjs _ Nothing = forever (yield 0)
    boneAdjs n (Just b) = animValues n b

showHex :: Int -> String
showHex = printf "%04x"

zipP :: (Traversable t, Monad m) => t (Producer a m r) -> Producer (t a) m r
zipP ps = do
  rs <- fmap sequence (traverse (lift . next) ps)
  case rs of
    Left r -> pure r
    Right xs -> do
      yield (fmap fst xs)
      zipP (fmap snd xs)

animValues :: Monad m => Int -> B.ByteString -> Producer Int16 m ()
animValues numframes = runGetPipe (getValue numframes)
  where
    getValue 0 = pure ()
    getValue left = do
      valid <- fromIntegral <$> lift getWord8
      total <- fromIntegral <$> lift getWord8

      let go 0 l = replicateM_ (total - valid) (yield l)
          go n _ = do
            x <- lift getInt16le
            yield x
            go (n - 1) x

      x <- lift getInt16le
      yield x

      go (valid - 1) x

      getValue (left - total)

skeletonConfig :: V.Vector Bone -> V.Vector SkelAdjustment -> V.Vector SkelTransform
skeletonConfig bones adjs = tree
  where
    tree = V.map buildTree (V.zip bones adjs)
    buildTree (b, adj) = case _boneParent b of
      -1 -> boneTransform b adj
      p -> composeTransform (boneTransform b adj) (tree V.! p)
    boneTransform b (Keyframe posadj rotadj) =
      let rot = fmap fromIntegral rotadj * _boneScaleRot b + _boneDefaultRot b
          pos = fmap fromIntegral posadj * _boneScalePos b + _boneDefaultPos b
       in (eulerToQuat rot, pos)

    composeTransform (r2, p2) (r1, p1) = (r2 * r1, p1 + rotate r1 p2)
