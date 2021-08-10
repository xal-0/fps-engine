{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Either (fromRight)
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
import System.Directory
import System.FilePath.Lens
import System.IO.MMap
import Debug.Trace (traceM)

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
    _textureIndices :: VS.Vector Word8,
    _texturePalette :: VS.Vector (V3 Word8)
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
    _meshData :: B.ByteString,
    _meshSkin :: Int,
    _meshVerts :: VS.Vector (V3 Float),
    _meshVertInfo :: VS.Vector Word8,
    _meshNorms :: VS.Vector (V3 Float),
    _meshNormInfo :: VS.Vector Word8
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
    _seqKeyframes :: V.Vector (Keyframe (Maybe B.ByteString)),
    _seqNumFrames :: Int
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

  let seekGetVec get = do
        num <- getInt32le
        off <- getInt32le
        seekGetVec' get num off

      seekGetVec' get num off =
        let g = do
              skip (fromIntegral off)
              V.replicateM (fromIntegral num) get
         in either fail pure (runGet g bs)

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
        _studioSeqs <- seekGetVec (getSeq (V.length _studioBones))
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
            _texturePalette = seekGetVecS 0x100 (off + width * height)
            _textureIndices = seekGetVecS (width * height) off
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

        let _meshVerts = seekGetVecS numverts vertindex
            _meshVertInfo = seekGetVecS numverts vertinfoindex

        numnorms <- getInt32le
        norminfoindex <- getInt32le
        normindex <- getInt32le

        let _meshNorms = seekGetVecS numnorms normindex
            _meshNormInfo = seekGetVecS numnorms norminfoindex

        skip 0x8

        _modelMeshes <-
          seekGetVec'
            (getMesh _meshVerts _meshVertInfo _meshNorms _meshNormInfo)
            nummesh
            meshindex

        pure (Model {..})

      getMesh _meshVerts _meshVertInfo _meshNorms _meshNormInfo = do
        _meshNumTris <- fromIntegral <$> getInt32le
        triindex <- getInt32le
        let _meshData = B.drop (fromIntegral triindex) bs
        _meshSkin <- fromIntegral <$> getInt32le
        skip 0x8
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

      getSeq numbones = do
        _seqName <- getName 32
        _seqFps <- getFloat32le

        skip 0xc
        skip 0x8 -- events
        _seqNumFrames <- fromIntegral <$> getWord32le
        skip 0x3c

        numblends <- getWord32le
        guard $ numblends == 1

        animindex <- getWord32le
        _seqKeyframes <- seekGetVec' getKeyframes numbones animindex

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

bodypartDefaultModel :: Traversal' Bodypart Model
bodypartDefaultModel f b = (bodypartModels . ix (_bodypartDefault b - 1)) f b

texturePixels :: Fold Texture (V3 Word8)
texturePixels = folding f
  where
    f Texture {..} = map ((_texturePalette VS.!) . fromIntegral) (VS.toList _textureIndices)

meshTris :: Fold Mesh Vertex
meshTris = folding (fromRight [] . unpackTris)
  where
    unpackTris mesh@Mesh {..} = runGet (getTris mesh) _meshData

    getTris mesh@Mesh {..} = do
      len <- fromIntegral <$> getInt16le
      if len == 0
        then pure []
        else do
          let len' = abs len
              getVert = do
                vertindex <- fromIntegral <$> getWord16le
                normindex <- fromIntegral <$> getWord16le
                st <- liftA2 V2 getWord16le getWord16le
                traceM "aaa"
                pure $
                  Vertex
                    { _vertexPos = _meshVerts VS.! vertindex,
                      _vertexNorm = _meshNorms VS.! normindex,
                      _vertexUV = st,
                      _vertexBone = fromIntegral (_meshVertInfo VS.! vertindex),
                      _vertexNormBone = fromIntegral (_meshNormInfo VS.! normindex)
                    }

          l <- replicateM len' getVert
          ls <- getTris mesh
          pure ((if len < 0 then fan else strip) l ++ ls)

strip :: [a] -> [a]
strip (v0' : v1' : vs') = goA v0' v1' vs'
  where
    goA _ _ [] = []
    goA v0 v1 (v2 : vs) = v0 : v1 : v2 : goB v1 v2 vs

    goB _ _ [] = []
    goB v0 v1 (v2 : vs) = v1 : v0 : v2 : goA v1 v2 vs
strip _ = error "strip"

fan :: [a] -> [a]
fan (centre : rest) = go centre rest
  where
    go v0 [v1, v2] = [v0, v1, v2]
    go v0 (v1 : v2 : vs) = v0 : v1 : v2 : go v0 (v2 : vs)
    go _ _ = []
fan _ = error "fan"

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

type SkelTransform = (Quaternion Float, V3 Float)

type SkelAdjustment = Keyframe Int16

seqAnim :: V.Vector Bone -> Seq -> [V.Vector SkelTransform]
seqAnim bones animseq = getZipList $ fmap (skeletonConfig bones) keyframes
  where
    keyframes = traverse boneKeyframes (animseq ^. seqKeyframes)

    boneKeyframes kf = traverse (boneAdjs (_seqNumFrames animseq)) kf

    boneAdjs n Nothing = ZipList (replicate n 0)
    boneAdjs n (Just b) = ZipList (animValues n b)

animValues :: Int -> B.ByteString -> [Int16]
animValues numframes b = fromRight (error "animValues") (runGet (getValue numframes) b)
  where
    getValue 0 = pure []
    getValue left = do
      valid <- fromIntegral <$> getWord8
      total <- fromIntegral <$> getWord8
      traceM $ "valid: " <> show valid <> " total: " <> show total
      values <- replicateM valid getInt16le
      let vs = pad (total - valid) values
      rest <- getValue (left - total)
      pure $ vs ++ rest

    pad _ [] = []
    pad n [x] = x : replicate n x
    pad n (x : xs) = x : pad n xs

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

    composeTransform (r2, p2) (r1, p1) = (r2 * r1, p2 + rotate r2 p1)
