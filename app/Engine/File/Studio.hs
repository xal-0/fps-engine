{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.File.Studio
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
    seqAdjs,
    seqEvents,
    SkelAdjustment,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word
import Linear hiding (trace)
import Pipes as P
import qualified Pipes.Prelude as P
import System.Directory
import System.FilePath.Lens
import System.IO.MMap
import Util.Pipes
import Util.SGet

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

type SkelAdjustment = V.Vector (Keyframe Int16)

data Seq = Seq
  { _seqName :: T.Text,
    _seqFps :: Float,
    _seqNumFrames :: Int,
    _seqAdjs :: Producer SkelAdjustment Identity (),
    _seqEvents :: V.Vector (Int, Event)
  }

data Keyframe a = Keyframe
  { _keyframePosData :: V3 a,
    _keyframeRotData :: V3 a
  }
  deriving (Functor, Foldable, Traversable)

data Event
  = EventSound FilePath
  | EventMuzzleFlash
  deriving (Show)

makeLenses ''Studio
makeLenses ''Texture
makeLenses ''Bodypart
makeLenses ''Model
makeLenses ''Mesh
makeLenses ''Seq
makeLenses ''Event

readStudio :: FilePath -> IO Studio
readStudio file = do
  bs <- mmapFileByteString file Nothing

  let fileT = file & basename %~ (<> "t")
  textureFileEx <- doesFileExist fileT
  extraTextures <-
    if textureFileEx
      then _studioTextures <$> readStudio fileT
      else pure mempty

  let r = flip runGet bs do
        magic <- getBytes 4
        version <- getInt32le
        guard $ magic == "IDST" && version == 10
        _studioName <- getName 0x40

        skip 0x44
        _studioBones <- seekGetVec bs getBone

        skip 0x10
        _studioSeqs <- seekGetVec bs (getSeq (fromIntegral (V.length _studioBones)))
        skip 0x8

        _studioTextures <- seekGetVec bs getTexture
        skip 0x10
        _studioBodyparts <- seekGetVec bs getBodypart
        pure (Studio {..})

      getTexture = do
        skip 0x44
        width <- getInt32le
        height <- getInt32le
        off <- getInt32le
        let _textureSize = V2 (fromIntegral width) (fromIntegral height)
            texturePalette :: VS.Vector (V3 Word8) = seekGetVecS bs 0x100 (off + width * height)
            textureIndices :: VS.Vector Word8 = seekGetVecS bs (width * height) off
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
        _bodypartModels <- seekGetVec' bs getModel num off
        pure (Bodypart {..})

      getModel = do
        skip 0x48
        nummesh <- getInt32le
        meshindex <- getInt32le
        numverts <- getInt32le
        vertinfoindex <- getInt32le
        vertindex <- getInt32le

        let verts = seekGetVecS bs numverts vertindex
            vertInfo = seekGetVecS bs numverts vertinfoindex

        numnorms <- getInt32le
        norminfoindex <- getInt32le
        normindex <- getInt32le

        let norms = seekGetVecS bs numnorms normindex
            normInfo = seekGetVecS bs numnorms norminfoindex

        skip 0x8

        _modelMeshes <-
          seekGetVec'
            bs
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

      getSeq numbones = do
        _seqName <- getName 32
        _seqFps <- getFloat32le

        skip 0xc

        _seqEvents <- seekGetVec bs getEvent

        _seqNumFrames <- fromIntegral <$> getWord32le
        skip 0x3c

        numblends <- getWord32le
        guard $ numblends == 1

        animindex <- getWord32le
        keyframeData <- seekGetVec' bs getKeyframes numbones animindex
        let _seqAdjs = pSeqAdjs _seqNumFrames keyframeData

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

      getEvent = do
        frame <- fromIntegral <$> getWord32le
        event <- getWord32le
        skip 0x4 -- "type"
        options <- getName 64
        let ev = case event of
              5001 -> EventMuzzleFlash
              5004 -> EventSound (T.unpack options)
              _ -> error "unknown event"
        pure (frame, ev)

  studio <- either fail pure r
  pure (studio & studioTextures %~ (<> extraTextures))

pSeqAdjs :: Monad m => Int -> V.Vector (Keyframe (Maybe B.ByteString)) -> Producer SkelAdjustment m ()
pSeqAdjs numframes keyframes = zipP (fmap pKeyframe keyframes)
  where
    pKeyframe keyframe = zipP (fmap prod keyframe)

    prod Nothing = forever (yield 0)
    prod (Just b) = pAdjs numframes b

pAdjs :: Monad m => Int -> B.ByteString -> Producer Int16 m ()
pAdjs numframes = runGetPipe (getValue numframes)
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

getName :: Int -> Get T.Text
getName len = T.decodeUtf8 . B.takeWhile (/= 0) <$> getBytes len

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
