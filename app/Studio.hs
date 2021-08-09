{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Either (fromRight)
import Data.Serialize.Get
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

data Studio = Studio
  { _studioName :: T.Text,
    _studioBodyparts :: V.Vector Bodypart,
    -- TODO: figure out why skins are indexed differently
    _studioTextures :: V.Vector Texture
  }
  deriving (Show)

data Texture = Texture
  { _textureSize :: V2 Int,
    _textureIndices :: VS.Vector Word8,
    _texturePalette :: VS.Vector (V3 Word8)
  }
  deriving (Show)

data Bodypart = Bodypart
  { _bodypartModels :: V.Vector Model,
    _bodypartDefault :: Int
  }
  deriving (Show)

newtype Model = Model
  { _modelMeshes :: V.Vector Mesh
  }
  deriving (Show)

data Mesh = Mesh
  { _meshNumTris :: Int,
    _meshData :: B.ByteString,
    _meshSkin :: Int,
    _meshVerts :: VS.Vector (V3 Float),
    _meshVertInfo :: VS.Vector Word8,
    _meshNorms :: VS.Vector (V3 Float),
    _meshNormInfo :: VS.Vector Word8
  }
  deriving (Show)

data Bone = Bone
  {
  }
  deriving (Show)

data Vertex = Vertex
  { _vertexPos :: !(V3 Float),
    _vertexNorm :: !(V3 Float),
    _vertexUV :: !(V2 Word16),
    _vertexBone :: !Int,
    _vertexNormBone :: !Int
  }
  deriving (Show)

makeLenses ''Studio
makeLenses ''Texture
makeLenses ''Bodypart
makeLenses ''Model
makeLenses ''Mesh

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
        skip 0x6c
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

  studio <- either fail pure r
  pure (studio & studioTextures %~ (<> extraTextures))

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
