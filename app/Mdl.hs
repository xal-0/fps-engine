{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Mdl
  ( Studio,
    studioBodyparts,
    studioTextures,
    Texture,
    textureSize,
    textureData,
    Bodypart,
    bodypartModels,
    bodypartDefault,
    bodypartDefaultModel,
    Model,
    modelMeshes,
    Mesh,
    meshSkin,
    meshTris,
    readStudio,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Linear
import System.Directory
import System.FilePath.Lens
import System.IO.MMap

data Studio = Studio
  { _studioBodyparts :: V.Vector Bodypart,
    -- TODO: figure out why skins are indexed differently
    _studioTextures :: V.Vector Texture
  }
  deriving (Show)

data Texture = Texture
  { _textureSize :: V2 Int,
    _textureData :: [V3 Word8]
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
  { _meshTris :: [Vertex],
    _meshSkin :: Int
  }
  deriving (Show)

data Vertex = Vertex
  { _vertexPos :: !(V3 Float),
    _vertexNorm :: !(V3 Float),
    _vertexUV :: !(V2 Word16)
  }
  deriving (Show)

makeLenses ''Studio
makeLenses ''Texture
makeLenses ''Bodypart
makeLenses ''Model
makeLenses ''Mesh

bodypartDefaultModel :: Traversal' Bodypart Model
bodypartDefaultModel f b = (bodypartModels . ix (_bodypartDefault b - 1)) f b

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
        seekGetVec' V.replicateM get num off

      seekGetVec' rep get num off =
        let g = do
              skip (fromIntegral off)
              rep (fromIntegral num) get
         in either fail pure (runGet g bs)

  let r = flip runGet bs do
        magic <- getBytes 4
        version <- getInt32le
        guard $ magic == "IDST" && version == 10
        skip 0xac
        _studioTextures <- seekGetVec getTexture
        skip 0x10
        _studioBodyparts <- seekGetVec getBodypart
        pure (Studio {..})

      getTexture = do
        skip 0x44
        width <- getInt32le
        height <- getInt32le
        off <- getInt32le
        palette <- seekGetVec' VU.replicateM getPixel (0x100 :: Int) (off + width * height)
        _textureData <- seekGetVec' replicateM (getIPixel palette) (width * height) off
        let _textureSize = V2 (fromIntegral width) (fromIntegral height)
        pure (Texture {..})

      getPixel = V3 <$> getWord8 <*> getWord8 <*> getWord8
      getIPixel pal = do i <- getWord8; pure $ pal VU.! fromIntegral i

      getBodypart = do
        -- Of course, this one array does not use the usual layout.
        skip 0x40
        num <- getInt32le
        _bodypartDefault <- fromIntegral <$> getInt32le
        off <- getInt32le
        _bodypartModels <- seekGetVec' V.replicateM getModel num off
        pure (Bodypart {..})

      getModel = do
        skip 0x48
        nummesh <- getInt32le
        meshindex <- getInt32le

        numverts <- getInt32le
        skip 0x4
        vertindex <- getInt32le
        modelVerts <- seekGetVec' VU.replicateM getV3 numverts vertindex

        numnorms <- getInt32le
        skip 0x4
        normindex <- getInt32le
        modelNorms <- seekGetVec' VU.replicateM getV3 numnorms normindex
        skip 0x8

        _modelMeshes <- seekGetVec' V.replicateM (getMesh modelVerts modelNorms) nummesh meshindex

        pure (Model {..})

      getV3 = liftA3 V3 getFloat32le getFloat32le getFloat32le

      getMesh verts norms = do
        skip 0x4
        triindex <- fromIntegral <$> getInt32le
        _meshTris <- either fail pure $ runGet (skip triindex *> getTris verts norms) bs
        _meshSkin <- fromIntegral <$> getInt32le
        skip 0x8
        pure (Mesh {..})

      getTris verts norms = do
        len <- fromIntegral <$> getInt16le
        if len == 0
          then pure []
          else do
            let len' = abs len
                getVert = do
                  vertindex <- fromIntegral <$> getWord16le
                  normindex <- fromIntegral <$> getWord16le
                  st <- liftA2 V2 getWord16le getWord16le
                  pure $ Vertex (verts VU.! vertindex) (norms VU.! normindex) st

            l <- replicateM len' getVert
            ls <- getTris verts norms
            pure ((if len < 0 then fan else strip) l ++ ls)

  studio <- either fail pure r
  pure (studio & studioTextures %~ (extraTextures <>))

strip :: [a] -> [a]
strip [] = []
strip [v0, v1, v2] = [v0, v1, v2]
strip (v0 : v1 : v2 : v3 : vs) = v0 : v1 : v2 : v3 : v2 : v1 : strip (v2 : v3 : vs)
strip _ = []

fan :: [a] -> [a]
fan (centre : rest) = go centre rest
  where
    go v0 [v1, v2] = [v0, v1, v2]
    go v0 (v1 : v2 : vs) = v0 : v1 : v2 : go v0 (v2 : vs)
    go _ _ = []
fan _ = []
