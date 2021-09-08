{-# LANGUAGE TemplateHaskell #-}

module Engine.File.Bsp
  ( readBsp,
    Bsp,
    bspTree,
    bspTextures,
    bspLeaves,
    BspTree,
    BspTreeF (..),
    LeafData,
    leafFaces,
    leafVis,
    Face (..),
    TexInfo,
    texinfoS,
    texinfoT,
    texinfoSD,
    texinfoTD,
    texinfoTexture,
  )
where

import Control.Lens
import Control.Monad
import Data.Bits (complement)
import Data.Bits.Lens
import qualified Data.ByteString as B
import Data.Functor.Foldable
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word
import Engine.BspTree
import Engine.File.Texture
import Engine.Util.SGet
import Linear (V2 (..), V3 (..), _yx)
import System.IO.MMap

data Face = Face
  { _facePlane :: !Plane,
    _faceSide :: !Bool,
    _faceEdges :: !(VS.Vector Edge),
    _faceTexInfo :: !TexInfo
  }
  deriving (Show)

type Edge = V2 (V3 Float)

data LeafData = LeafData
  { _leafFaces :: V.Vector Face,
    _leafVis :: Maybe (V.Vector Int)
  }
  deriving (Show)

data TexInfo = TexInfo
  { _texinfoS :: !(V3 Float),
    _texinfoT :: !(V3 Float),
    _texinfoSD :: !Float,
    _texinfoTD :: !Float,
    _texinfoTexture :: !Int
  }
  deriving (Show)

data Bsp = Bsp
  { _bspTree :: BspTree Int,
    _bspLeaves :: V.Vector LeafData,
    _bspTextures :: V.Vector (Either T.Text Texture)
  }

makeLenses ''Bsp
makeLenses ''LeafData
makeLenses ''TexInfo

readBsp :: FilePath -> IO Bsp
readBsp file = do
  bs <- mmapFileByteString file Nothing
  either fail pure (runGet (getBsp bs) bs)

getBsp :: B.ByteString -> Get Bsp
getBsp bs = do
  version <- getWord32le
  guard $ version == 30

  entitiesL <- getLump bs
  planesL <- getLump bs
  texturesL <- getLump bs
  verticesL <- getLump bs
  visL <- getLump bs
  nodesL <- getLump bs
  texinfoL <- getLump bs
  facesL <- getLump bs
  skip 0x8 -- lighting
  skip 0x8 -- clipnodes
  leavesL <- getLump bs
  marksurfacesL <- getLump bs
  edgesL <- getLump bs
  surfedgesL <- getLump bs
  skip 0x8 -- models
  let vertices :: VS.Vector (V3 Float) = readLumpS verticesL 0xc
  planes <- readLumpV planesL 0x14 getPlane

  _bspTextures <- either fail pure (runGet (getTextures texturesL) texturesL)

  let edgeix :: VS.Vector (V2 Word16) = readLumpS edgesL 0x4
      edges = VS.map (fmap ((vertices VS.!) . fromIntegral)) edgeix

      -- Indices in the surfedge table point into the edge table, but
      -- can be negative to flip the direction of the edge (important
      -- for backface culling).
      surfedgesix :: VS.Vector Int32 = readLumpS surfedgesL 0x4
      surfedges = VS.map (lookupSurfedge edges) surfedgesix

  texinfos <- readLumpV texinfoL 0x28 getTexInfo
  faces <- readLumpV facesL 0x14 (getFace surfedges planes texinfos)

  let marksurfaces :: VS.Vector Word16 = readLumpS marksurfacesL 0x2
      markfaces = V.map (\i -> faces V.! fromIntegral i) (V.convert marksurfaces)
      nleaves = B.length leavesL `div` 0x1c

  _bspLeaves <- readLumpV leavesL 0x1c (getLeaf markfaces nleaves visL)
  nodes <- readLumpV nodesL 0x18 (getNodeI planes)

  -- let entitiesT = T.decodeUtf8 entitiesL
  -- traceM (T.unpack entitiesT)

  let _bspTree = tieBspTree nodes

  pure Bsp {..}

lookupSurfedge :: VS.Vector Edge -> Int32 -> Edge
lookupSurfedge edges i
  | i < 0 = (edges VS.! fromIntegral (- i)) ^. _yx
  | otherwise = edges VS.! fromIntegral i

getNodeI :: V.Vector Plane -> Get ((BspTreeF l) Int16)
getNodeI planes = do
  iplane <- fromIntegral <$> getWord32le
  _nodeFront <- getInt16le
  _nodeBack <- getInt16le
  skip 0xc -- bbox
  skip 0x4 -- faces
  let _nodePlane = planes V.! iplane
  pure Node {..}

getLeaf :: V.Vector Face -> Int -> B.ByteString -> Get LeafData
getLeaf markfaces nleaves visL = do
  skip 0x4 -- contents
  visoff <- fromIntegral <$> getInt32le
  skip 0xc -- bbox
  imarksurface <- fromIntegral <$> getWord16le
  nmarksurface <- fromIntegral <$> getWord16le
  skip 0x4
  let _leafFaces = V.slice imarksurface nmarksurface markfaces
      _leafVis = if visoff == -1
        then Nothing
        else Just (V.fromList (unpackVisData nleaves 1 (B.drop visoff visL)))
  pure LeafData {..}

unpackVisData :: Int -> Int -> B.ByteString -> [Int]
unpackVisData nleaves l b
  | l >= nleaves = []
  | otherwise =
    if x == 0
      then unpackVisData nleaves (l + fromIntegral n * 8) xs'
      else
        (x ^.. (bits . filtered id . asIndex . to (+ l) . filtered (< nleaves)))
          ++ unpackVisData nleaves (l + 8) xs
  where
    (x, xs) = (B.head b, B.tail b)
    (n, xs') = (B.head xs, B.tail xs)

getPlane :: Get Plane
getPlane = do
  _planeNorm <- getV3
  _planeDist <- getFloat32le
  skip 0x4 -- type
  pure Plane {..}

getFace :: VS.Vector Edge -> V.Vector Plane -> V.Vector TexInfo -> Get Face
getFace edges planes texinfos = do
  iplane <- fromIntegral <$> getWord16le
  _faceSide <- (/= 0) <$> getWord16le
  iedge <- fromIntegral <$> getWord32le
  nedge <- fromIntegral <$> getWord16le
  itexinfo <- fromIntegral <$> getWord16le
  skip 0x4 -- styles
  skip 0x4 -- lightmapoff
  let _faceEdges = VS.slice iedge nedge edges
      _facePlane = planes V.! iplane
      _faceTexInfo = texinfos V.! itexinfo
  pure Face {..}

getTextures :: B.ByteString -> Get (V.Vector (Either T.Text Texture))
getTextures bs = do
  ntextures <- fromIntegral <$> getWord32le
  offs <- V.replicateM ntextures (fromIntegral <$> getWord32le)
  traverse skipGetTexture offs
  where
    skipGetTexture off =
      let bs' = B.drop off bs
       in either fail pure (runGet (getTexture bs') bs')

getTexture :: B.ByteString -> Get (Either T.Text Texture)
getTexture bs = do
  name <- getName 0x10
  width <- fromIntegral <$> getWord32le
  height <- fromIntegral <$> getWord32le
  mip0 <- fromIntegral <$> getWord32le
  if mip0 == 0
    then pure (Left name)
    else pure (Right (readTexture bs name (V2 width height) mip0))

getTexInfo :: Get TexInfo
getTexInfo = do
  _texinfoS <- getV3
  _texinfoSD <- getFloat32le
  _texinfoT <- getV3
  _texinfoTD <- getFloat32le
  _texinfoTexture <- fromIntegral <$> getWord32le
  skip 0x4 -- flags
  pure TexInfo {..}

tieBspTree :: V.Vector (BspTreeF Int Int16) -> BspTree Int
tieBspTree nodes = ana coalg 0
  where
    coalg n
      | n < 0 = Leaf (fromIntegral (complement n))
      | otherwise = nodes V.! fromIntegral n
