{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine.File.Bsp (readBsp) where

import Control.Lens
import Control.Monad
import Data.Bits (complement)
import qualified Data.ByteString as B
import Data.Fix (Fix (..))
import Data.Functor.Foldable
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word
import Debug.Trace (traceM, traceShowM)
import Linear (V2 (..), V3 (..))
import System.IO.MMap
import Util.SGet

data Plane = Plane {_planeNorm :: V3 Float, _planeDist :: Float}
  deriving (Show)

newtype Face = Face {_faceEdges :: VS.Vector Edge}
  deriving (Show)

type Edge = V2 (V3 Float)

data BspTreeF a
  = Node {_nodePlane :: Plane, _nodeFront :: a, _nodeBack :: a}
  | Leaf {_leafFaces :: V.Vector Face}
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''BspTreeF
makePrisms ''BspTreeF

type BspTree = Fix BspTreeF

newtype Bsp = Bsp {_bspTree :: BspTree}

makeLenses ''Bsp

readBsp :: FilePath -> IO Bsp
readBsp file = do
  bs <- mmapFileByteString file Nothing

  let r = runGet (getBsp bs) bs

  either fail pure r

getBsp :: B.ByteString -> Get Bsp
getBsp bs = do
  version <- getWord32le
  guard $ version == 30

  skip 0x8 -- entities
  planesL <- getLump bs
  texturesL <- getLump bs
  verticesL <- getLump bs
  skip 0x8 -- visibility
  nodesL <- getLump bs
  skip 0x8 -- texinfo
  facesL <- getLump bs
  skip 0x8 -- lighting
  skip 0x8 -- clipnodes
  leavesL <- getLump bs
  marksurfacesL <- getLump bs
  edgesL <- getLump bs
  skip 0x8 -- surfedges
  skip 0x8 -- models
  let vertices :: VS.Vector (V3 Float) = readLumpS verticesL 0xc
  planes <- readLumpV planesL 0x14 getPlane

  let edgeix :: VS.Vector (V2 Word16) = readLumpS edgesL 0x4
      marksurfaces :: VS.Vector Word16 = readLumpS marksurfacesL 0x2
      markfaces = V.map undefined (V.convert marksurfaces)

  leaves <- readLumpV leavesL 0x1c (getLeaf markfaces)
  nodes <- readLumpV nodesL 0x18 (getNodeI planes)

  let _bspTree = tieBspTree leaves nodes

  pure Bsp {..}

getNodeI :: V.Vector Plane -> Get (BspTreeF Int16)
getNodeI planes = do
  iplane <- fromIntegral <$> getWord32le
  _nodeFront <- getInt16le
  _nodeBack <- getInt16le
  skip 0xc -- bbox
  skip 0x4 -- faces
  let _nodePlane = planes V.! iplane
  pure Node {..}

getPlane :: Get Plane
getPlane = do
  _planeNorm <- getV3
  _planeDist <- getFloat32le
  skip 0x4 -- type
  pure Plane {..}

getLeaf :: V.Vector Face -> Get (BspTreeF a)
getLeaf markfaces = do
  skip 0x4 -- contents
  skip 0x4 -- visoff
  skip 0xc -- bbox
  imarksurface <- fromIntegral <$> getWord16le
  nmarksurface <- fromIntegral <$> getWord16le
  skip 0x4
  let _leafFaces = V.slice imarksurface nmarksurface markfaces
  pure Leaf {..}

tieBspTree :: V.Vector (BspTreeF Int16) -> V.Vector (BspTreeF Int16) -> BspTree
tieBspTree leaves nodes = ana coalg 0
  where
    coalg n
      | n < 0 = leaves V.! fromIntegral (complement n)
      | otherwise = nodes V.! fromIntegral n
