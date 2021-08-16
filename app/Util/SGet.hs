module Util.SGet
  ( getV3,
    seekGetVec,
    seekGetVec',
    seekGetVecS,
    getLump,
    readLump,
    readLumpV,
    readLumpS,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.ForeignPtr
import Foreign.Storable
import Linear.V3 (V3 (..))

getLump :: B.ByteString -> Get B.ByteString
getLump bs = do
  off <- getWord32le
  size <- getWord32le
  pure (B.take (fromIntegral size) (B.drop (fromIntegral off) bs))

readLump :: B.ByteString -> Get a -> Get a
readLump bs g = either fail pure (runGet g bs)

readLumpV :: B.ByteString -> Int -> Get a -> Get (V.Vector a)
readLumpV bs s g = either fail pure (runGet (V.replicateM (B.length bs `div` s) g) bs)

readLumpS :: Storable a => B.ByteString -> Int -> VS.Vector a
readLumpS bs s = byteStringToVector bs (B.length bs `div` s)

seekGetVec :: B.ByteString -> Get a -> Get (V.Vector a)
seekGetVec bs g = do
  num <- getWord32le
  off <- getWord32le
  seekGetVec' bs g num off

seekGetVec' :: Integral n => B.ByteString -> Get a -> n -> n -> Get (V.Vector a)
seekGetVec' bs g num off =
  let g' = do
        skip (fromIntegral off)
        V.replicateM (fromIntegral num) g
   in either fail pure (runGet g' bs)

seekGetVecS :: forall a n. (Storable a, Integral n) => B.ByteString -> n -> n -> VS.Vector a
seekGetVecS bs num off =
  byteStringToVector
    ( B.take
        (fromIntegral num * sizeOf (undefined :: a))
        (B.drop (fromIntegral off) bs)
    )
    (fromIntegral num)

getV3 :: Get (V3 Float)
getV3 = V3 <$> getFloat32le <*> getFloat32le <*> getFloat32le

byteStringToVector :: Storable a => B.ByteString -> Int -> VS.Vector a
byteStringToVector bs len = vec
  where
    vec = VS.unsafeFromForeignPtr0 (castForeignPtr (plusForeignPtr fptr off)) len
    (fptr, off, _) = B.toForeignPtr bs
