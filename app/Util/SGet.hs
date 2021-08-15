{-# LANGUAGE ScopedTypeVariables #-}

module Util.SGet (seekGetVec, seekGetVec', seekGetVecS) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Serialize.Get
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.ForeignPtr
import Foreign.Storable

seekGetVec :: B.ByteString -> Get a -> Get (V.Vector a)
seekGetVec bs g = do
  num <- getInt32le
  off <- getInt32le
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

byteStringToVector :: Storable a => B.ByteString -> Int -> VS.Vector a
byteStringToVector bs len = vec
  where
    vec = VS.unsafeFromForeignPtr0 (castForeignPtr (plusForeignPtr fptr off)) len
    (fptr, off, _) = B.toForeignPtr bs
