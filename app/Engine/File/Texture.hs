{-# LANGUAGE TemplateHaskell #-}

module Engine.File.Texture
  ( Texture,
    textureName,
    textureSize,
    texturePixels,
    readTexture,
  )
where

import Control.Lens
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Vector.Storable as VS
import Data.Word
import Engine.Util.SGet
import Linear
import Pipes as P
import qualified Pipes.Prelude as P

data Texture = Texture
  { _textureName :: Text,
    _textureSize :: V2 Int,
    _texturePixels :: Producer (V3 Word8) Identity ()
  }

makeLenses ''Texture

readTexture :: B.ByteString -> Text -> V2 Int -> Int -> Texture
readTexture bs _textureName _textureSize@(V2 width height) off = Texture {..}
  where
    texturePalette :: VS.Vector (V3 Word8) = seekGetVecS bs 0x100 (off + width * height)
    textureIndices :: VS.Vector Word8 = seekGetVecS bs (width * height) off
    _texturePixels =
      P.each (VS.toList textureIndices)
        >-> P.map ((texturePalette VS.!) . fromIntegral)
