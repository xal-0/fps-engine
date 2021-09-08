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

data Texture = Texture
  { _textureName :: Text,
    _textureSize :: V2 Int,
    _texturePixels :: VS.Vector (V3 Word8)
  }

makeLenses ''Texture

readTexture :: B.ByteString -> Text -> V2 Int -> Int -> Texture
readTexture bs _textureName _textureSize@(V2 width height) off = Texture {..}
  where
    texturePalette :: VS.Vector (V3 Word8) = seekGetVecS bs 0x100 (off + width * height)
    textureIndices :: VS.Vector Word8 = seekGetVecS bs (width * height) off
    _texturePixels = VS.map ((texturePalette VS.!) . fromIntegral) textureIndices
