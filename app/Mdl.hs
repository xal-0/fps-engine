{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mdl
  ( Studio (..),
    Model (..),
    Mesh (..),
    readStudio,
  )
where

import Control.Monad
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear.V3
import Studio
import System.IO.MMap

data Studio = Studio
  { _studioModels :: V.Vector Model,
    _studioTextures :: V.Vector Texture
  }
  deriving (Show)

data Texture = Texture
  deriving (Show)

newtype Bodypart = Bodypart
  { _bodypartModels :: V.Vector Model
  } deriving (Show)

data Model = Model
  { _modelVerts :: VS.Vector (V3 Float),
    _modelNorms :: VS.Vector (V3 Float),
    _modelMeshes :: V.Vector Mesh
  }
  deriving (Show)

newtype Mesh = Mesh
  { _meshTris :: [Word16]
  }
  deriving (Show)

-- readStudio :: FilePath -> IO Studio
-- readStudio file = do
--   (ptr, 0, size) <- mmapFileForeignPtr file ReadOnly Nothing
--   let arr :: forall a o. (Storable a, Integral o) => o -> o -> IO (VS.Vector a)
--       arr off num = do
--         guard $
--           off >= 0 && num >= 0
--             && fromIntegral off + fromIntegral num * sizeOf (undefined :: a) < size
--         pure $
--           VS.unsafeFromForeignPtr0
--             (plusForeignPtr ptr (fromIntegral off))
--             (fromIntegral num)

--       sPeek :: forall a o. (Storable a, Integral o) => o -> IO a
--       sPeek off = withForeignPtr ptr \ptr' -> do
--         guard $ off >= 0 && fromIntegral off + sizeOf (undefined :: a) < size
--         peek (plusPtr ptr' (fromIntegral off))

--   header <- sPeek 0

--   guard $ c'studiohdr_t'numbodyparts header <= 1
--   bodypart <- sPeek (c'studiohdr_t'bodypartindex header)

  -- guard $ c'studio

  -- models <- peekArray
  -- print (bodypart :: C'mstudiomodel_t)
  
  -- pure _

-- readMdl :: FilePath -> IO Model
-- readMdl file = do
--   header <- peek (castPtr ptr)

--   let peekIdx num idx a =
--         peekArray
--           (fromIntegral (num a))
--           (plusPtr ptr (fromIntegral (idx a)))

--   (bodypart : _) <- peekIdx c'studiohdr_t'numbodyparts c'studiohdr_t'bodypartindex header
--   (model : _) <- peekIdx c'mstudiobodyparts_t'nummodels c'mstudiobodyparts_t'modelindex bodypart

--   let readMesh mesh = do
--         let start = plusPtr ptr (fromIntegral (c'mstudiomesh_t'triindex mesh))
--         Mesh <$> readTriCmd start

--       readTriCmd p = do
--         len :: Int16 <- peek p
--         if len == 0
--           then pure []
--           else do
--             let (p' :: Ptr C'mstudiotrivert_t) = castPtr (advancePtr p 1)
--                 numtris = fromIntegral $ abs len
--             tris <- peekArray numtris p'
--             let cmd =
--                   (if len > 0 then TriCmdStrip else TriCmdFan)
--                     (map (fromIntegral . c'mstudiotrivert_t'vertindex) tris)
--             cmds <- readTriCmd (castPtr (advancePtr p' numtris))
--             pure $ cmd : cmds

--   verts <- peekIdx c'mstudiomodel_t'numverts c'mstudiomodel_t'vertindex model

--   meshes <-
--     peekIdx c'mstudiomodel_t'nummesh c'mstudiomodel_t'meshindex model
--       >>= traverse readMesh

--   pure $ Model verts meshes
