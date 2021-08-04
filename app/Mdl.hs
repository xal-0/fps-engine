{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mdl (Model (..), Mesh (..), readModel) where

import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear.V3
import Studio
import System.IO.MMap

data Model = Model
  { _modelVerts :: [V3 Float],
    _modelMeshes :: [Mesh]
  }
  deriving Show

newtype Mesh = Mesh
  { _meshTris :: [Int16]
  }
  deriving Show

readModel :: FilePath -> IO Model
readModel file = mmapWithFilePtr file ReadOnly Nothing \(ptr, _) -> do
  header :: C'studiohdr_t <- peek (castPtr ptr)

  let peekIdx num idx a =
        peekArray
          (fromIntegral (num a))
          (plusPtr ptr (fromIntegral (idx a)))

  [bodypart] :: [C'mstudiobodyparts_t] <-
    peekIdx c'studiohdr_t'numbodyparts c'studiohdr_t'bodypartindex header
  [model] :: [C'mstudiomodel_t] <-
    peekIdx c'mstudiobodyparts_t'nummodels c'mstudiobodyparts_t'modelindex bodypart

  let
    readMesh mesh = do
      pure undefined

    -- readTriCmds 

        -- Mesh . map (fromIntegral . c'mstudiotrivert_t'vertindex)
        --   <$> peekIdx c'mstudiomesh_t'numtris c'mstudiomesh_t'triindex mesh

  verts :: [V3 Float] <-
    peekIdx c'mstudiomodel_t'numverts c'mstudiomodel_t'vertindex model

  -- meshes <-
  --   peekIdx c'mstudiomodel_t'nummesh c'mstudiomodel_t'meshindex model
  --     >>= traverse readMesh

  pure $ Model verts undefined
