{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Render (loadMdl) where

import Control.Monad.IO.Class
import Data.Word
import Graphics.GPipe
import qualified Mdl as M

loadMdl :: ContextHandler ctx => FilePath -> ContextT ctx os IO (Render os (PrimitiveArray Triangles (B3 Float)))
loadMdl file = do
  model <- liftIO $ M.readMdl file
  modelVerts :: Buffer os (B3 Float) <- newBuffer (length (M._modelVerts model))
  writeBuffer modelVerts 0 (M._modelVerts model)

  let
    meshNum = 0
    indices = concatMap M._triCmdIndices (M._meshTris (M._modelMeshes model !! meshNum))
  modelIndexBuffer :: Buffer os (BPacked Word16) <- newBuffer (length indices)
  writeBuffer modelIndexBuffer 0 indices

  pure do
    indexArray <- newIndexArray modelIndexBuffer Nothing
    vertexArray <- newVertexArray modelVerts
    let prim [] _ = mempty
        prim (cmd : rest) array =
          let len = length (M._triCmdIndices cmd)
           in toPrimitiveArrayIndexed
                (case cmd of M.TriCmdStrip _ -> TriangleStrip; M.TriCmdFan _ -> TriangleFan)
                (takeIndices len array)
                vertexArray
                <> prim rest (dropIndices len array)
    pure $prim (M._meshTris (M._modelMeshes model !! meshNum)) indexArray
