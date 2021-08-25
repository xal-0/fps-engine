{-# LANGUAGE EmptyCase #-}
module Engine.Render.Bsp (BspGpu, loadBsp, renderBsp, visStats) where

import Control.Lens hiding (each)
import Control.Monad.State.Strict
import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Engine.BspTree
import Engine.File.Bsp
import Engine.Util.Geometry
import Engine.Util.Pipes
import Graphics.GPipe hiding (trace)
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P

data BspGpu os = BspGpu
  { _bspLeafVA :: LeafVA os,
    _bspCpu :: Bsp
  }

type LeafVA os =
  V.Vector (Render os (VertexArray () (B3 Float)))

loadBsp :: ContextHandler ctx => FilePath -> ContextT ctx os IO (BspGpu os)
loadBsp file = do
  _bspCpu <- liftIO $ readBsp file
  _bspLeafVA <- loadLeaves (_bspCpu ^. bspLeaves)
  pure BspGpu {..}

renderBsp :: BspGpu os -> V3 Float -> Render os (PrimitiveArray Triangles (B3 Float))
renderBsp BspGpu {..} pos = do
  let leaves = _bspCpu ^. bspLeaves
      leaf = leafAtPos (_bspCpu ^. bspTree) pos
  faceVAs <- case leaves ^? ix leaf . leafVis . _Just of
    Nothing -> sequence _bspLeafVA
    Just vis -> traverse (_bspLeafVA V.!) vis
  pure $ foldMap (toPrimitiveArray TriangleList) faceVAs

visStats :: BspGpu os -> V3 Float -> (Int, Int)
visStats BspGpu {..} pos =
  let leaves = _bspCpu ^. bspLeaves
      leaf = leafAtPos (_bspCpu ^. bspTree) pos
      vis = (leaves V.! leaf) ^. leafVis . to (maybe 0 V.length)
  in (vis, V.length leaves)

loadLeaves ::
  ContextHandler ctx =>
  V.Vector LeafData ->
  ContextT ctx os IO (LeafVA os)
loadLeaves leaves = do
  let (vertices, offs) = toListP' (evalStateT (distribute (traverse leafTris leaves)) 0)
      verticesV = V.fromList vertices
  buf :: Buffer _ (B3 Float) <- newBuffer (V.length verticesV)
  writeBuffer buf 0 (V.toList verticesV)
  let va = newVertexArray buf
  pure $ flip fmap offs \(start, len) ->
    takeVertices len . dropVertices start <$> va

leafTris :: LeafData -> Producer (V3 Float) (State Int) (Int, Int)
leafTris l = do
  start <- get
  traverse_ faceTris (l ^. leafFaces) `for` \v -> do
    yield v
    id += 1
  end <- get
  pure (start, end - start)

faceTris :: Monad m => Face -> Producer (V3 Float) m ()
faceTris Face {..} = do
  each (VS.toList _faceEdges) >-> P.map (view _x) >-> fan
