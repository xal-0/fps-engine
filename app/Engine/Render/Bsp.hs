module Engine.Render.Bsp (BspGpu, loadBsp, renderBsp) where

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
  faceVAs <- sequenceA _bspLeafVA
  pure $ foldMap (toPrimitiveArray TriangleList) faceVAs

loadLeaves ::
  ContextHandler ctx =>
  V.Vector LeafData ->
  ContextT ctx os IO (LeafVA os)
loadLeaves leaves = do
  let (vertices, (offs, total)) = toListP' (runStateT (distribute (traverse leafTris leaves)) 0)
  buf :: Buffer _ (B3 Float) <- newBuffer total
  writeBuffer buf 0 vertices
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
