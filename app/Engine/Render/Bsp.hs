{-# LANGUAGE EmptyCase #-}

module Engine.Render.Bsp (BspGpu, loadBsp, renderBsp, visStats) where

import Control.Lens hiding (each)
import Control.Monad.State.Strict
import Data.List
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Engine.BspTree
import Engine.File.Bsp
import Engine.Util.Geometry
import Graphics.GPipe hiding (trace)

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
  let (vertices, total) = fmap getSum (foldMap leafTris (V.toList leaves))
      offs = scanl' (\o (_, l) -> o + getSum l) 0 (fmap leafTris (V.toList leaves))
      lengths = fmap (getSum . snd . leafTris) (V.toList leaves)

  buf :: Buffer _ (B3 Float) <- newBuffer total
  writeBuffer buf 0 vertices
  let va = newVertexArray buf
      makeVa = \start len -> takeVertices len . dropVertices start <$> va
  pure $ V.fromListN (V.length leaves) (zipWith makeVa offs lengths)

leafTris :: LeafData -> ([V3 Float], Sum Int)
leafTris l = foldMap faceTris (l ^. leafFaces)

faceTris :: Face -> ([V3 Float], Sum Int)
faceTris Face {..} = (VS.toList _faceEdges & fmap (view _x) & fan, Sum $ 3 * (VS.length _faceEdges - 2))
