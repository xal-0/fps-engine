module Engine.Render.Bsp () where

import Control.Lens hiding (each)
import Data.Foldable
import Data.Functor.Foldable
import qualified Data.Vector.Storable as VS
import Engine.File.Bsp
import Engine.Util.Geometry
import Graphics.GPipe hiding (Max, Min)
import Pipes
import qualified Pipes.Prelude as P

bspToTris :: Monad m => BspTree -> Producer (V3 Float) m ()
bspToTris = cata alg
  where
    alg Leaf {..} = traverse_ faceTris _leafFaces
    alg Node {..} = _nodeFront >> _nodeBack

faceTris :: Monad m => Face -> Producer (V3 Float) m ()
faceTris Face {..} = each (VS.toList _faceEdges) >-> P.map (view _x) >-> fan

test = do
  b <- readBsp "maps/c1a0.bsp"
  pure ()
  let o =
        P.toList $
          bspToTris (b ^. bspTree)
            >-> P.map ((++ "\n") . concatMap ((++ " ") . show))
            >-> P.concat
  writeFile "maptris" o

minMaxTree :: BspTree -> IO ()
minMaxTree = cata alg
  where
    alg Leaf {..} = print (length _leafFaces)
    alg Node {..} = _nodeFront >> _nodeBack
