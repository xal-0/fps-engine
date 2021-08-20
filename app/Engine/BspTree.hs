{-# LANGUAGE DeriveTraversable #-}

module Engine.BspTree
  ( BspTreeF (..),
    BspTree,
    Plane (..),
    leafAtPos,
  )
where

import Data.Fix (Fix (..))
import Data.Functor.Foldable
import Linear

data Plane = Plane {_planeNorm :: !(V3 Float), _planeDist :: !Float}
  deriving (Show)

data BspTreeF l a
  = Node {_nodePlane :: Plane, _nodeFront :: a, _nodeBack :: a}
  | Leaf {_leafData :: l}
  deriving (Show, Functor, Foldable, Traversable)

type BspTree l = Fix (BspTreeF l)

planeDist :: Plane -> V3 Float -> Float
planeDist Plane {..} pos = _planeNorm `dot` pos - _planeDist

leafAtPos :: BspTree l -> V3 Float -> l
leafAtPos tree pos = cata alg tree
  where
    alg Leaf {..} = _leafData
    alg Node {..}
      | _nodePlane `planeDist` pos >= 0 = _nodeFront
      | otherwise = _nodeBack
