module Engine.Util.Geometry (fan, strip, eulerToQuat) where

import Linear

-- strip :: Monad m => Pipe a a m r
-- strip = do
--   v0 <- await
--   v1 <- await
--   goA v0 v1
--   where
--     goA v0 v1 = do
--       v2 <- await
--       yield v0
--       yield v1
--       yield v2
--       goB v1 v2

--     goB v0 v1 = do
--       v2 <- await
--       yield v1
--       yield v0
--       yield v2
--       goA v1 v2

-- fan :: Monad m => Pipe a a m r
-- fan = do
--   centre <- await
--   let go v1 = do
--         v2 <- await
--         yield centre
--         yield v1
--         yield v2
--         go v2

--   v1 <- await
--   go v1

-- fan' :: [a] -> [a]
-- fan' [] = []
-- fan' (v0 : rest) = go rest
--   where
--     go (v1 : v2 : rest) = v0 : v1 : v2 : go (v2 : rest)
--     go _ = []

strip = undefined
fan = undefined

eulerToQuat :: V3 Float -> Quaternion Float
eulerToQuat (V3 roll pitch yaw) = Quaternion qw (V3 qx qy qz) -- Right handed, x is forward
  where
    cy = cos (yaw * 0.5)
    sy = sin (yaw * 0.5)
    cp = cos (pitch * 0.5)
    sp = sin (pitch * 0.5)
    cr = cos (roll * 0.5)
    sr = sin (roll * 0.5)

    qw = cr * cp * cy + sr * sp * sy
    qx = sr * cp * cy - cr * sp * sy
    qy = cr * sp * cy + sr * cp * sy
    qz = cr * cp * sy - sr * sp * cy
