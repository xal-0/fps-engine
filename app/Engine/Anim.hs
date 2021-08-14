module Engine.Anim () where

eulerToQuat :: V3 Float -> Quaternion Float
eulerToQuat (V3 roll pitch yaw) = Quaternion qw (V3 qx qy qz)
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

pSeqAnim ::
  Monad m =>
  V.Vector Bone ->
  Int ->
  V.Vector (Keyframe (Maybe B.ByteString)) ->
  Producer (V.Vector SkelTransform) m ()
pSeqAnim bones numFrames keyframeData = keyframes >-> P.map (skeletonConfig bones)
  where
    keyframes = zipP (fmap (boneKeyframes numFrames) keyframeData)

    boneKeyframes n kf = zipP (fmap (boneAdjs n) kf)

    boneAdjs _ Nothing = forever (yield 0)
    boneAdjs n (Just b) = animValues n b

showHex :: Int -> String
showHex = printf "%04x"

zipP :: (Traversable t, Monad m) => t (Producer a m r) -> Producer (t a) m r
zipP ps = do
  rs <- fmap sequence (traverse (lift . next) ps)
  case rs of
    Left r -> pure r
    Right xs -> do
      yield (fmap fst xs)
      zipP (fmap snd xs)

type SkelTransform = (Quaternion Float, V3 Float)

skeletonConfig :: V.Vector Bone -> V.Vector SkelAdjustment -> V.Vector SkelTransform
skeletonConfig bones adjs = tree
  where
    tree = V.map buildTree (V.zip bones adjs)
    buildTree (b, adj) = case _boneParent b of
      -1 -> boneTransform b adj
      p -> composeTransform (boneTransform b adj) (tree V.! p)
    boneTransform b (Keyframe posadj rotadj) =
      let rot = fmap fromIntegral rotadj * _boneScaleRot b + _boneDefaultRot b
          pos = fmap fromIntegral posadj * _boneScalePos b + _boneDefaultPos b
       in (eulerToQuat rot, pos)

    composeTransform (r2, p2) (r1, p1) = (r1 * r2, p1 + rotate r1 p2)
