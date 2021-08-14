module Util.Pipes (zipP) where

import Pipes

zipP :: (Traversable t, Monad m) => t (Producer a m r) -> Producer (t a) m r
zipP ps = do
  rs <- fmap sequence (traverse (lift . next) ps)
  case rs of
    Left r -> pure r
    Right xs -> do
      yield (fmap fst xs)
      zipP (fmap snd xs)
