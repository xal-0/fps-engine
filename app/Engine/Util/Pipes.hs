{-# LANGUAGE FlexibleContexts #-}

module Engine.Util.Pipes (zipP, runGetPipe, toListP') where

import Control.Monad.Identity
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import Data.Serialize (Get, runGetState)
import Pipes
import Pipes.Internal
import Pipes.Lift

zipP :: (Traversable t, Monad m) => t (Producer a m r) -> Producer (t a) m r
zipP ps = do
  rs <- fmap sequence (traverse (lift . next) ps)
  case rs of
    Left r -> pure r
    Right xs -> do
      yield (fmap fst xs)
      zipP (fmap snd xs)

runGetPipe :: Monad m => Proxy a' a b' b Get r -> B.ByteString -> Proxy a' a b' b m r
runGetPipe p = evalStateT (distribute (hoist f p))
  where
    f g = do
      s <- get
      let Right (x, s') = runGetState g s 0
      put s'
      pure x

toListP' :: Producer a Identity b -> ([a], b)
toListP' = go
  where
    go prod =
      case prod of
        Request v _ -> closed v
        Respond a fu -> let (l, r) = go (fu ()) in (a : l, r)
        M m -> go (runIdentity m)
        Pure r -> ([], r)
{-# INLINE toListP' #-}
