module Util.Geometry (fan, strip) where

import Pipes

strip :: Monad m => Pipe a a m r
strip = do
  v0 <- await
  v1 <- await
  goA v0 v1
  where
    goA v0 v1 = do
      v2 <- await
      yield v0
      yield v1
      yield v2
      goB v1 v2

    goB v0 v1 = do
      v2 <- await
      yield v1
      yield v0
      yield v2
      goA v1 v2

fan :: Monad m => Pipe a a m r
fan = do
  centre <- await
  let go v1 = do
        v2 <- await
        yield centre
        yield v1
        yield v2
        go v2

  v1 <- await
  go v1
