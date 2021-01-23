{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Cache where

import Control.Algebra
import Control.Carrier.Error.Either (ErrorC, Throw, runError)
import Control.Carrier.Reader
import Control.Carrier.Throw.Either (throwError)
import qualified Database.Redis as R
import Import

---

data Cache (m :: Type -> Type) k where
  HllAdd :: ByteString -> [ByteString] -> Cache m Integer
  HllCount :: [ByteString] -> Cache m Integer

hllAdd :: (Has Cache sig m) => ByteString -> [ByteString] -> m Integer
hllAdd key values = send $ HllAdd key values

hllCount :: (Has Cache sig m) => [ByteString] -> m Integer
hllCount = send . HllCount

newtype CacheIOC m a = CacheIOC {runCacheIO :: ReaderC R.Connection m a}
  deriving (Applicative, Functor, Monad, MonadIO)

newtype CacheError = CacheError R.Reply
  deriving (Eq, Show)

runCacheWithConnection :: R.Connection -> CacheIOC m hs -> m hs
runCacheWithConnection conn = runReader conn . runCacheIO

instance
  (Has (Throw CacheError) sig m, MonadIO m, Algebra sig m) =>
  Algebra (Cache :+: sig) (CacheIOC m)
  where
  alg hdl sig ctx = CacheIOC $ case sig of
    L (HllAdd key values) -> do
      conn <- ask
      added <- liftIO $
        R.runRedis conn $ do
          R.pfadd key values
      (<$ ctx) <$> either (throwError . CacheError) pure added
    L (HllCount keys) -> do
      conn <- ask
      count <- liftIO $
        R.runRedis conn $ do
          R.pfcount keys
      (<$ ctx) <$> either (throwError . CacheError) pure count
    R other -> alg (runCacheIO . hdl) (R other) ctx

runCacheEither :: ErrorC CacheError m a -> m (Either CacheError a)
runCacheEither = runError @CacheError
