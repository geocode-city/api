{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Database where

import Control.Algebra
import Control.Carrier.Reader
import qualified Database.PostgreSQL.Simple as PG
import Import

---
--- ALGEBRA
---

data Database (m :: Type -> Type) k where
  Query :: (PG.ToRow q, PG.FromRow r) => PG.Query -> q -> Database m [r]
  Query_ :: (PG.FromRow r) => PG.Query -> Database m [r]
  Execute :: (PG.ToRow q) => PG.Query -> q -> Database m Int64
  Execute_ :: PG.Query -> Database m Int64

query :: (Has Database sig m, PG.ToRow q, PG.FromRow r) => PG.Query -> q -> m [r]
query q params = send $ Query q params

query_ :: (Has Database sig m, PG.FromRow r) => PG.Query -> m [r]
query_ = send . Query_

execute :: (Has Database sig m, PG.ToRow q) => PG.Query -> q -> m Int64
execute q params = send $ Execute q params

execute_ :: (Has Database sig m) => PG.Query -> m Int64
execute_ = send . Execute_

---
--- CARRIERS
---

newtype DatabaseIOC m a = DatabaseIOC {runDatabaseIO :: ReaderC PG.Connection m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

runDatabaseWithConnection :: PG.Connection -> DatabaseIOC m hs -> m hs
runDatabaseWithConnection conn = runReader conn . runDatabaseIO

instance
  (MonadIO m, Algebra sig m) =>
  Algebra (Database :+: sig) (DatabaseIOC m)
  where
  alg hdl sig ctx = DatabaseIOC $ case sig of
    L (Query q params) -> do
      conn <- ask
      (<$ ctx) <$> liftIO (PG.query conn q params)
    L (Query_ q) -> do
      conn <- ask
      (<$ ctx) <$> liftIO (PG.query_ conn q)
    L (Execute q params) -> do
      conn <- ask
      (<$ ctx) <$> liftIO (PG.execute conn q params)
    L (Execute_ q) -> do
      conn <- ask
      (<$ ctx) <$> liftIO (PG.execute_ conn q)
    R other -> alg (runDatabaseIO . hdl) (R other) ctx
