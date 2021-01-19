module Database.Migrations where

import Config
import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (..), MigrationContext (..), MigrationResult (..), runMigration)
import Import

-- TODO(luis) we may want to take a Bool parameter to send
-- in `MigrationContext`: right now it defaults to verbose.
runMigrations' :: Bool -> FilePath -> DatabaseUrl -> IO (Either String String)
runMigrations' isVerbose migrationsDir (DatabaseUrl conStr) = do
  con <- connectPostgreSQL $ encodeUtf8 conStr
  -- initialize the `schema_migrations` table
  void $
    withTransaction con $
      runMigration $
        MigrationContext MigrationInitialization isVerbose con
  -- run the actual migrations
  result <-
    withTransaction con $
      runMigration $
        MigrationContext
          (MigrationDirectory migrationsDir)
          isVerbose
          con

  case result of
    MigrationSuccess -> pure $ Right "All migrations ran."
    MigrationError e -> pure $ Left e

-- | run migrations verbosely in the given directory, connected to the given
-- DB URL
runMigrations :: FilePath -> DatabaseUrl -> IO (Either String String)
runMigrations = runMigrations' True

-- | Same as `runMigrations`, but without output.
runMigrationsSilent :: FilePath -> DatabaseUrl -> IO (Either String String)
runMigrationsSilent = runMigrations' False
