module Database.Queries where

import Data.Time
import Database.PostgreSQL.Simple.Types (Only (..))
import Effects
import Import

-- | Count all unique cities in the `geocode.city` database.
cityCount :: Has Database sig m => m Int
cityCount = do
  counts <- query_ "select count(geonameid) from geocode.city"
  pure $ maybe 0 fromOnly (listToMaybe counts)

-- | Find the newest modification as downloaded from Geonames.
latestUpdate :: Has Database sig m => m (Maybe Day)
latestUpdate = do
  updatedAts <- query_ "select max(modification) from raw.geonames"
  pure $ fromOnly =<< listToMaybe updatedAts

-- | Given an API Key, find out if it exists and is enabled.
isKeyEnabled :: Has Database sig m => Text -> m Bool
isKeyEnabled key = do
  exists <- query "select is_enabled from account.api_key where key = ?" (Only key)
  pure $ maybe False fromOnly (listToMaybe exists)

-- NOTES
{- to run queries manually:
-- stack ghci
λ> let (DatabaseUrl url) = defaultConfig & appDatabaseUrl
λ> conn <- PG.connectPostgreSQL $ encodeUtf8 url
λ> runDatabaseWithConnection conn $ Database.Queries.cityCount 
196591

-}
