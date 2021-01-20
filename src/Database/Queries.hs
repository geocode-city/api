module Database.Queries where

import Data.Time
import Database.PostgreSQL.Simple.Types (Only (..))
import Effects
import Import

-- | Count all unique cities in the `geocode.city` database.
cityCount :: Has Database sig m => m Int
cityCount = do
  counts <- query_ "select count(geonameid) from geocode.city"
  case counts of
    [] -> pure 0
    [Only c] -> pure c
    (Only x) : _xs -> pure x

-- | Find the newest modification as downloaded from Geonames.
latestUpdate :: Has Database sig m => m (Maybe Day)
latestUpdate = do
  updatedAts <- query_ "select max(modification) from raw.geonames"
  case updatedAts of
    [] -> pure Nothing
    [Only u] -> pure u
    (Only u : _xs) -> pure u

-- | Given an API Key, find out if it exists and is enabled.
isKeyEnabled :: Has Database sig m => Text -> m Bool
isKeyEnabled key = do
  exists <- query "select true from account.api_key where key = ? and is_enabled" (Only key)
  case exists of
    [] -> pure False
    [Only truth] -> pure truth
    (Only truth : _xs) -> pure truth

-- NOTES
{- to run queries manually:
-- stack ghci
λ> let (DatabaseUrl url) = defaultConfig & appDatabaseUrl
λ> conn <- PG.connectPostgreSQL $ encodeUtf8 url
λ> runDatabaseWithConnection conn $ Database.Queries.cityCount 
196591

-}
