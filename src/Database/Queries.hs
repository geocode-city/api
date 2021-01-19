module Database.Queries where

import Data.Time
import Database.PostgreSQL.Simple.Types (Only (..))
import Effects
import Import

cityCount :: Has Database sig m => m Int
cityCount = do
  counts <- query_ "select count(geonameid) from geocode.city"
  case counts of
    [] -> pure 0
    [Only c] -> pure c
    (Only x) : _xs -> pure x

latestUpdate :: Has Database sig m => m (Maybe Day)
latestUpdate = do
  updatedAts <- query_ "select max(modification) from raw.geonames"
  case updatedAts of
    [] -> pure Nothing
    [Only u] -> pure u
    (Only u : _xs) -> pure u

{- to run queries manually:
-- stack ghci
λ> let (DatabaseUrl url) = defaultConfig & appDatabaseUrl
λ> conn <- PG.connectPostgreSQL $ encodeUtf8 url
λ> runDatabaseWithConnection conn $ Database.Queries.cityCount 
196591

-}
