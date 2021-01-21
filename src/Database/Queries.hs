{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Database.Queries where

import Data.Time
import Database.PostgreSQL.Simple.Types (Only (..))
import Effects
import Import
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ
import Server.Types (Latitude (..), Longitude (..))

-- | Represents just enough data for autocomplete use cases
data CityAutocompleteQ = CityAutocompleteQ
  { caGeonameId :: Int,
    caCityName :: Text,
    caLongitude :: Double,
    caLatitude :: Double,
    caPopulation :: Int,
    caTimeZone :: Text,
    caCountryCode :: Maybe Text,
    caCountryName :: Maybe Text,
    caRegionName :: Maybe Text,
    caDistrictName :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromRow CityAutocompleteQ

-- | Represents a full search for a city
data CityQ = CityQ
  { cGeonameId :: Int,
    cCityName :: Text,
    cLongitude :: Double,
    cLatitude :: Double,
    cPopulation :: Int,
    cTimeZone :: Text,
    cElevation :: Maybe Int,
    cCountryCode :: Maybe Text,
    cCountryName :: Maybe Text,
    cRegionName :: Maybe Text,
    cDistrictName :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromRow CityQ
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


-- | Fast query for name autocomplete: biased towards more populous cities,
-- doesn't order by how close the name is to the input; uses a denormalized
-- materialized view.
cityAutoComplete :: Has Database sig m => Text -> Maybe Int -> m [CityAutocompleteQ]
cityAutoComplete q limit' = do
  let limit = defaultLimit 5 limit'
  query 
    "select * from geocode.city_autocomplete where name %> ? limit ?"
    (q, limit)

-- | Slightly slower, but more precise, query to find a city that
-- partially matches a name: it looks at alternate names, and 
-- sorts by how close each match is to the partial query;
-- returns almost the same data as `cityAutoComplete`, but it's meant
-- to accommodate more.
citySearch :: Has Database sig m => Text -> Maybe Int -> m [CityQ]
citySearch q limit' = do
  let limit = defaultLimit 5 limit'
  query 
    [sql|
      select geocode.city.geonameid, geocode.city.name,
            location[0] as longitude, location[1] as latitude,
            population, timezone, elevation,
            geocode.country.iso as country_code,
            geocode.country.name as country_name,
            geocode.region.name as region_name,
            geocode.district.name as district_name
          from geocode.city
           left join geocode.country using(isocode) 
           left join geocode.region using(isocode, regcode)
           left join geocode.district using(isocode, regcode, discode)
           where alternatenames %> ?
           order by population desc, alternatenames <-> ? limit ?
    |]
    (q, q, limit)

-- | Same as `citySearch`, but takes a pair of longitude,latitude
-- as input. Uses a GiST index on a POINT stored in geocode.city.location
-- for somewhat naïve retrieval.
reverseSearch :: Has Database sig m => (Longitude, Latitude) -> Maybe Int -> m [CityQ]
reverseSearch (Longitude longitude, Latitude latitude) limit' = do
  let limit = defaultLimit 5 limit'
  query
    [sql|
      select geocode.city.geonameid, geocode.city.name,
        location[0] as longitude, location[1] as latitude,
        population, timezone, elevation,
        geocode.country.iso as country_code,
        geocode.country.name as country_name,
        geocode.region.name as region_name,
        geocode.district.name as district_name
      from geocode.city
       left join geocode.country using(isocode) 
       left join geocode.region using(isocode, regcode)
       left join geocode.district using(isocode, regcode, discode)
       order by location <-> '(?, ?)' limit ?;
    |]
    (longitude, latitude, limit)


defaultLimit :: Int -> Maybe Int -> Int
defaultLimit def = 
  maybe def (\l -> if l > 100 then 100 else l)

-- NOTES
{- to run queries manually:
-- stack ghci
λ> let (DatabaseUrl url) = defaultConfig & appDatabaseUrl
λ> conn <- PG.connectPostgreSQL $ encodeUtf8 url
λ> runDatabaseWithConnection conn $ Database.Queries.cityCount 
196591

-}
