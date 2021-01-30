{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Database.Queries where

--
import Database.PostgreSQL.Simple.Types (Only (..))
import Effects
import Import
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ
import Server.Types (Latitude (..), Longitude (..))


data CityQ = CityQ
  { cGeonameId :: Int,
    cCityName :: Text,
    cLongitude :: Double,
    cLatitude :: Double,
    cPopulation :: Int,
    cTimeZone :: Text,
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


-- | Given an API Key, find out if it exists and is enabled;
-- return status and current quota.
findApiKey :: Has Database sig m => Text -> m (Bool, Maybe Integer)
findApiKey key = do
  exists <- query "select is_enabled, monthly_quota from account.api_key where key = ?" (Only key)
  pure $ fromMaybe (False, Just 0) (listToMaybe exists)


-- | Fast query for name autocomplete: uses a denormalized, materialized view
-- that precomputes city, region, district, country and alternate names
-- in a weighted full-text search vector
-- see:
-- https://www.postgresql.org/docs/12/textsearch-controls.html#TEXTSEARCH-RANKING
-- https://www.postgresql.org/docs/12/textsearch-controls.html#TEXTSEARCH-RANKING
-- https://rob.conery.io/2019/10/29/fine-tuning-full-text-search-with-postgresql-12/
-- Note that `web_to_tsquery_prefix` is a function we defined, to sanitize
-- input and interpret it as a simple query where each token is a prefix.
cityAutoComplete :: Has Database sig m => Text -> Maybe Int -> m [CityQ]
cityAutoComplete q limit' = do
  let limit = defaultLimit 5 limit'
  query
    [sql|
      select geonameid, name, longitude, latitude,
        population, timezone, country_code,
        country_name, region_name, district_name
      from geocode.city_autocomplete
      where 
        web_to_tsquery_prefix(?) @@ autocomplete_doc
      order by
        ts_rank(autocomplete_doc, web_to_tsquery_prefix(?)) desc
      limit ?
    |]
    (q, q, limit)

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
            population, timezone,
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
        population, timezone,
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
-- import qualified Database.PostgreSQL.Simple as PG
λ> let (DatabaseUrl url) = defaultConfig & appDatabaseUrl
λ> conn <- PG.connectPostgreSQL $ encodeUtf8 url
λ> runDatabaseWithConnection conn $ Database.Queries.cityCount 
196591

-}
