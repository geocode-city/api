{-# LANGUAGE RecordWildCards #-}
module Server.Handlers where

import Import
import Servant hiding (throwError)
import Server.Types
import Database.Queries as Q
import Control.Carrier.Error.Either (throwError)
import Server.Auth
import Control.Lens ((.~), (?~))
import Data.Swagger
import Servant.Swagger
import Data.Time
import Effects.Cache
import Effects.Time (now)

service :: AppM sig m => ServerT Service m
service =
  return swaggerSpec
    :<|> stats
    :<|> autoComplete
    :<|> search
    :<|> reverseGeocode

stats :: (AppM sig m) => RequestKey -> m Stats
stats apiKey = validateApiKey apiKey >> do
  count <- Q.cityCount
  update <- Q.latestUpdate
  return $ Stats update count

autoComplete :: (AppM sig m) => RequestKey -> Text -> Maybe Int -> m [CityAutocomplete]
autoComplete apiKey q limit =
  validateApiKey apiKey >> do
    results <- Q.cityAutoComplete q limit
    return $ map serializeAutocompleteResult results

-- | Search city by name
search :: (AppM sig m) => RequestKey -> Text -> Maybe Int -> m [City]
search apiKey q limit =
  validateApiKey apiKey >> do
    results <- Q.citySearch q limit
    return $ map serializeCityResult results

-- | Search city by (lat, lng)
reverseGeocode :: (AppM sig m) => RequestKey -> Latitude -> Longitude -> Maybe Int -> m [City]
reverseGeocode apiKey lat lng limit =
  validateApiKey apiKey >> do
    results <- Q.reverseSearch (lng, lat) limit
    return $ map serializeCityResult results

---
--- Rate Limiting
---

-- TODO: return requests left in quota, rate limit reset timestamp

-- | Rate limit based on api key: must be valid and under allocated quota in the current (UTC) month
validateApiKey :: (AppM sig m) => RequestKey -> m ()
validateApiKey (ByApiKey (ApiKey apiKey) (RequestID requestId)) = do
  currentTime <- now
  let cacheKey = "requests:" <> encodeUtf8 apiKey <> encodeUtf8 (monthString currentTime)
  _added <- hllAdd cacheKey [requestId]
  count <- hllCount [cacheKey]
  -- TODO: retrieve quota from DB
  isValidKey <- Q.isKeyEnabled apiKey
  if isValidKey
    then
      if count < 100000 then
        pure ()
      else
        throwError err429 {errBody = "Monthly request limit exceeded for API Key."}
    else throwError err403 {errBody = "Invalid API Key."}

-- | Rate-limit based on (real) IP: must be under 1000 requests in the current (UTC) day
validateApiKey (ByIP (IPAddress ipAddress) (RequestID requestId)) = do
  currentTime <- now
  let cacheKey = "requests:" <> ipAddress <> encodeUtf8 (dayString currentTime)
  _added <- hllAdd cacheKey [requestId]
  count <- hllCount [cacheKey]
  if count < 1000 then
    pure ()
  else
    throwError err429 {errBody = "Daily request limit exceeded for IP Address (try using an API Key)."}


err429 :: ServerError
err429 = 
  ServerError { errHTTPCode = 429
                    , errReasonPhrase = "Too Many Requests"
                    , errBody = ""
                    , errHeaders = []
                    }

---
--- Swagger
---

swaggerSpec :: Swagger
swaggerSpec =
  toSwagger proxyApi
    & info . title .~ "Geocode.city API"
    & info . version .~ "1.0"
    & info . description ?~ "City-only geocoding"

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
-- writeSwaggerJSON :: IO ()
-- writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty todoSwagger)

---
--- HELPERS
---

serializeAutocompleteResult :: Q.CityAutocompleteQ -> CityAutocomplete
serializeAutocompleteResult Q.CityAutocompleteQ {..} =
  CityAutocomplete
    { cityName = caCityName,
      cityLongitude = caLongitude,
      cityLatitude = caLatitude,
      cityCountry = caCountryName,
      cityCountryCode = caCountryCode,
      cityRegion = caRegionName,
      cityDistrict = caDistrictName
    }

serializeCityResult :: Q.CityQ -> City
serializeCityResult Q.CityQ {..} =
  City
    { geonamesId = cGeonameId,
      name = cCityName,
      longitude = cLongitude,
      latitude = cLatitude,
      country = cCountryName,
      countryCode = cCountryCode,
      region = cRegionName,
      district = cDistrictName,
      timezone = cTimeZone,
      elevation = cElevation,
      population = cPopulation
    }

dayString :: UTCTime -> String
dayString = formatTime defaultTimeLocale "%Y-%m-%d"

monthString :: UTCTime -> String
monthString = formatTime defaultTimeLocale "%Y-%m"
