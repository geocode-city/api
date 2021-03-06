{-# LANGUAGE RecordWildCards #-}
module Server.Handlers where

import Import
import Servant hiding (throwError)
import Server.Types
import Database.Queries as Q
import Control.Carrier.Error.Either (throwError)
import Server.Auth
import Control.Lens ((.~), (?~))
import Data.Swagger hiding (Info)
import Servant.Swagger
import Data.Time
import Config (LogMessage (..))
import Effects (hllAdd, hllCount, log, now)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Types as N

service :: AppM sig m => ServerT Service m
service =
  return swaggerSpec
    :<|> autoComplete
    :<|> search
    :<|> reverseGeocode

-- | Autocomplete based on partial name match    
autoComplete :: (AppM sig m) => RequestKey -> Text -> Maybe Int -> m (RateLimited [City])
autoComplete apiKey q limit = do
  rateLimitInfo <- checkUsage apiKey
  results <- Q.cityAutoComplete q limit
  return $ addRateLimitHeaders rateLimitInfo $ map serializeCityResult results

-- | Search city by name
search :: (AppM sig m) => RequestKey -> Text -> Maybe Int -> m (RateLimited [City])
search apiKey q limit = do
  rateLimitInfo <- checkUsage apiKey
  results <- Q.citySearch q limit
  return $ addRateLimitHeaders rateLimitInfo $ map serializeCityResult results

-- | Search city by (lat, lng)
reverseGeocode :: (AppM sig m) => RequestKey -> Latitude -> Longitude -> Maybe Int -> m (RateLimited [City])
reverseGeocode apiKey lat lng limit = do
  rateLimitInfo <- checkUsage apiKey
  results <- Q.reverseSearch (lng, lat) limit
  return $ addRateLimitHeaders rateLimitInfo $ map serializeCityResult results

---
--- Rate Limiting
---
-- | Rate limit based on api key: must be valid and under allocated quota in the current (UTC) month
-- note that a given key may not have any quota set: we currently interpret that
-- scenario as "unlimited" and return headers indicating that, just like `WithUnlimitedAccess`.
-- TODO(luis): add a "trusted origins" entry alongside api keys, for people who want to ensure
-- that only websites they control are sending api-key-authenticated requests.
checkUsage :: (AppM sig m) => RequestKey -> m RateLimitInfo
checkUsage (ByApiKey (ApiKey apiKey) (RequestID requestId)) = do
  (isValidKey, quota) <- Q.findApiKey apiKey
  if not isValidKey then
    throwError err403 {errBody = "Invalid API Key."}
  else do
    currentTime <- now
    let cacheKey = "requests:" <> encodeUtf8 apiKey <> ":" <> encodeUtf8 (monthString currentTime)
    _added <- hllAdd cacheKey [requestId]
    count <- hllCount [cacheKey]
    let limit     = fromMaybe 0 quota
        remaining = max 0 (limit - count)
        resetsAt  = case quota of
          Just _ -> nextMonthStart currentTime
          Nothing -> currentTime
        rateLimitInfo = RateLimitInfo limit remaining (resetsAt & utcTimeToPOSIXSeconds)
    -- no quota means unlimited (!)
    if maybe True (count <) quota  then
      pure rateLimitInfo
    else
      throwError
        err429
          { errBody = "Monthly request limit exceeded for API Key.",
            errHeaders = rateLimitHeaders rateLimitInfo
          }

-- | Rate-limit based on (real) IP: must be under 1000 requests in the current (UTC) day.
-- Useful for anonymous non-browser clients which won't include an `Origin` header.
checkUsage (ByIP (IPAddress ipAddress) requestId) = 
  --log (Info "Limiting by IP") >>
  limitByClientIdentifier ipAddress requestId

-- | Rate-limit based on Origin header; for browser-based clients. Same policy as per-IP.
-- NOTE(luis) at present, nothing prevents some enterprising scripter to send a bogus
-- Origin header with every request, circumventing ip limits. If they wish to go to these
-- lenghts, knowing that we may add User-Agent or IP-fallback restrictions, so be it:
-- an Api Key is free, we can talk about a higher quota if you're wiling to
-- collaborate, why do this!
checkUsage (ByOrigin (Origin origin) requestId) =
  -- TODO(luis) update the Log effect to ignore log events below a certain
  -- level, that way we don't have to comment/uncomment things!
  --log (Info $ "Limiting by Origin: " <> decodeUtf8 origin) >>
  limitByClientIdentifier origin requestId

-- | If given "unlimited access", don't do any rate limiting.
checkUsage WithUnlimitedAccess = do
  log $ Info "Request without rate limiting!"
  currentTime <- now
  pure $ RateLimitInfo 0 0 (currentTime & utcTimeToPOSIXSeconds)

limitByClientIdentifier :: AppM sig m => ByteString -> RequestID -> m RateLimitInfo 
limitByClientIdentifier clientIdentifier (RequestID requestId) = do
  currentTime <- now
  let cacheKey = "requests:" <> clientIdentifier <> ":" <> encodeUtf8 (dayString currentTime)
  _added <- hllAdd cacheKey [requestId]
  count <- hllCount [cacheKey]
  let limit = 1000
      remaining = max 0 (limit - count)
      resetsAt  = nextDayStart currentTime
      rateLimitInfo = RateLimitInfo limit remaining (resetsAt & utcTimeToPOSIXSeconds)
  if count < limit then
    pure rateLimitInfo
  else
    throwError
      err429
        { errBody = "Daily request limit exceeded for IP Address (try using an API Key).",
          errHeaders = rateLimitHeaders rateLimitInfo
        }


err429 :: ServerError
err429 = 
  ServerError { errHTTPCode = 429
                    , errReasonPhrase = "Too Many Requests"
                    , errBody = ""
                    , errHeaders = []
                    }

rateLimitHeaders :: RateLimitInfo -> [N.Header]
rateLimitHeaders RateLimitInfo {..} =
  [ ("X-RateLimit-Limit", encodeUtf8 . showString $ rateLimitTotal),
    ("X-RateLimit-Remaining", encodeUtf8 . showString $ rateLimitRemaining),
    ("X-RateLimit-Resets", encodeUtf8 . showString $ rateLimitResets)
  ]
  where
    showString :: Show a => a -> String
    showString = show

addRateLimitHeaders :: RateLimitInfo -> a -> RateLimited a
addRateLimitHeaders RateLimitInfo {..} =
  addHeader rateLimitTotal
    . addHeader rateLimitRemaining
    . addHeader rateLimitResets
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
serializeCityResult :: Q.CityQ -> City
serializeCityResult Q.CityQ {..} =
  City
    { 
      name = cCityName,
      longitude = cLongitude,
      latitude = cLatitude,
      country = cCountryName,
      countryCode = cCountryCode,
      region = cRegionName,
      district = cDistrictName,
      timezone = cTimeZone,
      population = cPopulation
    }

dayString :: UTCTime -> String
dayString = formatTime defaultTimeLocale "%Y-%m-%d"

monthString :: UTCTime -> String
monthString = formatTime defaultTimeLocale "%Y-%m"

nextDayStart :: UTCTime -> UTCTime
nextDayStart (UTCTime day _time) = UTCTime (addDays 1 day) 0

nextMonthStart :: UTCTime -> UTCTime
nextMonthStart (UTCTime day _time) =
  UTCTime monthStart 0
  where
    (y, m, _d) = toGregorian $ addGregorianMonthsClip 1 day
    monthStart = fromGregorian y m 1
