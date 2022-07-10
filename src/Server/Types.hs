{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types where

import Import hiding (Reader)
import Config (LogMessage)
import Control.Carrier.Error.Either (Throw)
import Effects (Cache, Database, Log, Time, Tracer)
import Data.Aeson (ToJSON)
import Data.Swagger (Swagger, ToParamSchema, ToSchema)
import Data.Time.Clock.POSIX (POSIXTime)
import Servant (AuthProtect, FromHttpApiData (parseUrlPiece), Get, Header, Headers, JSON, QueryParam, QueryParam', Required, ServerError, Strict, (:<|>), type (:>))
import Servant.Server.Experimental.Auth (AuthServerData)
import Server.Auth (RequestKey)
import Control.Carrier.Reader (Reader)

type StrictParam = QueryParam' '[Required, Strict]
type ApiKeyProtect = AuthProtect "api-key"
type ApiRoutes =
  ApiKeyProtect :> "autocomplete"
    :> StrictParam "q" Text 
    :> QueryParam  "limit" Int
    :> Get '[JSON] (RateLimited [City])
  :<|> ApiKeyProtect :> "search"
    :> StrictParam "name" Text
    :> QueryParam "limit" Int
    :> Get '[JSON] (RateLimited [City])
  :<|> ApiKeyProtect :> "locationSearch"
    :> StrictParam "lat" Latitude
    :> StrictParam "lng" Longitude
    :> QueryParam "limit" Int
    :> Get '[JSON] (RateLimited [City])

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type Service = SwaggerAPI :<|> ApiRoutes
type AppM sig m =
  ( Has (Log LogMessage) sig m,
    Has (Throw ServerError) sig m,
    Has Database sig m,
    Has Time sig m,
    Has Cache sig m,
    Has (Reader Tracer) sig m
  )

proxyService :: Proxy Service
proxyService = Proxy

proxyApi :: Proxy ApiRoutes
proxyApi = Proxy

---
--- INTERNAL TYPES
---
-- | Auth type for api keys
-- from: https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication
type instance AuthServerData (AuthProtect "api-key") = RequestKey
type RateLimited a =
  Headers
    '[ Header "X-RateLimit-Limit" Integer,
       Header "X-RateLimit-Remaining" Integer,
       Header "X-RateLimit-Resets" POSIXTime
     ]
    a

-- Inspired by Github:
-- https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting
data RateLimitInfo = RateLimitInfo
  { rateLimitTotal :: Integer,
    rateLimitRemaining :: Integer,
    rateLimitResets :: POSIXTime
  }
---
--- REQUEST TYPES
---

newtype Latitude = Latitude Double
  deriving newtype (Eq, Show, Num, Ord)
  deriving (ToParamSchema) via Double

newtype Longitude = Longitude Double
  deriving newtype (Eq, Show, Num, Ord)
  deriving (ToParamSchema) via Double

-- ranges from this wrong answer that turned out to be right for me:
-- https://stackoverflow.com/a/23914607
mkLatitude :: Double -> Maybe Latitude
mkLatitude l =
  maybeBetween (-90.0, 90.0) l >>= (Just . Latitude)

mkLongitude :: Double -> Maybe Longitude
mkLongitude l =
  maybeBetween (-180.0, 180.0) l >>= (Just . Longitude)

tryParse :: Read a => (a -> Maybe b) -> (Text -> Text) -> String -> Either Text b
tryParse constructor errMsg s =
  maybe
    (Left $ errMsg $ toText s)
    Right
    (readMaybe s >>= constructor)

prepend :: Text -> Text -> Text
prepend = flip (<>)

parseBounded :: Read a => (a -> Maybe b) -> Text -> Text -> Either Text b
parseBounded ctr err a =
  parseUrlPiece a >>= tryParse ctr (prepend err)

instance FromHttpApiData Latitude where
  parseUrlPiece = parseBounded mkLatitude " is not a valid latitude."

instance FromHttpApiData Longitude where
  parseUrlPiece = parseBounded mkLongitude " is not a valid longitude."
---
--- RESPONSE TYPES
---

-- API representation of an autocomplete/search result.
data City = City
  { 
    name :: Text,
    longitude :: Double,
    latitude :: Double,
    country :: Maybe Text,
    countryCode :: Maybe Text,
    region :: Maybe Text,
    district :: Maybe Text,
    timezone :: Text,
    population :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON City
instance ToSchema City
