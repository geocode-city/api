{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Server.Types where

import Import
import Config (LogMessage)
import Control.Carrier.Error.Either (Throw)
import Data.Time (Day)
import Effects (Cache, Database, Log, Time)
import Data.Aeson
  ( Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    genericToJSON,
  )
import Servant (AuthProtect, FromHttpApiData (parseUrlPiece), Get, JSON, QueryParam, QueryParam', Required, ServerError, Strict, (:<|>), type (:>))
import Servant.Server.Experimental.Auth
import Server.Auth
import Data.Swagger hiding (SchemaOptions (fieldLabelModifier))

type StrictParam = QueryParam' '[Required, Strict]
type ApiKeyProtect = AuthProtect "api-key"
type ApiRoutes =
  ApiKeyProtect :> "stats" :> Get '[JSON] Stats
  :<|> ApiKeyProtect :> "autocomplete" 
    :> StrictParam "q" Text 
    :> QueryParam  "limit" Int
    :> Get '[JSON] [CityAutocomplete]
  :<|> ApiKeyProtect :> "search"
    :> StrictParam "name" Text
    :> QueryParam "limit" Int
    :> Get '[JSON] [City]
  :<|> ApiKeyProtect :> "locationSearch"
    :> StrictParam "lat" Latitude
    :> StrictParam "lng" Longitude
    :> QueryParam "limit" Int
    :> Get '[JSON] [City]

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type Service = SwaggerAPI :<|> ApiRoutes
type AppM sig m =
  ( Has (Log LogMessage) sig m,
    Has (Throw ServerError) sig m,
    Has Database sig m,
    Has Time sig m,
    Has Cache sig m
  )

proxyService :: Proxy Service
proxyService = Proxy

proxyApi :: Proxy ApiRoutes
proxyApi = Proxy

-- | Auth type for api keys
-- from: https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication
type instance AuthServerData (AuthProtect "api-key") = RequestKey

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

data Stats = Stats
  { lastUpdated :: Maybe Day,
    cityCount :: Int
  } deriving (Eq, Show, Generic, Typeable)

instance ToJSON Stats
instance ToSchema Stats

-- API representation of an autocomplete result.
data CityAutocomplete = CityAutocomplete
  { cityName :: Text,
    cityLongitude :: Double,
    cityLatitude :: Double,
    cityCountry :: Maybe Text,
    cityCountryCode :: Maybe Text,
    cityRegion :: Maybe Text,
    cityDistrict :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON CityAutocomplete where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = dropPrefix "city"}

instance ToSchema CityAutocomplete

data City = City
  { geonamesId :: Int,
    name :: Text,
    longitude :: Double,
    latitude :: Double,
    country :: Maybe Text,
    countryCode :: Maybe Text,
    region :: Maybe Text,
    district :: Maybe Text,
    timezone :: Text,
    elevation :: Maybe Int,
    population :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON City where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelToSnake}

instance ToSchema City
