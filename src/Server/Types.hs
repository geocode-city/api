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
import Effects (Database, Log)
import Data.Aeson
  ( Options (fieldLabelModifier),
    ToJSON (toJSON),
    defaultOptions,
    genericToJSON,
  )
import Servant (AuthProtect, Get, JSON, QueryParam, QueryParam', Required, ServerError, Strict, (:<|>), type (:>))
import Servant.Server.Experimental.Auth
import Server.Auth
import Data.Swagger hiding (SchemaOptions (fieldLabelModifier))

type StrictParam = QueryParam' '[Required, Strict]
type ApiRoutes =
  AuthProtect "api-key" :> "stats" :> Get '[JSON] Stats
  :<|> AuthProtect "api-key" :> "autocomplete" 
    :> StrictParam "q" Text 
    :> QueryParam  "limit" Int
    :> Get '[JSON] [CityAutocomplete]

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type Service = SwaggerAPI :<|> ApiRoutes
type AppM sig m =
  ( Has (Log LogMessage) sig m,
    Has (Throw ServerError) sig m,
    Has Database sig m
  )

proxyService :: Proxy Service
proxyService = Proxy

proxyApi :: Proxy ApiRoutes
proxyApi = Proxy

-- | Auth type for api keys
-- from: https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication
type instance AuthServerData (AuthProtect "api-key") = ApiKey

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
