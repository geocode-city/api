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
import Data.Aeson (ToJSON)
import Servant (AuthProtect, Get, JSON, ServerError, (:<|>), type (:>))
import Servant.Server.Experimental.Auth
import Server.Auth
import Data.Swagger

type ApiRoutes =
  AuthProtect "api-key" :> "stats" :> Get '[JSON] Stats

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
---
--- RESPONSE TYPES
---

data Stats = Stats
  { lastUpdated :: Maybe Day,
    cityCount :: Int
  } deriving (Eq, Show, Generic, Typeable)

instance ToJSON Stats

-- from: https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication
type instance AuthServerData (AuthProtect "api-key") = ApiKey

instance ToSchema Stats
