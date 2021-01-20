{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Auth where

import qualified Data.List as L
import Import
import Network.Wai (Request (queryString, requestHeaders))
import Servant
  ((:>), AuthProtect,  Context (..),
    ServerError (errBody),
    err401,
    throwError,
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    mkAuthHandler,
  )
import qualified Control.Lens as L
import Data.Swagger
  ( ApiKeyLocation (..),
    ApiKeyParams (..),
    SecurityRequirement (..),
    SecurityScheme (..),
    SecuritySchemeType (..),
    allOperations,
    security,
    securityDefinitions,
  )
import Servant.Swagger

newtype ApiKey = ApiKey Text
  deriving (Eq, Show)

type ApiKeyAuth = AuthHandler Request ApiKey

mkApiKey :: ByteString -> ApiKey
mkApiKey = ApiKey . decodeUtf8

authHandler :: ApiKeyAuth
authHandler =
  mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    extractApiKeyHeader req =
      req
        & requestHeaders
        & L.lookup "x-geocode-city-api-key"
    extractApiKeyParam req =
      req
        & queryString
        & L.lookup "api-key"
        & fromMaybe Nothing
    handler req = either throw401 pure $ do
      extractApiKeyHeader req <|> extractApiKeyParam req
      <&> mkApiKey
      & maybeToEither "Missing API key header (X-Geocode-City-Api-Key) or query param (api-key)"

authContext :: Context (ApiKeyAuth ': '[])
authContext = authHandler :. EmptyContext

---
--- Swagger instances
---

-- from:
-- https://hackage.haskell.org/package/servant-auth-swagger-0.2.10.1/docs/src/Servant.Auth.Swagger.html#local-6989586621679074700
-- and:
-- https://stackoverflow.com/questions/59620798/how-are-generalized-authentication-combinators-documented-with-swagger2-and-serv

instance HasSwagger api => HasSwagger (AuthProtect "api-key" :> api) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      L.& securityDefinitions L.<>~ mkSec (fromList secs)
      L.& allOperations . security L.<>~ secReqs
    where
      secs = [("ApiKeyAuth", securityScheme)]
      secReqs = [SecurityRequirement (fromList [(s, [])]) | (s, _) <- secs]
      mkSec = id
      securityScheme = SecurityScheme type_ (Just desc)
      type_ = SecuritySchemeApiKey (ApiKeyParams "api-key" ApiKeyQuery)
      desc = "JSON Web Token-based API key (can also be provided in the X-Geocode-City-Api-Key header)"
