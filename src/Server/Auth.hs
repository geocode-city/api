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
import qualified Data.ByteString.Char8 as B8

newtype ApiKey = ApiKey Text
  deriving (Eq, Show)

newtype RequestID = RequestID ByteString
  deriving (Eq, Show)

newtype IPAddress = IPAddress ByteString
  deriving (Eq, Show)

-- | How a request is identified for rate-limiting purposes.
data RequestKey
  = ByIP IPAddress RequestID
  | ByApiKey ApiKey RequestID
  deriving (Eq, Show)

-- | Authentication (identification) by either Api Key or IP
type ApiKeyAuth = AuthHandler Request RequestKey

-- | Given a handler, try to extract an identifier: either an API Key or
-- IP Address. A little bit of Maybe blindness here: we don't really
-- care to publicize that "anonymous" requests can be made, since the only
-- way an IP address can't be found is outside of a production env.
authHandler :: ApiKeyAuth
authHandler =
  mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = either throw401 pure $ do
      authWithApiKey req <|> authWithIP req
      & maybeToEither "Missing API key header (X-Geocode-City-Api-Key) or query param (api-key)"

authContext :: Context (ApiKeyAuth ': '[])
authContext = authHandler :. EmptyContext


---
--- HELPERS
---
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right

-- | Make an ApiKey
mkApiKey :: ByteString -> ApiKey
mkApiKey = ApiKey . decodeUtf8

-- TODO: maybe generate a random UUID? This is only really
-- useful for dev/test, when it's annoying to set `x-request-id`
-- by hand
mkRequestId :: Maybe ByteString -> Maybe RequestID
mkRequestId Nothing = Just . RequestID $ "fake-id"
mkRequestId rid = RequestID <$> rid 

getLastIP :: ByteString -> Maybe IPAddress
getLastIP bs =
  B8.split ',' bs
    & lastMaybe
    <&> IPAddress

-- | Find API Key in @X-Geocode-City-Api-Key@ header
extractApiKeyHeader :: Request -> Maybe ByteString
extractApiKeyHeader req =
  req
    & requestHeaders
    & L.lookup "x-geocode-city-api-key"

-- | Find API Key in @api-key@ querystring parameter
extractApiKeyParam :: Request -> Maybe ByteString
extractApiKeyParam req =
  req
    & queryString
    & L.lookup "api-key"
    & fromMaybe Nothing

-- Both x-request-id and x-forwarded-for are Heroku-isms;
-- they're not reliable (or set!) in other environments
-- but we only use them to identify requests (not authenticate/authorize.)
-- | Extract request ID from `x-request-id` header. Default to  
extractRequestId :: Request -> Maybe RequestID
extractRequestId req =
  req
    & requestHeaders
    & L.lookup "x-request-id"
    & mkRequestId

extractRequestIP :: Request -> Maybe IPAddress
extractRequestIP req =
  req
    & requestHeaders
    & L.lookup "x-forwarded-for"
    <&> getLastIP
    & fromMaybe Nothing
authWithApiKey :: Request -> Maybe RequestKey
authWithApiKey req = do
  apiKey <- mkApiKey <$> (extractApiKeyHeader req <|> extractApiKeyParam req)
  requestID <- extractRequestId req
  pure $ ByApiKey apiKey requestID

authWithIP :: Request -> Maybe RequestKey
authWithIP req = do
  ip <- extractRequestIP req
  requestID <- extractRequestId req
  pure $ ByIP ip requestID

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
