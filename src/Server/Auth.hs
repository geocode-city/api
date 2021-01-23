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
import Data.Char (isSpace)
import Config (AnonAccess (..))

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
  | WithUnlimitedAccess
  deriving (Eq, Show)

-- | Authentication (identification) by either Api Key or IP
type ApiKeyAuth = AuthHandler Request RequestKey

-- | Given a handler, try to extract an identifier: either an API Key or
-- IP Address. A little bit of Maybe blindness here: we don't really
-- care to publicize that "anonymous" requests can be made, since the only
-- way an IP address can't be found is outside of a production env.
authHandler :: AnonAccess -> ApiKeyAuth
authHandler anonCriterion =
  mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = either throw401 pure $ do
      authWithApiKey req <|> authWithIP req <|> authAnon anonCriterion
      & maybeToEither "Missing API key header (X-Geocode-City-Api-Key) or query param (api-key)"

authContext :: AnonAccess -> Context (ApiKeyAuth ': '[])
authContext aa = authHandler aa :. EmptyContext


---
--- HELPERS
---
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right

-- | Make an ApiKey
mkApiKey :: ByteString -> ApiKey
mkApiKey = ApiKey . decodeUtf8

-- | Given a bytestring containing potentially many ip addresses
-- separated by commas, get the last (or only) one.
--
-- HEROKU SANS PROXY SPECIFIC:
-- Note that getting the last IP, in the case of multiple addresses
-- being present (due to spoofing or proxies) isn't _fully_ reliable:
-- Heroku appends the IP _it_ saw as connecting to their origin to
-- the `x-forwarded-for` header, which means the last one is likely
-- the real one even in the presence of spoofing; but if a proxy sits
-- in front of Heroku (e.g. Fastly,) then that one will end up in the
-- last position, as is canonical for the header:
-- https://stackoverflow.com/a/37061471
-- https://en.wikipedia.org/wiki/X-Forwarded-For
-- in our case though, when most traffic will fall within the only-one-IP
-- or many-IPs-but-likely-spoofing scenario, this is good enough.
mkIpAddress :: ByteString -> Maybe IPAddress
mkIpAddress bs = do
  let addresses = B8.split ',' bs
  case addresses of
    [ip] -> pure . IPAddress $ ip
    l@(_:_ips) -> do
      lastIP <- lastMaybe l
      pure . IPAddress $ B8.dropWhile isSpace lastIP
    _    -> Nothing

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


-- | Extract request ID from `x-request-id` header:
-- https://devcenter.heroku.com/articles/http-request-id
extractRequestId :: Request -> Maybe RequestID
extractRequestId req =
  req
    & requestHeaders
    & L.lookup "x-request-id"
    <&> RequestID 

-- | The request IP is the "real" IP as populated by Heroku:
-- https://devcenter.heroku.com/articles/http-routing#heroku-headers
-- there's also [remoteHost](https://hackage.haskell.org/package/wai-3.2.3/docs/Network-Wai.html#v:remoteHost)
-- but that's unusable in production.
extractRequestIP :: Request -> Maybe IPAddress
extractRequestIP req =
  req
    & requestHeaders
    & L.lookup "x-forwarded-for"
    <&> mkIpAddress
    & fromMaybe Nothing

-- | Attempt to extract the api key from the request: either from our custom header, or the querystring.
authWithApiKey :: Request -> Maybe RequestKey
authWithApiKey req = do
  apiKey <- mkApiKey <$> (extractApiKeyHeader req <|> extractApiKeyParam req)
  requestID <- extractRequestId req
  pure $ ByApiKey apiKey requestID

-- | Identify a request by IP. This is a very Heroku-centric approach: we rely
-- on the x-request-id and x-forwarded-for headers; we _could_ use
-- wai's `remoteHost` as a fallback, but it's unrealistic in the current
-- deployment environment: it's either populated upstream, or not.
authWithIP :: Request -> Maybe RequestKey
authWithIP req = do
  ip <- extractRequestIP req
  requestID <- extractRequestId req
  pure $ ByIP ip requestID

-- | Given an @AnonAccess@ criterion, give them "unkeyed" access,
-- or deny. Useful for dev/test, or if you want to deploy
-- without api key/rate limiting.
authAnon :: AnonAccess -> Maybe RequestKey
authAnon AlwaysAllowAnon = Just WithUnlimitedAccess
authAnon AlwaysDenyAnon  = Nothing 

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
      desc = "JSON Web Token-based API key (can also be provided in the X-Geocode-City-Api-Key header.) If omitted, the client IP will be used for rate limiting."
