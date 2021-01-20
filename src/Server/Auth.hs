{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Auth where

import qualified Data.List as L
import Import
import Network.Wai (Request (queryString, requestHeaders))
import Servant
  ( Context (..),
    ServerError (errBody),
    err401,
    throwError,
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    mkAuthHandler,
  )

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
