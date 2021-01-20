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
      let header = maybeToEither "Missing API Key header (X-Geocode-City-Api-Key)" $ extractApiKeyHeader req
          param = maybeToEither "Missing API Key param (api-key)" $ extractApiKeyParam req
      case param of
        Left _ ->
          case header of
            Left e -> Left e
            Right h -> Right $ mkApiKey h
        Right p -> Right $ mkApiKey p

authContext :: Context (ApiKeyAuth ': '[])
authContext = authHandler :. EmptyContext
