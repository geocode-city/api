{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Run where

import Config (AnonAccess (..), AppConfig (..), AppContext (..), Environment (Production), RedisUrl (..), renderLogMessage)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import Control.Carrier.Reader (runReader)
import qualified Data.Pool as P
import qualified Database.Pool as DB
import Effects
  (CacheError, runCacheWithConnection,  LogStdoutC (runLogStdout),
    reinterpretLog,
    runDatabaseWithConnection,
    runTimeIO
  )
import Import
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, HasServer (hoistServerWithContext), ServerError (errBody), err500, serveWithContext, throwError)
import Server.Handlers (service)
import Server.Types (proxyService)
import Server.Auth (ApiKeyAuth, authContext)
import qualified Database.Redis as R
import Network.Wai.Middleware.Cors
import OpenTelemetry.Trace (initializeGlobalTracerProvider, makeTracer, tracerOptions, Tracer)
import Effects.Tracing (shutdownTracerProvider)
import Control.Exception (bracket)

-- | Build a wai app with a connection pool
application :: AppContext -> Application
application appCtx =
  serveWithContext proxyService (authContext (ctxAnonAccess appCtx)) $
    hoistServerWithContext
      proxyService
      (Proxy :: Proxy '[ApiKeyAuth])
      transform
      service
  where
    transform handler = do
      response <- P.withResource (ctxDatabasePool appCtx) runEffects
      either Servant.throwError pure (handleError response)
      where
        runEffects conn =
          handler
            & runTimeIO
            & runCacheWithConnection (ctxRedisConnection appCtx)
            & runDatabaseWithConnection conn
            & reinterpretLog renderLogMessage
            & runLogStdout
            & runError @ServerError
            & runError @CacheError
            & runReader (ctxTracer appCtx)
            & runM

-- FIXME: we can probably have a runCacheSafe interpreter somewhere
-- that allows us to handle this deeper in, in the handlers;
-- though maybe it's preferrable to handle it here?
-- At the moment, it is, since we don't really care about specific
-- cache errors in each call site.
handleError :: Either CacheError (Either ServerError a) -> Either ServerError a
handleError err =
  case err of
    Left _cacheError -> Servant.throwError err500 {errBody = "Cache error"}
    Right s -> s

-- | Given config, start the app
start' :: Tracer -> AppConfig -> IO ()
start' tracer cfg = do
  pool <- DB.initPool (appDatabaseUrl cfg)
  redis <- case R.parseConnectInfo (appRedisUrl cfg & un) of
    Left e -> fail e
    Right connectInfo -> R.connect connectInfo
  let env = AppContext {
    ctxRedisConnection = redis
  , ctxDatabasePool = pool
  , ctxAnonAccess = if Production == appDeployEnv cfg then AlwaysDenyAnon else AlwaysAllowAnon
  , ctxTracer = tracer
  } 
  Warp.run (appPort cfg) (corsMiddleware $ application env)
  where
    corsMiddleware = cors $ const $ Just corsPolicy
    corsPolicy = 
      simpleCorsResourcePolicy {
        corsExposedHeaders = Just $ simpleResponseHeaders <> rateLimitingHeaders
      }
    rateLimitingHeaders = ["X-RateLimit-Limit", "X-RateLimit-Remaining", "X-RateLimit-Resets"]

start :: AppConfig -> IO ()
start cfg = withTracer $ \tracer -> do
 start' tracer cfg
 where
   withTracer f = bracket
     initializeGlobalTracerProvider
     shutdownTracerProvider
     (\tracerProvider -> do
         let tracer = makeTracer tracerProvider "geocode-city" tracerOptions
         f tracer
     )
