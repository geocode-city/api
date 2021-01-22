{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Run where

import Config (AppConfig (..), renderLogMessage)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import qualified Data.Pool as P
import qualified Database.Pool as DB
import qualified Database.PostgreSQL.Simple as PG
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

-- | Build a wai app with a connection pool
application :: R.Connection -> P.Pool PG.Connection -> Application
application cacheConn pool =
  serveWithContext proxyService authContext $
    hoistServerWithContext
      proxyService
      (Proxy :: Proxy '[ApiKeyAuth])
      transform
      service
  where
    transform handler = do
      res <- P.withResource pool runEffects
      either Servant.throwError pure (handleError res)
      where
        runEffects conn =
          handler
            & runTimeIO
            & runCacheWithConnection cacheConn
            & runDatabaseWithConnection conn
            & reinterpretLog renderLogMessage
            & runLogStdout
            & runError @ServerError
            & runError @CacheError
            & runM

-- TODO: we can probably have a runCacheSafe interpreter somewhere
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
start :: AppConfig -> IO ()
start cfg = do
  pool <- DB.initPool (appDatabaseUrl cfg)
  -- TODO: get REDIS_URL from config, parse (and fail if parsing fails)
  redis <- R.checkedConnect R.defaultConnectInfo 
  Warp.run (appPort cfg) (application redis pool)
