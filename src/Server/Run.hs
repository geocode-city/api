{-# LANGUAGE FlexibleContexts #-}

module Server.Run where

import Config (AppConfig (..), renderLogMessage)
import Control.Carrier.Error.Either (runError)
import Control.Carrier.Lift (runM)
import qualified Data.Pool as P
import qualified Database.Pool as DB
import qualified Database.PostgreSQL.Simple as PG
import Effects
  ( LogStdoutC (runLogStdout),
    reinterpretLog,
    runDatabaseWithConnection,
  )
import Import
  ( Applicative (pure),
    IO,
    Proxy (..),
    either,
    ($),
    (&),
  )
import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, hoistServer, serve, throwError)
import Server.Handlers (service)
import Server.Types (Service)

proxyService :: Proxy Service
proxyService = Proxy

application :: P.Pool PG.Connection -> Application
application pool =
  serve proxyService $
    hoistServer
      proxyService
      transform
      service
  where
    transform handler = do
      res <- P.withResource pool runEffects
      either Servant.throwError pure res
      where
        runEffects conn =
          handler
            & runDatabaseWithConnection conn
            & reinterpretLog renderLogMessage
            & runLogStdout
            & runError
            & runM

start :: AppConfig -> IO ()
start cfg = do
  pool <- DB.initPool (appDatabaseUrl cfg)
  Warp.run (appPort cfg) (application pool)
