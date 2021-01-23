{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Config where

import Import
import System.Envy
  ( FromEnv (..),
    Option (dropPrefixCount),
    Var (..),
    defOption,
    gFromEnvCustom,
  )
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as R

data Environment
  = Development
  | Test
  | Production
  deriving stock (Eq, Show, Enum, Read)

data AnonAccess
  = AlwaysDenyAnon
  | AlwaysAllowAnon
  deriving stock (Eq, Show)
instance Var Environment where
  toVar = show
  fromVar = readMaybe

newtype DatabaseUrl = DatabaseUrl Text
  deriving newtype (Eq, Show)
  deriving (Var) via Text

-- | Configuration as it comes from the environment; flat, static.
data AppConfig = AppConfig
  { appPort :: !Int,
    appDeployEnv :: !Environment,
    appDatabaseUrl :: !DatabaseUrl
  }
  deriving stock (Eq, Show, Generic)

instance FromEnv AppConfig where
  -- drop the `app*` prefix that e.g. Heroku will add:
  fromEnv = gFromEnvCustom defOption {dropPrefixCount = 3}

-- opaque "env" to carry/specify runtime dependencies.
data AppContext = AppContext
  { ctxRedisConnection :: !R.Connection,
    ctxDatabasePool :: P.Pool PG.Connection,
    ctxAnonAccess :: !AnonAccess
  }

-- | Default app config. Override with environment variables.
defaultConfig :: AppConfig
defaultConfig =
  AppConfig
    { appPort = 3000,
      appDeployEnv = Development,
      appDatabaseUrl = DatabaseUrl "postgresql://localhost/geocode_city_dev?user=luis"
    }

-- | Log levels
data LogMessage
  = Debug Text
  | Info Text
  | Warn Text
  | Error Text

renderLogMessage :: LogMessage -> Text
renderLogMessage = \case
  Debug m -> "[DEBUG] " <> m
  Info m -> "[INFO] " <> m
  Warn m -> "[WARN] " <> m
  Error m -> "[ERROR] " <> m
