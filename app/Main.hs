module Main where

import Import
import Config
import Database.Migrations
import Options.Applicative
import Server.Run
import System.Envy (decodeWithDefaults)

data Opts = Opts {optMigrate :: !Bool}

-- | Run server (or send the `migrate` flag to run migrations and exit)
main :: IO ()
main = do
  appConfig <- decodeWithDefaults defaultConfig
  opts <- execParser optsParser
  if optMigrate opts
    then do
      didMigrate <- runMigrations "migrations" (appDatabaseUrl appConfig)
      case didMigrate of
        Left e -> putStrLn $ "Error migrating: " <> e
        Right s -> putStrLn s
    else start appConfig
  where
    -- see: https://www.fpcomplete.com/haskell/library/optparse-applicative/
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> mainOptions)
        (fullDesc <> progDesc "Run server, or migrate")
    mainOptions :: Parser Opts
    mainOptions =
      Opts <$> switch (long "migrate" <> help "Init migrations table idempotently, run missing migrations")
