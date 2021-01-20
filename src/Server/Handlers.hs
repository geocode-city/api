module Server.Handlers where

import Import
import Servant hiding (throwError)
import Server.Types
import Database.Queries as Q
import Control.Carrier.Error.Either (throwError)
import Server.Auth
import Control.Lens ((.~), (?~))
import Data.Swagger
import Servant.Swagger

service :: AppM sig m => ServerT Service m
service = return swaggerSpec :<|> stats

stats :: (AppM sig m) => ApiKey -> m Stats
stats apiKey = validateApiKey apiKey >> do
  count <- Q.cityCount
  update <- Q.latestUpdate
  return $ Stats update count

validateApiKey :: (AppM sig m) => ApiKey -> m ()
validateApiKey (ApiKey apiKey) = do
  isValidKey <- Q.isKeyEnabled apiKey
  if isValidKey
    then pure ()
    else throwError err403 {errBody = "Invalid API Key."}

---
--- Swagger
---

swaggerSpec :: Swagger
swaggerSpec =
  toSwagger proxyApi
    & info . title .~ "Geocode.city API"
    & info . version .~ "1.0"
    & info . description ?~ "City-only geocoding"

-- | Output generated @swagger.json@ file for the @'TodoAPI'@.
-- writeSwaggerJSON :: IO ()
-- writeSwaggerJSON = BL8.writeFile "example/swagger.json" (encodePretty todoSwagger)
