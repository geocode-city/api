{-# LANGUAGE RecordWildCards #-}
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
service = return swaggerSpec :<|> stats :<|> autoComplete

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

autoComplete :: (AppM sig m) => ApiKey -> Text -> Maybe Int -> m [CityAutocomplete]
autoComplete apiKey q limit =
  validateApiKey apiKey >> do
    results <- Q.cityAutoComplete q limit
    return $ map serializeAutocompleteResult results
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

---
--- HELPERS
---

serializeAutocompleteResult :: Q.CityAutocompleteQ -> CityAutocomplete
serializeAutocompleteResult Q.CityAutocompleteQ {..} =
  CityAutocomplete
    { cityName = caCityName,
      cityLongitude = caLongitude,
      cityLatitude = caLatitude,
      cityCountry = caCountryName,
      cityCountryCode = caCountryCode,
      cityRegion = caRegionName,
      cityDistrict = caDistrictName
    }
