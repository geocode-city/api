module Server.Handlers where

import Import
import Servant
import Server.Types
import Database.Queries as Q

service :: AppM sig m => ServerT Service m
service = stats

stats :: (AppM sig m) => m Stats
stats = do
  count <- Q.cityCount
  update <- Q.latestUpdate
  return $ Stats update count
