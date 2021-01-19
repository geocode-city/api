module Server.Handlers where

import Import
import Servant
import Server.Types

service :: AppM sig m => ServerT Service m
service = stats

stats :: (AppM sig m) => m Text
stats = pure "IMOK"
