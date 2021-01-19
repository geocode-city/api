{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types where

import Config (LogMessage)
import Control.Algebra (Has)
import Control.Carrier.Error.Either (Throw)
import Data.Time (UTCTime)
import Effects (Database, Log)
import Import (Int64, Text)
import Servant (Get, PlainText, ServerError, type (:>))

type Service =
  "stats" :> Get '[PlainText] Text

type AppM sig m =
  ( Has (Log LogMessage) sig m,
    Has (Throw ServerError) sig m,
    Has Database sig m
  )

---
--- RESPONSE TYPES
---

data Stats = Stats
  { lastUpdated :: UTCTime,
    cityCount :: Int64
  }
