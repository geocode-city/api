module Import
  ( module Relude
  , module Relude.Extra.Newtype
  , module Control.Algebra
  , headMaybe
  , lastMaybe
  , camelToSnake
  , dropPrefix
  ) where

-- we hide `Reader` and other mtl-isms in favor of `fused-effects` implementations
import Relude hiding (ask, runReader)
import Relude.Extra.Newtype

import Control.Algebra
import Data.Aeson.Types (camelTo2)

-- | total version of `head`
headMaybe :: [b] -> Maybe b
headMaybe = viaNonEmpty head

-- | total version of `last`
lastMaybe :: [b] -> Maybe b
lastMaybe = viaNonEmpty last

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

dropPrefix :: String -> String -> String
dropPrefix p = drop (length $ p <> "_") . camelToSnake
