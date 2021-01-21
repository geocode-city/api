module Import
  ( module Relude
  , module Relude.Extra.Newtype
  , module Control.Algebra
  , headMaybe
  , lastMaybe
  , camelToSnake
  , dropPrefix
  , between
  , maybeBetween
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

between :: Ord a => (a, a) -> a -> Bool
between (begin, end) x =
  x >= begin && x <= end

maybeBetween :: Ord a => (a, a) -> a -> Maybe a
maybeBetween range x =
  if between range x then Just x else Nothing
