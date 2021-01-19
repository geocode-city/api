module Import
  ( module Relude
  , module Relude.Extra.Newtype
  , headMaybe
  , lastMaybe
  ) where

-- we hide `Reader` and other mtl-isms in favor of `fused-effects` implementations
import Relude hiding (ask, runReader)
import Relude.Extra.Newtype

headMaybe :: [b] -> Maybe b
headMaybe = viaNonEmpty head

lastMaybe :: [b] -> Maybe b
lastMaybe = viaNonEmpty last
