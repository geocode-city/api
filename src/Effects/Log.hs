{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Log where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Import
  ( Applicative,
    Functor ((<$)),
    Monad,
    MonadFail,
    MonadIO (..),
    Text,
    ToString (toString),
    Type,
    putStrLn,
    ($),
    (.),
  )

data Log (a :: Type) (m :: Type -> Type) (k :: Type) where
  Log :: a -> Log a m ()

log :: Has (Log a) sig m => a -> m ()
log x = send (Log x)

-- Carrier one: log strings to stdout.
newtype LogStdoutC m a = LogStdoutC {runLogStdout :: m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

instance-- So long as the 'm' monad can interpret the 'sig' effects (and also
-- perform IO)...

  ( Algebra sig m,
    MonadIO m
  ) =>
  -- ... the 'LogStdoutC m' monad can interpret 'Log String :+: sig' effects
  Algebra (Log Text :+: sig) (LogStdoutC m)
  where
  alg hdl sig ctx = case sig of
    L (Log message) -> ctx <$ liftIO (putStrLn (toString message))
    R other -> LogStdoutC (alg (runLogStdout . hdl) other ctx)

-- Carrier two: reinterpret a program that logs 's's into one that logs 't's
-- using a function (provided at runtime) from 's' to 't'.
newtype ReinterpretLogC s t m a = ReinterpretLogC {runReinterpretLogC :: ReaderC (s -> t) m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

instance-- So long as the 'm' monad can interpret the 'sig' effects, one of which
-- is 'Log t'...

  Has (Log t) sig m =>
  -- ... the 'ReinterpretLogC s t m' monad can interpret 'Log s :+: sig'
  -- effects
  Algebra (Log s :+: sig) (ReinterpretLogC s t m)
  where
  alg hdl sig ctx = ReinterpretLogC $ case sig of
    L (Log s) -> do
      f <- ask @(s -> t)
      ctx <$ log (f s)
    R other -> alg (runReinterpretLogC . hdl) (R other) ctx

reinterpretLog :: (s -> t) -> ReinterpretLogC s t m a -> m a
reinterpretLog f = runReader f . runReinterpretLogC
-- ^ from:
-- https://github.com/fused-effects/fused-effects/blob/79710ac4377f895db1f59ec65736422328acda9a/examples/ReinterpretLog.hs
