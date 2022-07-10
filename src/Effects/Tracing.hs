{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

module Effects.Tracing (
  Tracing(..),
  inSpan',
  module OpenTelemetry.Trace
) where

import Control.Algebra
import Control.Carrier.Error.Either (ErrorC, Throw, runError)
import Control.Carrier.Reader
import Control.Monad.IO.Unlift

import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context

import Import hiding (Reader)
import OpenTelemetry.Trace
    ( TracerProviderOptions(..),
      TracerOptions(..),
      HasTracer(..),
      TracerProvider,
      SpanStatus(..),
      SpanKind(..),
      SpanContext(..),
      NewLink(..),
      NewEvent(..),
      Link,
      InstrumentationLibrary(..),
      ImmutableSpan(..),
      Event,
      ToPrimitiveAttribute(..),
      ToAttribute(..),
      PrimitiveAttribute(..),
      Attribute(..),
      initializeTracerProvider,
      initializeGlobalTracerProvider,
      getTracerProviderInitializationOptions',
      getTracerProviderInitializationOptions,
      detectBuiltInResources,
      updateName,
      tracerOptions,
      spanGetAttributes,
      shutdownTracerProvider,
      setStatus,
      setGlobalTracerProvider,
      recordException,
      makeTracer,
      inSpan'',
      getGlobalTracerProvider,
      endSpan,
      emptyTracerProviderOptions,
      defaultSpanArguments,
      createTracerProvider,
      createSpanWithoutCallStack,
      addEvent,
      addAttributes,
      addAttribute,
      Tracer(..),
      Span,
      SpanArguments(..) )
import OpenTelemetry.Trace qualified as Trace

data Tracing (m :: Type -> Type) k where
  InSpan' :: forall m a. Text -> SpanArguments -> (Span -> m a) -> Tracing m a

inSpan' :: forall sig m a. (Has Tracing sig m) => Text -> SpanArguments -> (Span -> m a) -> m a
inSpan' name args f = send $ InSpan' name args f

--- CARRIERS

newtype TracingIOC m a = TracingIOC {runTracingIO :: ReaderC Tracer m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadFail)

-- -- Orphan, there's an actual instance in the latest version of fused-effects:
-- -- https://github.com/fused-effects/fused-effects/pull/420/files#diff-568405ead22bae1fe93c05b4c50397c0dc552943b8694ee30f53528df619a0ac
-- instance MonadUnliftIO m => MonadUnliftIO (ReaderC r m) where
--   withRunInIO inner = ReaderC $ \r -> withRunInIO $ \run' -> inner (run' . runReader r)

-- instance MonadUnliftIO m => MonadUnliftIO (TracingIOC m) where
--   withRunInIO = wrappedWithRunInIO TracingIOC runTracingIO


runTracingWithTracer :: Tracer -> TracingIOC m hs -> m hs
runTracingWithTracer tracer = runReader tracer . runTracingIO

instance
  forall m sig. (MonadIO m, Algebra sig m) =>
  Algebra (Tracing :+: sig) (TracingIOC m)
  where
    alg hdl sig ctx = TracingIOC $ case sig of
      L (InSpan' name args f) -> do
        tracer <- ask
        span <- liftIO $ do
          ctx <- Context.getContext
          s <- createSpanWithoutCallStack tracer ctx name args
          --adjustContext (insertSpan s)
          pure s
        -- this is where I get stuck. No idea how to tell haskell that
        -- this is supposed to be the same monad??
        res <- lift $ f span
        endSpan span Nothing
        (<$ ctx) <$> res
