{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Log (
  Log (..),
  error,
  warn,
  info,
  debug,
) where

import Polysemy

import qualified Data.Aeson as A

import NNU.Prelude hiding (error, log)

data Log m a where
  Log :: CallStack -> LogLevel -> A.Value -> Log m ()

makeSem ''Log

error :: (HasCallStack, Member Log r, A.ToJSON msg) => msg -> Sem r ()
error = log ?callStack LevelError . A.toJSON

warn :: (HasCallStack, Member Log r, A.ToJSON msg) => msg -> Sem r ()
warn = log ?callStack LevelWarn . A.toJSON

info :: (HasCallStack, Member Log r, A.ToJSON msg) => msg -> Sem r ()
info = log ?callStack LevelInfo . A.toJSON

debug :: (HasCallStack, Member Log r, A.ToJSON msg) => msg -> Sem r ()
debug = log ?callStack LevelDebug . A.toJSON
