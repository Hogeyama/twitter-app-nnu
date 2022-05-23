{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module NNU.Logger (
  Has,
  error,
  warn,
  info,
  debug,
  LogFunc',
  LogItem,
  defaultLogFunc',
  mkLogFunc',
) where

import qualified Data.Aeson as A
import Network.HTTP.Simple as HTTP
import qualified RIO.Text as T

import NNU.Prelude hiding (error)
import qualified NNU.TH as TH

type LogFunc' = GLogFunc LogItem
data LogItem = LogItem LogLevel A.Value

class (HasGLogFunc env, GMsg env ~ LogItem) => Has env
instance (HasGLogFunc env, GMsg env ~ LogItem) => Has env

defaultLogFunc' :: LogFunc'
defaultLogFunc' = mkLogFunc' $ \level msg -> do
  let msg' = decodeUtf8Lenient $ toStrictBytes $ A.encode msg
  when (level `elem` [LevelInfo, LevelError]) $ notifySlack msg'
  hPutBuilder stderr $ encodeUtf8Builder msg' <> "\n"
  where
    notifySlack msg = do
      let url =
            "https://hooks.slack.com/services/***REMOVED***"
          body = A.object ["text" A..= msg]
      req <-
        HTTP.parseRequest url
          <&> HTTP.setRequestMethod "POST"
          <&> setRequestBodyJSON body
      void $ httpNoBody req

mkLogFunc' :: (LogLevel -> A.Value -> IO ()) -> LogFunc'
mkLogFunc' p = mkGLogFunc $ \stack (LogItem level msg) -> do
  let level' = case level of
        LevelDebug -> "debug"
        LevelInfo -> "info"
        LevelWarn -> "warning"
        LevelError -> "error"
        LevelOther txt -> txt
  p level $
    A.object
      [ "location" A..= replaceColon (utf8BuilderToText (displayCallStack stack))
      , "log_level" A..= level'
      , "body" A..= msg
      , "revision" A..= ($(TH.revision) :: Text)
      ]
  where
    replaceColon = T.map (\c -> if c == ':' then '-' else c)

-- Slackだと:5:がemojiになってしまうため

log' ::
  forall l env m.
  (HasCallStack, Has env, MonadReader env m, MonadIO m, A.ToJSON l) =>
  LogLevel ->
  l ->
  m ()
log' level = glog . LogItem level . A.toJSON

error ::
  forall l env m.
  (HasCallStack, Has env, MonadReader env m, MonadIO m, A.ToJSON l) =>
  l ->
  m ()
error = log' LevelError

warn ::
  forall l env m.
  (HasCallStack, Has env, MonadReader env m, MonadIO m, A.ToJSON l) =>
  l ->
  m ()
warn = log' LevelWarn

info ::
  forall l env m.
  (HasCallStack, Has env, MonadReader env m, MonadIO m, A.ToJSON l) =>
  l ->
  m ()
info = log' LevelInfo

debug ::
  forall l env m.
  (HasCallStack, Has env, MonadReader env m, MonadIO m, A.ToJSON l) =>
  l ->
  m ()
debug = log' LevelDebug
