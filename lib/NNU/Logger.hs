{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module NNU.Logger
  ( LogFunc'
  , LogItem
  , HasLogFunc'
  , defaultLogFunc'
  , mkLogFunc'
  , logE
  , logW
  , logI
  , logD
  ) where

import qualified Data.Aeson                    as J
import qualified NNU.TH                        as TH
import           Network.HTTP.Simple           as HTTP
import           RIO
import qualified RIO.Text                      as T

type LogFunc' = GLogFunc LogItem
type HasLogFunc' env = (HasGLogFunc env, GMsg env ~ LogItem)
data LogItem = LogItem LogLevel J.Value

defaultLogFunc' :: LogFunc'
defaultLogFunc' = mkLogFunc' $ \level msg -> do
  let msg' = decodeUtf8Lenient $ toStrictBytes $ J.encode msg
  when (level >= LevelInfo) $ notifySlack msg'
  hPutBuilder stderr $ encodeUtf8Builder msg' <> "\n"
 where
  notifySlack msg = do
    let
      url
        = "https://hooks.slack.com/services/***REMOVED***"
      body = J.object ["text" J..= msg]
    req <-
      HTTP.parseRequest url
      <&> HTTP.setRequestMethod "POST"
      <&> setRequestBodyJSON body
    void $ httpNoBody req

mkLogFunc' :: (LogLevel -> J.Value -> IO ()) -> LogFunc'
mkLogFunc' p = mkGLogFunc $ \stack (LogItem level msg) -> do
  let level' = case level of
        LevelDebug     -> "debug"
        LevelInfo      -> "info"
        LevelWarn      -> "warning"
        LevelError     -> "error"
        LevelOther txt -> txt
  p level $ J.object
    [ "location" J..= replaceColon (utf8BuilderToText (displayCallStack stack))
    , "log_level" J..= level'
    , "body" J..= msg
    , "revision" J..= ($(TH.revision) :: Text)
    ]
  where replaceColon = T.map (\c -> if c == ':' then '-' else c)
    -- Slackだと:5:がemojiになってしまうため

log'
  :: forall l env
   . (HasCallStack, HasLogFunc' env, J.ToJSON l)
  => LogLevel
  -> l
  -> RIO env ()
log' level = glog . LogItem level . J.toJSON

logE
  :: forall l env
   . (HasCallStack, HasLogFunc' env, J.ToJSON l)
  => l
  -> RIO env ()
logE = log' LevelError

logW
  :: forall l env
   . (HasCallStack, HasLogFunc' env, J.ToJSON l)
  => l
  -> RIO env ()
logW = log' LevelWarn

logI
  :: forall l env
   . (HasCallStack, HasLogFunc' env, J.ToJSON l)
  => l
  -> RIO env ()
logI = log' LevelInfo

logD
  :: forall l env
   . (HasCallStack, HasLogFunc' env, J.ToJSON l)
  => l
  -> RIO env ()
logD = log' LevelDebug

