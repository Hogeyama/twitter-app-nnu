{-# LANGUAGE ImplicitParams #-}

module NNU.Logger
  ( LogFunc'
  , HasLogFunc'
  , defaultLogFunc'
  , mkLogFunc'
  , logE
  , logW
  , logI
  , logD
  ) where

import qualified Data.Aeson                    as J
import qualified NNU.Util                      as Util
import           RIO

type LogFunc' = GLogFunc LogItem
type HasLogFunc' env = (HasGLogFunc env, GMsg env ~ LogItem)
data LogItem = LogItem LogLevel J.Value

defaultLogFunc' :: LogFunc'
defaultLogFunc' = mkLogFunc' $ \level msg -> do
  let msg' = decodeUtf8Lenient $ toStrictBytes $ J.encode msg
  when (level >= LevelInfo) $ Util.notifyHogeyamaSlack msg'
  hPutBuilder stderr $ encodeUtf8Builder msg' <> "\n"

mkLogFunc' :: (LogLevel -> J.Value -> IO ()) -> LogFunc'
mkLogFunc' p = mkGLogFunc $ \stack (LogItem level msg) -> do
  let level' = case level of
        LevelDebug     -> "debug"
        LevelInfo      -> "info"
        LevelWarn      -> "warning"
        LevelError     -> "error"
        LevelOther txt -> txt
  p level $ J.object
    [ "location" J..= utf8BuilderToText (displayCallStack stack)
    , "log_level" J..= level'
    , "message" J..= msg
    ]


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

