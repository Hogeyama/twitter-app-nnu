
module Util where

import           RIO
import           RIO.List
import qualified RIO.Text as T
import           RIO.Time
import           System.Process
import           Data.Char      (showLitChar, isAscii)

printAnyError :: MonadUnliftIO m => m () -> m ()
printAnyError m = tryAnyDeep m >>= \case
    Left e -> do
      hPutBuilder stderr $ getUtf8Builder $ displayShow e
      notifyHogeyamaSlack (tshow e)
    _ -> return ()

print :: (MonadIO m, Show a) => a -> m ()
print = hPutBuilder stdout . getUtf8Builder . (<>"\n") . displayShow

notifyHogeyamaSlack :: MonadIO m => Text -> m ()
notifyHogeyamaSlack msg = liftIO $ do
    callCommand $ RIO.List.intercalate " "
      [ "curl -X POST -H 'Content-type: application/json'"
      , "--data '{\"text\":" <> quote (T.unpack msg) <> "}'"
      , "https://hooks.slack.com/services/***REMOVED***"
      ]
  where
    quote s = "\"" ++ concatMap escapeChar s ++ "\""
    escapeChar c
      | c == '"'  = "\\\""
      | isAscii c = showLitChar c ""
      | otherwise = [c]

_JST :: TimeZone
_JST = hoursToTimeZone 9

utcToJST :: UTCTime -> LocalTime
utcToJST = utcToLocalTime _JST

jstToUTC :: LocalTime -> UTCTime
jstToUTC = localTimeToUTC _JST

getCurrentTimeInJST :: MonadIO m => m LocalTime
getCurrentTimeInJST = utcToLocalTime (hoursToTimeZone 9) <$> getCurrentTime
