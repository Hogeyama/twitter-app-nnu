module NNU.Util
  ( utcToJST
  , jstToUTC
  , getCurrentTimeInJST
  , loopWithDelaySec
  , notifyHogeyamaSlack
  ) where

import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Network.HTTP.Simple           as HTTP
import           RIO
import           RIO.Time

notifyHogeyamaSlack :: MonadIO m => Text -> m ()
notifyHogeyamaSlack msg = liftIO $ do
  let
    url
      = "https://hooks.slack.com/services/***REMOVED***"
    body = object ["text" .= msg]
  req <-
    HTTP.parseRequest url
    <&> HTTP.setRequestMethod "POST"
    <&> setRequestBodyJSON body
  void $ httpNoBody req

utcToJST :: UTCTime -> LocalTime
utcToJST = utcToLocalTime (hoursToTimeZone 9)

jstToUTC :: LocalTime -> UTCTime
jstToUTC = localTimeToUTC (hoursToTimeZone 9)

getCurrentTimeInJST :: MonadIO m => m LocalTime
getCurrentTimeInJST = utcToLocalTime (hoursToTimeZone 9) <$> getCurrentTime

loopWithDelaySec :: MonadIO m => Int -> m () -> m ()
loopWithDelaySec n m = forever $ m >> threadDelay (n * 1000 * 1000)

