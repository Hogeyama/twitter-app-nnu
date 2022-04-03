{-# LANGUAGE UndecidableInstances #-}
module NNU.Handler.Twitter
  ( Handler(..)
  , Has(..)
  , ApiError(..)
  , call
  , call'
  , Tweet(..)
  , tweet
  , User(..)
  , module X
  ) where

import           Data.Aeson                     ( FromJSON )
import qualified Data.Aeson                    as J
import           RIO.Partial                    ( read )
import qualified RIO.Text                      as T
import           RIO.Time
import qualified Web.Twitter.Conduit           as Twitter
import           Web.Twitter.Conduit           as X
                                         hiding ( call
                                                , call'
                                                )
import           Web.Twitter.Conduit.Parameters
                                               as X
import           Web.Twitter.Conduit.Status    as X
                                                ( update )

import           NNU.Prelude             hiding ( Handler )

{-# ANN module ("HLint: ignore Use <$>" :: String) #-}

newtype Handler m = Handler
  { callApi :: forall apiName r _r
             . (FromJSON r, Typeable r)
            => Twitter.APIRequest apiName _r
            -> m r
  }
class Has env where
  twitterApiL :: Lens' env (Handler (RIO env))

newtype ApiError = ApiError J.Value
  deriving stock (Show)
instance Exception ApiError

call'
  :: forall r env apiName _r
   . (Has env, FromJSON r, Typeable r)
  => APIRequest apiName _r
  -> RIO env r
call' req = do
  Handler { callApi } <- view twitterApiL
  callApi req
call
  :: forall env r apiName
   . (Has env, FromJSON r, Typeable r)
  => APIRequest apiName r
  -> RIO env r
call = call'

data Tweet = Tweet
  { tweetId   :: Natural
  , createdAt :: ZonedTime
  }
  deriving stock (Show, Generic)

instance FromJSON Tweet where
  parseJSON = J.withObject "Tweet" $ \o -> do
    pure Tweet
      <*> (read <$> o J..: "id_str")
      <*> (pure . utcToZonedTime jst . fromTwitterTime =<< o J..: "created_at")
   where
    jst = TimeZone { timeZoneMinutes    = 9 * 60
                   , timeZoneSummerOnly = False
                   , timeZoneName       = "JST"
                   }

tweet :: (Has env) => T.Text -> RIO env Tweet
tweet = call' . statusesUpdate

data User = User
  { userId   :: Natural
  , userName :: Text
  }
  deriving stock (Show, Eq, Generic)
instance FromJSON User where
  parseJSON = J.withObject "User" $ \o -> do
    pure User <*> (read <$> o J..: "id_str") <*> o J..: "name"

newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }
twitterTimeFormat :: String
twitterTimeFormat = "%a %b %d %T %z %Y"
instance FromJSON TwitterTime where
  parseJSON = J.withText "TwitterTime" $ \t ->
    case parseTimeM True defaultTimeLocale twitterTimeFormat (T.unpack t) of
      Just d  -> pure $ TwitterTime d
      Nothing -> fail $ "Could not parse twitter time: " ++ T.unpack t
