{-# LANGUAGE UndecidableInstances #-}
module NNU.Handler.Twitter
  ( Handler(..)
  , Has(..)
  , ApiError(..)
  , call
  , call'
  , defaultHandler
  , TwConfig(..)
  , HasTwConfig(..)
  , Tweet(..)
  , tweet
  , twConfigFromEnv
  , User(..)
  , module X
  ) where

import           RIO                     hiding ( Handler )
import qualified RIO.Text                      as T
import           RIO.Time

import           Data.Aeson                     ( FromJSON )
import qualified Data.Aeson                    as J
import qualified Data.ByteString.Char8         as BC
import           Data.Generics.Product          ( the )
import           Prelude                        ( read )
import           System.Environment             ( getEnv )
import           System.IO.Unsafe               ( unsafePerformIO )
import qualified Web.Twitter.Conduit           as Twitter
import           Web.Twitter.Conduit           as X
                                         hiding ( call
                                                , call'
                                                )
import           Web.Twitter.Conduit.Parameters
                                               as X
import           Web.Twitter.Conduit.Status    as X
                                                ( update )

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

defaultHandler :: forall m . (MonadIO m) => TwConfig -> Handler m
defaultHandler TwConfig {..} = Handler { callApi }
 where
  callApi
    :: forall apiName r _responseType
     . FromJSON r
    => Twitter.APIRequest apiName _responseType
    -> m r
  callApi param = do
    v <- liftIO $ tryAnyDeep $ Twitter.call' twInfo twManager param
    case v of
      Right resp -> case J.fromJSON resp of
        (J.Success x) -> return x
        (J.Error   e) -> do
          let msg = J.object
                [ "message" J..= ("JSON decode error" :: Text)
                , "request" J..= tshow param
                , "response" J..= resp
                , "error" J..= e
                ]
          throwIO $ ApiError msg
      Left e -> do
        let msg = J.object
              [ "message" J..= ("Twitter API call error" :: Text)
              , "request" J..= show param
              , "error" J..= show e
              ]
        throwIO $ ApiError msg

data TwConfig = TwConfig
  { twManager :: Manager
  , twInfo    :: TWInfo
  }
  deriving stock Generic
class HasTwConfig env where
  twConfigL  :: Lens' env TwConfig
  twManagerL :: Lens' env Manager
  default twManagerL :: Lens' env Manager
  twManagerL = twConfigL . the @"twManager"
  twInfoL :: Lens' env TWInfo
  default twInfoL :: Lens' env TWInfo
  twInfoL = twConfigL . the @"twInfo"
instance HasTwConfig TwConfig where
  twConfigL = id

twConfigFromEnv :: MonadIO m => String -> m TwConfig
twConfigFromEnv prefix = liftIO $ do
  twInfo <- twInfoFromEnv prefix
  return TwConfig { twManager = defaultTwManager, twInfo = twInfo }

{-# NOINLINE defaultTwManager #-}
defaultTwManager :: Manager
defaultTwManager = unsafePerformIO $ newManager tlsManagerSettings

twInfoFromEnv :: String -> IO TWInfo
twInfoFromEnv prefix = do
  tokens     <- tokensFromEnv
  credential <- credentialFromEnv prefix
  return def { twToken = def { twOAuth = tokens, twCredential = credential }
             , twProxy = Nothing
             }

tokensFromEnv :: IO OAuth
tokensFromEnv = do
  key    <- getEnv "OAUTH_CONSUMER_KEY"
  secret <- getEnv "OAUTH_CONSUMER_SECRET"
  return $ twitterOAuth { oauthConsumerKey    = BC.pack key
                        , oauthConsumerSecret = BC.pack secret
                        }

credentialFromEnv :: String -> IO Credential
credentialFromEnv prefix = do
  token  <- getEnv $ prefix ++ "_OAUTH_ACCESS_TOKEN"
  secret <- getEnv $ prefix ++ "_OAUTH_ACCESS_SECRET"
  return $ Credential
    [("oauth_token", BC.pack token), ("oauth_token_secret", BC.pack secret)]

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
tweet = call' . update

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
