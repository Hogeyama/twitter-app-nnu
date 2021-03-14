module Web.Twitter.API
  ( TwitterApi(..)
  , HasTwitterAPI(..)
  , call
  , call'
  , defaultImpl
  , TwConfig(..)
  , HasTwConfig(..)
  , Tweet(..)
  , tweet
  , MyUser(..)
  , module X
  ,twConfigFromEnv) where

import           RIO
import qualified RIO.Text                       as T
import           RIO.Time

import qualified Util
import qualified Data.Aeson                     as J
import           Data.Generics.Labels           ()
import qualified Data.ByteString.Char8          as BC
import qualified Web.Twitter.Conduit            as Twitter
import           Web.Twitter.Conduit            as X hiding
                                                   ( call
                                                   , call'
                                                   )
import           Web.Twitter.Conduit.Parameters as X hiding
                                                   ( map )
import Data.Aeson (FromJSON)
import           System.IO.Unsafe               (unsafePerformIO)
import           Prelude                        (read)
import           System.Environment             (getEnv)

newtype TwitterApi m = TwitterApi
  { callApi :: forall apiName r _responseType
             . FromJSON r
            => Twitter.APIRequest apiName _responseType
            -> m r
  }
class HasTwitterAPI m env where
  twitterApiL :: Lens' env (TwitterApi m)

call' :: forall r m env apiName _responseType
      .( MonadReader env m
       , HasTwitterAPI m env
       , FromJSON r
       )
      => APIRequest apiName _responseType
      -> m r
call' req = do
  TwitterApi{callApi} <- view twitterApiL
  callApi req
call :: forall m env r apiName
     .( MonadReader env m
      , HasTwitterAPI m env
      , FromJSON r
      )
     => APIRequest apiName r
     -> m r
call = call'

defaultImpl :: forall m. (MonadIO m) => TwConfig -> TwitterApi m
defaultImpl TwConfig{..} = TwitterApi { callApi }
  where
    callApi :: forall apiName r _responseType
             . FromJSON r
            => Twitter.APIRequest apiName _responseType
            -> m r
    callApi param = do
      v <- liftIO $ Twitter.call' twInfo twManager param
      case J.fromJSON v of
        J.Success x -> return x
        J.Error e -> do
          let msg = T.unlines
                [ "Error"
                , "  request : " <> tshow param
                , "  response: " <> decodeUtf8Lenient (toStrictBytes (J.encode v))
                , "  error   : " <> tshow e
                ] :: Text
          Util.notifyHogeyamaSlack msg
          hPutBuilder stdout $ encodeUtf8Builder msg
          throwString e

data TwConfig = TwConfig
  { twManager :: Manager
  , twInfo    :: TWInfo
  }
  deriving stock (Generic)
class HasTwConfig env where
  twConfigL  :: Lens' env TwConfig
  twManagerL :: Lens' env Manager
  default twManagerL :: Lens' env Manager
  twManagerL = twConfigL . #twManager
  twInfoL :: Lens' env TWInfo
  default twInfoL :: Lens' env TWInfo
  twInfoL = twConfigL . #twInfo
instance HasTwConfig TwConfig where
  twConfigL = id

twConfigFromEnv :: MonadIO m => String -> m TwConfig
twConfigFromEnv prefix = liftIO $ do
    twInfo <- twInfoFromEnv prefix
    return TwConfig
      { twManager = defaultTwManager
      , twInfo = twInfo
      }

{-# NOINLINE defaultTwManager #-}
defaultTwManager :: Manager
defaultTwManager = unsafePerformIO $ newManager tlsManagerSettings

twInfoFromEnv :: String -> IO TWInfo
twInfoFromEnv prefix = do
  tokens <- tokensFromEnv
  credential <- credentialFromEnv prefix
  return def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

tokensFromEnv :: IO OAuth
tokensFromEnv = do
    key <- getEnv "OAUTH_CONSUMER_KEY"
    secret <- getEnv "OAUTH_CONSUMER_SECRET"
    return $ twitterOAuth
      { oauthConsumerKey = BC.pack key
      , oauthConsumerSecret = BC.pack secret
      }

credentialFromEnv :: String -> IO Credential
credentialFromEnv prefix = do
    token <- getEnv $ prefix ++ "_OAUTH_ACCESS_TOKEN"
    secret <- getEnv $ prefix ++ "_OAUTH_ACCESS_SECRET"
    return $ Credential
      [ ("oauth_token", BC.pack token)
      , ("oauth_token_secret", BC.pack secret)
      ]

data Tweet = Tweet
  { tweetId   :: Integer
  , createdAt :: UTCTime
  } deriving stock (Show, Eq, Typeable, Generic)
instance FromJSON Tweet where
  parseJSON = J.withObject "Tweet" $ \o -> do
    pure Tweet <*> (read <$> o J..: "id_str")
               <*> (o J..:  "created_at" >>= return . fromTwitterTime)

tweet :: (MonadReader env m, HasTwitterAPI m env) => T.Text -> m Tweet
tweet = call' . update

data MyUser = MyUser
  { myUserId :: Natural
  , myUserName :: Text
  } deriving stock (Show, Eq, Typeable, Generic)
instance FromJSON MyUser where
  parseJSON = J.withObject "MyUser" $ \o -> do
    pure MyUser <*> (read <$> o J..: "id_str")
                <*> o J..: "name"

newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }
twitterTimeFormat :: String
twitterTimeFormat = "%a %b %d %T %z %Y"
instance FromJSON TwitterTime where
    parseJSON = J.withText "TwitterTime" $ \t ->
        case parseTimeM True defaultTimeLocale twitterTimeFormat (T.unpack t) of
            Just  d -> pure $ TwitterTime d
            Nothing -> fail $ "Could not parse twitter time: " ++ T.unpack t

