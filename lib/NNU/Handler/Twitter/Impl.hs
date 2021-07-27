{-# LANGUAGE UndecidableInstances #-}
module NNU.Handler.Twitter.Impl
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

import           Data.Aeson                     ( FromJSON )
import qualified Data.Aeson                    as J
import qualified Data.ByteString.Char8         as BC
import           Data.Generics.Product          ( the )
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

import           NNU.Prelude             hiding ( Handler )
import           NNU.Handler.Twitter

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
