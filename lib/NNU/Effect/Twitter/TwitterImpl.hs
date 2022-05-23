module NNU.Effect.Twitter.TwitterImpl (
  runTwitter,
  TwConfig (..),
  configFromEnv,
) where

import Polysemy

import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BC
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Web.Twitter.Conduit as Twitter

import NNU.Effect.Twitter
import NNU.Prelude hiding (Handler)

runTwitter ::
  forall a r.
  Member (Embed IO) r =>
  TwConfig ->
  Sem (Twitter ': r) a ->
  Sem r a
runTwitter TwConfig {..} = interpret $ \case
  Call_ (req :: APIRequest apiName _a) -> do
    v <- embed $ tryAnyDeep $ Twitter.call' twInfo twManager req
    case v of
      Right resp -> case J.fromJSON resp of
        (J.Success x) -> return (Right x)
        (J.Error e) -> do
          let msg =
                J.object
                  [ "message" J..= ("JSON decode error" :: Text)
                  , "request" J..= tshow req
                  , "response" J..= resp
                  , "error" J..= e
                  ]
          pure $ Left $ Error msg
      Left e -> do
        let msg =
              J.object
                [ "message" J..= ("Twitter API call error" :: Text)
                , "request" J..= show req
                , "error" J..= show e
                ]
        pure $ Left $ Error msg

data TwConfig = TwConfig
  { twManager :: Manager
  , twInfo :: TWInfo
  }
  deriving stock (Generic)

configFromEnv :: MonadIO m => String -> m TwConfig
configFromEnv prefix = liftIO $ do
  twInfo <- twInfoFromEnv prefix
  return TwConfig {twManager = defaultTwManager, twInfo = twInfo}

{-# NOINLINE defaultTwManager #-}
defaultTwManager :: Manager
defaultTwManager = unsafePerformIO $ newManager tlsManagerSettings

twInfoFromEnv :: String -> IO TWInfo
twInfoFromEnv prefix = do
  tokens <- tokensFromEnv
  credential <- credentialFromEnv prefix
  return
    def
      { twToken = def {twOAuth = tokens, twCredential = credential}
      , twProxy = Nothing
      }

tokensFromEnv :: IO OAuth
tokensFromEnv = do
  key <- getEnv "OAUTH_CONSUMER_KEY"
  secret <- getEnv "OAUTH_CONSUMER_SECRET"
  return $
    twitterOAuth
      { oauthConsumerKey = BC.pack key
      , oauthConsumerSecret = BC.pack secret
      }

credentialFromEnv :: String -> IO Credential
credentialFromEnv prefix = do
  token <- getEnv $ prefix ++ "_OAUTH_ACCESS_TOKEN"
  secret <- getEnv $ prefix ++ "_OAUTH_ACCESS_SECRET"
  return $
    Credential
      [("oauth_token", BC.pack token), ("oauth_token_secret", BC.pack secret)]
