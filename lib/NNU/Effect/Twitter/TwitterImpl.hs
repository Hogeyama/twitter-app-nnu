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
import qualified Web.Twitter.Conduit as TwConduit

import NNU.Effect.Twitter
import NNU.Prelude hiding (Handler)

runTwitter ::
  forall a r.
  Member (Embed IO) r =>
  TwConfig ->
  Sem (Twitter ': r) a ->
  Sem r a
runTwitter TwConfig {..} = interpret $ \case
  Tweet body -> do
    let req = TwConduit.statusesUpdate body
    sendReq @TweetResp req
  ListsMembers ListsMembersParam {listId, count} -> do
    let req =
          TwConduit.listsMembers
            (TwConduit.ListIdParam $ fromIntegral listId)
            & #count
            .~ count
    sendReq @ListsMembersResp req
  where
    sendReq ::
      forall b apiName _b.
      J.FromJSON b =>
      TwConduit.APIRequest apiName _b ->
      Sem r (Either Error b)
    sendReq req = do
      v <- embed $ tryAnyDeep $ TwConduit.call' twInfo twManager req
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
  { twManager :: TwConduit.Manager
  , twInfo :: TwConduit.TWInfo
  }
  deriving stock (Generic)

configFromEnv :: MonadIO m => String -> m TwConfig
configFromEnv prefix = liftIO $ do
  twInfo <- twInfoFromEnv prefix
  return TwConfig {twManager = defaultTwManager, twInfo = twInfo}

{-# NOINLINE defaultTwManager #-}
defaultTwManager :: TwConduit.Manager
defaultTwManager = unsafePerformIO $ TwConduit.newManager TwConduit.tlsManagerSettings

twInfoFromEnv :: String -> IO TwConduit.TWInfo
twInfoFromEnv prefix = do
  tokens <- tokensFromEnv
  credential <- credentialFromEnv prefix
  return
    TwConduit.def
      { TwConduit.twToken =
          TwConduit.def
            { TwConduit.twOAuth = tokens
            , TwConduit.twCredential = credential
            }
      , TwConduit.twProxy = Nothing
      }

tokensFromEnv :: IO TwConduit.OAuth
tokensFromEnv = do
  key <- getEnv "OAUTH_CONSUMER_KEY"
  secret <- getEnv "OAUTH_CONSUMER_SECRET"
  return $
    TwConduit.twitterOAuth
      { TwConduit.oauthConsumerKey = BC.pack key
      , TwConduit.oauthConsumerSecret = BC.pack secret
      }

credentialFromEnv :: String -> IO TwConduit.Credential
credentialFromEnv prefix = do
  token <- getEnv $ prefix ++ "_OAUTH_ACCESS_TOKEN"
  secret <- getEnv $ prefix ++ "_OAUTH_ACCESS_SECRET"
  return $
    TwConduit.Credential
      [("oauth_token", BC.pack token), ("oauth_token_secret", BC.pack secret)]
