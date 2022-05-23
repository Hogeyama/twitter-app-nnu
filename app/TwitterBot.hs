module Main (
  main,
) where

import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy

import GHC.IO.Encoding (
  setFileSystemEncoding,
  setLocaleEncoding,
  utf8,
 )
import System.ReadEnvVar (lookupEnv)

import qualified Data.Aeson as J
import NNU.App.TwitterBot (AppConfig (group))
import qualified NNU.App.TwitterBot as Bot
import qualified NNU.Effect.Db as Db
import qualified NNU.Effect.Db.DynamoDbImpl as DynamoDb
import qualified NNU.Effect.Log as Log
import qualified NNU.Effect.Log.StdoutLogImpl as LogStdout
import qualified NNU.Effect.Twitter as Twitter
import qualified NNU.Effect.Twitter.TwitterImpl as TheTwitter
import qualified NNU.Nijisanji as NNU
import NNU.Prelude hiding (logError)
import qualified Polysemy.State as Polysemy
import RIO.Char (toUpper)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  isTest <- readEnvFlag "NNU_TEST" False
  cfgTableName <-
    lookupEnv @Text "NNU_TABLE_NAME" >>= \case
      Just t -> pure t
      Nothing -> throwString "NNU_TABLE_NAME not set"
  let configs =
        if isTest
          then
            [ Bot.testAppConfig
            ]
          else
            [ Bot.nijisanjiAppConfig
            , Bot.gamersAppConfig
            , Bot.seedsAppConfig
            , Bot.since2019AppConfig
            ]
      loopConfig =
        Bot.LoopConfig
          { loopDelaySec = 60
          , loopCount = Nothing
          }
  awsConfig <- DynamoDb.configGlobalAwsFromEnv
  withResourceMap $ \resourceMap -> do
    forConcurrently_ configs $ \appConfig -> do
      let prefix = map toUpper $ show $ NNU.groupLabel $ group appConfig
      state <- Bot.newAppState
      twConfig <- TheTwitter.configFromEnv prefix
      runFinal
        . Polysemy.embedToFinal @IO
        . LogStdout.runLog
        . logError @Db.Error
        . logError @Twitter.Error
        . Polysemy.runReader appConfig
        . Polysemy.evalState state
        . Polysemy.runReader resourceMap
        . DynamoDb.runDynamoDb awsConfig
        . TheTwitter.runTwitter twConfig
        $ do
          Log.info $
            J.object
              [ "msg" J..= ("NNU on AWS has started" :: Text)
              , "is_test" J..= isTest
              , "NNU_TABLE_NAME" J..= (cfgTableName :: Text)
              ]
          Bot.app loopConfig

logError ::
  forall e r.
  ( Member Log.Log r
  , J.ToJSON e
  , HasCallStack
  ) =>
  Sem (Polysemy.Error e ': r) () ->
  Sem r ()
logError action =
  Polysemy.runError action >>= \case
    Left e -> Log.error e
    Right () -> pure ()
