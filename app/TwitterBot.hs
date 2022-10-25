module Main (
  main,
) where

import Polysemy
import qualified Polysemy.Reader as Polysemy

import GHC.IO.Encoding (
  setFileSystemEncoding,
  setLocaleEncoding,
  utf8,
 )
import System.ReadEnvVar (lookupEnv)

import qualified Data.Aeson as J
import NNU.App.TwitterBot (AppConfig (..), initialAppState)
import qualified NNU.App.TwitterBot as Bot
import qualified NNU.Effect.Db.DynamoDbImpl as DynamoDb
import qualified NNU.Effect.Log as Log
import qualified NNU.Effect.Log.StdoutLogImpl as LogStdout
import qualified NNU.Effect.Sleep.IO as SleepIO
import qualified NNU.Effect.Twitter.TwitterImpl as TheTwitter
import qualified NNU.Nijisanji as NNU
import NNU.Prelude
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
    runFinal
      . Polysemy.embedToFinal @IO
      . LogStdout.runLog
      $ Log.info $
        J.object
          [ "msg" J..= ("NNU on AWS has started" :: Text)
          , "is_test" J..= isTest
          , "NNU_TABLE_NAME" J..= (cfgTableName :: Text)
          ]
    forConcurrently_ configs $ \appConfig -> do
      let prefix = map toUpper $ show $ NNU.groupLabel $ group appConfig
      twConfig <- TheTwitter.configFromEnv prefix
      appState <- newIORef initialAppState
      runFinal
        . Polysemy.embedToFinal @IO
        . Polysemy.runReader resourceMap
        . LogStdout.runLog
        . SleepIO.runSleep
        . DynamoDb.runDynamoDb awsConfig
        . TheTwitter.runTwitter twConfig
        . Polysemy.runReader appConfig
        . Polysemy.runStateIORef appState
        $ Bot.app loopConfig
