module Main (
  main,
) where

import qualified Data.Aeson as J
import GHC.IO.Encoding (
  setFileSystemEncoding,
  setLocaleEncoding,
  utf8,
 )
import qualified NNU.App.TwitterBot as Bot
import qualified NNU.Logger as Logger
import NNU.Prelude
import System.ReadEnvVar (lookupEnv)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  runRIO Logger.defaultLogFunc' $
    logAnyError $ do
      isTest <- readEnvFlag "NNU_TEST" False
      cfgTableName <-
        lookupEnv "NNU_TABLE_NAME" >>= \case
          Just t -> pure t
          -- テーブルを間違えると悲惨なことになるので死ぬべき
          Nothing -> throwString "NNU_TABLE_NAME not set"
      Logger.info $
        J.object
          [ "msg" J..= ("NNU on AWS has started" :: Text)
          , "is_test" J..= isTest
          , "NNU_TABLE_NAME" J..= (cfgTableName :: Text)
          ]
      let appConfigs =
            if isTest
              then [Bot.testAppConfig]
              else
                [ Bot.nijisanjiAppConfig
                , Bot.gamersAppConfig
                , Bot.seedsAppConfig
                , Bot.since2019AppConfig
                ]
      Bot.runApps appConfigs
  where
    logAnyError :: HasCallStack => RIO Logger.LogFunc' a -> RIO Logger.LogFunc' a
    logAnyError = flip withException $ Logger.error . show @SomeException
