module Main
  ( main
  ) where

import           RIO

import           GHC.IO.Encoding                ( setFileSystemEncoding
                                                , setLocaleEncoding
                                                , utf8
                                                )
import qualified Data.Aeson                    as J
import           System.ReadEnvVar              ( readEnvDef )

import qualified NNU.App.TwitterBot            as Bot
import qualified NNU.Logger                    as Logger


main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  runRIO Logger.defaultLogFunc' $ logAnyError $ do
    isTest <- readEnvDef "NNU_TEST" False
    Logger.info $ J.object
      [ "msg" J..= ("NNU on AWS has started" :: Text)
      , "is_test" J..= isTest
      ]
    let appConfigs = if isTest
          then [Bot.testAppConfig]
          else
            [ Bot.nijisanjiAppConfig
            , Bot.gamersAppConfig
            , Bot.seedsAppConfig
            , Bot.since2019AppConfig
            ]
    Bot.runApps appConfigs
  where logAnyError = flip withException $ Logger.error . show @SomeException
