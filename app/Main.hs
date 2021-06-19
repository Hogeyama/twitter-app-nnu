module Main
  ( main
  ) where

import           GHC.IO.Encoding
import           NNU.App.TwitterBot            as Bot
import           NNU.Logger
import           RIO
import           System.ReadEnvVar              ( readEnvDef )
import qualified Data.Aeson as J

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  runRIO defaultLogFunc' $ logAnyError $ do
    isTest <- readEnvDef "NNU_TEST" False
    logI $ J.object
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
  where logAnyError = flip withException $ logE . show @SomeException
