module Main (main) where

import           RIO
import qualified NNU.Util           as Util
import           NNU.App.TwitterBot as Bot
import           System.ReadEnvVar  (readEnvDef)
import           GHC.IO.Encoding

main :: IO ()
main = Util.printAnyError $ do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    isTest <- readEnvDef "NNU_TEST" False
    Util.notifyHogeyamaSlack "NNU on AWS has started"
    Util.notifyHogeyamaSlack $ "isTest = " <> tshow isTest
    let appConfigs =
          if isTest then
            [ Bot.testAppConfig ]
          else
            [ Bot.nijisanjiAppConfig
            , Bot.gamersAppConfig
            , Bot.seedsAppConfig
            , Bot.since2019AppConfig
            ]
    runConc $ mconcat $ map conc
      [ Bot.runApps appConfigs
      ]
