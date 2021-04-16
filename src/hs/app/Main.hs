module Main (main) where

import           RIO
import qualified Util
import           App.TwitterBot as Bot
import           GHC.IO.Encoding

main :: IO ()
main = Util.printAnyError $ do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    Util.notifyHogeyamaSlack "NNU on AWS has started"
    runConc $ mconcat $ map conc
      [ Bot.app
          [ -- Bot.testAppConfig
            Bot.nijisanjiAppConfig
          , Bot.gamersAppConfig
          , Bot.seedsAppConfig
          , Bot.since2019AppConfig
          ]
      ]
