module Main (main) where

import           RIO
import qualified Util
import           App.TwitterBot as Bot

main :: IO ()
main = Util.printAnyError $ do
    runConc $ mconcat $ map conc
      [ Bot.app
          [ Bot.testAppConfig
          -- , Bot.nijisanjiAppConfig
          -- , Bot.gamersAppConfig
          -- , Bot.seedsAppConfig
          -- , Bot.since2019AppConfig
          ]
      ]
