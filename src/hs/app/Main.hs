module Main (main) where

import           RIO
import           RIO.Process
import qualified Util
import           System.Environment (getEnv)
import           App.TwitterBot as Bot
import           GHC.IO.Encoding

catDhall :: IO ()
catDhall = do
    opt <- logOptionsHandle stdout True
    (logFunc, clean) <- newLogFunc opt
    processContext <- mkDefaultProcessContext
    runRIO Env{..} $ proc "cat" ["./src/dhall/Nijisanji.dhall"] $ \config -> do
      (code,out,err) <- readProcess config
      Util.print (code,out,err)
      Util.notifyHogeyamaSlack $ tshow (code,out,err)
    x <- readFileUtf8 "./src/dhall/Nijisanji.dhall"
    Util.print x
    Util.notifyHogeyamaSlack $ tshow x
    clean

data Env = Env
  { logFunc :: LogFunc
  , processContext :: ProcessContext
  }
instance HasLogFunc Env where
  logFuncL = lens logFunc (\s b -> s { logFunc = b})
instance HasProcessContext Env where
  processContextL = lens processContext (\s b -> s { processContext = b})

main :: IO ()
main = Util.printAnyError $ do
    x <- tryAnyDeep $ getEnv "TEST_OAUTH_ACCESS_TOKEN"
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    Util.print x
    Util.notifyHogeyamaSlack $ tshow x
    catDhall
    runConc $ mconcat $ map conc
      [ Bot.app
          [ Bot.testAppConfig
          -- , Bot.nijisanjiAppConfig
          -- , Bot.gamersAppConfig
          -- , Bot.seedsAppConfig
          -- , Bot.since2019AppConfig
          ]
      ]
