module Main (main) where

import           RIO
import qualified Server
import qualified Config
import qualified Util

main :: IO ()
main = Util.printAnyError $ do
    config <- Config.createConfigFromEnvVars
    runRIO config Server.defaultMain

