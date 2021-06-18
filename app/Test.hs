module Main (main) where

import           RIO
import           NNU.Logger as Logger
import           Data.Generics.Labels ()

main :: IO ()
main = runRIO defaultLogFunc' $ do
  logW $ Just (1 :: Int)
