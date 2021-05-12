module Main (main) where

import           RIO
import           RIO.Time (getZonedTime)
import           RIO.Orphans
import           NNU.Handler.Db as Db
import           Data.Generics.Labels ()
import           Data.Generics.Product.Any (HasAny(the))

main :: IO ()
main = do
    withResourceMap $ \r ->
      runRIO r $ do
        dbh <- defaultHandler @(RIO Env) @(RIO ResourceMap)
        mapRIO (`Env` dbh) $ do
          time <- getZonedTime
          let item = NameUpdateItem
                { twitterId  = "MitoTsukino"
                , updateTime = time
                , screenName = "月ノ美兎"
                , isActive   = True
                }
          invoke #putNameUpdateItem item

data Env = Env
  { resourceMap :: ResourceMap
  , dbHandler   :: Db.Handler (RIO Env)
  }
  deriving stock (Generic)
instance HasResourceMap Env where
  resourceMapL = #resourceMap
instance HasDb (RIO Env) Env where
  dbL = the @"dbHandler"

