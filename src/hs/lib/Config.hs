
module Config
  ( runDb
  , makePoolFromUrl
  , createConfigFromEnvVars
  , Config
  , ConfigField
  , HasConfig
  , HasPort
  , HasPool
  ) where

import           RIO                         hiding (Handler)

import           Control.Monad.Logger        (runStdoutLoggingT)

import           Data.Extensible.Reexport
import           Database.Persist.Postgresql (ConnectionString,
                                              createPostgresqlPool)
import           Database.Persist.Sql        (ConnectionPool, SqlBackend, runSqlPool)
import           Network.Wai.Handler.Warp    (Port)
import           System.ReadEnvVar           (lookupEnvDef, readEnvDef)

-- TODO move
-- | Run a Persistent query.
runDb :: (MonadUnliftIO m, MonadReader env m, HasPool env)
      => ReaderT SqlBackend m a -> m a
runDb query = do
  pool <- view #pool
  runSqlPool query pool

-- | This 'Config' object is used to store environment about our application.
-- It is created on startup and passed to all the handlers.
type ConfigField =
  '[ "pool" >: ConnectionPool
   , "port" >: Port
   ]
type HasPort env = Associate' "port" Port env
type HasPool env = Associate' "pool" ConnectionPool env
type HasConfig env = IncludeAssoc' env ConfigField
type Config = Record ConfigField

-- | Number of simultaneous database connections to use in the
-- 'ConnectionPool'.
type DbPoolConnNum = Int

-- | Create a 'ConnectionPool' for database connections based on a
-- 'ConnectionString'.
makePoolFromUrl
  :: DbPoolConnNum      -- ^ Number of database connections to use.
  -> ConnectionString
  -> IO ConnectionPool
makePoolFromUrl dbConnNum connectionString =
  runStdoutLoggingT $ createPostgresqlPool connectionString dbConnNum

-- | Create a 'Config' based on environment variables, using defaults if the
-- environment variables don't exist.
createConfigFromEnvVars :: IO Config
createConfigFromEnvVars = do
  port <- readEnvDef "PORT" 3000
  dbConnNum <- readEnvDef "DATABASE_CONNECTION_NUM" 10
  dbConnectionString <-
    lookupEnvDef
      "DATABASE_URL"
      "postgres://mydbuser:mydbpass@localhost:5432/twidb"
  pool <- makePoolFromUrl dbConnNum dbConnectionString
  return $ #pool @= pool
        <! #port @= port
        <! nil

