
module Server (defaultMain) where

import           RIO                                    hiding (Handler)
import           RIO.Time
import qualified RIO.Map                                as M
import qualified RIO.Text                               as T

import           Control.Monad.Except                   (ExceptT (ExceptT))
import           Data.Extensible.Reexport
import           Database.Persist                       as Persist
import           Database.Persist.Sql                   (runMigration, runSqlPool)
import           Network.Wai.Handler.Warp               (run)
import           Network.Wai.Middleware.Cors            (cors,
                                                         corsRequestHeaders,
                                                         simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger   (logStdoutDev)
import           Servant
import           Network.HTTP.Media                     ((//), (/:))

import           Util
import           Config                                 (Config, HasConfig, runDb)
import qualified App.TwitterBot                         as Twitter
import qualified App.MitoSchedule                       as MitoSchedule
import qualified App.MakeSnapshot                       as MakeSnapshot (makeSnapshot)
import           Nijisanji
import           Models
--import qualified App.InitializeDB                       as InitializeDB

----------
-- Main --
----------

defaultMain :: HasConfig env => RIO env ()
defaultMain = do
    config <- ask
    runSqlPool (runMigration Models.migrateUpdateInfo) $ config^. #pool
    runSqlPool (runMigration Models.migrateRecordOfDay) $ config^. #pool
    hPutBuilder stdout . getUtf8Builder
      $ "running servant-on-heroku on port "
        <> displayShow (config^. #port)
        <> "...\n"
    mapConcurrently_ id
      [ liftIO $ run (config^. #port)
          $ cors (const $ Just policy)
          $ logStdoutDev $ app (shrinkAssoc config)
      , runRIO config Twitter.app
      -- , runRIO config MakeSnapshot.recordOfDay
      , runRIO config MitoSchedule.app
      -- , tryAny (runRIO config InitializeDB.app) >>= \case
      --     Left e -> print e >> notifyHogeyamaSlack (tshow e)
      --     _ -> return ()
      ]
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }

---------------------
-- Web Application --
---------------------

app :: Config -> Application
app config = serve (Proxy @API) apiServer
  where
    apiServer :: Server API
    apiServer = hoistServer (Proxy @API) transformation serverRoot
    transformation :: RIO Config a -> Handler a
    transformation = Handler . ExceptT . try . runRIO config

type API = Get '[HTML] LByteString
      :<|> "ping"      :> Get '[JSON] Text
      :<|> "nijisanji" :> Get '[JSON] (Maybe (Map Text Text))
      :<|> "Gamers"    :> Get '[JSON] (Maybe (Map Text Text))
      :<|> "SEEDs"     :> Get '[JSON] (Maybe (Map Text Text))
      :<|> "Since2019" :> Get '[JSON] (Maybe (Map Text Text))
      :<|> "All"       :> Get '[JSON] (Maybe (Map Text Text))
      :<|> "snapshot"
            :> QueryParam "date" LocalTime
            :> Get '[JSON] SnapShot
      :<|> "timeline"
            :> QueryParam "liver" Text
               -- x < maxTweetId
            :> QueryParam "maxTweetId" Int64
               -- minTweetId <= x (twitterのAPIと逆にしたので注意(jsででかい整数扱うのどうすればいいんだ))
            :> QueryParam "minTweetId" Int64
            :> Get '[JSON] [(LiverName, UpdateInfo')]
      :<|> Raw

serverRoot :: ServerT API (RIO Config)
serverRoot = index
        :<|> pingHandler
        :<|> readIORef Twitter.nijisanjiData
        :<|> readIORef Twitter.gamersData
        :<|> readIORef Twitter.seedsData
        :<|> readIORef Twitter.since2019Data
        :<|> readIORef Twitter.allData
        :<|> snapshotHandler
        :<|> timelineHandler
        :<|> serveDirectoryWebApp "public"
  where
    index = fromStrictBytes <$> readFileBinary "index.html"
      `catchAny` \e -> return (encodeUtf8 $ tshow e)

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML LByteString where
  mimeRender _ bs = bs

pingHandler :: RIO Config Text
pingHandler = return "Pong"

-- TODO 今まで一度も変更していない人のアレをそれする
-- TODO NameMapのupdatTimeがUTCTimeなので直す
-- 例: "2015-10-03T14:55:21.687"
-- 例: "2015-10-03T14:55"
snapshotHandler :: Maybe LocalTime -> RIO Config SnapShot
snapshotHandler mtime = do
    time <- case mtime of
              Just time -> return time
              Nothing -> utcToJST <$> getCurrentTime
    toSnapShot <$> MakeSnapshot.makeSnapshot time

timelineHandler
  :: Maybe Text
  -> Maybe Int64 -- maxTweetId
  -> Maybe Int64 -- minTweetId
  -> RIO Config [(LiverName, UpdateInfo')]
timelineHandler mliver mMaxTweetId mMinTweetId = do
    let filter' = catMaybes
          [ mliver >>= \liver ->
                return Filter
                  { filterField  = TwitterNameUpdateName
                  , filterValue  = FilterValues (T.split (==',') liver)
                  , filterFilter = Persist.In
                  }
          , mMinTweetId >>= \ minTweetId ->
                return Filter
                  { filterField  = TwitterNameUpdateTweetId
                  , filterValue  = FilterValue minTweetId
                  , filterFilter = Persist.Ge
                  }
          , mMaxTweetId >>= \maxTweetId ->
                return Filter
                  { filterField  = TwitterNameUpdateTweetId
                  , filterValue  = FilterValue maxTweetId
                  , filterFilter = Persist.Lt
                  }
          ]
    updates <- fmap (fmap entityVal) $
        runDb $ Persist.selectList
          filter'
          [ Persist.Desc TwitterNameUpdateUpdateTime
          , Persist.LimitTo 50
          ]
    let ans = [ ( twitterNameUpdateName
                , toUpdateInfo' UpdateInfo
                     { twitterName = twitterNameUpdateTwitterName
                     , updateTime  = twitterNameUpdateUpdateTime
                     , tweetId     = twitterNameUpdateTweetId
                     }
                )
              | TwitterNameUpdate{..} <- updates ]
    print (length ans)
    return ans

-------------------------------------------------------------------------------

type LiverName = Text
type SnapShot = Map LiverName UpdateInfo'

-- TODO 名前
type UpdateInfo' = Record
  '[ "twitterName" >: Text
   , "updateTime"  >: Text -- JST
   , "tweetId"     >: Text
   ]

toSnapShot :: NameMap -> SnapShot
toSnapShot = M.map toUpdateInfo'

toUpdateInfo' :: UpdateInfo -> UpdateInfo'
toUpdateInfo' UpdateInfo{..} =
       #twitterName @= twitterName
    <! #updateTime  @= p (utcToJST updateTime)
    <! #tweetId     @= tshow tweetId
    <! nil
  where
    p (LocalTime (toGregorian->(y,m,d)) (TimeOfDay h m' _)) = T.concat
      [ tshow y, "年", tshow m, "月", tshow d, "日"
      , " "
      , zeroPad (tshow h), ":", zeroPad (tshow m')
      ]
    zeroPad x = if T.length x == 1 then "0"<>x else x

