
module App.InitializeDB where

import           Prelude                        (succ)
import           RIO                            hiding (Data)
import qualified RIO.ByteString.Lazy            as BL
import qualified RIO.List                       as List
import qualified RIO.List.Partial               as List
import qualified RIO.Map                        as M
import qualified RIO.Text                       as T
import qualified RIO.Text.Lazy                  as TL

import           Control.Lens                   ((?~))
import qualified Data.Text.Internal.Builder     as T
import           Data.Aeson                     as J
import           Data.Time
import           Database.Persist               as Persist
import           Database.Persist.Sql           (SqlBackend)
import           HTMLEntities.Decoder           (htmlEncodedText)
import           Web.Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as P
import qualified Web.Twitter.Types.Lens         as L

import           App.TwitterBot                 (HasDbConfig, HasTwConfig,
                                                 call_)
import qualified App.TwitterBot                 as Twitter
import           Config                         (runDb)
import           Models
import           Nijisanji

app :: forall env. HasDbConfig env => RIO env ()
app = do
    when True $ do
      hPutBuilder stdout "fetchAll start"
      fetchAll
      hPutBuilder stdout "fetchAll done"

    (es, rs) <- fmap (bimap concat concat . List.unzip) $
      forM [Nijisanji, Gamers, SEEDs, Since2019] $ \group ->
        loadHistory group <*
          hPutBuilder stdout ("load " <> fromString (show group) <> " done")
    --(e1,r1) <- loadHistory Nijisanji
    --hPutBuilder stdout "load Nijisanji done"
    --(e2,r2) <- loadHistory Gamers
    --hPutBuilder stdout "load Gamers done"
    --(e3,r3) <- loadHistory SEEDs
    --hPutBuilder stdout "load SEEDs done"
    --(e4,r4) <- loadHistory Since2019
    --hPutBuilder stdout "load Since2019 done"
    print es
    -- 先頭が古い記録
    --let r = List.sortOn twitterNameUpdateUpdateTime (concat [r1,r2,r3,r4])
    let r = List.sortOn twitterNameUpdateUpdateTime rs

    forM_ (drop (length r - 10) r) $ \x ->
      BL.hPutStr stdout $ (<>"\n") $ J.encode x
    when True $ do
     hPutBuilder stdout "start inserting name_update"
     runDb $ setUpdateInfos r
    when True $ do
     hPutBuilder stdout "start inserting record_of_day"
     runDb $ setRecordOfDays r

    hPutBuilder stdout "DONE!!!!!"
  where
    setUpdateInfos :: [TwitterNameUpdate] -> ReaderT SqlBackend (RIO env) ()
    setUpdateInfos = Persist.putMany

    setRecordOfDays :: [TwitterNameUpdate] -> ReaderT SqlBackend (RIO env) ()
    setRecordOfDays updates =
        Persist.putMany $ go [] firstDay M.empty updates
      where
        firstDay = localDay $ toJST $
                     twitterNameUpdateUpdateTime (List.head updates)

        -- TODO 最終日が登録されたりされなかったりする（次の日のupdateが0個だとされない）
        go :: [TwitterNameRecordOfDay] -> Day -> NameMap -> [TwitterNameUpdate] -> [TwitterNameRecordOfDay]
        go !acc !_ !_ [] = acc
        go acc day nameMap (x:xs) =
            let day' = localDay . toJST $ twitterNameUpdateUpdateTime x
            in if | day == day' ->
                      -- let msg = T.unlines
                      --       [ twitterNameUpdateName x <> ": " <> twitterNameUpdateTwitterName x
                      --       , decodeUtf8Lenient (toStrictBytes (J.encode (step x nameMap)))
                      --       ]
                      -- in trace msg $
                          go acc day (applyUpdate x nameMap) xs
                  | day < day' ->
                      -- let msg = "[End of day]"
                      -- in trace msg $
                          go (TwitterNameRecordOfDay day nameMap : acc) (succ day) nameMap (x:xs)
                  | otherwise -> error "マ？"

toJST :: UTCTime -> LocalTime
toJST = utcToLocalTime (hoursToTimeZone 9)

loadHistory :: MonadIO m => Group -> m ([Text], [TwitterNameUpdate])
loadHistory g =
    partitionEithers . map (parseNameUpdate g)
      <$> loadHistoryFile (historyFilePath g)
  where
    loadHistoryFile file = do
      x <- BL.readFile file
      case J.eitherDecode x of
        Right s -> return s
        Left e  -> error e

parseNameUpdate :: Group -> L.Status -> Either Text TwitterNameUpdate
parseNameUpdate group status = nameUpdate
  where
    nameUpdate = do
      (liverName, newTwitterName) <- case T.lines $ view L.statusText status of
        [head,"",_old,"⇓", new] -> return (T.takeWhile (/='(') head, htmlDecode new)
        [head,"", new,"⇑",_old] -> return (T.takeWhile (/='(') head, htmlDecode new)
        _                       -> Left (view L.statusText status)
      return TwitterNameUpdate
        { twitterNameUpdateName         = liverName
        , twitterNameUpdateExGroup      = group
        , twitterNameUpdateTwitterName  = newTwitterName
        , twitterNameUpdateTweetId      = fromIntegral (status^.L.statusId)
        , twitterNameUpdateUpdateTime   = status^.L.statusCreatedAt
        }
    htmlDecode = TL.toStrict . T.toLazyText . htmlEncodedText

fetchAll :: MonadIO m => m ()
fetchAll = do
  twConfig <- liftIO Twitter.mkAllConfig
  runRIO twConfig $ mapM_ fetchAllTweet [Nijisanji, Gamers, SEEDs]
fetchAllTweet :: forall env. HasTwConfig env => Group -> RIO env ()
fetchAllTweet group = do
    tweets <- allTweets
    writeFileUtf8 (historyFilePath group) $
      decodeUtf8Lenient . toStrictBytes . J.encode $ tweets
  where
    allTweets :: RIO env [L.Status]
    allTweets = go [] Nothing
      where
        go :: [L.Status] -> Maybe Integer -> RIO env [L.Status]
        go acc maxId = do
          print (length acc, maxId)
          ls <- call_ $ userTimeline (botScreenName group)
                      & P.includeRts ?~ False
                      & P.excludeReplies ?~ True
                      & P.count ?~ 200
                      & P.maxId .~ maxId
          case ls of
            [] -> return acc
            _  -> go (acc<>ls) (Just $ List.last ls ^. L.statusId - 1)
                  -- maxIdは自身を含むものが帰ってくる．
                  -- 帰って来て欲しくないので -1 する．

botScreenName :: Group -> UserParam
botScreenName group = case group of
  Nijisanji -> ScreenNameParam "2434NameUpdate"
  Gamers    -> ScreenNameParam "2434GamersName"
  SEEDs     -> ScreenNameParam "2434SEEDsName"
  _         -> undefined

historyFilePath :: Group -> FilePath
historyFilePath g = T.unpack $ "./history/" <> showListName g

print :: (Show a, MonadIO m) => a -> m ()
print = hPutBuilder stdout . (<>"\n") . getUtf8Builder . displayShow

