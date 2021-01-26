module App.TwitterBot where

import           Prelude                        (read)
import           RIO                            hiding (Data)
import qualified RIO.List                       as List
import           RIO.Char                       (isAscii)
import qualified RIO.Map                        as M
import qualified RIO.Text                       as T
import           RIO.Time

import           Control.Lens                   (both, (?~))
import           Data.Aeson                     (FromJSON)
import qualified Data.Aeson                     as J
import qualified Data.ByteString.Char8          as BC
import           Data.Extensible.Reexport       hiding (Member)
import qualified Data.Text.IO                   as T
import           Database.Persist               (insert_)
import           Database.Persist.Sql           (ConnectionPool)
import           System.Environment             (getEnv)
import           System.IO.Unsafe               (unsafePerformIO)
import           Web.Twitter.Conduit
import           Web.Twitter.Conduit.Parameters as L

import           Util
import           Models
import           Config                         (runDb, HasPool)
import           Nijisanji

-------------------------------------------------------------------------------
-- Data Types{{{
-------------------------------------------------------------------------------

type SimpleNameMap = Map Text Text

type Config = Record ConfigF
type ConfigF = DbConfigF ++ AppConfigF ++ TwConfigF
type HasConfig env = IncludeAssoc' env ConfigF

type DbConfig = Record DbConfigF
type DbConfigF =
  '[ "pool" >: ConnectionPool ]
type HasDbConfig env = IncludeAssoc' env DbConfigF -- = HasPool

type AppConfig = Record AppConfigF
type AppConfigF =
  '[ "groupName"    >: Group
   , "groupMembers" >: [Member]
   , "appData"      >: IORef (Maybe SimpleNameMap)
   ]
type HasAppConfig env = IncludeAssoc' env AppConfigF

type TwConfig = Record TwConfigF
type TwConfigF =
  [ "twManager" >: Manager
  , "twInfo"    >: TWInfo
  ]
type HasTwConfig env = IncludeAssoc' env TwConfigF

-- }}}

-------------------------------------------------------------------------------
-- App{{{
-------------------------------------------------------------------------------

data Tweet = Tweet
  { tweetId   :: Integer
  , createdAt :: UTCTime
  } deriving stock (Show, Eq, Typeable, Generic)
instance FromJSON Tweet where
  parseJSON = J.withObject "Tweet" $ \o -> do
    pure Tweet <*> (read <$> o J..: "id_str")
               <*> (o J..:  "created_at" >>= return . fromTwitterTime)
data MyUser = MyUser
  { myUserId :: Natural
  , myUserName :: Text
  } deriving stock (Show, Eq, Typeable, Generic)
instance FromJSON MyUser where
  parseJSON = J.withObject "MyUser" $ \o -> do
    pure MyUser <*> (read <$> o J..: "id_str")
                <*> o J..: "name"

newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }
twitterTimeFormat :: String
twitterTimeFormat = "%a %b %d %T %z %Y"
instance FromJSON TwitterTime where
    parseJSON = J.withText "TwitterTime" $ \t ->
        case parseTimeM True defaultTimeLocale twitterTimeFormat (T.unpack t) of
            Just  d -> pure $ TwitterTime d
            Nothing -> fail $ "Could not parse twitter time. Text was: " ++ T.unpack t


app :: forall env. HasPool env => RIO env ()
app = do
    dbConfig <- asks @_ @_ @DbConfig shrinkAssoc
    liftIO $ mapConcurrently_  (mainWrapper dbConfig)
        [ mkNijisanjiConfig
        , mkSEEDsConfig
        , mkGamersConfig
        , mkSince2019Config
        ]
  where
    mainWrapper dbConfig mkAppTwConfig = do
      appTwConfig <- mkAppTwConfig
      runRIO (dbConfig `happend` appTwConfig) mainLoop

mainLoop :: forall m env
    .  (MonadUnliftIO m, MonadReader env m, HasConfig env)
    => m ()
mainLoop = do
    list <- getListParam
    loopWithDelaySec 60 $ printAnyError $ do
      print =<< getCurrentTime
      readData >>= \case
        Nothing ->
          fetchData list >>= writeData
        Just oldData -> do
          -- newDataが欠けている場合はoldを使う
          newData <- M.unionWith (\_o n -> n) oldData <$> fetchData list
          writeData newData
          tweetIfUpdated oldData newData
  where
    readData :: m (Maybe SimpleNameMap)
    readData = view #appData >>= readIORef

    writeData :: SimpleNameMap -> m ()
    writeData d = view #appData >>= writeIORef `flip` Just d

    -- `list` must contain all members
    fetchData :: ListParam -> m SimpleNameMap
    fetchData list = do
        members <- view #groupMembers
        WithCursor{contents = users} <-
          call_' @(WithCursor Integer UsersCursorKey MyUser) (listsMembers list & L.count ?~ 1000)
        fmap (M.fromList . catMaybes) $
          forM members $ \Member{liverName,userId} ->
            case List.find (\u -> myUserId u == userId) users of
              Nothing -> do
                let msg = "fail to get the name of " <> liverName <> "\n"
                hPutBuilder stdout $ encodeUtf8Builder msg
                notifyHogeyamaSlack msg
                return Nothing
              Just u -> return $ Just (liverName, myUserName u)

    tweetIfUpdated :: SimpleNameMap -> SimpleNameMap -> m ()
    tweetIfUpdated oldData newData = do
        members <- view #groupMembers
        forM_ members $ \m@Member{..} -> do
          (oldName, newName) <-
              case (oldData, newData) & both %~ M.lookup liverName of
                (Just oldName', Just newName') -> return (oldName', newName')
                x -> error $ T.unpack $ liverName  <> ": " <> tshow x
          when (oldName /= newName) $ do
            msg <- if isTestMember m
                   then do
                     time <- liftIO getZonedTime
                     return $ "@tos test " <> utf8BuilderToText (displayShow time)
                   else return $ mkInfo m oldName newName
            liftIO $ T.putStrLn msg
            if T.all isAscii newName || T.all isAscii oldName then do
              notifyHogeyamaSlack $ "possibly personal information: " <> liverName
            else do
              Tweet{tweetId, createdAt} <- tweet msg
              unless (isTestMember m) $
               runDb $ insert_ TwitterNameUpdate
                 { twitterNameUpdateName         = liverName
                 , twitterNameUpdateExGroup      = exGroup
                 , twitterNameUpdateTwitterName  = newName
                 , twitterNameUpdateTweetId      = fromIntegral tweetId
                 , twitterNameUpdateUpdateTime   = createdAt
                 }
      where
        mkInfo :: Member -> Text -> Text -> Text
        mkInfo Member{..} oldName newName = T.unlines
            [ liverName <> "(" <> url' <> ")" <> "さんが名前を変更しました"
            , ""
            , newName
            , "⇑"
            , oldName
            ]
          where
            url' = "twitter.com/" <> screenName

    getListParam :: m ListParam
    getListParam = do
      view #groupName >>= \case
        -- TODO dhallで管理
        Nijisanji   -> return $ ListIdParam 1086003364310208512
        SEEDs       -> return $ ListIdParam 1086003364251557888
        Gamers      -> return $ ListIdParam 1086003363018428416
        Since2019   -> return $ ListIdParam 1086023933881712640
        o           -> error $ "getListParam: " <> show o
-- }}}

-------------------------------------------------------------------------------
-- Config {{{
-------------------------------------------------------------------------------

nijisanjiAppConfig :: AppConfig
nijisanjiAppConfig =
       #groupName    @= Nijisanji
    <! #groupMembers @= nijisanjiMembers
    <! #appData      @= nijisanjiData
    <! nil
mkNijisanjiConfig :: IO (Record (AppConfigF++TwConfigF))
mkNijisanjiConfig = happend nijisanjiAppConfig <$> twConfigFromEnv Nothing

seedsAppConfig :: AppConfig
seedsAppConfig =
       #groupName    @= SEEDs
    <! #groupMembers @= seedsMembers
    <! #appData      @= seedsData
    <! nil
mkSEEDsConfig :: IO (Record (AppConfigF++TwConfigF))
mkSEEDsConfig = happend seedsAppConfig <$> twConfigFromEnv (Just "SEEDs")

gamersAppConfig :: AppConfig
gamersAppConfig =
       #groupName    @= Gamers
    <! #groupMembers @= gamersMembers
    <! #appData      @= gamersData
    <! nil
mkGamersConfig :: IO (Record (AppConfigF++TwConfigF))
mkGamersConfig = happend gamersAppConfig <$> twConfigFromEnv (Just "Gamers")

since2019AppConfig :: AppConfig
since2019AppConfig =
       #groupName    @= Since2019
    <! #groupMembers @= since2019Members
    <! #appData      @= since2019Data
    <! nil
mkSince2019Config :: IO (Record (AppConfigF++TwConfigF))
mkSince2019Config = happend since2019AppConfig <$> twConfigFromEnv (Just "Since2019")

allAppConfig :: AppConfig
allAppConfig =
       #groupName    @= Other "All"
    <! #groupMembers @= nijisanjiMembers ++ seedsMembers ++ gamersMembers ++ since2019Members
    <! #appData      @= allData
    <! nil
mkAllConfig :: IO (Record (AppConfigF++TwConfigF))
mkAllConfig = happend allAppConfig <$> twConfigFromEnv (Just "ALL")

nijisanjiData, seedsData, gamersData, since2019Data, allData :: IORef (Maybe SimpleNameMap)
nijisanjiData = unsafePerformIO $ newIORef Nothing
seedsData     = unsafePerformIO $ newIORef Nothing
gamersData    = unsafePerformIO $ newIORef Nothing
since2019Data = unsafePerformIO $ newIORef Nothing
allData       = unsafePerformIO $ newIORef Nothing
{-# NOINLINE nijisanjiData #-}
{-# NOINLINE seedsData     #-}
{-# NOINLINE gamersData    #-}
{-# NOINLINE since2019Data #-}
{-# NOINLINE allData       #-}

twConfigFromEnv :: MonadIO m => Maybe String -> m TwConfig
twConfigFromEnv mPrefix = liftIO $ do
  twInfo <- twInfoFromEnv mPrefix
  return $
       #twManager @= twManager
    <! #twInfo    @= twInfo
    <! nil

{-# NOINLINE twManager #-}
twManager :: Manager
twManager = unsafePerformIO $ newManager tlsManagerSettings

twInfoFromEnv :: Maybe String -> IO TWInfo
twInfoFromEnv mPrefix = do
  tokens <- tokensFromEnv
  credential <- credentialFromEnv mPrefix
  return def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

tokensFromEnv :: IO OAuth
tokensFromEnv = do
    key <- getEnv "OAUTH_CONSUMER_KEY"
    secret <- getEnv "OAUTH_CONSUMER_SECRET"
    return $ twitterOAuth
      { oauthConsumerKey = BC.pack key
      , oauthConsumerSecret = BC.pack secret
      }

credentialFromEnv :: Maybe String -> IO Credential
credentialFromEnv mPrefix = do
    let prefix = case mPrefix of
          Just s  -> s ++ "_"
          Nothing -> ""
    token <- getEnv $ prefix ++ "OAUTH_ACCESS_TOKEN"
    secret <- getEnv $ prefix ++ "OAUTH_ACCESS_SECRET"
    return $ Credential
      [ ("oauth_token", BC.pack token)
      , ("oauth_token_secret", BC.pack secret)
      ]
-- }}}

-------------------------------------------------------------------------------
-- Util{{{
-------------------------------------------------------------------------------

loopWithDelaySec :: MonadIO m => Int -> m () -> m ()
loopWithDelaySec n m = forever $ m >> threadDelay (n * 1000 * 1000)

-- Twitter API
----------------------------------------

tweet :: (MonadUnliftIO m, MonadReader env m, HasTwConfig env) => T.Text -> m Tweet
tweet = call_' . update

call_' :: ( FromJSON r
          , MonadUnliftIO m
          , MonadReader env m
          , HasTwConfig env
          )
       => APIRequest apiName _responseType
       -> m r
call_' param = do
  info <- view #twInfo
  mgr <- view #twManager
  v   <- liftIO $ call' info mgr param
  case J.fromJSON v of
    J.Success x -> return x
    J.Error e -> do
      let msg = T.unlines
            [ "Error"
            , "  request : " <> tshow param
            , "  response: " <> decodeUtf8Lenient (toStrictBytes (J.encode v))
            , "  error   : " <> tshow e
            ] :: Text
      notifyHogeyamaSlack msg
      hPutBuilder stdout $ encodeUtf8Builder msg
      throwIO (stringException e)

call_ :: ( FromJSON responseType
         , MonadUnliftIO m
         , MonadReader env m
         , HasTwConfig env
         )
      => APIRequest apiName responseType
      -> m responseType
call_ = call_'
-- }}}

