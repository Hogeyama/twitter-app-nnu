module App.TwitterBot
  ( AppConfig(..)
  , app
  , nijisanjiAppConfig
  , gamersAppConfig
  , seedsAppConfig
  , since2019AppConfig
  , testAppConfig
  )
  where

import           RIO                            hiding (Data)
import qualified RIO.List                       as List
import           RIO.Char                       (isAscii, toUpper)
import qualified RIO.Map                        as M
import qualified RIO.Text                       as T
import           RIO.Time

import           Lens.Micro.Platform            as L
import qualified Data.Text.IO                   as T
import           System.IO.Unsafe               (unsafePerformIO)
import           Web.Twitter.API                as TwitterApi
import           Data.Generics.Labels           ()

import           Util
import           Nijisanji


-------------------------------------------------------------------------------
-- Interface {{{
-------------------------------------------------------------------------------

app :: MonadUnliftIO m => [AppConfig] -> m ()
app configs = runConc $ mconcat $ map mainWrapper configs
  where
    mainWrapper appConfig = conc $ do
      let prefix = map toUpper $ show $ groupLabel $ group appConfig
      twitterApi <- TwitterApi.defaultImpl <$> twConfigFromEnv prefix
      runRIO Env{appConfig,twitterApi} mainLoop

data AppConfig = AppConfig
  { group :: Group
  , store :: IORef (Maybe SimpleNameMap)
  }
  deriving stock (Generic)
class HasAppConfig env where
  appConfigL :: Lens' env AppConfig
instance HasAppConfig AppConfig where
  appConfigL = id

-- }}}

-------------------------------------------------------------------------------
-- Other data {{{
-------------------------------------------------------------------------------

type SimpleNameMap = Map Text Text

data Env = Env
  { appConfig  :: AppConfig
  , twitterApi :: TwitterApi (RIO Env)
  } deriving stock (Generic)
instance HasAppConfig Env where
  appConfigL = #appConfig
instance HasTwitterAPI (RIO Env) Env where
  twitterApiL = #twitterApi

-- }}}

-------------------------------------------------------------------------------
-- Main loop {{{
-------------------------------------------------------------------------------

mainLoop :: forall m env.
          ( MonadUnliftIO m
          , MonadReader env m
          , HasAppConfig env
          , HasTwitterAPI m env
          )
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
    readData = view (appConfigL . #store) >>= readIORef

    writeData :: SimpleNameMap -> m ()
    writeData d = view (appConfigL . #store) >>= writeIORef `flip` Just d

    -- `list` must contain all members
    fetchData :: ListParam -> m SimpleNameMap
    fetchData list = do
        Group{members} <- view (appConfigL . #group)
        WithCursor{contents = users} <-
          call' @(WithCursor Integer UsersCursorKey MyUser) (listsMembers list & TwitterApi.count ?~ 1000)
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
        Group{members} <- view (appConfigL . #group)
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
              void $ tweet msg
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
      Group{listId} <- view (appConfigL . #group)
      return $ ListIdParam $ fromIntegral listId
-- }}}

-------------------------------------------------------------------------------
-- Config {{{
-------------------------------------------------------------------------------

nijisanjiAppConfig :: AppConfig
nijisanjiAppConfig = AppConfig
    { group = nijisanji
    , store = nijisanjiData
    }

seedsAppConfig :: AppConfig
seedsAppConfig = AppConfig
    { group = seeds
    , store = seedsData
    }

gamersAppConfig :: AppConfig
gamersAppConfig = AppConfig
    { group = gamers
    , store = gamersData
    }

since2019AppConfig :: AppConfig
since2019AppConfig = AppConfig
    { group = since2019
    , store = since2019Data
    }

testAppConfig :: AppConfig
testAppConfig = AppConfig
    { group = testGroup
    , store = testData
    }

nijisanjiData, seedsData, gamersData, since2019Data, testData :: IORef (Maybe SimpleNameMap)
nijisanjiData = unsafePerformIO $ newIORef Nothing
seedsData     = unsafePerformIO $ newIORef Nothing
gamersData    = unsafePerformIO $ newIORef Nothing
since2019Data = unsafePerformIO $ newIORef Nothing
testData       = unsafePerformIO $ newIORef Nothing
{-# NOINLINE nijisanjiData #-}
{-# NOINLINE seedsData     #-}
{-# NOINLINE gamersData    #-}
{-# NOINLINE since2019Data #-}
{-# NOINLINE testData       #-}

-- }}}

