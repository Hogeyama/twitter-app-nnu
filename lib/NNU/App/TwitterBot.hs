module NNU.App.TwitterBot
  ( AppConfig(..)
  , runApps
  , mainLoop
  , -- for test
    nijisanjiAppConfig
  , gamersAppConfig
  , seedsAppConfig
  , since2019AppConfig
  , testAppConfig
  ) where

import qualified Data.Aeson                    as J
import           Data.Generics.Labels           ( )
import           Data.Generics.Product          ( HasAny(the) )
import qualified Data.Text.IO                  as T
import           Lens.Micro.Platform           as L
                                                ( (?~)
                                                , both
                                                )
import           NNU.Handler.Twitter            ( HasTwitterAPI
                                                , TwitterApi
                                                )
import qualified NNU.Handler.Twitter           as TwitterApi
import           NNU.Logger
import           NNU.Nijisanji
import           NNU.Util
import           RIO                     hiding ( Data )
import           RIO.Char                       ( isAscii
                                                , toUpper
                                                )
import qualified RIO.List                      as List
import qualified RIO.Map                       as M
import qualified RIO.Text                      as T
import           RIO.Time                       ( getCurrentTime
                                                , getZonedTime
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )

-------------------------------------------------------------------------------
-- Main {{{
-------------------------------------------------------------------------------

runApps :: (HasLogFunc' env) => [AppConfig] -> RIO env ()
runApps configs = runConc $ mconcat $ map (conc . runApp) configs

runApp :: (HasLogFunc' env) => AppConfig -> RIO env ()
runApp appConfig = do
  let prefix = map toUpper $ show $ groupLabel $ group appConfig
  twitterApi <- TwitterApi.defaultImpl <$> TwitterApi.twConfigFromEnv prefix
  mapRIO (\super -> Env { appConfig, twitterApi, super }) mainLoop

data AppConfig = AppConfig
  { group :: Group
  , store :: IORef (Maybe SimpleNameMap)
  }
  deriving stock Generic
class HasAppConfig env where
  appConfigL :: Lens' env AppConfig

-- }}}

-------------------------------------------------------------------------------
-- Local data types {{{
-------------------------------------------------------------------------------

type SimpleNameMap = Map Text Text

data Env super = Env
  { appConfig  :: AppConfig
  , twitterApi :: TwitterApi (RIO (Env super))
  , super      :: super
  }
  deriving stock Generic
instance HasAppConfig (Env super) where
  appConfigL = the @"appConfig"
instance HasTwitterAPI (Env super) where
  twitterApiL = the @"twitterApi"
instance HasGLogFunc super => HasGLogFunc (Env super) where
  type GMsg (Env super) = GMsg super
  gLogFuncL = the @"super" . gLogFuncL

-- }}}

-------------------------------------------------------------------------------
-- Config {{{
-------------------------------------------------------------------------------

nijisanjiAppConfig :: AppConfig
nijisanjiAppConfig = AppConfig { group = nijisanji, store = nijisanjiData }

seedsAppConfig :: AppConfig
seedsAppConfig = AppConfig { group = seeds, store = seedsData }

gamersAppConfig :: AppConfig
gamersAppConfig = AppConfig { group = gamers, store = gamersData }

since2019AppConfig :: AppConfig
since2019AppConfig = AppConfig { group = since2019, store = since2019Data }

testAppConfig :: AppConfig
testAppConfig = AppConfig { group = testGroup, store = testData }

nijisanjiData, seedsData, gamersData, since2019Data, testData
  :: IORef (Maybe SimpleNameMap)
nijisanjiData = unsafePerformIO $ newIORef Nothing
seedsData = unsafePerformIO $ newIORef Nothing
gamersData = unsafePerformIO $ newIORef Nothing
since2019Data = unsafePerformIO $ newIORef Nothing
testData = unsafePerformIO $ newIORef Nothing
{-# NOINLINE nijisanjiData #-}
{-# NOINLINE seedsData #-}
{-# NOINLINE gamersData #-}
{-# NOINLINE since2019Data #-}
{-# NOINLINE testData #-}

-- }}}

-------------------------------------------------------------------------------
-- Main loop {{{
-------------------------------------------------------------------------------


logAndIgnoreError :: HasLogFunc' env => RIO env () -> RIO env ()
logAndIgnoreError m = m `catchAnyDeep` (logE . show)

mainLoop
  :: forall env
   . (HasAppConfig env, HasTwitterAPI env, HasLogFunc' env)
  => RIO env ()
mainLoop = do
  list <- getListParam
  loopWithDelaySec 60 $ logAndIgnoreError $ do
    logD =<< do
      now <- getCurrentTime
      pure $ J.object ["message" J..= ("loop start" :: Text), "clock" J..= now]
    readData >>= \case
      Nothing      -> fetchData list >>= writeData
      Just oldData -> do
        -- newDataが欠けている場合はoldを使う
        newData <- M.unionWith (\_o n -> n) oldData <$> fetchData list
        writeData newData
        filterUpdated oldData newData >>= mapM_ processUpdated
 where
  getListParam :: RIO env TwitterApi.ListParam
  getListParam = do
    Group { listId } <- view (appConfigL . #group)
    return $ TwitterApi.ListIdParam $ fromIntegral listId

  readData :: RIO env (Maybe SimpleNameMap)
  readData = view (appConfigL . #store) >>= readIORef

  writeData :: SimpleNameMap -> RIO env ()
  writeData d = view (appConfigL . #store) >>= writeIORef `flip` Just d

  -- `list` must contain all members
  fetchData :: TwitterApi.ListParam -> RIO env SimpleNameMap
  fetchData list = do
    Group { members }                          <- view (appConfigL . #group)
    TwitterApi.WithCursor { contents = users } <-
      TwitterApi.call'
        @( TwitterApi.WithCursor
            Integer
            TwitterApi.UsersCursorKey
            TwitterApi.User
        )
        (TwitterApi.listsMembers list & TwitterApi.count ?~ 1000)
    fmap (M.fromList . catMaybes)
      $ forM members
      $ \Member { liverName, userId } ->
          case
              List.find
                (\TwitterApi.User { userId = userId' } -> userId == userId')
                users
            of
              Nothing -> do
                logE $ liverName <> " not found"
                return Nothing
              Just TwitterApi.User { userName } ->
                return $ Just (liverName, userName)

  filterUpdated
    :: SimpleNameMap -> SimpleNameMap -> RIO env [(Member, Text, Text)]
  filterUpdated oldData newData = do
    Group { members } <- view (appConfigL . #group)
    let updates = do
          m@Member {..} <- members
          case (oldData, newData) & both %~ M.lookup liverName of
            (Just oldName, Just newName) | oldName /= newName ->
              return (m, oldName, newName)
            _ -> []
    return updates

  processUpdated :: (Member, Text, Text) -> RIO env ()
  processUpdated (m@Member {..}, oldName, newName) = do
    doTweet
   where
    doTweet
      | T.all isAscii newName || T.all isAscii oldName
      = notifyHogeyamaSlack $ "possibly personal information: " <> liverName
      | isTestMember m
      = do
        time <- liftIO getZonedTime
        void $ TwitterApi.tweet $ "@tos test " <> utf8BuilderToText
          (displayShow time)
      | otherwise
      = do
        let msg = T.unlines
              [ liverName <> "(" <> url' <> ")" <> "さんが名前を変更しました"
              , ""
              , newName
              , "⇑"
              , oldName
              ]
            url' = "twitter.com/" <> screenName
        liftIO $ T.putStrLn msg
        void $ TwitterApi.tweet msg

-- }}}

