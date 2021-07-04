{-# LANGUAGE BlockArguments #-}

module NNU.App.TwitterBot
  ( AppConfig(..)
  , runApps
  , nijisanjiAppConfig
  , gamersAppConfig
  , seedsAppConfig
  , since2019AppConfig
  , testAppConfig
  -- for test
  , mainLoop
  , AppState(..)
  , HasAppConfig(..)
  , HasAppState(..)
  , newAppState
  , LoopConfig(..)
  ) where

import qualified Data.Aeson                    as A
import           Data.Generics.Product          ( HasAny(the) )
import           Lens.Micro.Platform           as L
                                                ( (?~)
                                                , both
                                                )
import qualified NNU.Handler.Twitter           as Twitter
import qualified NNU.Logger                    as Logger
import           NNU.Nijisanji
import           RIO                     hiding ( Data )
import           RIO.Char                       ( isAscii
                                                , toUpper
                                                )
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import           RIO.Time                       ( getZonedTime )

-------------------------------------------------------------------------------
-- Main {{{
-------------------------------------------------------------------------------

runApps :: (Logger.Has env) => [AppConfig] -> RIO env ()
runApps configs = runConc $ mconcat $ map (conc . runApp) configs

runApp :: forall env . (Logger.Has env) => AppConfig -> RIO env ()
runApp appConfig = do
  let prefix = map toUpper $ show $ groupLabel $ group appConfig
  twitter      <- Twitter.defaultHandler <$> Twitter.twConfigFromEnv prefix
  appState     <- newAppState
  mapRIO
      (\super ->
        Env { appConfig, appState, twitter, super }
      )
    $ mainLoop LoopConfig { loopDelaySec = 60, loopCount = Nothing }

data AppConfig = AppConfig
  { group :: Group
  }
  deriving stock Generic
class HasAppConfig env where
  appConfigL :: Lens' env AppConfig

data AppState = AppState
  { store :: IORef (Maybe SimpleNameMap)
  }
  deriving stock Generic
class HasAppState env where
  appStateL :: Lens' env AppState

newAppState :: MonadIO m => m AppState
newAppState = do
  store <- newIORef Nothing
  pure AppState {..}

-- }}}

-------------------------------------------------------------------------------
-- Local data types {{{
-------------------------------------------------------------------------------

-- memberName -> current twitter name
type SimpleNameMap = Map Text Text

data Env super = Env
  { appConfig :: AppConfig
  , appState  :: AppState
  , twitter   :: Twitter.Handler (RIO (Env super))
  , super     :: super
  }
  deriving stock Generic
instance HasAppConfig (Env super) where
  appConfigL = the @"appConfig"
instance HasAppState (Env super) where
  appStateL = the @"appState"
instance Twitter.Has (Env super) where
  twitterApiL = the @"twitter"
instance HasGLogFunc super => HasGLogFunc (Env super) where
  type GMsg (Env super) = GMsg super
  gLogFuncL = the @"super" . gLogFuncL

-- }}}

-------------------------------------------------------------------------------
-- Config {{{
-------------------------------------------------------------------------------

nijisanjiAppConfig :: AppConfig
nijisanjiAppConfig = AppConfig { group = nijisanji }

seedsAppConfig :: AppConfig
seedsAppConfig = AppConfig { group = seeds }

gamersAppConfig :: AppConfig
gamersAppConfig = AppConfig { group = gamers }

since2019AppConfig :: AppConfig
since2019AppConfig = AppConfig { group = since2019 }

testAppConfig :: AppConfig
testAppConfig = AppConfig { group = testGroup }

-- }}}

-------------------------------------------------------------------------------
-- Main loop {{{
-------------------------------------------------------------------------------

logAndIgnoreError :: Logger.Has env => RIO env () -> RIO env ()
logAndIgnoreError m = m `catchAnyDeep` (Logger.error . show)

data LoopConfig = LoopConfig
  { loopDelaySec :: Int
  , loopCount    :: Maybe Int
  }
  deriving stock (Show, Eq, Ord, Generic)

mainLoop
  :: forall env
   . ( HasAppConfig env
     , HasAppState env
     , Logger.Has env
     , Twitter.Has env
     )
  => LoopConfig
  -> RIO env ()
mainLoop LoopConfig {..} = do
  let loop = case loopCount of
        Nothing -> forever
        Just n -> forM_ [1 .. n] . const
  loop $ do
    logAndIgnoreError oneLoop
    threadDelay (loopDelaySec * 1000 * 1000)

data DiffMember = DiffMember
  { member  :: Member
  , oldName :: Text
  , newName :: Text
  }

{-# ANN oneLoop ("hlint: ignore Redundant <$>" :: Text) #-}
oneLoop
  :: forall env
   . ( HasAppConfig env
     , HasAppState env
     , Logger.Has env
     , Twitter.Has env
     )
  => RIO env ()
oneLoop = do
  readData >>= \case
    Nothing -> fetchData >>= writeData
    Just oldData -> do
      -- newDataが欠けている場合はoldを使う
      newData <- Map.unionWith (\_o n -> n) oldData <$> fetchData
      posted  <- filterM postTweet =<< calcDiff oldData newData
      writeData $ oldData `patch` posted
 where
  patch store p = Map.unionWith (\_o n -> n) store
    $ Map.fromList [ (memberName member, newName) | DiffMember {..} <- p ]

readData :: forall env . (HasAppState env) => RIO env (Maybe SimpleNameMap)
readData = view (appStateL . to store) >>= readIORef

writeData :: forall env. HasAppState env => SimpleNameMap -> RIO env ()
writeData d = view (appStateL . to store) >>= writeIORef `flip` Just d

fetchData
  :: forall env
   . (HasAppConfig env, Logger.Has env, Twitter.Has env)
  => RIO env SimpleNameMap
fetchData = do
  Group { members } <- view (appConfigL . to group)
  mkSimpleNameMap userId members =<< fetchDataFromTw
 where
  fetchDataFromTw = do
    Group { listId } <- view (appConfigL . the @"group")
    let listParam = Twitter.ListIdParam $ fromIntegral listId
    Twitter.WithCursor { contents = users } <-
      Twitter.call' @(Twitter.WithCursor Integer Twitter.UsersCursorKey _)
      $ Twitter.listsMembers listParam
      & #count
      ?~ 1000
    pure $ Map.fromList $ map (\Twitter.User {..} -> (userId, userName)) users

  mkSimpleNameMap field members map' =
    fmap (Map.fromList . catMaybes) $ forM members $ \member -> do
      case Map.lookup (field member) map' of
        Nothing -> do
          Logger.error $ memberName member <> " not found"
          return Nothing
        Just x -> pure $ Just (memberName member, x)

calcDiff
  :: forall env
   . HasAppConfig env
  => SimpleNameMap
  -> SimpleNameMap
  -> RIO env [DiffMember]
calcDiff oldData newData = do
  Group { members } <- view (appConfigL . to group)
  let updates = do
        member@Member {..} <- members
        case (oldData, newData) & both %~ Map.lookup memberName of
          (Just oldName, Just newName) | oldName /= newName ->
            return DiffMember { .. }
          _ -> []
  return updates

-- returns whether posted or not
postTweet
  :: forall env
   . (Logger.Has env, Twitter.Has env)
  =>DiffMember
  -> RIO env Bool
postTweet DiffMember {..}
  | T.all isAscii newName || T.all isAscii oldName = do
    Logger.error $ "possibly personal information: " <> memberName
    pure False
  | isTestMember member = do
    time <- liftIO getZonedTime
    void $ Twitter.tweet $ "@tos test " <> utf8BuilderToText
      (displayShow time)
    Logger.info ("test user updated" :: Text)
    pure True
  | otherwise = do
    resp <- tryAny $ Twitter.tweet msg
    case resp of
      Right _ -> do
        Logger.debug logMsg
        pure True
      Left e -> do
        Logger.error $ A.object
          [ "message" A..= ("tweet post failed" :: Text)
          , "error" A..= e'
          , "original" A..= logMsg
          ]
        -- ツイート重複エラーのときは状態を更新してよい
        pure $ "Status is a duplicate." `T.isInfixOf` tshow e
       where
        e' | Just (Twitter.ApiError v) <- fromException e = v
           | otherwise = A.toJSON (tshow e)
 where
 Member {..} = member
 msg         = T.unlines
   [ memberName <> "(" <> url' <> ")" <> "さんが名前を変更しました"
   , ""
   , newName
   , "⇑"
   , oldName
   ]
 logMsg = A.object
   [ "message" A..= ("twitter name changed" :: Text)
   , "member" A..= memberName
   , "before" A..= oldName
   , "after" A..= newName
   ]
 url' = "twitter.com/" <> screenName

-- }}}

