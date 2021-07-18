{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import qualified NNU.Handler.Db                as Db
import qualified NNU.Handler.Twitter           as Twitter
import qualified NNU.Logger                    as Logger
import           NNU.Nijisanji
import           RIO                     hiding ( Data )
import           RIO.Char                       ( isAscii
                                                , toUpper
                                                )
import qualified RIO.Map                       as Map
import           RIO.Orphans                    ( HasResourceMap(..)
                                                , ResourceMap
                                                , withResourceMap
                                                )
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T

-------------------------------------------------------------------------------
-- Main {{{
-------------------------------------------------------------------------------

runApps :: (Logger.Has env) => [AppConfig] -> RIO env ()
runApps configs = runConc $ mconcat $ map (conc . runApp) configs

runApp :: forall env . (Logger.Has env) => AppConfig -> RIO env ()
runApp appConfig = withResourceMap $ \resourceMap -> do
  let prefix = map toUpper $ show $ groupLabel $ group appConfig
  twitter  <- Twitter.defaultHandler <$> Twitter.twConfigFromEnv prefix
  db       <- Db.defaultHandler =<< Db.newAwsEnv
  appState <- newAppState
  mapRIO
    (\super -> Env { appConfig, appState, twitter, db, resourceMap, super })
    (mainLoop LoopConfig { loopDelaySec = 60, loopCount = Nothing })

data AppConfig = AppConfig
  { group :: Group
  }
  deriving stock Generic
class HasAppConfig env where
  appConfigL :: Lens' env AppConfig

data AppState = AppState
  { store          :: IORef (Maybe SimpleNameMap)
  , dbPendingItems :: IORef (Map Member DbPendingItem)
  }
  deriving stock Generic
class HasAppState env where
  appStateL :: Lens' env AppState

data DbPendingItem = DbPendingItem
  { updateCurrentNameItem :: Maybe Db.UpdateCurrentNameItem
  , historyItems          :: Set Db.HistoryItem
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass A.ToJSON
emptyDbPendingItem :: DbPendingItem
emptyDbPendingItem = DbPendingItem Nothing Set.empty

data DiffMember = DiffMember
  { member  :: Member
  , oldName :: Text
  , newName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

newAppState :: MonadIO m => m AppState
newAppState = do
  store          <- newIORef mempty
  dbPendingItems <- newIORef mempty
  pure AppState { .. }

getDbPendigItems :: HasAppState env => RIO env (Map Member DbPendingItem)
getDbPendigItems = readIORef =<< view (appStateL . the @"dbPendingItems")

updatePendingItem
  :: HasAppState env => Member -> (DbPendingItem -> DbPendingItem) -> RIO env ()
updatePendingItem member f = do
  r <- view $ appStateL . the @"dbPendingItems"
  modifyIORef' r $ Map.alter `flip` member $ \case
    Nothing     -> Just (f emptyDbPendingItem)
    (Just dpi') -> Just (f dpi')

normalizePendingItems :: HasAppState env => RIO env ()
normalizePendingItems = do
  r            <- view $ appStateL . the @"dbPendingItems"
  pendingItems <- readIORef r
  let pendingItems' = Map.filter (/= emptyDbPendingItem) pendingItems
  writeIORef r pendingItems'

-- }}}

-------------------------------------------------------------------------------
-- Local data types {{{
-------------------------------------------------------------------------------

-- memberName -> current twitter name
type SimpleNameMap = Map Text Text

data Env super = Env
  { appConfig   :: AppConfig
  , appState    :: AppState
  , twitter     :: Twitter.Handler (RIO (Env super))
  , db          :: Db.Handler (RIO (Env super))
  , resourceMap :: ResourceMap
  , super       :: super
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
instance Db.Has (Env super) where
  dbL = the @"db"
instance HasResourceMap (Env super) where
  resourceMapL = the @"resourceMap"

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
     , Db.Has env
     )
  => LoopConfig
  -> RIO env ()
mainLoop LoopConfig {..} = do
  let loop = case loopCount of
        Nothing -> forever
        Just n  -> forM_ [1 .. n] . const
  loop $ do
    logAndIgnoreError oneLoop
    threadDelay (loopDelaySec * 1000 * 1000)

oneLoop
  :: forall env
   . ( HasAppConfig env
     , HasAppState env
     , Logger.Has env
     , Twitter.Has env
     , Db.Has env
     )
  => RIO env ()
oneLoop = do
  readData >>= \case
    Nothing      -> fetchData >>= writeData
    Just oldData -> do
      retryUpdateDb
      -- newDataが欠けている場合はoldを使う
      newData <- Map.unionWith (\_o n -> n) oldData <$> fetchData
      posted  <- filterM postTweet =<< calcDiff oldData newData
      writeData $ oldData `patch` posted
 where
  patch store p = Map.unionWith (\_o n -> n) store
    $ Map.fromList [ (memberName member, newName) | DiffMember {..} <- p ]

readData :: forall env . (HasAppState env) => RIO env (Maybe SimpleNameMap)
readData = do
  view (appStateL . to store) >>= readIORef

writeData :: forall env . HasAppState env => SimpleNameMap -> RIO env ()
writeData d = view (appStateL . to store) >>= writeIORef `flip` Just d

fetchData
  :: forall env
   . (HasAppConfig env, Logger.Has env, Twitter.Has env, Db.Has env)
  => RIO env SimpleNameMap
fetchData = do
  g@Group { members } <- view (appConfigL . to group)
  if False
    then mkSimpleNameMap memberName members =<< fetchDataFromDb g
    else mkSimpleNameMap userId members =<< fetchDataFromTw g
 where
  fetchDataFromDb group = do
    Map.fromList
      .   map (\Db.CurrentNameItem {..} -> (memberName, twitterName))
      <$> Db.invoke (the @"getCurrentNamesOfGroup") group

  fetchDataFromTw Group { listId } = do
    let listParam = Twitter.ListIdParam $ fromIntegral listId
    Twitter.WithCursor { contents = users } <-
      Twitter.call' @(Twitter.WithCursor Integer Twitter.UsersCursorKey _)
      $  Twitter.listsMembers listParam
      &  #count
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
{-# ANN postTweet ("HLint: ignore Redundant <$>" :: String) #-}
postTweet
  :: forall env
   . (HasAppState env, Logger.Has env, Twitter.Has env, Db.Has env)
  => DiffMember
  -> RIO env Bool
postTweet diff@DiffMember {..}
  | T.all isAscii newName || T.all isAscii oldName
  = do
    Logger.error $ "possibly personal information: " <> memberName
    pure False
  | isTestMember member
  = do
    Logger.info ("test user updated" :: Text)
    pure True
  | otherwise
  = do
    alreadyPosted >>= \case
      Nothing -> do
        tweet <- Twitter.tweet msg
        updateDb tweet diff
        Logger.debug logMsg
        pure True
      Just reason -> do
        Logger.info $ A.object
          [ "message" A..= ("tweet already posted" :: Text)
          , "reason" A..= reason
          , "original" A..= logMsg
          ]
        pure True
  `catches` [ Handler $ \(Db.Error v) -> do
              Logger.error $ A.object
                [ "message" A..= ("DB error" :: Text)
                , "error" A..= v
                , "original" A..= logMsg
                ]
              pure True
            , Handler $ \(Twitter.ApiError v) -> do
              Logger.error $ A.object
                [ "message" A..= ("tweet post failed" :: Text)
                , "error" A..= v
                , "original" A..= logMsg
                ]
              -- ツイート重複エラーのときは状態を更新してよい
              pure $ "Status is a duplicate." `T.isInfixOf` tshow v
            , Handler $ \(e :: SomeException) -> do
              Logger.error $ A.object
                [ "message" A..= ("tweet post failed" :: Text)
                , "error" A..= tshow e
                , "original" A..= logMsg
                ]
              pure False
            ]
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

  -- * dbPendingItems に入っている => このインスタンスでツイート済み
  --   TODO 名前の一致を見ないとダメだ。あー結構大変な感じになってきたな
  -- * newNameとDbの最新値が一致 => 他のインスタンスがツイート済み
  -- ツイート済みのときは理由を返す
  alreadyPosted :: RIO env (Maybe Text)
  alreadyPosted = do
    Map.lookup member <$> getDbPendigItems >>= \case
      Just (DbPendingItem (Just ucni) _)
        | ucni ^. the @"twitterName" == newName -> do
          pure (Just "in dbPendingItems")
      _ -> do
        try (Db.invoke (the @"getCurrentName") member) >>= \case
          Right Db.CurrentNameItem { twitterName = dbCurrentName }
            | dbCurrentName == newName
            -> pure . Just $ "DB's current name == newName"
            | otherwise
            -> pure Nothing
          Left (Db.Error e) -> do
            Logger.error $ A.object
              [ "message" A..= ("DB error" :: Text)
              , "error" A..= e
              , "original" A..= logMsg
              ]
            pure Nothing

{-# ANN updateDb ("HLint: ignore Redundant id" :: String) #-}
-- brittany-disable-next-binding
updateDb
  :: forall env
   . (Db.Has env, HasAppState env)
  => Twitter.Tweet
  -> DiffMember
  -> RIO env ()
updateDb Twitter.Tweet {..} DiffMember {..} = do
  Db.invoke (the @"updateCurrentName") ucni `onException` do
    updatePendingItem member $ id
      . (the @"updateCurrentNameItem" .~ Just ucni)
      . (the @"historyItems" %~ Set.insert hi)
  Db.invoke (the @"putHistory") hi `onException` do
    updatePendingItem member $ id
      . (the @"historyItems" %~ Set.insert hi)
  updatePendingItem member $ id
      . (the @"updateCurrentNameItem" .~ Nothing)
 where
  ucni = Db.UpdateCurrentNameItem { member      = member
                                  , twitterName = newName
                                  , updateTime  = createdAt
                                  }
  hi = Db.HistoryItem { member      = member
                      , twitterName = newName
                      , tweetId     = tweetId
                      , updateTime  = createdAt
                      }

retryUpdateDb :: (Db.Has env, Logger.Has env, HasAppState env) => RIO env ()
retryUpdateDb = ignoreAnyError $ do
  normalizePendingItems
  pendingItems <- getDbPendigItems
  unless (Map.size pendingItems == 0) $ do
    Logger.debug $ A.object
      [ "message" A..= ("dbPendingItems remaining" :: Text)
      , "dbPendingItems" A..= Map.mapKeys memberName pendingItems
      ]
  forM_ (Map.toList pendingItems) $ \(member, DbPendingItem mucni his) -> do
    forM_ mucni $ \ucni -> do
      Db.invoke (the @"updateCurrentName") ucni
      updatePendingItem member $ the @"updateCurrentNameItem" .~ Nothing
    forM_ his $ \hi -> do
      Db.invoke (the @"putHistory") hi
      updatePendingItem member $ the @"historyItems" %~ Set.delete hi
  where ignoreAnyError = handleAnyDeep (const (pure ()))

-- }}}

