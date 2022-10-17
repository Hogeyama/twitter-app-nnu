{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}

module NNU.App.TwitterBot (
  app,
  Effs,
  AppConfig (..),
  initialAppState,
  LoopConfig (..),
  nijisanjiAppConfig,
  gamersAppConfig,
  seedsAppConfig,
  since2019AppConfig,
  testAppConfig,
  -- for test
  AppState (..),
) where

import Polysemy
import Polysemy.Error as Polysemy
import Polysemy.Reader as Polysemy
import Polysemy.State as Polysemy

import qualified Data.Aeson as A
import Data.Generics.Product (HasField (field))
import Data.Monoid (Endo (..))
import RIO.Char (isAscii)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T

import NNU.Effect.Db (NnuDb)
import qualified NNU.Effect.Db as Db
import NNU.Effect.Log (Log)
import qualified NNU.Effect.Log as Logger
import NNU.Effect.Sleep (Sleep)
import qualified NNU.Effect.Sleep as Sleep
import NNU.Effect.Twitter (Twitter)
import qualified NNU.Effect.Twitter as Twitter
import qualified NNU.Nijisanji as Nnu
import NNU.Prelude hiding (
  Reader,
  ask,
  asks,
  catch,
  fromEither,
  onException,
  try,
 )

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

data AppConfig = AppConfig
  { group :: Nnu.Group
  }
  deriving stock (Generic)

data AppState = AppState
  { nameMapCache :: Maybe SimpleNameMap
  , dbPendingItems :: Map Nnu.Member DbPendingItem
  }
  deriving stock (Generic)

instance Semigroup AppState where
  AppState a1 b1 <> AppState a2 b2 = AppState (a1 <> a2) (b1 <> b2)

instance Monoid AppState where
  mempty = AppState mempty mempty

initialAppState :: AppState
initialAppState = mempty

-- | エラーで投入できなかったアイテム
data DbPendingItem = DbPendingItem
  { updateCurrentNameItem :: Maybe Db.UpdateCurrentNameItem
  , historyItems :: Set Db.HistoryItem
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)

emptyDbPendingItem :: DbPendingItem
emptyDbPendingItem = DbPendingItem Nothing Set.empty

data DiffMember = DiffMember
  { member :: Nnu.Member
  , oldName :: Text
  , newName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

-- memberName -> current twitter name
type SimpleNameMap = Map Text Text

getDbPendigItems ::
  ( Member (State AppState) r
  ) =>
  Sem r (Map Nnu.Member DbPendingItem)
getDbPendigItems = do
  normalizePendingItems
  gets dbPendingItems

updatePendingItem ::
  ( Member (State AppState) r
  ) =>
  Nnu.Member ->
  (DbPendingItem -> DbPendingItem) ->
  Sem r ()
updatePendingItem member f = do
  modify' $
    field @"dbPendingItems"
      %~ Map.alter
        \case
          Nothing -> Just (f emptyDbPendingItem)
          (Just dpi') -> Just (f dpi')
        member

normalizePendingItems ::
  ( Member (State AppState) r
  ) =>
  Sem r ()
normalizePendingItems = do
  modify' $ field @"dbPendingItems" %~ Map.filter (/= emptyDbPendingItem)

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

nijisanjiAppConfig :: AppConfig
nijisanjiAppConfig = AppConfig {group = Nnu.nijisanji}

seedsAppConfig :: AppConfig
seedsAppConfig = AppConfig {group = Nnu.seeds}

gamersAppConfig :: AppConfig
gamersAppConfig = AppConfig {group = Nnu.gamers}

since2019AppConfig :: AppConfig
since2019AppConfig = AppConfig {group = Nnu.since2019}

testAppConfig :: AppConfig
testAppConfig = AppConfig {group = Nnu.testGroup}

-------------------------------------------------------------------------------
-- Main loop
-------------------------------------------------------------------------------

data LoopConfig = LoopConfig
  { loopDelaySec :: Int
  , loopCount :: Maybe Int -- Nothing means infinite loop
  }
  deriving stock (Show, Eq, Ord, Generic)

runLoop ::
  forall r.
  ( Members '[Sleep] r
  ) =>
  LoopConfig ->
  Sem r () ->
  Sem r ()
runLoop LoopConfig {..} action = loop $ do
  action
  Sleep.sleepSec loopDelaySec
  where
    loop = case loopCount of
      Nothing -> forever
      Just n -> forM_ [1 .. n] . const

type Effs =
  '[ Reader AppConfig
   , State AppState
   , Twitter
   , NnuDb
   , Sleep
   , Log
   ]

app ::
  forall r.
  ( Members Effs r
  ) =>
  LoopConfig ->
  Sem r ()
app loopConfig = runLoop loopConfig do
  catchAndLogFetchError $
    getCache >>= \case
      Nothing -> fetchData >>= updateCache
      Just oldData -> do
        retryUpdateDb
        -- newDataが欠けている場合はoldDataを使う
        newData <- Map.unionWith (\_o n -> n) oldData <$> fetchData
        diffs <- diff oldData newData
        cacheDiffs <- filterM (fmap isCacheUpdateNeeded . tweetNameUpdate) diffs
        updateCache $ applyPatch oldData cacheDiffs
  where
    isCacheUpdateNeeded = \case
      TweetSucceeded -> True
      TweetAlreadyPosted -> True
      TweetSkipped -> False
      TweetFailed PrecheckFailed {} -> False
      TweetFailed TweetPostFailed {} -> False
      TweetFailed PostDbUpdateFailed {} -> True -- ツイートは成功しているので
    applyPatch oldData p =
      Map.unionWith (\_o n -> n) oldData $
        Map.fromList [(Nnu.memberName member, newName) | DiffMember {..} <- p]

    catchAndLogFetchError action =
      Polysemy.runError action >>= \case
        Right () -> pure ()
        Left (e :: Twitter.ListsMembersError) -> Logger.info e

getCache ::
  forall r.
  ( Members '[State AppState] r
  ) =>
  Sem r (Maybe SimpleNameMap)
getCache = gets nameMapCache

updateCache ::
  forall r.
  ( Members '[State AppState] r
  ) =>
  SimpleNameMap ->
  Sem r ()
updateCache d = modify' $ field @"nameMapCache" .~ Just d

fetchData ::
  forall r.
  ( Members
      '[ Reader AppConfig
       , State AppState
       , Error Twitter.ListsMembersError
       , Log
       , Twitter
       , NnuDb
       ]
      r
  ) =>
  Sem r SimpleNameMap
fetchData = do
  g@Nnu.Group {members} <- asks group
  mkSimpleNameMap Nnu.userId members =<< fetchDataFromTw g
  where
    -- DBから取得する。
    -- + 起動していない時間が続くと次の起動時に大量ツイートしてしまう。
    -- + 可用性はTwitterよりも高いはず。
    -- 1個目のデメリットが怖いのでTwitterの方を採用する。
    -- _fetchDataFromDb :: Nnu.Group -> Sem r SimpleNameMap
    -- _fetchDataFromDb group = do
    --   Map.fromList
    --     . map (\Db.CurrentNameItem {..} -> (memberName, twitterName))
    --     <$> (Db.getCurrentNamesOfGroup group >>= fromEither)

    -- Twitterから取得する。
    -- + 再起動中に名前の変更があると見過ごしてしまう。
    -- + Twitterが落ちていると取得できない。経験上、取得に失敗する
    --   ほどの障害は稀なのでそんなに気にしないで良い。
    fetchDataFromTw :: Nnu.Group -> Sem r (Map Natural Text)
    fetchDataFromTw Nnu.Group {listId} = do
      Twitter.WithCursor {contents = users} <-
        Twitter.listsMembers
          Twitter.ListsMembersParam
            { listId = listId
            , count = Just 1000 -- FIXME magic number
            }
          >>= fromEither
      pure $ Map.fromList $ map (\Twitter.User {..} -> (userId, userName)) users

    mkSimpleNameMap ::
      Ord k =>
      (Nnu.Member -> k) ->
      [Nnu.Member] ->
      Map k a ->
      Sem r (Map Text a)
    mkSimpleNameMap field' members map' =
      fmap (Map.fromList . catMaybes) . forM members $ \member -> do
        case Map.lookup (field' member) map' of
          Nothing -> do
            Logger.error $ Nnu.memberName member <> " not found"
            return Nothing
          Just x -> pure $ Just (Nnu.memberName member, x)

diff ::
  forall r.
  ( Members '[Reader AppConfig] r
  ) =>
  SimpleNameMap ->
  SimpleNameMap ->
  Sem r [DiffMember]
diff oldData newData = do
  Nnu.Group {members} <- asks group
  let updates = do
        member@Nnu.Member {..} <- members
        [Just oldName, Just newName] <-
          pure $ Map.lookup memberName <$> [oldData, newData]
        guard (oldName /= newName)
        return DiffMember {..}
  return updates

data TweetResult
  = TweetSucceeded
  | TweetAlreadyPosted
  | TweetSkipped
  | TweetFailed TweetError
  deriving stock (Show, Eq, Ord)

data TweetError
  = PrecheckFailed A.Value
  | TweetPostFailed A.Value
  | PostDbUpdateFailed A.Value
  deriving stock (Show, Eq, Ord)

-- |
-- 名前の変更をツイートする。
tweetNameUpdate ::
  forall r r'.
  ( Members
      '[ State AppState
       , Log
       , Twitter
       , NnuDb
       ]
      r
  , r' ~ (Error TweetError : r)
  ) =>
  DiffMember ->
  Sem r TweetResult
tweetNameUpdate d@DiffMember {..}
  | isPossiblyPersonalInformation newName
      || isPossiblyPersonalInformation oldName = do
    Logger.error $ "possibly personal information: " <> memberName
    pure TweetSkipped
  | otherwise =
    handleError $
      isAlreadyPosted >>= \case
        Nothing -> tweetAndUpdateDb
        Just reason -> justLog reason
  where
    tweetAndUpdateDb :: Sem r' TweetResult
    tweetAndUpdateDb =
      Twitter.tweet msg >>= \case
        Right tw -> do
          updateDb tw d
            & mapError (PostDbUpdateFailed . A.toJSON @Db.UpdateCurrentNameError)
            & mapError (PostDbUpdateFailed . A.toJSON @Db.PutHistoryError)
          Logger.debug logMsg
          pure TweetSucceeded
        Left err ->
          Polysemy.throw $ TweetPostFailed $ A.toJSON err

    justLog :: Text -> Sem r' TweetResult
    justLog reason = do
      Logger.info $
        A.object
          [ "message" A..= ("This tweet is already posted" :: Text)
          , "reason" A..= reason
          , "original" A..= logMsg
          ]
      pure TweetAlreadyPosted

    -- ツイート済みのときは理由を返す
    isAlreadyPosted :: Sem r' (Maybe Text)
    isAlreadyPosted = do
      isInPendingItem >>= \case
        True -> pure (Just "newName is in dbPendingItems")
        False ->
          isEqualToDbCurrentName >>= \case
            True -> pure (Just "DB's current name == newName")
            False -> pure Nothing
      where
        isInPendingItem :: Sem r' Bool
        isInPendingItem = do
          mItem <- Map.lookup member <$> getDbPendigItems
          case mItem of
            Just (DbPendingItem (Just Db.UpdateCurrentNameItem {twitterName}) _) ->
              return $ twitterName == newName
            _ -> return False

        isEqualToDbCurrentName :: Sem r' Bool
        isEqualToDbCurrentName = do
          current <- getCurrentName member
          pure (current == newName)

        getCurrentName :: Nnu.Member -> Sem r' Text
        getCurrentName m = do
          r <- fmap (view (field @"twitterName")) <$> Db.getCurrentName m
          fromEither $ mapLeft (PrecheckFailed . A.toJSON) r

    handleError :: Sem r' TweetResult -> Sem r TweetResult
    handleError m =
      runError m >>= \case
        Right r -> pure r
        Left e@(TweetPostFailed v) | "Status is a duplicate." `T.isInfixOf` tshow e -> do
          Logger.warn $
            A.object
              [ "message" A..= ("This tweet is already posted" :: Text)
              , "error" A..= v
              , "original" A..= logMsg
              ]
          pure TweetAlreadyPosted
        Left e@(TweetPostFailed v) -> do
          Logger.error $
            A.object
              [ "message" A..= ("Tweet post failed. Will be retried in the next loop" :: Text)
              , "error" A..= v
              , "original" A..= logMsg
              ]
          pure $ TweetFailed e
        Left e@(PrecheckFailed v) -> do
          Logger.error $
            A.object
              [ "message" A..= ("DB error" :: Text)
              , "error" A..= v
              , "original" A..= logMsg
              , "note" A..= ("Tweet is not posted yet. Will be retried in the next loop" :: Text)
              ]
          pure $ TweetFailed e
        Left e@(PostDbUpdateFailed v) -> do
          Logger.error $
            A.object
              [ "message" A..= ("DB error" :: Text)
              , "error" A..= v
              , "original" A..= logMsg
              , "note" A..= ("Tweet is posted successfully" :: Text)
              ]
          pure $ TweetFailed e

    isPossiblyPersonalInformation = T.all isAscii

    Nnu.Member {memberName, screenName} = member
    msg =
      T.unlines
        [ memberName <> "(" <> url' <> ")" <> "さんが名前を変更しました"
        , ""
        , newName
        , "⇑"
        , oldName
        ]
    logMsg =
      A.object
        [ "message" A..= ("twitter name changed" :: Text)
        , "member" A..= memberName
        , "before" A..= oldName
        , "after" A..= newName
        ]
    url' = "twitter.com/" <> screenName

-- |
-- 名前の変更をDBに保存する。
updateDb ::
  forall r.
  ( Members
      '[ State AppState
       , Error Db.UpdateCurrentNameError
       , Error Db.PutHistoryError
       , NnuDb
       ]
      r
  ) =>
  Twitter.TweetResp ->
  DiffMember ->
  Sem r ()
updateDb Twitter.TweetResp {..} DiffMember {..} = do
  Db.updateCurrentName nameUpdateItem >>= \case
    Right () ->
      -- 更新できたのでpendingItemsから削除
      updatePendingItem member . applys $
        [ field @"updateCurrentNameItem" .~ Nothing
        ]
    Left e -> do
      updatePendingItem member . applys $
        [ field @"updateCurrentNameItem" .~ Just nameUpdateItem
        , field @"historyItems" %~ Set.insert historyItem
        ]
      Polysemy.throw e
  Db.putHistory historyItem >>= \case
    Right () ->
      -- 履歴は永続的なのでpendingItemsから削除しない
      pure ()
    Left e -> do
      updatePendingItem member . applys $
        [ field @"historyItems" %~ Set.insert historyItem
        ]
      Polysemy.throw e
  where
    nameUpdateItem =
      Db.UpdateCurrentNameItem
        { member = member
        , twitterName = newName
        , updateTime = createdAt
        }
    historyItem =
      Db.HistoryItem
        { member = member
        , twitterName = newName
        , tweetId = tweetId
        , updateTime = createdAt
        }

-- |
-- エラーで投入できなかったアイテムを再投入する。
-- 再びエラーできた場合はログに出力する。
retryUpdateDb ::
  forall r r'.
  ( Members
      '[ State AppState
       , Log
       , NnuDb
       ]
      r
  , r' ~ (Error Db.UpdateCurrentNameError : Error Db.PutHistoryError : r)
  ) =>
  Sem r ()
retryUpdateDb = ignoreDbError $ do
  pendingItems <- getDbPendigItems
  when (Map.size pendingItems > 0) $ do
    logCurrentPendingItems pendingItems
    forM_ (Map.toList pendingItems) $
      \(member, DbPendingItem {updateCurrentNameItem, historyItems}) -> do
        forM_ updateCurrentNameItem $ \item -> do
          Db.updateCurrentName item >>= fromEither
          updatePendingItem member $ field @"updateCurrentNameItem" .~ Nothing
        forM_ historyItems $ \historyItem -> do
          Db.putHistory historyItem >>= fromEither
          updatePendingItem member $ field @"historyItems" %~ Set.delete historyItem
    logRetryFinished
  where
    logCurrentPendingItems :: Map Nnu.Member DbPendingItem -> Sem r' ()
    logCurrentPendingItems pendingItems = do
      Logger.debug $
        A.object
          [ "message" A..= ("dbPendingItems remaining" :: Text)
          , "dbPendingItems" A..= Map.mapKeys Nnu.memberName pendingItems
          ]

    logRetryFinished :: Sem r' ()
    logRetryFinished = do
      Logger.debug $
        A.object
          [ "message" A..= ("All pending items successfully posted" :: Text)
          ]

    ignoreDbError :: Sem r' () -> Sem r ()
    ignoreDbError m =
      runError (runError m) >>= \case
        _ -> pure ()

applys :: [a -> a] -> a -> a
applys = appEndo . foldMap Endo
