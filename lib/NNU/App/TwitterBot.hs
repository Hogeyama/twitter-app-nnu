{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NNU.App.TwitterBot (
  app,
  Effs,
  AppConfig (..),
  LoopConfig (..),
  nijisanjiAppConfig,
  gamersAppConfig,
  seedsAppConfig,
  since2019AppConfig,
  testAppConfig,
  -- for test
  AppState (..),
  initialAppState,
) where

import Polysemy
import Polysemy.Error as Polysemy
import Polysemy.Reader as Polysemy
import Polysemy.State as Polysemy

import qualified Data.Aeson as A
import Data.Generics.Product (HasAny (the))
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
  { store :: Maybe SimpleNameMap
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
getDbPendigItems = gets dbPendingItems

updatePendingItem ::
  ( Member (State AppState) r
  ) =>
  Nnu.Member ->
  (DbPendingItem -> DbPendingItem) ->
  Sem r ()
updatePendingItem member f = do
  modify' $
    the @"dbPendingItems"
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
  modify' $ the @"dbPendingItems" %~ Map.filter (/= emptyDbPendingItem)

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
  , loopCount :: Maybe Int
  }
  deriving stock (Show, Eq, Ord, Generic)

type Effs =
  '[ Reader AppConfig
   , State AppState
   , Log
   , Twitter
   , NnuDb
   , Sleep
   ]

-- TODO Error を union でまとめる
newtype TwitterGetCurrentNameError = TwitterGetCurrentNameError Twitter.Error deriving newtype (A.ToJSON)
newtype TwitterPostError = TwitterPostError Twitter.Error deriving newtype (A.ToJSON)
newtype DbUpdateCurrentNameError = DbUpdateCurrentNameError Db.Error deriving newtype (A.ToJSON)
newtype DbPutHistoryError = DbPutHistoryError Db.Error deriving newtype (A.ToJSON)

app ::
  forall r.
  Members Effs r =>
  LoopConfig ->
  Sem r ()
app LoopConfig {..} = do
  let loop = case loopCount of
        Nothing -> forever
        Just n -> forM_ [1 .. n] . const
  loop $ do
    logAndIgnoreError oneLoop
    Sleep.sleepSec loopDelaySec
  where
    logAndIgnoreError m =
      m
        & logAndIgnoreError1 (Logger.info @_ @TwitterGetCurrentNameError)
        & logAndIgnoreError1 (Logger.warn @_ @TwitterPostError)
        & logAndIgnoreError1 (Logger.error @_ @DbUpdateCurrentNameError)
        & logAndIgnoreError1 (Logger.error @_ @DbPutHistoryError)

    logAndIgnoreError1 logger action =
      Polysemy.runError action >>= \case
        Right () -> pure ()
        Left e -> logger e

oneLoop ::
  forall r.
  ( Members
      '[ Reader AppConfig
       , State AppState
       , Polysemy.Error DbUpdateCurrentNameError
       , Polysemy.Error DbPutHistoryError
       , Polysemy.Error TwitterPostError
       , Polysemy.Error TwitterGetCurrentNameError
       , Log
       , Twitter
       , NnuDb
       ]
      r
  ) =>
  Sem r ()
oneLoop = do
  readData >>= \case
    Nothing -> fetchData >>= writeData
    Just oldData -> do
      retryUpdateDb
      -- newDataが欠けている場合はoldを使う
      newData <- Map.unionWith (\_o n -> n) oldData <$> fetchData
      posted <- filterM postTweet =<< calcDiff oldData newData
      writeData $ oldData `patch` posted
  where
    patch store p =
      Map.unionWith (\_o n -> n) store $
        Map.fromList [(Nnu.memberName member, newName) | DiffMember {..} <- p]

readData ::
  forall r.
  ( Members '[State AppState] r
  ) =>
  Sem r (Maybe SimpleNameMap)
readData = gets store

writeData ::
  forall r.
  ( Members '[State AppState] r
  ) =>
  SimpleNameMap ->
  Sem r ()
writeData d = modify' $ the @"store" .~ Just d

fetchData ::
  forall r.
  ( Members
      '[ Reader AppConfig
       , State AppState
       , Polysemy.Error DbUpdateCurrentNameError
       , Polysemy.Error TwitterGetCurrentNameError
       , Log
       , Twitter
       , NnuDb
       ]
      r
  ) =>
  Sem r SimpleNameMap
fetchData = do
  g@Nnu.Group {members} <- asks group
  if False
    then mkSimpleNameMap Nnu.memberName members =<< fetchDataFromDb g
    else mkSimpleNameMap Nnu.userId members =<< fetchDataFromTw g
  where
    fetchDataFromDb :: Nnu.Group -> Sem r SimpleNameMap
    fetchDataFromDb group = do
      Map.fromList
        . map (\Db.CurrentNameItem {..} -> (memberName, twitterName))
        <$> throwingError DbUpdateCurrentNameError (Db.getCurrentNamesOfGroup group)

    fetchDataFromTw :: Nnu.Group -> Sem r (Map Natural Text)
    fetchDataFromTw Nnu.Group {listId} = do
      let listParam = Twitter.ListIdParam $ fromIntegral listId
      res <- do
        let param = Twitter.listsMembers listParam & #count ?~ 1000
        Twitter.call' @_
          @(Twitter.WithCursor Integer Twitter.UsersCursorKey Twitter.User)
          param
      case res of
        Right Twitter.WithCursor {contents = users} ->
          pure $ Map.fromList $ map (\Twitter.User {..} -> (userId, userName)) users
        Left err -> do
          Polysemy.throw (TwitterGetCurrentNameError err)

    mkSimpleNameMap ::
      Ord k =>
      (Nnu.Member -> k) ->
      [Nnu.Member] ->
      Map k a ->
      Sem r (Map Text a)
    mkSimpleNameMap field members map' =
      fmap (Map.fromList . catMaybes) $
        forM members $ \member -> do
          case Map.lookup (field member) map' of
            Nothing -> do
              Logger.error $ Nnu.memberName member <> " not found"
              return Nothing
            Just x -> pure $ Just (Nnu.memberName member, x)

calcDiff ::
  forall r.
  Members '[Reader AppConfig] r =>
  SimpleNameMap ->
  SimpleNameMap ->
  Sem r [DiffMember]
calcDiff oldData newData = do
  Nnu.Group {members} <- asks group
  let updates = do
        member@Nnu.Member {..} <- members
        case (oldData, newData) & both %~ Map.lookup memberName of
          (Just oldName, Just newName)
            | oldName /= newName ->
              return DiffMember {..}
          _ -> []
  return updates

-- returns whether posted or not
postTweet ::
  forall r.
  ( Members
      '[ State AppState
       , Polysemy.Error DbUpdateCurrentNameError
       , Polysemy.Error DbPutHistoryError
       , Polysemy.Error TwitterPostError
       , Log
       , Twitter
       , NnuDb
       ]
      r
  ) =>
  DiffMember ->
  Sem r Bool
postTweet diff@DiffMember {..}
  | isPossiblyPersonalInformation newName
      || isPossiblyPersonalInformation oldName = do
    Logger.error $ "possibly personal information: " <> memberName
    pure False
  | otherwise =
    handleError $
      isAlreadyPosted >>= \case
        Nothing -> tweetAndUpdateDb
        Just reason -> justLog reason
  where
    tweetAndUpdateDb :: Sem r Bool
    tweetAndUpdateDb =
      Twitter.tweet msg >>= \case
        Right tw -> do
          updateDb tw diff
          Logger.debug logMsg
          pure True
        Left err ->
          Polysemy.throw (TwitterPostError err)

    justLog :: Text -> Sem r Bool
    justLog reason = do
      Logger.info $
        A.object
          [ "message" A..= ("tweet already posted" :: Text)
          , "reason" A..= reason
          , "original" A..= logMsg
          ]
      pure True

    handleError :: Sem r Bool -> Sem r Bool
    handleError m =
      m
        & catch `flip` \case DbUpdateCurrentNameError v -> handleDbUpdateError v
        & catch `flip` \case DbPutHistoryError v -> handleDbUpdateError v
        & catch `flip` \case TwitterPostError v -> handleTwitterPostError v
      where
        handleTwitterPostError v = do
          let isTweetDuplicated = "Status is a duplicate." `T.isInfixOf` tshow v
              log'
                | isTweetDuplicated = Logger.warn
                | otherwise = Logger.error
          log' $
            A.object
              [ "message" A..= ("tweet post failed" :: Text)
              , "error" A..= v
              , "original" A..= logMsg
              ]
          -- ツイート重複エラーのときは状態を更新してよい。
          -- XXX もう少しロバストな判定方法があればよいが……
          pure isTweetDuplicated
        handleDbUpdateError v = do
          Logger.error $
            A.object
              [ "message" A..= ("DB error" :: Text)
              , "error" A..= v
              , "original" A..= logMsg
              ]
          pure True

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

    -- ツイート済みのときは理由を返す
    isAlreadyPosted :: Sem r (Maybe Text)
    isAlreadyPosted = do
      isInPendingItem >>= \case
        True -> pure (Just "newName is in dbPendingItems")
        False ->
          isEqualToDbCurrentName >>= \case
            Right True -> pure (Just "DB's current name == newName")
            Right False -> pure Nothing
            Left (Db.Error e) -> do
              Logger.error $
                A.object
                  [ "message" A..= ("DB error" :: Text)
                  , "error" A..= e
                  , "original" A..= logMsg
                  ]
              pure Nothing
      where
        isInPendingItem :: Sem r Bool
        isInPendingItem = do
          mItem <- Map.lookup member <$> getDbPendigItems
          case mItem of
            Just (DbPendingItem (Just Db.UpdateCurrentNameItem {twitterName}) _) ->
              return $ twitterName == newName
            _ -> return False

        isEqualToDbCurrentName :: Sem r (Either Db.Error Bool)
        isEqualToDbCurrentName = do
          mCurrentDbName <- getCurrentName member
          case mCurrentDbName of
            Right current -> pure (Right (current == newName))
            Left e -> pure (Left e)

        getCurrentName :: Nnu.Member -> Sem r (Either Db.Error Text)
        getCurrentName m =
          fmap (view (the @"twitterName")) <$> Db.getCurrentName m

{-# ANN updateDb ("HLint: ignore Redundant id" :: String) #-}
updateDb ::
  forall r.
  ( Members
      '[ State AppState
       , Polysemy.Error DbUpdateCurrentNameError
       , Polysemy.Error DbPutHistoryError
       , NnuDb
       ]
      r
  ) =>
  Twitter.Tweet ->
  DiffMember ->
  Sem r ()
updateDb Twitter.Tweet {..} DiffMember {..} = do
  throwingError DbUpdateCurrentNameError (Db.updateCurrentName ucni)
    `onException` do
      updatePendingItem member $
        id
          . (the @"updateCurrentNameItem" .~ Just ucni)
          . (the @"historyItems" %~ Set.insert hi)
  updatePendingItem member $
    id
      . (the @"updateCurrentNameItem" .~ Nothing)
  throwingError DbPutHistoryError (Db.putHistory hi)
    `onException` do
      updatePendingItem member $
        id
          . (the @"historyItems" %~ Set.insert hi)
  where
    ucni =
      Db.UpdateCurrentNameItem
        { member = member
        , twitterName = newName
        , updateTime = createdAt
        }
    hi =
      Db.HistoryItem
        { member = member
        , twitterName = newName
        , tweetId = tweetId
        , updateTime = createdAt
        }

    -- XXX 忘れるのが怖い
    -- TODO Error を union でまとめる
    onException :: Sem r () -> Sem r () -> Sem r ()
    onException m n =
      m
        & catch @DbUpdateCurrentNameError `flip` \case _ -> n
        & catch @DbPutHistoryError `flip` \case _ -> n

retryUpdateDb ::
  forall r.
  ( Members
      '[ State AppState
       , Polysemy.Error DbUpdateCurrentNameError
       , Polysemy.Error DbPutHistoryError
       , Log
       , NnuDb
       ]
      r
  ) =>
  Sem r ()
retryUpdateDb = ignoreAnyError $ do
  normalizePendingItems
  pendingItems <- getDbPendigItems
  unless (Map.size pendingItems == 0) $ logCurrentPendingItems pendingItems
  forM_ (Map.toList pendingItems) $ \(member, DbPendingItem mucni his) -> do
    forM_ mucni $ \ucni -> do
      throwingError DbUpdateCurrentNameError (Db.updateCurrentName ucni)
      updatePendingItem member $ the @"updateCurrentNameItem" .~ Nothing
    forM_ his $ \hi -> do
      throwingError DbPutHistoryError (Db.putHistory hi)
      updatePendingItem member $ the @"historyItems" %~ Set.delete hi
  unless (Map.size pendingItems == 0) logRetryFinished
  where
    logCurrentPendingItems :: Map Nnu.Member DbPendingItem -> Sem r ()
    logCurrentPendingItems pendingItems = do
      Logger.debug $
        A.object
          [ "message" A..= ("dbPendingItems remaining" :: Text)
          , "dbPendingItems" A..= Map.mapKeys Nnu.memberName pendingItems
          ]

    logRetryFinished :: Sem r ()
    logRetryFinished = do
      Logger.debug $
        A.object
          [ "message" A..= ("All pending items successfully posted" :: Text)
          ]

    -- ここも忘れるの怖い
    ignoreAnyError :: Sem r () -> Sem r ()
    ignoreAnyError m =
      m
        & catch @DbUpdateCurrentNameError `flip` \case _ -> pure ()
        & catch @DbPutHistoryError `flip` \case _ -> pure ()

throwingError ::
  Members '[Polysemy.Error e] r =>
  (e' -> e) ->
  Sem r (Either e' a) ->
  Sem r a
throwingError wrap action =
  action >>= \case
    Right x -> pure x
    Left err -> Polysemy.throw (wrap err)
