{-# LANGUAGE BlockArguments #-}

-- | White-box tests for the 'NNU.App.TwitterBot' module.
module NNU.App.TwitterBotSpec (
  spec,
) where

import Polysemy
import Polysemy.Error as Polysemy
import Polysemy.Reader as Polysemy
import Polysemy.State as Polysemy

import qualified Data.Aeson as J
import Data.Generics.Product (HasField (field))
import RIO hiding (logError, when)
import qualified RIO.HashMap as HM
import qualified RIO.Map as M
import qualified RIO.Map as Map
import RIO.Orphans (ResourceMap)
import RIO.Partial (toEnum)
import qualified RIO.Partial as Partial
import qualified RIO.Text as T
import RIO.Time (
  TimeZone (TimeZone),
  UTCTime (UTCTime),
  ZonedTime,
  utcToZonedTime,
 )
import qualified Web.Twitter.Conduit as OrigTwitter

import NNU.App.TwitterBot (
  AppConfig (..),
  DbPendingItem,
  LoopConfig (..),
  normalizePendingItems,
 )
import qualified NNU.App.TwitterBot as Bot
import qualified NNU.Effect.Db as Db
import qualified NNU.Effect.Log as Log
import qualified NNU.Effect.Twitter as Twitter
import NNU.Nijisanji (
  Group (..),
  GroupName (Other),
 )
import qualified NNU.Nijisanji as NNU
import qualified NNU.Prelude

import NNU.Effect.Sleep (Sleep)
import qualified NNU.Effect.Sleep as Sleep
import Test.Hspec
import NNU.Prelude (when)

-------------------------------------------------------------------------------
-- Mock

data ResultRecord = ResultRecord
  { dbState :: IORef MockDbState
  , logRecord :: IORef [J.Value]
  , tweetRecord :: IORef [Text]
  , appState :: IORef Bot.AppState
  }
  deriving stock (Generic)

data MockConfig = MockConfig
  { twListsMembersResp :: [Either Twitter.ListsMembersError Twitter.ListsMembersResp]
  , twTweetResp :: [Either Twitter.TweetError Twitter.TweetResp]
  , dbInitialState :: MockDbState
  , dbMockConfig :: MockDbConfig
  , loopConfig :: Bot.LoopConfig
  }

data MockDbState = MockDbState
  { nameMap :: Map Text Db.CurrentNameItem
  , getCurrentNameCount :: Int
  , getCurrentNamesOfGroup :: Int
  , updateCurrentNameCount :: Int
  , putHistoryCount :: Int
  , getHistoryCount :: Int
  }
  deriving (Generic)

data MockDbConfig = MockDbConfig
  { -- | @errorIfMatch action nth == Just err@ means that @action@ will throw @err@ on @nth@ call.
    --   @Nothing@ means that @action@ will not throw any error.
    --   @nth@ starts from 1.
    --   On counting the number of calls, fields of actions are ignored. i.e.,
    --   There is no distinguish between @GetCurrentName "tanaka"@ and @GetCurrentName "suzuki"@.
    --   Each of them is counted as one call of the same @GetCurrentName@ action.
    errorIfMatch :: forall k (m :: k) a. Db.NnuDb m a -> Int -> Maybe Text
  }

mockDbStateFromList :: [Db.CurrentNameItem] -> MockDbState
mockDbStateFromList xs =
  MockDbState
    { nameMap = Map.fromList [(memberName, x) | x@Db.CurrentNameItem {..} <- xs]
    , getCurrentNameCount = 0
    , getCurrentNamesOfGroup = 0
    , updateCurrentNameCount = 0
    , putHistoryCount = 0
    , getHistoryCount = 0
    }

runApp :: ResourceMap -> MockConfig -> IO ResultRecord
runApp resourceMap mockConfig = do
  -- Record
  dbState <- newIORef (dbInitialState mockConfig)
  logRecord <- newIORef @_ @[J.Value] []
  tweetRecord <- newIORef @_ @[Text] []
  appState <- newIORef Bot.initialAppState
  let appConfig = AppConfig {group = mockGroup}
  runFinal
    . Polysemy.embedToFinal @IO
    -- mock log
    . Polysemy.runStateIORef logRecord
    . runLogMock
    . throwError @String
    -- mock sleep
    . runSleepMock
    -- mock tweet
    . Polysemy.evalState (twListsMembersResp mockConfig)
    . Polysemy.evalState (twTweetResp mockConfig)
    . Polysemy.runStateIORef tweetRecord
    . runTwitterMock
    -- mock db
    . Polysemy.runReader (dbMockConfig mockConfig)
    . Polysemy.runStateIORef dbState
    . runDbMock
    -- application config and state
    . Polysemy.runReader resourceMap
    . Polysemy.runStateIORef appState
    . Polysemy.runReader appConfig
    $ Bot.app (loopConfig mockConfig)
      >> normalizePendingItems
  pure ResultRecord {..}

throwError ::
  forall e r a.
  Show e =>
  Polysemy.Member (Embed IO) r =>
  Sem (Polysemy.Error e ': r) a ->
  Sem r a
throwError action =
  Polysemy.runError action >>= \case
    Left e -> embed @IO $ throwIO (stringException $ show e)
    Right a -> pure a

runLogMock ::
  Polysemy.Member (Polysemy.State [J.Value]) r =>
  Sem (Log.Log ': r) a ->
  Sem r a
runLogMock = interpret $ \case
  Log.Log _ _ msg -> Polysemy.modify' (msg :)

runTwitterMock ::
  forall r a.
  Polysemy.Members
    '[ Polysemy.Error String
     , Polysemy.State [Text] -- record tweet
     , Polysemy.State [Either Twitter.ListsMembersError Twitter.ListsMembersResp] -- api call response
     , Polysemy.State [Either Twitter.TweetError Twitter.TweetResp] -- api call response
     ]
    r =>
  Sem (Twitter.Twitter ': r) a ->
  Sem r a
runTwitterMock = interpret $ \case
  Twitter.ListsMembers _ -> do
    mockSimpleAction "listMembers"
  Twitter.Tweet body -> do
    tw <- mockSimpleAction "tweet"
    when (isRight tw) do
      Polysemy.modify' @[Text] (body :)
    pure tw
  where
    mockSimpleAction ::
      forall x e r'.
      Polysemy.Member (Polysemy.Error String) r' =>
      Polysemy.Member (Polysemy.State [Either e x]) r' =>
      String ->
      Sem r' (Either e x)
    mockSimpleAction name = do
      Polysemy.get @[Either e x] >>= \case
        x : xs -> do
          Polysemy.put xs
          pure x
        [] -> Polysemy.throw @String $ name <> " called too many times"

runDbMock ::
  forall r a.
  ( Member (State MockDbState) r
  , Member (Reader MockDbConfig) r
  ) =>
  Sem (Db.NnuDb ': r) a ->
  Sem r a
runDbMock = do
  interpret \e -> do
    MockDbConfig {errorIfMatch} <- ask @MockDbConfig
    case e of
      Db.GetCurrentName member -> do
        n <- increment (field @"getCurrentNameCount")
        case errorIfMatch e n of
          Just err -> mkErr Db.GetCurrentNameError err
          Nothing -> Right <$> getCurrentName_ member
      Db.GetCurrentNamesOfGroup group -> do
        n <- increment (field @"getCurrentNamesOfGroup")
        case errorIfMatch e n of
          Just err -> mkErr Db.GetCurrentNamesOfGroupError err
          Nothing -> Right <$> getCurrentNamesOfGroup_ group
      Db.UpdateCurrentName name -> do
        n <- increment (field @"updateCurrentNameCount")
        case errorIfMatch e n of
          Just err -> mkErr Db.UpdateCurrentNameError err
          Nothing -> Right <$> updateCurrentName_ name
      Db.PutHistory _ -> do
        n <- increment (field @"putHistoryCount")
        case errorIfMatch e n of
          Just err -> mkErr Db.PutHistoryError err
          Nothing -> pure $ Right ()
      Db.GetHistroy _ -> error "not implemented"
  where
    getCurrentName_ :: NNU.Member -> Sem r Db.CurrentNameItem
    getCurrentName_ member = do
      let memberName = member ^. field @"memberName"
      MockDbState {nameMap} <- Polysemy.get @MockDbState
      pure $ Partial.fromJust $ Map.lookup memberName nameMap
    getCurrentNamesOfGroup_ :: NNU.Group -> Sem r [Db.CurrentNameItem]
    getCurrentNamesOfGroup_ Group {members} = do
      mapM getCurrentName_ members
    updateCurrentName_ :: Db.UpdateCurrentNameItem -> Sem r ()
    updateCurrentName_ Db.UpdateCurrentNameItem {..} = do
      let memberName = member ^. field @"memberName"
          cni = Db.CurrentNameItem {..}
      Polysemy.modify' @MockDbState $ field @"nameMap" %~ Map.insert memberName cni

    increment :: Lens' MockDbState Int -> Sem r Int
    increment l = do
      Polysemy.modify' $ l %~ (+ 1)
      Polysemy.gets (view l)

    mkErr :: forall e b. (J.Value -> e) -> Text -> Sem r (Either e b)
    mkErr con err = pure . Left . con . J.String $ err

runSleepMock :: Sem (Sleep ': r) a -> Sem r a
runSleepMock = interpret $ \case
  Sleep.SleepSec _ -> pure ()

-- Mock Operation
-----------------

getLogRecord :: MonadIO m => ResultRecord -> m [J.Value]
getLogRecord ResultRecord {..} = reverse <$> readIORef logRecord

getTweetRecord :: MonadIO m => ResultRecord -> m [Text]
getTweetRecord ResultRecord {..} = reverse <$> readIORef tweetRecord

getCurrentState :: MonadIO m => ResultRecord -> m (Maybe (Map Text Text))
getCurrentState ResultRecord {..} = view (field @"nameMapCache") <$> readIORef appState

getDbPendingItems :: MonadIO m => ResultRecord -> m (Map NNU.Member DbPendingItem)
getDbPendingItems ResultRecord {..} = view (field @"dbPendingItems") <$> readIORef appState

-- Mock Data
----------------

mockGroup :: Group
mockGroup =
  Group
    { groupLabel = mockGroupName
    , listId = 0
    , members = [mockMember1, mockMember2]
    }

mockGroupName :: GroupName
mockGroupName = Other "mock"

mockMember1 :: NNU.Member
mockMember1 = NNU.Member mockGroupName "Yamada Taro" "t_yamada" 1

mockMember2 :: NNU.Member
mockMember2 = NNU.Member mockGroupName "Tanaka Hanako" "h_tanaka" 2

mkTwitterUser1 :: Text -> Twitter.User
mkTwitterUser1 = Twitter.User 1

mkTwitterUser2 :: Text -> Twitter.User
mkTwitterUser2 = Twitter.User 2

mockDbinitialState :: MockDbState
mockDbinitialState =
  mockDbStateFromList
    [ Db.CurrentNameItem
        { memberName = mockMember1 ^. field @"memberName"
        , updateTime = testTime
        , twitterName = "Yamada Mark-Ⅱ"
        }
    , Db.CurrentNameItem
        { memberName = mockMember2 ^. field @"memberName"
        , updateTime = testTime
        , twitterName = "Tanaka Mark-Ⅱ"
        }
    ]

-------------------------------------------------------------------------------
-- Spec

spec :: ResourceMap -> Spec
spec resourceMap = do
  let runApp' = runApp resourceMap
  describe "twitter name change" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbinitialState
            , dbMockConfig = MockDbConfig {errorIfMatch = \_ _ -> Nothing}
            , twListsMembersResp =
                [ Right
                    Twitter.WithCursor
                      { nextCursor = Nothing
                      , previousCursor = Nothing
                      , contents =
                          [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                          , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                          ]
                      }
                , Right
                    Twitter.WithCursor
                      { nextCursor = Nothing
                      , previousCursor = Nothing
                      , contents =
                          [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                          , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                          ]
                      }
                ]
            , twTweetResp =
                [ Right
                    Twitter.TweetResp
                      { tweetId = 0
                      , createdAt = testTime
                      }
                ]
            , loopConfig = LoopConfig {loopCount = Just 2, loopDelaySec = 0}
            }
    result <- runIO $ runApp' mockConfig
    it "is logged" $ do
      getLogRecord result
        `shouldReturn` [ J.object
                          [ "message" J..= ("twitter name changed" :: Text)
                          , "member" J..= ("Yamada Taro" :: Text)
                          , "before" J..= ("Yamada Mark-Ⅱ" :: Text)
                          , "after" J..= ("Yamada Mark-Ⅲ" :: Text)
                          ]
                       ]
    it "is tweeted" $ do
      getTweetRecord result
        `shouldReturn` [ T.unlines
                          [ "Yamada Taro(twitter.com/t_yamada)さんが名前を変更しました"
                          , ""
                          , "Yamada Mark-Ⅲ"
                          , "⇑"
                          , "Yamada Mark-Ⅱ"
                          ]
                       ]
    it "alters state" $ do
      getCurrentState result
        `shouldReturn` Just
          ( M.fromList
              [("Tanaka Hanako", "Tanaka Mark-Ⅱ"), ("Yamada Taro", "Yamada Mark-Ⅲ")]
          )
  describe "user not in list" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbinitialState
            , dbMockConfig = MockDbConfig {errorIfMatch = \_ _ -> Nothing}
            , twListsMembersResp =
                [ Right
                    Twitter.WithCursor
                      { nextCursor = Nothing
                      , previousCursor = Nothing
                      , contents = [mkTwitterUser1 "Yamada Mark-Ⅱ"]
                      }
                ]
            , twTweetResp = []
            , loopConfig = LoopConfig {loopCount = Just 1, loopDelaySec = 0}
            }
    result <- runIO $ runApp' mockConfig
    it "is logged" $ do
      getLogRecord result `shouldReturn` ["Tanaka Hanako not found"]
    it "is not tweeted" $ do
      getTweetRecord result `shouldReturn` []
    it "alters state" $ do
      getCurrentState result
        `shouldReturn` Just (M.fromList [("Yamada Taro", "Yamada Mark-Ⅱ")])
  describe "error in listsMembers response" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbinitialState
            , dbMockConfig = MockDbConfig {errorIfMatch = \_ _ -> Nothing}
            , twListsMembersResp = [Left (Twitter.ListsMembersError "Twitter Down")]
            , twTweetResp = []
            , loopConfig = LoopConfig {loopCount = Just 1, loopDelaySec = 0}
            }
    result <- runIO $ runApp' mockConfig
    it "is logged" $ do
      logs <- getLogRecord result
      logs `shouldSatisfy` \case
        [J.String e] -> "Twitter Down" `T.isInfixOf` e
        _ -> False
    it "is not tweeted" $ do
      getTweetRecord result `shouldReturn` []
  describe "error in first tweet post" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbinitialState
            , dbMockConfig = MockDbConfig {errorIfMatch = \_ _ -> Nothing}
            , twListsMembersResp =
                [ Right
                    Twitter.WithCursor
                      { nextCursor = Nothing
                      , previousCursor = Nothing
                      , contents =
                          [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                          , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                          ]
                      }
                , Right
                    Twitter.WithCursor
                      { nextCursor = Nothing
                      , previousCursor = Nothing
                      , contents =
                          [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                          , mkTwitterUser2 "Tanaka Mark-Ⅲ"
                          ]
                      }
                ]
            , twTweetResp =
                [ Left (Twitter.TweetError "Twitter Down")
                , Right
                    Twitter.TweetResp
                      { tweetId = 0
                      , createdAt = testTime
                      }
                ]
            , loopConfig = LoopConfig {loopCount = Just 2, loopDelaySec = 0}
            }
    result <- runIO $ runApp' mockConfig
    it "is logged" $ do
      logs <- getLogRecord result
      logs `shouldSatisfy` any \case
        J.Object o
          | Just (J.String e) <- HM.lookup "error" o ->
            "Twitter Down" `T.isInfixOf` e
        J.String e -> "Twitter Down" `T.isInfixOf` e
        _ -> False
    it "does not affect other tweets" $ do
      tweetRecord <- getTweetRecord result
      tweetRecord `shouldSatisfy` (length >>> (== 1))
  describe "retry after error on tweet post" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbinitialState
            , dbMockConfig = MockDbConfig {errorIfMatch = \_ _ -> Nothing}
            , twListsMembersResp =
                [ Right . mkTwitterListMembersResp $
                    [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                    , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                    ]
                , Right . mkTwitterListMembersResp $
                    [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                    , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                    ]
                , Right . mkTwitterListMembersResp $
                    [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                    , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                    ]
                ]
            , twTweetResp =
                [ Left (Twitter.TweetError "Twitter Down")
                , Right
                    Twitter.TweetResp
                      { tweetId = 0
                      , createdAt = testTime
                      }
                ]
            , loopConfig = LoopConfig {loopCount = Just 3, loopDelaySec = 0}
            }
    result <- runIO $ runApp' mockConfig
    it "is retried in the next loop" $ do
      getTweetRecord result
        `shouldReturn` [ T.unlines
                          [ "Yamada Taro(twitter.com/t_yamada)さんが名前を変更しました"
                          , ""
                          , "Yamada Mark-Ⅲ"
                          , "⇑"
                          , "Yamada Mark-Ⅱ"
                          ]
                       ]

-------------------------------------------------------------------------------

testTime :: ZonedTime
testTime = utcToZonedTime jst $ UTCTime (toEnum 0) 0
  where
    jst =
      TimeZone
        { timeZoneMinutes = 9 * 60
        , timeZoneSummerOnly = False
        , timeZoneName = "JST"
        }
