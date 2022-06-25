{-# LANGUAGE BlockArguments #-}

module NNU.App.TwitterBotSpec (
  spec,
) where

import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import qualified Polysemy.State as Polysemy

import qualified Data.Aeson as J
import Data.Data (eqT)
import Data.Generics.Product (HasAny (the))
import Data.Typeable (type (:~:) (Refl))
import RIO hiding (logError, when)
import qualified RIO.HashMap as HM
import qualified RIO.Map as M
import qualified RIO.Map as Map
import RIO.Orphans
import RIO.Partial (toEnum)
import qualified RIO.Partial as Partial
import qualified RIO.Text as T
import RIO.Time (
  TimeZone (TimeZone),
  UTCTime (UTCTime),
  ZonedTime,
  utcToZonedTime,
 )
import Test.Hspec
import Web.Twitter.Conduit.Request.Internal (
  rawParam,
 )

import NNU.App.TwitterBot (
  AppConfig (..),
  LoopConfig (..),
  initialAppState,
 )
import qualified NNU.App.TwitterBot as Bot
import qualified NNU.Effect.Db as Db
import qualified NNU.Effect.Db.DynamoDbImpl as DynamoDb
import qualified NNU.Effect.Log as Log
import NNU.Effect.Sleep (Sleep)
import qualified NNU.Effect.Sleep as Sleep
import qualified NNU.Effect.Twitter as Twitter
import NNU.Nijisanji (
  Group (..),
  GroupName (Other),
 )
import qualified NNU.Nijisanji as NNU
import NNU.Prelude (when)
import qualified NNU.Prelude

-------------------------------------------------------------------------------
-- Mock

data ResultRecord = ResultRecord
  { logRecord :: IORef [J.Value]
  , tweetRecord :: IORef [Text]
  , appState :: IORef Bot.AppState
  }
  deriving stock (Generic)

data MockConfig = MockConfig
  { twListsMembersResp :: [Either Twitter.Error ListsMembersResp]
  , twTweetResp :: [Either Twitter.Error Twitter.Tweet]
  , dbInitialState :: MockDbState
  , loopConfig :: Bot.LoopConfig
  }

newtype MockDbState = MockDbState
  { currentNameMap :: Map Text Db.UpdateCurrentNameItem
  }

mockDbStateFromList :: [Db.UpdateCurrentNameItem] -> MockDbState
mockDbStateFromList xs =
  MockDbState $
    Map.fromList
      [(member ^. the @"memberName", x) | x@Db.UpdateCurrentNameItem {..} <- xs]

runApp :: ResourceMap -> MockConfig -> IO ResultRecord
runApp resourceMap mockConfig = do
  logRecord <- newIORef @_ @[J.Value] []
  tweetRecord <- newIORef @_ @[Text] []
  appState <- newIORef initialAppState
  let resultRecord = ResultRecord {..}
  dynamoConfig <- DynamoDb.configLocalAwsFromEnv
  let appConfig = AppConfig {group = mockGroup}
  -- Setup dbInitialState
  forM_ (currentNameMap (dbInitialState mockConfig)) $ \nameItem -> do
    runFinal
      . Polysemy.embedToFinal @IO
      . Polysemy.runReader resourceMap
      . DynamoDb.runDynamoDb dynamoConfig
      $ Db.updateCurrentName nameItem
  -- Run app
  runFinal
    . Polysemy.embedToFinal @IO
    . Polysemy.runReader resourceMap
    -- mock sleep
    . runSleepMock
    -- mock log
    . Polysemy.runStateIORef logRecord
    . runLogMock
    . throwError @String
    -- mock tweet
    . Polysemy.evalState (twListsMembersResp mockConfig)
    . Polysemy.evalState (twTweetResp mockConfig)
    . Polysemy.runStateIORef tweetRecord
    . runTwitterMock
    -- mock db
    . DynamoDb.runDynamoDb dynamoConfig
    -- application config and state
    . Polysemy.runStateIORef appState
    . Polysemy.runReader appConfig
    $ Bot.app (loopConfig mockConfig)
  pure resultRecord

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
     , Polysemy.State [Either Twitter.Error ListsMembersResp] -- api call response
     , Polysemy.State [Either Twitter.Error Twitter.Tweet] -- api call response
     ]
    r =>
  Sem (Twitter.Twitter ': r) a ->
  Sem r a
runTwitterMock = interpret $ \case
  -- TODO Typeable が必要な時点で設計が間違っている
  Twitter.Call_ req :: Twitter.Twitter m a0
    | isListsMemberQuery req -> do
      case eqT @a0 @(Either Twitter.Error ListsMembersResp) of
        Just Refl -> mockSimpleAction @ListsMembersResp "listMembers"
        Nothing -> unknownRequestError req
    | isTweetQuery req -> do
      case eqT @a0 @(Either Twitter.Error Twitter.Tweet) of
        Just Refl -> do
          -- ロジックに踏み込み過ぎでは？という気もするが……
          tw <- mockSimpleAction @Twitter.Tweet "tweet"
          when (isRight tw) do
            Polysemy.modify' @[Text] (Partial.fromJust (req ^. rawParam "status") :)
          pure tw
        Nothing -> unknownRequestError req
    | otherwise -> unknownRequestError req
  where
    unknownRequestError :: forall apiName a0 b. Twitter.APIRequest apiName a0 -> Sem r b
    unknownRequestError req =
      Polysemy.throw $ "mockTwitter: Unexpected request: " <> show req

    mockSimpleAction ::
      forall x r'.
      Polysemy.Member (Polysemy.Error String) r' =>
      Polysemy.Member (Polysemy.State [Either Twitter.Error x]) r' =>
      String ->
      Sem r' (Either Twitter.Error x)
    mockSimpleAction name = do
      Polysemy.get @[Either Twitter.Error x] >>= \case
        x : xs -> do
          Polysemy.put xs
          pure x
        [] -> Polysemy.throw @String $ name <> " called too many times"

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
getCurrentState ResultRecord {..} = do
  s <- readIORef appState
  pure $ s ^. the @"store"

-- Mock Internal
----------------

isListsMemberQuery :: Twitter.APIRequest name r -> Bool
isListsMemberQuery q =
  Twitter._url q == Twitter._url (Twitter.listsMembers mockListParam)

isTweetQuery :: Twitter.APIRequest name r -> Bool
isTweetQuery q = Twitter._url q == Twitter._url (Twitter.statusesUpdate "")

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

mockListId :: Natural
mockListId = 0

mockListParam :: Twitter.ListParam
mockListParam = Twitter.ListIdParam $ fromIntegral mockListId

mockDbnitialState :: MockDbState
mockDbnitialState =
  mockDbStateFromList
    [ Db.UpdateCurrentNameItem
        { member = mockMember1
        , updateTime = testTime
        , twitterName = "Yamada Mark-Ⅱ"
        }
    , Db.UpdateCurrentNameItem
        { member = mockMember2
        , updateTime = testTime
        , twitterName = "Tanaka Mark-Ⅱ"
        }
    ]

-------------------------------------------------------------------------------
-- Spec

type ListsMembersResp =
  Twitter.WithCursor Integer Twitter.UsersCursorKey Twitter.User

spec :: ResourceMap -> Spec
spec resourceMap = do
  let runApp' = runApp resourceMap
  describe "twitter name change" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbnitialState
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
                    Twitter.Tweet
                      { tweetId = 0
                      , createdAt = testTime
                      }
                ]
            , loopConfig = LoopConfig {loopCount = Just 2, loopDelaySec = 0}
            }
    env <- runIO $ runApp' mockConfig
    it "is logged" $ do
      getLogRecord env
        `shouldReturn` [ J.object
                          [ "message" J..= ("twitter name changed" :: Text)
                          , "member" J..= ("Yamada Taro" :: Text)
                          , "before" J..= ("Yamada Mark-Ⅱ" :: Text)
                          , "after" J..= ("Yamada Mark-Ⅲ" :: Text)
                          ]
                       ]
    it "is tweeted" $ do
      getTweetRecord env
        `shouldReturn` [ T.unlines
                          [ "Yamada Taro(twitter.com/t_yamada)さんが名前を変更しました"
                          , ""
                          , "Yamada Mark-Ⅲ"
                          , "⇑"
                          , "Yamada Mark-Ⅱ"
                          ]
                       ]
    it "alters state" $ do
      getCurrentState env
        `shouldReturn` Just
          ( M.fromList
              [("Tanaka Hanako", "Tanaka Mark-Ⅱ"), ("Yamada Taro", "Yamada Mark-Ⅲ")]
          )
  describe "user not in list" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbnitialState
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
    env <- runIO $ runApp' mockConfig
    it "is logged" $ do
      getLogRecord env `shouldReturn` ["Tanaka Hanako not found"]
    it "is not tweeted" $ do
      getTweetRecord env `shouldReturn` []
    it "alters state" $ do
      getCurrentState env
        `shouldReturn` Just (M.fromList [("Yamada Taro", "Yamada Mark-Ⅱ")])
  describe "error in listsMembers response" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbnitialState
            , twListsMembersResp = [Left (Twitter.Error "Twitter Down")]
            , twTweetResp = []
            , loopConfig = LoopConfig {loopCount = Just 1, loopDelaySec = 0}
            }
    env <- runIO $ runApp' mockConfig
    it "is logged" $ do
      logs <- getLogRecord env
      logs `shouldSatisfy` \case
        [J.String e] -> "Twitter Down" `T.isInfixOf` e
        _ -> False
    it "is not tweeted" $ do
      getTweetRecord env `shouldReturn` []
  describe "error in first tweet post" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbnitialState
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
                [ Left (Twitter.Error "Twitter Down")
                , Right
                    Twitter.Tweet
                      { tweetId = 0
                      , createdAt = testTime
                      }
                ]
            , loopConfig = LoopConfig {loopCount = Just 2, loopDelaySec = 0}
            }
    env <- runIO $ runApp' mockConfig
    it "is logged" $ do
      logs <- getLogRecord env
      logs `shouldSatisfy` any \case
        J.Object o
          | Just (J.String e) <- HM.lookup "error" o ->
            "Twitter Down" `T.isInfixOf` e
        J.String e -> "Twitter Down" `T.isInfixOf` e
        _ -> False
    it "does not affect other tweets" $ do
      getTweetRecord env
        `shouldReturn` [ T.unlines
                          [ "Tanaka Hanako(twitter.com/h_tanaka)さんが名前を変更しました"
                          , ""
                          , "Tanaka Mark-Ⅲ"
                          , "⇑"
                          , "Tanaka Mark-Ⅱ"
                          ]
                       ]
  describe "error in first tweet post" $ do
    let mockConfig =
          MockConfig
            { dbInitialState = mockDbnitialState
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
                [ Left (Twitter.Error "Twitter Down")
                , Right
                    Twitter.Tweet
                      { tweetId = 0
                      , createdAt = testTime
                      }
                ]
            , loopConfig = LoopConfig {loopCount = Just 3, loopDelaySec = 0}
            }
    env <- runIO $ runApp' mockConfig
    it "is retried in the next loop" $ do
      getTweetRecord env
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
