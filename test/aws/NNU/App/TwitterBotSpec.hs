{-# LANGUAGE BlockArguments #-}

module NNU.App.TwitterBotSpec (
  spec,
) where

import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Reader as Polysemy
import qualified Polysemy.State as Polysemy

import qualified Data.Aeson as J
import Data.Generics.Product (HasField(field))
import RIO hiding (logError, when)
import qualified RIO.Map as M
import qualified RIO.Map as Map
import RIO.Orphans
import RIO.Partial (toEnum)
import qualified RIO.Text as T
import RIO.Time (
  TimeZone (TimeZone),
  UTCTime (UTCTime),
  ZonedTime,
  utcToZonedTime,
 )
import Test.Hspec
import qualified Web.Twitter.Conduit as OrigTwitter

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
  { twListsMembersResp :: [Either Twitter.ListsMembersError Twitter.ListsMembersResp]
  , twTweetResp :: [Either Twitter.TweetError Twitter.TweetResp]
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
      [(member ^. field @"memberName", x) | x@Db.UpdateCurrentNameItem {..} <- xs]

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
     , Polysemy.State [Either Twitter.ListsMembersError ListsMembersResp] -- api call response
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
  pure $ s ^. field @"nameMapCache"

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

mockDbInitialState :: MockDbState
mockDbInitialState =
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
            { dbInitialState = mockDbInitialState
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
