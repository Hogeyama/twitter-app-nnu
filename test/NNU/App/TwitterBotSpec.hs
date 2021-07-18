{-# LANGUAGE BlockArguments #-}

module NNU.App.TwitterBotSpec
  ( spec
  ) where


import qualified Data.Aeson                    as J
import           Data.Dynamic                   ( fromDynamic
                                                , toDyn
                                                )
import           Data.Generics.Product          ( HasAny(the) )
import           Data.Typeable                  ( typeOf
                                                , typeRep
                                                )
import           NNU.App.TwitterBot             ( AppConfig(..)
                                                , AppState(..)
                                                , HasAppConfig(..)
                                                , HasAppState(..)
                                                , LoopConfig(..)
                                                , mainLoop
                                                )
import qualified NNU.Handler.Db                as Db
import qualified NNU.Handler.Twitter           as TwitterApi
import qualified NNU.Handler.Twitter           as Twitter
import           NNU.Logger                     ( LogFunc'
                                                , LogItem
                                                , mkLogFunc'
                                                )
import           NNU.Nijisanji                  ( Group(..)
                                                , GroupName(Other)
                                                , Member(Member)
                                                )
import           RIO                     hiding ( when )
import qualified RIO.HashMap                   as HM
import qualified RIO.Map                       as M
import           RIO.Orphans
import qualified RIO.Partial                   as Partial
import           RIO.Partial                    ( toEnum )
import qualified RIO.Text                      as T
import qualified RIO.Time                      as Time
import           RIO.Time                       ( TimeZone(TimeZone)
                                                , UTCTime(UTCTime)
                                                , ZonedTime
                                                , utcToZonedTime
                                                )
import           Test.Hspec

import qualified RIO.Map                       as Map
import           Web.Twitter.Conduit.Request.Internal
                                                ( rawParam )


-------------------------------------------------------------------------------
-- Mock

data MockEnv = MockEnv
  { appConfig   :: AppConfig
  , appState    :: AppState
  , logFunc     :: LogFunc'
  , twitter     :: Twitter.Handler (RIO MockEnv)
  , db          :: Db.Handler (RIO MockEnv)
  , dbState     :: IORef MockDbState
  , logRecord   :: IORef [J.Value]
  , tweetRecord :: IORef [Text]
  , resourceMap :: ResourceMap
  }
  deriving stock Generic
instance HasGLogFunc MockEnv where
  type GMsg MockEnv = LogItem
  gLogFuncL = the @"logFunc"
instance HasAppConfig MockEnv where
  appConfigL = the @"appConfig"
instance HasAppState MockEnv where
  appStateL = the @"appState"
instance Twitter.Has MockEnv where
  twitterApiL = the @"twitter"
instance Db.Has MockEnv where
  dbL = the @"db"
instance HasResourceMap MockEnv where
  resourceMapL = the @ResourceMap

data MockConfig = MockConfig
  { twListsMembersResp :: [Either SomeException ListsMembersResp]
  , twTweetResp        :: [Either SomeException Twitter.Tweet]
  , dbInitialState     :: MockDbState
  }

data MockDbState = MockDbState (Map Text Db.CurrentNameItem)

mockDbStateFromList :: [Db.CurrentNameItem] -> MockDbState
mockDbStateFromList xs = MockDbState
  $ Map.fromList [ (memberName, x) | x@Db.CurrentNameItem {..} <- xs ]

mockEnv :: forall m . (MonadUnliftIO m) => MockConfig -> m MockEnv
mockEnv MockConfig {..} = withResourceMap $ \resourceMap -> do
  tweetRecord          <- newIORef ([] :: [Text])
  (logFunc, logRecord) <- mockLogFunc
  appConfig            <- mockAppConfig
  appState             <- mockAppState
  mockListsMembers     <- mockSimpleAction "listsMembers" twListsMembersResp
  mockTweet            <- mockSimpleAction "tweet" twTweetResp
  dbState              <- newIORef dbInitialState
  let twitter = mockTwitterHandler mockListsMembers mockTweet tweetRecord
      db      = mockDbHandler
  pure MockEnv { .. }

mockTwitterHandler
  :: RIO MockEnv ListsMembersResp
  -> RIO MockEnv Twitter.Tweet
  -> IORef [Text]
  -> Twitter.Handler (RIO MockEnv)
mockTwitterHandler mockListsMembers mockTweet tweetRecord =
  Twitter.Handler $ \case
    r
      | isListsMemberQuery r -> do
        unsafeCoerceM' =<< mockListsMembers
      | isTweetQuery r -> do
        tryAny mockTweet >>= \case
          Right tweet -> do
            modifyIORef' tweetRecord
                         (Partial.fromJust (r ^. rawParam "status") :)
            unsafeCoerceM' tweet
          Left e -> throwIO e
      | otherwise -> do
        throwString $ "mockTwitter: Unexpected request: " <> show r
 where
  unsafeCoerceM'
    :: forall x y n . (Typeable x, Typeable y, MonadIO n) => x -> n y
  unsafeCoerceM' x = case fromDynamic (toDyn x) of
    Just y -> pure y
    Nothing ->
      throwIO
        $  stringException
        $  "unsafeCoerce': fail to  coerce "
        <> show (typeOf x)
        <> " to "
        <> show (typeRep (Proxy @y))

mockDbHandler :: Db.Handler (RIO MockEnv)
mockDbHandler = Db.Handler { .. }
 where
  getCurrentName member = do
    let memberName = member ^. the @"memberName"
    MockDbState m <- readIORef =<< view (to dbState)
    pure $ Partial.fromJust $ Map.lookup memberName m
  getCurrentNamesOfGroup Group { members } = mapM getCurrentName members
  updateCurrentName Db.UpdateCurrentNameItem {..} = do
    let memberName = member ^. the @"memberName"
        cni        = Db.CurrentNameItem { .. }
    ref <- view (to dbState)
    modifyIORef' ref $ \case
      MockDbState m -> MockDbState (Map.insert memberName cni m)
  putHistory _ = pure ()
  getHistroy _ = undefined

-- Mock Operation
-----------------

getLogRecord :: MonadIO m => MockEnv -> m [J.Value]
getLogRecord MockEnv {..} = reverse <$> readIORef logRecord

getTweetRecord :: MonadIO m => MockEnv -> m [Text]
getTweetRecord MockEnv {..} = reverse <$> readIORef tweetRecord

getCurrentState :: MonadIO m => MockEnv -> m (Maybe (Map Text Text))
getCurrentState MockEnv {..} = readIORef $ appState ^. the @"store"

-- Mock Internal
----------------

mockLogFunc :: MonadIO m => m (LogFunc', IORef [J.Value])
mockLogFunc = do
  ref <- newIORef []
  let logFunc = mkLogFunc' $ \_level value -> do
        case value of
          J.Object hm | Just body <- HM.lookup "body" hm ->
            modifyIORef' ref (body :)
          _ -> throwString "impossible"
        pure ()
  return (logFunc, ref)

mockAppConfig :: MonadIO m => m AppConfig
mockAppConfig = pure AppConfig { group = mockGroup }

mockAppState :: MonadIO m => m AppState
mockAppState = do
  store                 <- newIORef Nothing
  inconsistentDbMembers <- newIORef mempty
  pure AppState { .. }

mockSimpleAction
  :: (MonadIO m, MonadIO n) => String -> [Either SomeException a] -> m (n a)
mockSimpleAction name xs = do
  ref <- newIORef xs
  pure $ readIORef ref >>= \case
    [] ->
      throwString
        $  "mockAction `"
        <> name
        <> "` called more than "
        <> show (length xs)
        <> " times"
    y : ys -> do
      writeIORef ref ys
      either throwIO pure y

isListsMemberQuery :: Twitter.APIRequest name r -> Bool
isListsMemberQuery q =
  Twitter._url q == Twitter._url (Twitter.listsMembers mockListParam)

isTweetQuery :: Twitter.APIRequest name r -> Bool
isTweetQuery q = Twitter._url q == Twitter._url (Twitter.update "")

-- Mock Data
----------------

mockGroup :: Group
mockGroup = Group { groupLabel = mockGroupName
                  , listId     = 0
                  , members    = [mockMember1, mockMember2]
                  }

mockGroupName :: GroupName
mockGroupName = Other "mock"

mockMember1 :: Member
mockMember1 = Member mockGroupName "Yamada Taro" "t_yamada" 1

mockMember2 :: Member
mockMember2 = Member mockGroupName "Tanaka Hanako" "h_tanaka" 2

mkTwitterUser1 :: Text -> Twitter.User
mkTwitterUser1 = Twitter.User 1

mkTwitterUser2 :: Text -> Twitter.User
mkTwitterUser2 = Twitter.User 2

mockListId :: Natural
mockListId = 0

mockListParam :: Twitter.ListParam
mockListParam = Twitter.ListIdParam $ fromIntegral mockListId

mockDbInitialState :: MockDbState
mockDbInitialState = mockDbStateFromList
  [ Db.CurrentNameItem { memberName  = mockMember1 ^. the @"memberName"
                       , updateTime  = testTime
                       , twitterName = "Yamada Mark-Ⅱ"
                       }
  , Db.CurrentNameItem { memberName  = mockMember2 ^. the @"memberName"
                       , updateTime  = testTime
                       , twitterName = "Tanaka Mark-Ⅱ"
                       }
  ]

-------------------------------------------------------------------------------
-- Spec

type ListsMembersResp
  = Twitter.WithCursor Integer Twitter.UsersCursorKey Twitter.User

spec :: Spec
spec = do
  describe "twitter name change" $ do
    env <- runIO $ mockEnv MockConfig
      { dbInitialState     = mockDbInitialState
      , twListsMembersResp = [ Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                            ]
                               }
                             , Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                            ]
                               }
                             ]
      , twTweetResp        = [ Right Twitter.Tweet { tweetId   = 0
                                                   , createdAt = testTime
                                                   }
                             ]
      }
    runIO $ runRIO env $ mainLoop LoopConfig { loopCount    = Just 2
                                             , loopDelaySec = 0
                                             }
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
      getCurrentState env `shouldReturn` Just
        (M.fromList
          [("Tanaka Hanako", "Tanaka Mark-Ⅱ"), ("Yamada Taro", "Yamada Mark-Ⅲ")]
        )
  describe "user not in list" $ do
    env <- runIO $ mockEnv MockConfig
      { dbInitialState     = mockDbInitialState
      , twListsMembersResp = [ Right Twitter.WithCursor
                                 { nextCursor = Nothing
                                 , previousCursor = Nothing
                                 , contents = [mkTwitterUser1 "Yamada Mark-Ⅱ"]
                                 }
                             ]
      , twTweetResp        = []
      }
    runIO $ runRIO env $ mainLoop LoopConfig { loopCount    = Just 1
                                             , loopDelaySec = 0
                                             }
    it "is logged" $ do
      getLogRecord env `shouldReturn` ["Tanaka Hanako not found"]
    it "is not tweeted" $ do
      getTweetRecord env `shouldReturn` []
    it "alters state" $ do
      getCurrentState env
        `shouldReturn` Just (M.fromList [("Yamada Taro", "Yamada Mark-Ⅱ")])
  describe "error in listsMembers response" $ do
    env <- runIO $ mockEnv MockConfig
      { dbInitialState     = mockDbInitialState
      , twListsMembersResp = [ Left $ SomeException $ stringException
                                 "Twitter Down"
                             ]
      , twTweetResp        = []
      }
    runIO $ runRIO env $ mainLoop LoopConfig { loopCount    = Just 1
                                             , loopDelaySec = 0
                                             }
    it "is logged" $ do
      logs <- getLogRecord env
      logs `shouldSatisfy` \case
        [J.String e] -> "Twitter Down" `T.isInfixOf` e
        _            -> False
    it "is not tweeted" $ do
      getTweetRecord env `shouldReturn` []
  describe "error in first tweet post" $ do
    env <- runIO $ mockEnv MockConfig
      { dbInitialState = mockDbInitialState
      , twListsMembersResp = [ Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                            ]
                               }
                             , Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅲ"
                                            ]
                               }
                             ]
      , twTweetResp = [ Left $ SomeException $ stringException "Twitter Down"
                      , Right Twitter.Tweet { tweetId   = 0
                                            , createdAt = testTime
                                            }
                      ]
      }
    runIO $ runRIO env $ mainLoop LoopConfig { loopCount    = Just 2
                                             , loopDelaySec = 0
                                             }
    it "is logged" $ do
      logs <- getLogRecord env
      logs `shouldSatisfy` any \case
        J.Object o | Just (J.String e) <- HM.lookup "error" o ->
          "Twitter Down" `T.isInfixOf` e
        J.String e -> "Twitter Down" `T.isInfixOf` e
        _          -> False
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
    env <- runIO $ mockEnv MockConfig
      { dbInitialState = mockDbInitialState
      , twListsMembersResp = [ Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                            ]
                               }
                             , Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                            ]
                               }
                             , Right Twitter.WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                            , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                            ]
                               }
                             ]
      , twTweetResp = [ Left $ SomeException $ stringException "Twitter Down"
                      , Right Twitter.Tweet { tweetId   = 0
                                            , createdAt = testTime
                                            }
                      ]
      }
    runIO $ runRIO env $ mainLoop LoopConfig { loopCount    = Just 3
                                             , loopDelaySec = 0
                                             }
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
  jst = TimeZone { timeZoneMinutes    = 9 * 60
                 , timeZoneSummerOnly = False
                 , timeZoneName       = "JST"
                 }

