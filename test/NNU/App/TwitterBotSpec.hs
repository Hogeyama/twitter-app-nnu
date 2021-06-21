{-# LANGUAGE BlockArguments #-}

module NNU.App.TwitterBotSpec
  ( spec
  ) where

import qualified Data.Aeson                    as J
import           Data.Generics.Product          ( HasAny(the) )
import           NNU.App.TwitterBot             ( AppConfig(..)
                                                , HasAppConfig(..)
                                                , LoopConfig(..)
                                                , mainLoop
                                                )
import           NNU.Handler.Twitter            ( APIRequest(_url)
                                                , HasTwitterAPI(..)
                                                , ListParam
                                                , Tweet(Tweet)
                                                , TwitterApi(TwitterApi)
                                                , User(User)
                                                , UsersCursorKey
                                                , WithCursor(..)
                                                )
import qualified NNU.Handler.Twitter           as TwitterApi
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
import           RIO.Partial                    ( toEnum )
import qualified RIO.Partial                   as Partial
import qualified RIO.Text                      as T
import qualified RIO.Time                      as Time
import           Test.Hspec
import           Test.Method                    ( ArgsMatcher(args)
                                                , Dynamic
                                                , FromDyn(fromDyn)
                                                , ToDyn(toDyn)
                                                , anything
                                                , mockup
                                                , thenAction
                                                , thenMethod
                                                , thenReturn
                                                , when
                                                )
import           Test.Method.Mock               ( Mock )
import           Web.Twitter.Conduit.Request.Internal
                                                ( rawParam )


-------------------------------------------------------------------------------
-- Mock

data MockEnv = MockEnv
  { appConfig   :: AppConfig
  , logFunc     :: LogFunc'
  , twitterApi  :: TwitterApi (RIO MockEnv)
  , logRecord   :: IORef [J.Value]
  , tweetRecord :: IORef [Text]
  }
  deriving stock Generic
instance HasGLogFunc MockEnv where
  type GMsg MockEnv = LogItem
  gLogFuncL = the @"logFunc"
instance HasAppConfig MockEnv where
  appConfigL = the @"appConfig"
instance HasTwitterAPI MockEnv where
  twitterApiL = the @"twitterApi"

data MockConfig = MockConfig
  { listsMembersResp :: [Either SomeException ListsMembersResp]
  , tweetResp        :: [Either SomeException Tweet]
  }

mockEnv :: forall m . MonadIO m => MockConfig -> m MockEnv
mockEnv MockConfig {..} = do
  tweetRecord          <- newIORef ([] :: [Text])
  (logFunc, logRecord) <- mockLogFunc
  appConfig            <- mockAppConfig
  mockListsMembers     <- mockSimpleAction "listsMembers" listsMembersResp
  mockTweet            <- mockSimpleAction "tweet" tweetResp
  let twitterApi = mockTwitterApi $ do
        when (args isListsMemberQuery) `thenAction` do
          toDyn <$> mockListsMembers
        when (args isTweetQuery) `thenMethod` \(SomeAPIRequest r) -> do
          tryAny mockTweet >>= \case
            Right tweet -> do
              modifyIORef' tweetRecord
                           (Partial.fromJust (r ^. rawParam "status") :)
              pure $ toDyn tweet
            Left e -> throwIO e
        -- for better error message
        when (args anything) `thenReturn` toDyn ()
  pure MockEnv { .. }

-- Mock Operation
-----------------

getLogRecord :: MonadIO m => MockEnv -> m [J.Value]
getLogRecord MockEnv {..} = reverse <$> readIORef logRecord

getTweetRecord :: MonadIO m => MockEnv -> m [Text]
getTweetRecord MockEnv {..} = reverse <$> readIORef tweetRecord

getCurrentState :: MonadIO m => MockEnv -> m (Maybe (Map Text Text))
getCurrentState MockEnv {..} = readIORef $ appConfig ^. the @"store"

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
mockAppConfig = do
  store <- newIORef Nothing
  pure AppConfig { group = mockGroup, store }

mockTwitterApi
  :: Mock (SomeAPIRequest -> RIO MockEnv Dynamic) -> TwitterApi (RIO MockEnv)
mockTwitterApi mock = TwitterApi $ fromDyn' . mockup mock . SomeAPIRequest
  where fromDyn' x = evaluate . fromDyn =<< x

data SomeAPIRequest where
  SomeAPIRequest ::TwitterApi.APIRequest apiName r -> SomeAPIRequest

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

isListsMemberQuery :: SomeAPIRequest -> Bool
isListsMemberQuery (SomeAPIRequest q) =
  _url q == _url (TwitterApi.listsMembers mockListParam)

isTweetQuery :: SomeAPIRequest -> Bool
isTweetQuery (SomeAPIRequest q) = _url q == _url (TwitterApi.update "")

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

mkTwitterUser1 :: Text -> User
mkTwitterUser1 = User 1

mkTwitterUser2 :: Text -> User
mkTwitterUser2 = User 2

mockListId :: Natural
mockListId = 0

mockListParam :: ListParam
mockListParam = TwitterApi.ListIdParam $ fromIntegral mockListId

-------------------------------------------------------------------------------
-- Spec

type ListsMembersResp = WithCursor Integer UsersCursorKey User

spec :: Spec
spec = do
  describe "twitter name change" $ do
    env <- runIO $ mockEnv MockConfig
      { listsMembersResp = [ Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                                                , mkTwitterUser2 "Tanaka Hanako"
                                                ]
                             }
                           , Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                                , mkTwitterUser2 "Tanaka Hanako"
                                                ]
                             }
                           ]
      , tweetResp        = [ Right Tweet { tweetId   = 0
                                         , createdAt = Time.UTCTime (toEnum 0) 0
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
          [("Tanaka Hanako", "Tanaka Hanako"), ("Yamada Taro", "Yamada Mark-Ⅲ")]
        )
  describe "user not in list" $ do
    env <- runIO $ mockEnv MockConfig
      { listsMembersResp = [ Right WithCursor
                               { nextCursor = Nothing
                               , previousCursor = Nothing
                               , contents = [mkTwitterUser1 "Yamada Mark-Ⅱ"]
                               }
                           ]
      , tweetResp        = []
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
      { listsMembersResp = [ Left $ SomeException $ stringException
                               "Twitter Down"
                           ]
      , tweetResp        = []
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
      { listsMembersResp = [ Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                                                , mkTwitterUser2 "Tanaka Mark-Ⅱ"
                                                ]
                             }
                           , Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                                , mkTwitterUser2 "Tanaka Mark-Ⅲ"
                                                ]
                             }
                           ]
      , tweetResp = [ Left $ SomeException $ stringException "Twitter Down"
                    , Right Tweet { tweetId   = 0
                                  , createdAt = Time.UTCTime (toEnum 0) 0
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
      { listsMembersResp = [ Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅱ"
                                                , mkTwitterUser2 "Tanaka"
                                                ]
                             }
                           , Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                                , mkTwitterUser2 "Tanaka"
                                                ]
                             }
                           , Right WithCursor
                             { nextCursor     = Nothing
                             , previousCursor = Nothing
                             , contents       = [ mkTwitterUser1 "Yamada Mark-Ⅲ"
                                                , mkTwitterUser2 "Tanaka"
                                                ]
                             }
                           ]
      , tweetResp = [ Left $ SomeException $ stringException "Twitter Down"
                    , Right Tweet { tweetId   = 0
                                  , createdAt = Time.UTCTime (toEnum 0) 0
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

