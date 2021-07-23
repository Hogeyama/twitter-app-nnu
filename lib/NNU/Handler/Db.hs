{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-binds  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module NNU.Handler.Db
  ( Handler(..)
  , Error(..)
  , Has(..)
  , CurrentNameItem(..)
  , HistoryItem(..)
  , UpdateCurrentNameItem(..)
  , invoke
  , defaultHandler
  , newAwsEnv
  , newLocalAwsEnv
  ) where

import           Lens.Micro.Platform
import           RIO                     hiding ( Handler )

import           Control.Error.Util             ( note )
import           Control.Exception.Safe         ( MonadCatch )
import           Control.Method                 ( Base
                                                , Method
                                                )
import qualified Control.Method                as Method
import qualified Control.Monad.Trans.AWS       as AWST
import qualified Data.Aeson                    as A
import           Data.Generics.Product          ( the )
import           Data.Time.Format.ISO8601       ( ISO8601(iso8601Format)
                                                , formatShow
                                                , iso8601ParseM
                                                )
import qualified Network.AWS                   as AWS
import qualified Network.AWS.DynamoDB          as Dynamo
import qualified RIO.HashMap                   as HM
import           RIO.Orphans                    ( HasResourceMap(..) )
import qualified RIO.Text                      as T
import           RIO.Time                       ( ZonedTime )
import           System.ReadEnvVar              ( readEnv )

import           Data.Time                      ( zonedTimeToUTC )
import           NNU.Nijisanji                  ( Group
                                                , GroupName(..)
                                                , Member
                                                )

data Handler m = Handler
  { getCurrentName         :: Member -> m CurrentNameItem
  , getCurrentNamesOfGroup :: Group -> m [CurrentNameItem]
  , getHistroy             :: [Member] -> m [HistoryItem]
  , updateCurrentName      :: UpdateCurrentNameItem -> m ()
  , putHistory             :: HistoryItem -> m ()
  }
  deriving stock Generic

class Has env where
  dbL :: Lens' env (Handler (RIO env))

newtype Error = Error A.Value
  deriving anyclass Exception
instance Show Error where
  show (Error v) = encodeStr v
    where encodeStr = T.unpack . decodeUtf8Lenient . toStrictBytes . A.encode

type MemberName = Text
data CurrentNameItem = CurrentNameItem
  { memberName  :: MemberName
  , updateTime  :: ZonedTime
  , twitterName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass A.ToJSON
data UpdateCurrentNameItem = UpdateCurrentNameItem
  { member      :: Member
  , updateTime  :: ZonedTime
  , twitterName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass A.ToJSON
data HistoryItem = HistoryItem
  { member      :: Member
  , updateTime  :: ZonedTime
  , twitterName :: Text
  , tweetId     :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass A.ToJSON

-- スキーマ
--             | PK              | SK                              | Name       | other |
-- CurrentName | Member#月ノ美兎 | Current                         | 月ノ美兎🐰 | 2021-07-03T23:20:00.123(Time) |
-- History     | Member#月ノ美兎 | History#2021-07-03T23:20:00.123 | 月ノ美兎🐰 | 1234...(TweetId) |
-- History     | Member#月ノ美兎 | History#2021-07-02T22:10:00.233 | 月ノ美兎   | 1233...(TweetId) |

invoke
  :: (Method method, Has env, Base method ~ RIO env)
  => SimpleGetter (Handler (Base method)) method
  -> method
invoke l = Method.invoke (dbL . l)

-- brittany-disable-next-binding
newAwsEnv :: (MonadIO m, MonadCatch m) => m AWS.Env
newAwsEnv = AWST.newEnv AWST.Discover
  <&> AWST.envRegion .~ AWST.Tokyo
  <&> AWST.configure Dynamo.dynamoDB

newLocalAwsEnv :: (MonadIO m, MonadCatch m) => m AWS.Env
newLocalAwsEnv = do
  port <- fromMaybe 8000 <$> readEnv "LOCAL_DYNAMODB_PORT"
  let dynamoLocal = AWST.setEndpoint False "localhost" port Dynamo.dynamoDB
  newAwsEnv <&> AWST.configure dynamoLocal

mkS :: Text -> Dynamo.AttributeValue
mkS s = Dynamo.attributeValue & Dynamo.avS ?~ s

lookupS :: Text -> HashMap Text Dynamo.AttributeValue -> Either Text Text
lookupS f m = do
  v <- note (f <> " not found") $ HM.lookup f m
  note (f <> " is not a text") $ view Dynamo.avS v

mkN :: Text -> Dynamo.AttributeValue
mkN s = Dynamo.attributeValue & Dynamo.avN ?~ s


-------------------------------------------------------------------------------
-- default implementation
-------------------------------------------------------------------------------

-- brittany-disable-next-binding
defaultHandler
  :: forall n env
   . (MonadIO n, HasResourceMap env)
  => AWS.Env
  -> n (Handler (RIO env))
defaultHandler awsEnv = pure Handler { .. }
 where
  -- TODO
  -- 'length members' が十分に大きかったら
  -- SK=Current で SGI にクエリする
  getCurrentName :: Member -> RIO env CurrentNameItem
  getCurrentName member = do
    res <- rethrowingAny (throw'.tshow) $ runAws $ AWST.send getItem
    let statusCode = res ^. Dynamo.girsResponseStatus
    case (statusCode, parseCurrentNameItem (res ^. Dynamo.girsItem)) of
      (200, Right cnItem) -> pure cnItem
      (200, Left err) -> throw' err
      _ -> throw' statusCode
   where
    getItem = Dynamo.getItem _TABLE_NAME
      & Dynamo.giKey .~ HM.fromList [("PK", mkS pk), ("SK", mkS sk)]
      & Dynamo.giConsistentRead ?~ True
    throw' :: A.ToJSON e => e -> RIO env a
    throw' = throwError "getCurrentName" getItem
    memberName = member ^. the @"memberName"
    pk         = "Member#" <> memberName
    sk         = "Current"

  getCurrentNamesOfGroup :: Group -> RIO env [CurrentNameItem]
  getCurrentNamesOfGroup group = do
    res <- rethrowingAny (throw'.tshow) $ runAws $ AWST.send query
    let statusCode = res ^. Dynamo.qrsResponseStatus
        (errors, cnItems) = partitionEithers $
          map parseCurrentNameItem (res ^. Dynamo.qrsItems)
    case (statusCode, errors) of
      (200, []) -> pure cnItems
      (200, _ ) -> throw' errors
      _ -> throw' statusCode
   where
    query = Dynamo.query _TABLE_NAME
      & Dynamo.qIndexName ?~ _CURRENT_NAME_INDEX
      & Dynamo.qKeyConditionExpression ?~ condition
      & Dynamo.qExpressionAttributeValues .~ values
    throw' :: A.ToJSON e => e -> RIO env a
    throw' = throwError "getCurrentNamesOfGroup" query
    (condition, values) = case group ^. the @"groupLabel" of
      Other _ -> ("SK := Current", HM.fromList [])
      l       -> ("SK := Current and Group = :group",
                  HM.fromList [(":group", mkS (tshow l))])

  updateCurrentName :: UpdateCurrentNameItem -> RIO env ()
  updateCurrentName UpdateCurrentNameItem {..} = do
    res <- rethrowingAny (throw'.tshow) $ runAws $ AWST.send putItem
    let statusCode = res ^. Dynamo.pirsResponseStatus
    unless (statusCode == 200) $ throw' statusCode
   where
    putItem = Dynamo.putItem "NNU" & Dynamo.piItem .~ HM.fromList
      [ ("PK"   , mkS $ "Member#" <> member ^. the @"memberName")
      , ("SK"   , mkS "Current")
      , ("Time" , mkS updateTime')
      , ("Name" , mkS twitterName)
      , ("Group", mkS $ tshow $ member ^. the @"exGroup")
      ]
    throw' :: A.ToJSON e => e -> RIO env a
    throw' = throwError "updateCurrentName" putItem
    updateTime' = T.pack $ formatShow iso8601Format updateTime

  putHistory :: HistoryItem -> RIO env ()
  putHistory HistoryItem {..} = do
    res <- rethrowingAny (throw'.tshow) $ runAws $ AWST.send putItem
    let statusCode = res ^. Dynamo.pirsResponseStatus
    unless (statusCode == 200) $ throw' statusCode
   where
    putItem = Dynamo.putItem "NNU" & Dynamo.piItem .~ HM.fromList
      [ ("PK"     , mkS $ "Member#" <> member ^. the @"memberName")
      , ("SK"     , mkS $ "History#" <> updateTime')
      , ("Name"   , mkS twitterName)
      , ("Group"  , mkS $ tshow $ member ^. the @"exGroup")
      , ("TweetId", mkS $ tshow tweetId)
      ]
    throw' :: A.ToJSON e => e -> RIO env a
    throw' = throwError "putHistory" putItem
    updateTime' = T.pack $ formatShow iso8601Format updateTime

  getHistroy _ = undefined

  runAws :: forall a. RIO (TmpEnv env) a -> RIO env a
  runAws = mapRIO $ TmpEnv awsEnv

-- TODO 環境変数で扱う
_TABLE_NAME :: Text
_TABLE_NAME = "NNU"

_CURRENT_NAME_INDEX :: Text
_CURRENT_NAME_INDEX = "CURRENT_NAME-index"

data TmpEnv env = TmpEnv
  { awsEnv :: AWS.Env
  , super  :: env
  }
  deriving stock Generic
instance HasResourceMap env => HasResourceMap (TmpEnv env) where
  resourceMapL = the @"super" . resourceMapL
instance AWS.HasEnv (TmpEnv env) where
  environment = the @"awsEnv"

rethrowingAny
  :: (MonadUnliftIO m, NFData a) => (SomeException -> m a) -> m a -> m a
rethrowingAny throw' action = tryAnyDeep action >>= \case
  Right res -> pure res
  Left  err -> throw' err

throwError
  :: (MonadIO m, A.ToJSON req, A.ToJSON err) => Text -> req -> err -> m a
throwError methodName req err = throwIO $ Error $ A.object
  ["message" A..= methodName, "request" A..= req, "error" A..= err]

-- TODO doctest
parseCurrentNameItem
  :: HashMap Text Dynamo.AttributeValue -> Either Text CurrentNameItem
parseCurrentNameItem item = do
  case T.stripPrefix "Member#" <$> lookupS "PK" item of
    Right (Just memberName) -> mapLeft ((memberName <> ": ") <>) $ do
      updateTimeS <- lookupS "Time" item
      updateTime  <- note ("iso8601ParseM" <> updateTimeS)
        $ iso8601ParseM (T.unpack updateTimeS)
      twitterName <- lookupS "Name" item
      pure CurrentNameItem { .. }
    Right Nothing ->
      Left "parseCurrentNameItem: impossible: 'Member#' not found"
    Left err -> Left err

-------------------------------------------------------------------------------
-- Orphan instance (TODO: to be moved)
-------------------------------------------------------------------------------

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC
instance Ord ZonedTime where
  compare = compare `on` zonedTimeToUTC

