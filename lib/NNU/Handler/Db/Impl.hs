module NNU.Handler.Db.Impl (
  defaultHandler,
  Config (..),
  configFromEnv,
  configGlobalAwsFromEnv,
  configLocalAwsFromEnv,
) where

import Control.Error.Util (note)
import Control.Exception.Safe (MonadCatch)
import qualified Control.Monad.Trans.AWS as AWST
import qualified Data.Aeson as A
import Data.Generics.Product (the)
import Data.Time.Format.ISO8601 (
  formatShow,
  iso8601ParseM,
 )
import qualified Network.AWS as AWS
import qualified Network.AWS.DynamoDB as Dynamo
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import System.ReadEnvVar (
  lookupEnv,
  readEnv,
 )

import NNU.Handler.Db
import NNU.Nijisanji (
  Group,
  GroupName (..),
  Member,
 )
import NNU.Prelude hiding (Handler)

data Config = Config
  { cfgTableName :: Text
  , cfgAwsEnv :: AWS.Env
  }

-- | Use local DynamoDB if @NNU_USE_LOCAL_AWS=1@.
configFromEnv :: (MonadIO m, MonadCatch m) => m Config
configFromEnv =
  readEnv "NNU_USE_LOCAL_AWS" >>= \case
    Just (1 :: Int) -> configLocalAwsFromEnv
    _ -> configGlobalAwsFromEnv

-- brittany-disable-next-binding

-- | Force to use global DynamoDB.
configGlobalAwsFromEnv :: (MonadIO m, MonadCatch m) => m Config
configGlobalAwsFromEnv = do
  cfgTableName <-
    lookupEnv "NNU_TABLE_NAME" >>= \case
      Just t -> pure t
      -- テーブルを間違えると悲惨なことになるので死ぬべき
      Nothing -> throwString "NNU_TABLE_NAME not set"
  cfgAwsEnv <-
    AWST.newEnv AWST.Discover
      <&> AWST.envRegion .~ AWST.Tokyo
      <&> AWST.configure Dynamo.dynamoDB
  pure Config {..}

-- | Force to use local DynamoDB.
configLocalAwsFromEnv :: (MonadIO m, MonadCatch m) => m Config
configLocalAwsFromEnv = do
  cfg <- configGlobalAwsFromEnv
  port <- fromMaybe 8000 <$> readEnv "NNU_LOCAL_DYNAMODB_PORT"
  let dynamoLocal = AWST.setEndpoint False "localhost" port Dynamo.dynamoDB
  pure cfg {cfgAwsEnv = AWST.configure dynamoLocal (cfgAwsEnv cfg)}

-- brittany-disable-next-binding
defaultHandler ::
  forall n env.
  (MonadIO n, HasResourceMap env) =>
  Config ->
  n (Handler (RIO env))
defaultHandler Config {..} = pure Handler {..}
  where
    getCurrentName :: Member -> RIO env CurrentNameItem
    getCurrentName member = do
      res <- rethrowingAny (throw' . tshow) $ runAws $ AWST.send getItem
      let statusCode = res ^. Dynamo.girsResponseStatus
      case (statusCode, parseCurrentNameItem (res ^. Dynamo.girsItem)) of
        (200, Right cnItem) -> pure cnItem
        (200, Left err) -> throw' err
        _ -> throw' statusCode
      where
        getItem =
          Dynamo.getItem cfgTableName
            & Dynamo.giKey .~ HM.fromList [("PK", mkS pk), ("SK", mkS sk)]
            & Dynamo.giConsistentRead ?~ True
        throw' :: A.ToJSON e => e -> RIO env a
        throw' = throwError "getCurrentName" getItem
        memberName = member ^. the @"memberName"
        pk = "Member#" <> memberName
        sk = "Current"

    getCurrentNamesOfGroup :: Group -> RIO env [CurrentNameItem]
    getCurrentNamesOfGroup group = do
      res <- rethrowingAny (throw' . tshow) $ runAws $ AWST.send query
      let statusCode = res ^. Dynamo.qrsResponseStatus
          (errors, cnItems) =
            partitionEithers $
              map parseCurrentNameItem (res ^. Dynamo.qrsItems)
      case (statusCode, errors) of
        (200, []) -> pure cnItems
        (200, _) -> throw' errors
        _ -> throw' statusCode
      where
        query =
          Dynamo.query cfgTableName
            & Dynamo.qIndexName ?~ _CURRENT_NAME_INDEX
            & Dynamo.qKeyConditionExpression ?~ condition
            & Dynamo.qExpressionAttributeValues .~ values
        throw' :: A.ToJSON e => e -> RIO env a
        throw' = throwError "getCurrentNamesOfGroup" query
        (condition, values) = case group ^. the @"groupLabel" of
          Other _ -> ("SK := Current", HM.fromList [])
          l ->
            ( "SK := Current and Group = :group"
            , HM.fromList [(":group", mkS (tshow l))]
            )

    updateCurrentName :: UpdateCurrentNameItem -> RIO env ()
    updateCurrentName UpdateCurrentNameItem {..} = do
      res <- rethrowingAny (throw' . tshow) $ runAws $ AWST.send putItem
      let statusCode = res ^. Dynamo.pirsResponseStatus
      unless (statusCode == 200) $ throw' statusCode
      where
        putItem =
          Dynamo.putItem cfgTableName & Dynamo.piItem
            .~ HM.fromList
              [ ("PK", mkS $ "Member#" <> member ^. the @"memberName")
              , ("SK", mkS "Current")
              , ("Time", mkS updateTime')
              , ("Name", mkS twitterName)
              , ("Group", mkS $ tshow $ member ^. the @"exGroup")
              ]
        throw' :: A.ToJSON e => e -> RIO env a
        throw' = throwError "updateCurrentName" putItem
        updateTime' = T.pack $ formatShow iso8601Format updateTime

    putHistory :: HistoryItem -> RIO env ()
    putHistory HistoryItem {..} = do
      res <- rethrowingAny (throw' . tshow) $ runAws $ AWST.send putItem
      let statusCode = res ^. Dynamo.pirsResponseStatus
      unless (statusCode == 200) $ throw' statusCode
      where
        putItem =
          Dynamo.putItem cfgTableName & Dynamo.piItem
            .~ HM.fromList
              [ ("PK", mkS $ "Member#" <> member ^. the @"memberName")
              , ("SK", mkS $ "History#" <> updateTime')
              , ("Name", mkS twitterName)
              , ("Group", mkS $ tshow $ member ^. the @"exGroup")
              , ("TweetId", mkS $ tshow tweetId)
              ]
        throw' :: A.ToJSON e => e -> RIO env a
        throw' = throwError "putHistory" putItem
        updateTime' = T.pack $ formatShow iso8601Format updateTime

    getHistroy _ = undefined

    runAws :: forall a. RIO (TmpEnv env) a -> RIO env a
    runAws = mapRIO $ TmpEnv cfgAwsEnv

_CURRENT_NAME_INDEX :: Text
_CURRENT_NAME_INDEX = "CURRENT_NAME-index"

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

data TmpEnv env = TmpEnv
  { awsEnv :: AWS.Env
  , super :: env
  }
  deriving stock (Generic)
instance HasResourceMap env => HasResourceMap (TmpEnv env) where
  resourceMapL = the @"super" . resourceMapL
instance AWS.HasEnv (TmpEnv env) where
  environment = the @"awsEnv"

throwError ::
  (MonadIO m, A.ToJSON req, A.ToJSON err) => Text -> req -> err -> m a
throwError methodName req err =
  throwIO $
    Error $
      A.object
        ["message" A..= methodName, "request" A..= req, "error" A..= err]

rethrowingAny ::
  (MonadUnliftIO m, NFData a) => (SomeException -> m a) -> m a -> m a
rethrowingAny throw' action =
  tryAnyDeep action >>= \case
    Right res -> pure res
    Left err -> throw' err

-- TODO doctest
parseCurrentNameItem ::
  HashMap Text Dynamo.AttributeValue -> Either Text CurrentNameItem
parseCurrentNameItem item = do
  case T.stripPrefix "Member#" <$> lookupS "PK" item of
    Right (Just memberName) -> mapLeft ((memberName <> ": ") <>) $ do
      updateTimeS <- lookupS "Time" item
      updateTime <-
        note ("iso8601ParseM" <> updateTimeS) $
          iso8601ParseM (T.unpack updateTimeS)
      twitterName <- lookupS "Name" item
      pure CurrentNameItem {..}
    Right Nothing ->
      Left "parseCurrentNameItem: impossible: 'Member#' not found"
    Left err -> Left err

mkS :: Text -> Dynamo.AttributeValue
mkS s = Dynamo.attributeValue & Dynamo.avS ?~ s

lookupS :: Text -> HashMap Text Dynamo.AttributeValue -> Either Text Text
lookupS f m = do
  v <- note (f <> " not found") $ HM.lookup f m
  note (f <> " is not a text") $ view Dynamo.avS v
