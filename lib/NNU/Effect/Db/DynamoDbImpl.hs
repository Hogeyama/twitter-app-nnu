module NNU.Effect.Db.DynamoDbImpl (
  runDynamoDb,
  Config (..),
  configFromEnv,
  configGlobalAwsFromEnv,
  configLocalAwsFromEnv,
) where

import Polysemy
import qualified Polysemy.Reader as Polysemy

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

import NNU.Effect.Db
import qualified NNU.Nijisanji as Nnu (
  Group,
  GroupName (..),
  Member,
 )
import NNU.Prelude hiding (Handler)

runDynamoDb ::
  forall r a.
  Member (Embed IO) r =>
  Member (Polysemy.Reader ResourceMap) r =>
  Config ->
  Sem (NnuDb ': r) a ->
  Sem r a
runDynamoDb Config {..} = interpret $ \case
  GetCurrentName member -> getCurrentName_ member
  GetCurrentNamesOfGroup group -> getCurrentNamesOfGroup_ group
  GetHistroy members -> getHistroy_ members
  UpdateCurrentName currentName -> updateCurrentName_ currentName
  PutHistory history -> putHistory_ history
  where
    getCurrentName_ :: Nnu.Member -> Sem r (Either Error CurrentNameItem)
    getCurrentName_ member = do
      runAWS (AWST.send getItem) >>= \case
        Right res -> do
          let statusCode = res ^. Dynamo.girsResponseStatus
          case (statusCode, parseCurrentNameItem (res ^. Dynamo.girsItem)) of
            (200, Right cnItem) -> pure (Right cnItem)
            (200, Left err) -> throw' err
            _ -> throw' statusCode
        Left err -> throw' (tshow err)
      where
        getItem :: Dynamo.GetItem
        getItem =
          Dynamo.getItem cfgTableName
            & Dynamo.giKey .~ HM.fromList [("PK", mkS pk), ("SK", mkS sk)]
            & Dynamo.giConsistentRead ?~ True

        memberName = member ^. the @"memberName"
        pk = "Member#" <> memberName
        sk = "Current"

        throw' :: forall e b. A.ToJSON e => e -> Sem r (Either Error b)
        throw' = throwError "getCurrentName" getItem

    getCurrentNamesOfGroup_ :: Nnu.Group -> Sem r (Either Error [CurrentNameItem])
    getCurrentNamesOfGroup_ group = do
      runAWS (AWST.send query) >>= \case
        Right res -> do
          let statusCode = res ^. Dynamo.qrsResponseStatus
              (errors, cnItems) =
                partitionEithers $
                  map parseCurrentNameItem $ res ^. Dynamo.qrsItems
          case (statusCode, errors) of
            (200, []) -> pure (Right cnItems)
            (200, _) -> throw' errors
            _ -> throw' statusCode
        Left err -> throw' (tshow err)
      where
        query =
          Dynamo.query cfgTableName
            & Dynamo.qIndexName ?~ _CURRENT_NAME_INDEX
            & Dynamo.qKeyConditionExpression ?~ condition
            & Dynamo.qExpressionAttributeValues .~ values

        (condition, values) = case group ^. the @"groupLabel" of
          Nnu.Other _ ->
            ( "SK := Current"
            , HM.fromList []
            )
          l ->
            ( "SK := Current and Group = :group"
            , HM.fromList [(":group", mkS (tshow l))]
            )

        throw' :: forall e b. A.ToJSON e => e -> Sem r (Either Error b)
        throw' = throwError "getCurrentNamesOfGroup" query

    updateCurrentName_ UpdateCurrentNameItem {..} = do
      runAWS (AWST.send putItem) >>= \case
        Right res -> do
          let statusCode = res ^. Dynamo.pirsResponseStatus
          if statusCode == 200
            then pure (Right ())
            else throw' statusCode
        Left err -> throw' (tshow err)
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

        updateTime' = T.pack $ formatShow iso8601Format updateTime

        throw' :: forall e b. A.ToJSON e => e -> Sem r (Either Error b)
        throw' = throwError "updateCurrentName" putItem

    putHistory_ :: HistoryItem -> Sem r (Either Error ())
    putHistory_ HistoryItem {..} = do
      runAWS (AWST.send putItem) >>= \case
        Right res -> do
          let statusCode = res ^. Dynamo.pirsResponseStatus
          if statusCode == 200
            then pure (Right ())
            else throw' statusCode
        Left err -> throw' (tshow err)
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

        throw' :: forall e b. A.ToJSON e => e -> Sem r (Either Error b)
        throw' = throwError "putHistory" putItem

        updateTime' = T.pack $ formatShow iso8601Format updateTime

    getHistroy_ _ = throwError "updateCurrentName" () ("not implemented" :: Text)

    runAWS :: forall b. NFData b => RIO AwsResource b -> Sem r (Either SomeException b)
    runAWS aws = do
      resourceMap <- Polysemy.ask @ResourceMap
      embed $ tryAnyDeep $ runRIO (AwsResource cfgAwsEnv resourceMap) aws

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

_CURRENT_NAME_INDEX :: Text
_CURRENT_NAME_INDEX = "CURRENT_NAME-index"

--------------------------------------------------------------------------------

throwError ::
  ( A.ToJSON req
  , A.ToJSON err
  ) =>
  Text ->
  req ->
  err ->
  Sem r (Either Error a)
throwError methodName req err =
  pure $
    Left $
      Error $
        A.object
          ["message" A..= methodName, "request" A..= req, "error" A..= err]

data AwsResource = AwsResource {awsEnv :: AWS.Env, resourceMap :: ResourceMap}
  deriving stock (Generic)

instance HasResourceMap AwsResource where
  resourceMapL = the @"resourceMap"

instance AWS.HasEnv AwsResource where
  environment = the @"awsEnv"

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
