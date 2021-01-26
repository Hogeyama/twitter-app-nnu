
module Nijisanji where

import           RIO
import           RIO.Time
import           Data.Aeson as J
import           Database.Persist
import           Database.Persist.Sql
import           Dhall
import           System.IO.Unsafe (unsafePerformIO)

type NameMap = Map Text UpdateInfo
data UpdateInfo = UpdateInfo
  { twitterName :: Text
  , updateTime  :: UTCTime
  , tweetId     :: Int64
  }
  deriving stock (Eq, Show, Typeable, Generic)
instance ToJSON UpdateInfo
instance FromJSON UpdateInfo
instance NFData UpdateInfo
instance PersistField UpdateInfo where
  toPersistValue x =
    PersistText . decodeUtf8Lenient . toStrictBytes . J.encode $ x
  fromPersistValue (PersistText t) =
    case J.decode (fromStrictBytes (encodeUtf8 t)) of
      Nothing -> Left "fromPersistValue: UpdteInfo"
      Just x  -> Right x
  fromPersistValue _ = Left "fromPersistValue: UpdteInfo"

instance PersistFieldSql UpdateInfo where
  sqlType _ = SqlString

-------------------------------------------------------------------------------
-- Member
-------------------------------------------------------------------------------

data Member = Member
  { exGroup    :: Group
  , liverName  :: Text
  , screenName :: Text
  , userId     :: Natural
  } deriving stock (Show, Generic)
instance NFData Member
instance FromDhall Member

{-# NOINLINE nijisanjiMembers #-}
{-# NOINLINE seedsMembers #-}
{-# NOINLINE gamersMembers #-}
{-# NOINLINE since2019Members #-}
nijisanjiMembers, gamersMembers, seedsMembers, since2019Members :: [Member]
nijisanjiMembers = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/Nijisanji.dhall"
gamersMembers    = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/Gamers.dhall"
seedsMembers     = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/SEEDs.dhall"
since2019Members = (testMember :)
                 $ unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/Since2019.dhall"

testMember :: Member
testMember = Member (Other "test") "test" "-" 2426613613 -- @gan

isTestMember :: Member -> Bool
isTestMember m = userId m == userId testMember

-------------------------------------------------------------------------------
-- Group
-------------------------------------------------------------------------------

data Group = Nijisanji | Gamers | SEEDs | Since2019 | Other { _label :: Text }
  deriving stock (Show, Eq, Ord, Generic)
instance NFData Group
instance FromDhall Group

instance ToJSON Group where
  toJSON = \case
    Nijisanji -> String "一期生・二期生"
    Gamers    -> String "ゲーマーズ"
    SEEDs     -> String "SEEDs"
    Since2019 -> String "Since2019"
    Other g   -> String g
instance FromJSON Group where
  parseJSON = withText "Group" $ \case
    "一期生・二期生" -> return Nijisanji
    "ゲーマーズ"     -> return Gamers
    "SEEDs"          -> return SEEDs
    "Since2019"      -> return Since2019
    e                -> return (Other e)
instance PersistField Group where
  toPersistValue = \case
    Nijisanji -> PersistText "一期生・二期生"
    Gamers    -> PersistText "ゲーマーズ"
    SEEDs     -> PersistText "SEEDs"
    Since2019 -> PersistText "Since2019"
    Other g   -> PersistText g
  fromPersistValue = \case
    PersistText "一期生・二期生" -> return Nijisanji
    PersistText "ゲーマーズ"     -> return Gamers
    PersistText "SEEDs"          -> return SEEDs
    PersistText "Since2019"      -> return Since2019
    PersistText g                -> return (Other g)
    _ -> Left "Parse error: Twitter.Types.Group"
instance PersistFieldSql Group where
  sqlType _ = SqlString

showListName :: Group -> Text
showListName = \case
    Nijisanji -> "nijisanji"
    Gamers    -> "Gamers"
    SEEDs     -> "SEEDs"
    Since2019 -> "Since2019"
    Other g   -> g

