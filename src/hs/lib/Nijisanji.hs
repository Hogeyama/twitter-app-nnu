
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Nijisanji where

import           RIO
import           RIO.Time
import qualified RIO.Text as T
import           Data.Aeson as J
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

-------------------------------------------------------------------------------
-- Member
-------------------------------------------------------------------------------

data Member = Member
  { exGroup    :: GroupName
  , liverName  :: Text
  , screenName :: Text
  , userId     :: Natural
  } deriving stock (Show, Generic)
instance NFData Member
instance FromDhall Member

testMember :: Member
testMember = Member (Other "test") "test" "-" 2426613613 -- @gan

isTestMember :: Member -> Bool
isTestMember m = userId m == userId testMember

-------------------------------------------------------------------------------
-- GroupName
-------------------------------------------------------------------------------

data GroupName = Nijisanji | Gamers | SEEDs | Since2019 | Other { label :: Text }
  deriving stock (Eq, Ord, Generic)
instance NFData GroupName
instance FromDhall GroupName

instance Show GroupName where
  show = \case
    Nijisanji -> "Nijisanji"
    Gamers    -> "Gamers"
    SEEDs     -> "SEEDs"
    Since2019 -> "Since2019"
    Other l   -> T.unpack l
instance ToJSON GroupName where
  toJSON = \case
    Nijisanji -> String "一期生・二期生"
    Gamers    -> String "ゲーマーズ"
    SEEDs     -> String "SEEDs"
    Since2019 -> String "Since2019"
    Other g   -> String g
instance FromJSON GroupName where
  parseJSON = withText "GroupName" $ \case
    "一期生・二期生" -> return Nijisanji
    "ゲーマーズ"     -> return Gamers
    "SEEDs"          -> return SEEDs
    "Since2019"      -> return Since2019
    l                -> return (Other l)

-------------------------------------------------------------------------------
-- GroupName
-------------------------------------------------------------------------------

data Group = Group
  { groupLabel :: GroupName
  , listId     :: Natural
  , members    :: [Member]
  }
  deriving stock (Show, Generic)
instance NFData Group
instance FromDhall Group

{-# NOINLINE nijisanji #-}
{-# NOINLINE seeds #-}
{-# NOINLINE gamers #-}
{-# NOINLINE since2019 #-}
{-# NOINLINE testGroup #-}
nijisanji, gamers, seeds, since2019, testGroup :: Group
nijisanji = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/Nijisanji.dhall"
gamers    = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/Gamers.dhall"
seeds     = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/SEEDs.dhall"
since2019 = unsafePerformIO $ Dhall.detailed $ Dhall.inputFile Dhall.auto "./src/dhall/Since2019.dhall"
testGroup = Group
    { groupLabel = Other "Test"
    , listId     = 1371129437970862080
    , members    = testMember : concat
                    [ members nijisanji
                    , members gamers
                    , members seeds
                    , members since2019
                    ]
    }

