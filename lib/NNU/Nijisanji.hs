{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module NNU.Nijisanji
  ( Member(..)
  , testMember
  , isTestMember
  , Group(..)
  , GroupName(..)
  , nijisanji
  , gamers
  , seeds
  , since2019
  , testGroup
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import qualified Data.Aeson                    as A
import qualified Dhall
import           RIO
import qualified RIO.Text                      as T
import           System.IO.Unsafe               ( unsafePerformIO )

-------------------------------------------------------------------------------
-- Member
-------------------------------------------------------------------------------

data Member = Member
  { exGroup    :: GroupName
  , memberName :: Text
  , screenName :: Text
  , userId     :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass A.ToJSON
instance NFData Member
instance Dhall.FromDhall Member

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
instance Dhall.FromDhall GroupName

instance Show GroupName where
  show = \case
    Nijisanji -> "Nijisanji"
    Gamers    -> "Gamers"
    SEEDs     -> "SEEDs"
    Since2019 -> "Since2019"
    Other l   -> T.unpack l
instance ToJSON GroupName where
  toJSON = \case
    Nijisanji -> A.String "一期生・二期生"
    Gamers    -> A.String "ゲーマーズ"
    SEEDs     -> A.String "SEEDs"
    Since2019 -> A.String "Since2019"
    Other g   -> A.String g
instance FromJSON GroupName where
  parseJSON = A.withText "GroupName" $ \case
    "一期生・二期生"   -> return Nijisanji
    "ゲーマーズ"     -> return Gamers
    "SEEDs"     -> return SEEDs
    "Since2019" -> return Since2019
    l           -> return (Other l)

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
instance Dhall.FromDhall Group

{-# NOINLINE nijisanji #-}
{-# NOINLINE seeds #-}
{-# NOINLINE gamers #-}
{-# NOINLINE since2019 #-}
{-# NOINLINE testGroup #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
nijisanji, gamers, seeds, since2019, testGroup :: Group
nijisanji = unsafePerformIO $ readDhall "./conf/Nijisanji.dhall"
gamers = unsafePerformIO $ readDhall "./conf/Gamers.dhall"
seeds = unsafePerformIO $ readDhall "./conf/SEEDs.dhall"
since2019 = unsafePerformIO $ readDhall "./conf/Since2019.dhall"
testGroup = Group
  { groupLabel = Other "Test"
  , listId     = 1371129437970862080
  , members    = testMember : concat
    [members nijisanji, members gamers, members seeds, members since2019]
  }
readDhall :: FilePath -> IO Group
readDhall f = Dhall.detailed $ Dhall.input Dhall.auto $ fromString f

-------------------------------------------------------------------------------
-- DB
-------------------------------------------------------------------------------

-- type NameMap = Map Text UpdateInfo
-- data UpdateInfo = UpdateInfo
--   { twitterName :: Text
--   , updateTime  :: UTCTime
--   , tweetId     :: Int64
--   }
--   deriving stock (Eq, Show, Generic)
-- instance ToJSON UpdateInfo
-- instance FromJSON UpdateInfo
-- instance NFData UpdateInfo

