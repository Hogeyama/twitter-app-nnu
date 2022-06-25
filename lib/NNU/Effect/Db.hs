{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Db (
  NnuDb (..),
  getCurrentName,
  getCurrentNamesOfGroup,
  getHistroy,
  updateCurrentName,
  putHistory,
  CurrentNameItem (..),
  UpdateCurrentNameItem (..),
  HistoryItem (..),
  Error (..),
) where

import Polysemy

import qualified Data.Aeson as A

import qualified NNU.Nijisanji as Nnu (
  Group,
  Member,
 )
import NNU.Prelude hiding (Handler)

newtype Error = Error A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)

-- TwitterNameRepository とかにする？
data NnuDb m a where
  GetCurrentName :: Nnu.Member -> NnuDb m (Either Error CurrentNameItem)
  GetCurrentNamesOfGroup :: Nnu.Group -> NnuDb m (Either Error [CurrentNameItem])
  GetHistroy :: [Nnu.Member] -> NnuDb m (Either Error [HistoryItem])
  UpdateCurrentName :: UpdateCurrentNameItem -> NnuDb m (Either Error ())
  PutHistory :: HistoryItem -> NnuDb m (Either Error ())

type MemberName = Text

data CurrentNameItem = CurrentNameItem
  { memberName :: MemberName
  , updateTime :: ZonedTime
  , twitterName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)

data UpdateCurrentNameItem = UpdateCurrentNameItem
  { member :: Nnu.Member
  , updateTime :: ZonedTime
  , twitterName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)

data HistoryItem = HistoryItem
  { member :: Nnu.Member
  , updateTime :: ZonedTime
  , twitterName :: Text
  , tweetId :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)

makeSem ''NnuDb
