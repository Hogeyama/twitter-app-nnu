{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Db (
  NnuDb (..),
  getCurrentName,
  GetCurrentNameError (..),
  getCurrentNamesOfGroup,
  GetCurrentNamesOfGroupError (..),
  getHistroy,
  GetHistoryError (..),
  updateCurrentName,
  UpdateCurrentNameError (..),
  putHistory,
  PutHistoryError (..),
  CurrentNameItem (..),
  UpdateCurrentNameItem (..),
  HistoryItem (..),
) where

import Polysemy

import qualified Data.Aeson as A

import qualified NNU.Nijisanji as Nnu (
  Group,
  Member,
 )
import NNU.Prelude hiding (Handler)

-- TwitterNameRepository とかにする？
data NnuDb m a where
  GetCurrentName ::
    Nnu.Member ->
    NnuDb m (Either GetCurrentNameError CurrentNameItem)
  GetCurrentNamesOfGroup ::
    Nnu.Group ->
    NnuDb m (Either GetCurrentNamesOfGroupError [CurrentNameItem])
  GetHistroy ::
    [Nnu.Member] ->
    NnuDb m (Either GetHistoryError [HistoryItem])
  UpdateCurrentName ::
    UpdateCurrentNameItem ->
    NnuDb m (Either UpdateCurrentNameError ())
  PutHistory ::
    HistoryItem ->
    NnuDb m (Either PutHistoryError ())

newtype GetCurrentNameError = GetCurrentNameError A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)

newtype GetCurrentNamesOfGroupError = GetCurrentNamesOfGroupError A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)

newtype GetHistoryError = GetHistoryError A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)

newtype UpdateCurrentNameError = UpdateCurrentNameError A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)

newtype PutHistoryError = PutHistoryError A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)

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
