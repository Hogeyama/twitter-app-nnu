{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module NNU.Handler.Db (
  -- * Interface
  Handler (..),
  Error (..),
  Has (..),
  CurrentNameItem (..),
  HistoryItem (..),
  UpdateCurrentNameItem (..),
  invoke,
) where

import Control.Method (
  Base,
  Method,
 )
import qualified Control.Method as Method
import qualified Data.Aeson as A
import qualified RIO.Text as T

import NNU.Nijisanji (
  Group,
  Member,
 )
import NNU.Prelude hiding (Handler)

data Handler m = Handler
  { getCurrentName :: Member -> m CurrentNameItem
  , getCurrentNamesOfGroup :: Group -> m [CurrentNameItem]
  , getHistroy :: [Member] -> m [HistoryItem]
  , updateCurrentName :: UpdateCurrentNameItem -> m ()
  , putHistory :: HistoryItem -> m ()
  }
  deriving stock (Generic)

class Has env where
  dbL :: Lens' env (Handler (RIO env))

newtype Error = Error A.Value
  deriving anyclass (Exception)
instance Show Error where
  show (Error v) = encodeStr v
    where
      encodeStr = T.unpack . decodeUtf8Lenient . toStrictBytes . A.encode

type MemberName = Text
data CurrentNameItem = CurrentNameItem
  { memberName :: MemberName
  , updateTime :: ZonedTime
  , twitterName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)
data UpdateCurrentNameItem = UpdateCurrentNameItem
  { member :: Member
  , updateTime :: ZonedTime
  , twitterName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)
data HistoryItem = HistoryItem
  { member :: Member
  , updateTime :: ZonedTime
  , twitterName :: Text
  , tweetId :: Natural
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (A.ToJSON)

invoke ::
  (Method method, Has env, Base method ~ RIO env) =>
  SimpleGetter (Handler (Base method)) method ->
  method
invoke l = Method.invoke (dbL . l)
