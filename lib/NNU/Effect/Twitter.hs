{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Twitter (
  Twitter (..),
  tweet,
  listsMembers,
  TweetResp (..),
  User (..),
  ListsMembersParam (..),
  ListsMembersResp,
  Error (..),
  module X,
) where

import Polysemy

import qualified Data.Aeson as A
import RIO.Partial (read)
import qualified RIO.Text as T
import RIO.Time
import Web.Twitter.Conduit as X (
  UsersCursorKey,
  WithCursor (..),
 )

import NNU.Prelude hiding (Handler)

{-# ANN module ("HLint: ignore Use <$>" :: String) #-}

newtype Error = Error A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)
instance Exception Error

data Twitter m a where
  Tweet :: Text -> Twitter m (Either Error TweetResp)
  ListsMembers :: ListsMembersParam -> Twitter m (Either Error ListsMembersResp)

data TweetResp = TweetResp
  { tweetId :: Natural
  , createdAt :: ZonedTime
  }
  deriving stock (Show, Generic)

data ListsMembersParam = ListsMembersParam
  { listId :: Natural
  , count :: Maybe Integer
  }

data User = User
  { userId :: Natural
  , userName :: Text
  }
  deriving stock (Show, Eq, Generic)

type ListsMembersResp = WithCursor Integer UsersCursorKey User

makeSem ''Twitter

instance A.FromJSON TweetResp where
  parseJSON = A.withObject "Tweet" $ \o -> do
    pure TweetResp
      <*> (read <$> o A..: "id_str")
      <*> (pure . utcToZonedTime jst . fromTwitterTime =<< o A..: "created_at")
    where
      jst =
        TimeZone
          { timeZoneMinutes = 9 * 60
          , timeZoneSummerOnly = False
          , timeZoneName = "JST"
          }

instance A.FromJSON User where
  parseJSON = A.withObject "User" $ \o -> do
    pure User <*> (read <$> o A..: "id_str") <*> o A..: "name"

newtype TwitterTime = TwitterTime {fromTwitterTime :: UTCTime}

instance A.FromJSON TwitterTime where
  parseJSON = A.withText "TwitterTime" $ \t ->
    case parseTimeM True defaultTimeLocale twitterTimeFormat (T.unpack t) of
      Just d -> pure $ TwitterTime d
      Nothing -> fail $ "Could not parse twitter time: " ++ T.unpack t
    where
      twitterTimeFormat :: String
      twitterTimeFormat = "%a %b %d %T %z %Y"
