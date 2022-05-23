{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Twitter (
  Twitter (..),
  call,
  call',
  tweet,
  Tweet (..),
  User (..),
  Error (..),
  module X,
) where

import Polysemy

import qualified Data.Aeson as A
import RIO.Partial (read)
import qualified RIO.Text as T
import RIO.Time
import Web.Twitter.Conduit as X hiding (
  call,
  call',
 )
import qualified Web.Twitter.Conduit as Twitter

import NNU.Prelude hiding (Handler)

{-# ANN module ("HLint: ignore Use <$>" :: String) #-}

newtype Error = Error A.Value
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON)
instance Exception Error

data Twitter m a where
  Call_ :: -- brittany broken
    forall apiName a _a m.
    (A.FromJSON a, Typeable a) =>
    Twitter.APIRequest apiName _a ->
    Twitter m (Either Error a)

makeSem ''Twitter

call' ::
  forall (r :: [Effect]) a apiName _a.
  ( Member Twitter r
  , A.FromJSON a
  , Typeable a
  ) =>
  Twitter.APIRequest apiName _a ->
  Sem r (Either Error a)
call' = call_

call ::
  forall (r :: [Effect]) a apiName.
  ( Member Twitter r
  , A.FromJSON a
  , Typeable a
  ) =>
  Twitter.APIRequest apiName a ->
  Sem r (Either Error a)
call = call_

tweet ::
  Member Twitter r =>
  T.Text ->
  Sem r (Either Error Tweet)
tweet msg = call' $ Twitter.statusesUpdate msg

data Tweet = Tweet
  { tweetId :: Natural
  , createdAt :: ZonedTime
  }
  deriving stock (Show, Generic)

instance A.FromJSON Tweet where
  parseJSON = A.withObject "Tweet" $ \o -> do
    pure Tweet
      <*> (read <$> o A..: "id_str")
      <*> (pure . utcToZonedTime jst . fromTwitterTime =<< o A..: "created_at")
    where
      jst =
        TimeZone
          { timeZoneMinutes = 9 * 60
          , timeZoneSummerOnly = False
          , timeZoneName = "JST"
          }

data User = User
  { userId :: Natural
  , userName :: Text
  }
  deriving stock (Show, Eq, Generic)

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
