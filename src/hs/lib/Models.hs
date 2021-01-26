{-# LANGUAGE FlexibleInstances                   #-}
{-# LANGUAGE GADTs                               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving          #-}
{-# LANGUAGE MultiParamTypeClasses               #-}
{-# LANGUAGE QuasiQuotes                         #-}
{-# LANGUAGE TemplateHaskell                     #-}
{-# LANGUAGE TypeFamilies                        #-}
{-# LANGUAGE DerivingStrategies                  #-}
{-# LANGUAGE UndecidableInstances                #-}
{-# LANGUAGE NoStrictData                        #-} -- https://github.com/yesodweb/persistent/issues/953
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Models where

import           RIO
import qualified RIO.Map             as M

import           Data.Time.Clock     (UTCTime)
import           Data.Time.Calendar  (Day)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           Nijisanji

share
  [mkPersist sqlSettings, mkMigrate "migrateUpdateInfo"]
    [persistLowerCase|
      TwitterNameUpdate json
        name        Text
        exGroup     Group
        twitterName Text
        tweetId     Int64
        updateTime  UTCTime

        UniqueTweetId tweetId
        deriving Eq
        deriving Show
        deriving Typeable
        deriving Generic
    |]
instance NFData TwitterNameUpdate

share
  [mkPersist sqlSettings, mkMigrate "migrateRecordOfDay"]
    [persistLowerCase|
      TwitterNameRecordOfDay json
        jstDay      Day
        nameMap     NameMap

        UniqueDay   jstDay
        deriving Eq
        deriving Show
        deriving Typeable
        deriving Generic
    |]
instance NFData TwitterNameRecordOfDay

applyUpdate :: TwitterNameUpdate -> NameMap -> NameMap
applyUpdate TwitterNameUpdate{..} =
  M.insert twitterNameUpdateName UpdateInfo
    { twitterName = twitterNameUpdateTwitterName
    , updateTime  = twitterNameUpdateUpdateTime
    , tweetId     = twitterNameUpdateTweetId
    }

