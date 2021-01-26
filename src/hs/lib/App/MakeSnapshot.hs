{-# OPTIONS_GHC -Wno-unused-imports #-}
module App.MakeSnapshot where

import           Prelude                        (pred)
import           RIO                            hiding (Data)
import qualified RIO.List                       as List
import qualified RIO.Map                        as M
import           RIO.Partial                    (fromJust)
import qualified RIO.Text                       as T
import           RIO.Time

import           Control.Lens                   (both, views, (%~), (?~))
import           Data.Aeson                     (FromJSON)
import qualified Data.ByteString.Char8          as BC
import           Data.Extensible.Reexport       hiding (Member)
import qualified Data.Text.IO                   as T
import           Database.Persist               as Persist
import           Database.Persist.Sql           (ConnectionPool)
import           System.Environment             (getEnv)
import           System.IO.Unsafe               (unsafePerformIO)
import           Web.Twitter.Conduit
import qualified Web.Twitter.Conduit.Parameters as L (count)
import qualified Web.Twitter.Types.Lens         as L

import           Util
import           Models
import           Config                         (runDb, HasPool)
import           Nijisanji

recordOfDay :: forall env. HasPool env => RIO env ()
recordOfDay = do
    now <- getCurrentTimeInJST
    -- Util.notifyHogeyamaSlack $ "recordOfDay: " <> tshow now
    waitEndOfDay now
  where
    waitEndOfDay :: LocalTime -> RIO env ()
    waitEndOfDay now = do
      let today =
              localDay now
          restOfDay = -- micro second
              24 * 60 * 60 * 1000 * 1000
              - diffTimeToPicoseconds (timeOfDayToTime (localTimeOfDay now))
                    `div` (1000 * 1000)
      threadDelay $ fromIntegral restOfDay
      processEndOfDay today

    processEndOfDay :: Day -> RIO env ()
    processEndOfDay today = do
        now <- getCurrentTimeInJST
        -- Util.notifyHogeyamaSlack $ "processEndOfDay: " <> tshow now
        when (localDay now > today) $ do
          nameMap <- makeDaySnapshot today
          runDb $ insert_ (TwitterNameRecordOfDay today nameMap)
        waitEndOfDay now

    makeDaySnapshot :: Day -> RIO env NameMap
    makeDaySnapshot today =
        makeSnapshot $ LocalTime (pred today) (TimeOfDay 23 59 60)

-- TODO defaultの名前
makeSnapshot :: HasPool env => LocalTime -> RIO env NameMap
makeSnapshot time = do
    let day = localDay time
    yesterdaySnapshot <- runDb (Persist.getBy (UniqueDay (pred day))) >>= \case
        Nothing -> return M.empty
        Just x -> return $ twitterNameRecordOfDayNameMap (entityVal x)
    let min' = jstToUTC (LocalTime day midnight)
        max' = jstToUTC time
    updates <- fmap (fmap entityVal) $
        runDb $ Persist.selectList
          [ Filter
              { filterField  = TwitterNameUpdateUpdateTime
              , filterValue  = FilterValue min'
              , filterFilter = Persist.Ge
              }
          , Filter
              { filterField  = TwitterNameUpdateUpdateTime
              , filterValue  = FilterValue max'
              , filterFilter = Persist.Le
              }
          ]
          [ Persist.Asc TwitterNameUpdateUpdateTime ]
    return $ foldl' (flip applyUpdate) yesterdaySnapshot updates

