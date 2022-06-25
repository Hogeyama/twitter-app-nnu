{-# OPTIONS_GHC -Wno-orphans #-}

module NNU.Prelude (
  module Export,
  ISO8601 (..),
  readEnvFlag,
) where

import Data.Time.Format.ISO8601 (ISO8601 (..))
import Lens.Micro.Platform as Export
import RIO as Export
import RIO.Orphans as Export
import RIO.Time as Export
import System.ReadEnvVar (lookupEnv)

-------------------------------------------------------------------------------
-- Orphan instance
-------------------------------------------------------------------------------

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC
instance Ord ZonedTime where
  compare = compare `on` zonedTimeToUTC

readEnvFlag :: MonadIO m => String -> Bool -> m Bool
readEnvFlag var def =
  lookupEnv var >>= \case
    Nothing -> pure def
    Just val
      | Just n <- readMaybe @Natural val -> pure (n > 0)
      | Just b <- readMaybe @Bool val -> pure b
      | otherwise -> pure False
