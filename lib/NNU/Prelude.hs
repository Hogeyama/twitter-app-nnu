{-# OPTIONS_GHC -Wno-orphans #-}

module NNU.Prelude (
  module Export,
  ISO8601 (..),
) where

import Data.Time.Format.ISO8601 (ISO8601 (..))
import Lens.Micro.Platform as Export
import RIO as Export
import RIO.Orphans as Export
import RIO.Time as Export

-------------------------------------------------------------------------------
-- Orphan instance
-------------------------------------------------------------------------------

instance Eq ZonedTime where
  (==) = (==) `on` zonedTimeToUTC
instance Ord ZonedTime where
  compare = compare `on` zonedTimeToUTC
