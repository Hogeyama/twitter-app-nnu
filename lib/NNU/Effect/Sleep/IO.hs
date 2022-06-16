module NNU.Effect.Sleep.IO (
  runSleep,
) where

import Polysemy

import NNU.Effect.Sleep
import NNU.Prelude

runSleep :: Member (Embed IO) r => Sem (Sleep ': r) a -> Sem r a
runSleep = interpret $ \case
  SleepSec sec -> embed $ threadDelay (sec * 1000 * 1000)
