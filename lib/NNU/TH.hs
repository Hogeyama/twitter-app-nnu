module NNU.TH
  ( revision
  ) where

import           Language.Haskell.TH
import           System.ReadEnvVar              ( lookupEnvDef )

import           NNU.Prelude

revision :: Q Exp
revision = stringE =<< runIO (lookupEnvDef "GIT_REVISION" "UNKNOWN")

