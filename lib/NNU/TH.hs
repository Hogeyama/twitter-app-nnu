module NNU.TH
  ( revision
  ) where

import           Language.Haskell.TH
import           RIO
import           System.ReadEnvVar              ( lookupEnvDef )

revision :: Q Exp
revision = stringE =<< runIO (lookupEnvDef "GIT_REVISION" "UNKNOWN")

