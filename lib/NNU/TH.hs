{-# LANGUAGE BlockArguments #-}
module NNU.TH
  ( revision
  ) where

import           Language.Haskell.TH
import qualified RIO.Text as T

import           NNU.Prelude

revision :: Q Exp
revision = stringE =<< runIO (T.unpack <$> readFileUtf8 "./git-revision")
