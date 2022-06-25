{-# LANGUAGE BlockArguments #-}

module NNU.TH (
  revision,
) where

import Language.Haskell.TH
import NNU.Prelude
import qualified RIO.Text as T

revision :: Q Exp
revision = stringE =<< runIO (T.unpack <$> readFileUtf8 "./git-revision")
