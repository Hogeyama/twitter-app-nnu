{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Log.StdoutLogImpl (runLog) where

import Polysemy

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified RIO.Text as T

import NNU.Effect.Log
import NNU.Prelude hiding (Handler, (.=))
import qualified NNU.TH as TH

runLog ::
  forall a r.
  Member (Embed IO) r =>
  Sem (Log ': r) a ->
  Sem r a
runLog = interpret $ \case
  Log callStack logLevel message -> do
    let levelT = case logLevel of
          LevelDebug -> "debug"
          LevelInfo -> "info"
          LevelWarn -> "warning"
          LevelError -> "error"
          LevelOther txt -> txt
        msg =
          A.object
            [ "location" .= replaceColon (utf8BuilderToText (displayCallStack callStack))
            , "log_level" .= levelT
            , "body" .= message
            , "revision" A..= ($(TH.revision) :: Text)
            ]
        msgT = decodeUtf8Lenient $ toStrictBytes $ A.encode msg
    hPutBuilder stderr $ encodeUtf8Builder msgT <> "\n"
  where
    -- Slack で :5: とかが絵文字になってしまう
    replaceColon = T.map (\c -> if c == ':' then '-' else c)
