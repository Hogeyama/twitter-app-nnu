{-# LANGUAGE TemplateHaskell #-}

module NNU.Effect.Sleep (
  Sleep (..),
  sleepSec,
) where

import Polysemy

import NNU.Prelude

data Sleep m a where
  SleepSec :: Int -> Sleep m ()

makeSem ''Sleep
