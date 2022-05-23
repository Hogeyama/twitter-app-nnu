module Main where

import Build_doctests (flags, module_sources, pkgs)
import RIO
import Test.DocTest (doctest)

main :: IO ()
main = doctest args
  where
    args = flags ++ ["-fplugin=Polysemy.Plugin"] ++ pkgs ++ module_sources
