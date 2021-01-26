{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Data.Extensible.Reexport
  ( module D
  , Associate'
  , IncludeAssoc'
  ) where

import Data.Extensible as D
import GHC.TypeLits
import Data.Kind (Type)

type family Fields env :: [Assoc Symbol Type] where
  Fields (Record xs) = xs
  Fields ty = TypeError (Text "Data.Extensible.Reexport.Fields: " :<>: ShowType ty :<>: Text " is not a record type")
type IsRecord' env = Record (Fields env) ~ env
type Associate' k v env = (IsRecord' env, Lookup (Fields env) k v)
type IncludeAssoc' env xs = (IsRecord' env, IncludeAssoc (Fields env) xs)

