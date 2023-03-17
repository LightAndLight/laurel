module Dblang.Type (Type (..), record) where

import Data.Text (Text)
import Data.Vector (Vector)

data Type
  = Name Text
  | App Type Type
  | RCons Text Type Type
  | RNil
  deriving (Eq, Show)

record :: Vector (Text, Type) -> Type
record = App (Name "Record") . foldr (uncurry RCons) RNil