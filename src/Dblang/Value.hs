module Dblang.Value (Multiset, fromVector, Value (..)) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

newtype Multiset a = Multiset (Vector a)
  deriving (Eq, Show)

fromVector :: Vector a -> Multiset a
fromVector = Multiset

data Value
  = Relation (Multiset Value)
  | Record (HashMap Text Value)
  | Int Int
  | Bool Bool
  | String Text
  | Unit
  deriving (Eq, Show)