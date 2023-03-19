module Dblang.Definition (Definition (..), Constraint (..)) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Dblang.Expr (Expr)
import Dblang.Type (Type)

data Definition = Table
  { name :: Text
  , types :: Vector (Text, Type)
  , inFields :: Vector (Text, Type)
  , outFields :: Vector (Text, Type)
  , constraints :: Vector Constraint
  }
  deriving (Eq, Show)

data Constraint
  = Default {field :: Text, value :: Expr Void}
  | Key {values :: Vector Text}
  | PrimaryKey {values :: Vector Text}
  deriving (Eq, Show)