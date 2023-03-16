module Dblang.Definition (Definition (..), Item (..)) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Dblang.Expr (Expr)
import Dblang.Type (Type)

data Definition = Table {name :: Text, items :: Vector Item}

data Item
  = Field {name :: Text, type_ :: Type}
  | Constraint {name :: Text, arguments :: Vector (Expr Void)}
  deriving (Eq, Show)