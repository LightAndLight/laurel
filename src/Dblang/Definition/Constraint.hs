module Dblang.Definition.Constraint (Constraint (..)) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Dblang.Expr (Expr)

data Constraint
  = Default {field :: Text, value :: Expr Void}
  | Key {values :: Vector Text}
  | PrimaryKey {values :: Vector Text}
  deriving (Eq, Show)