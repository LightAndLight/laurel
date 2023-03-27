module Dblang.Command (Command (..)) where

import Data.Void (Void)
import Dblang.Definition.Table (Table)
import Dblang.Expr (Expr)

data Command = Insert {table :: Table, value :: Expr Void}
  deriving (Eq, Show)