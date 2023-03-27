module Dblang.Command (Command (..)) where

import Data.Void (Void)
import Dblang.Definition.Table (Table)
import Dblang.Expr (Expr)
import Dblang.Type (Type)

data Command
  = Eval {value :: Expr Void, type_ :: Type}
  | Insert {table :: Table, value :: Expr Void, type_ :: Type}
  deriving (Eq, Show)