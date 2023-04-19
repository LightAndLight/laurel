module Laurel.Command (Command (..), result) where

import Data.Void (Void)
import Laurel.Definition.Table (Table)
import Laurel.Expr (Expr)
import Laurel.Type (Type)

data Command
  = Eval {value :: Expr Void, type_ :: Type}
  | Insert {table :: Table, value :: Expr Void}
  deriving (Eq, Show)

result :: Command -> Maybe Type
result command =
  case command of
    Eval{type_} -> Just type_
    Insert{} -> Nothing