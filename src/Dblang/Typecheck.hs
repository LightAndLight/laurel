module Dblang.Typecheck (checkDefinition, checkExpr) where

import Dblang.Definition (Definition)
import Dblang.Expr (Expr)
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)

checkDefinition :: Definition -> m ()
checkDefinition = error "TODO: checkDefinition"

checkExpr :: (a -> Type) -> Syntax.Expr a -> Type -> m Expr
checkExpr = error "TODO: checkExpr"