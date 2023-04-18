module Dblang.Expr.Pretty (prettyExpr) where

import qualified Data.Text as Text
import Dblang.Expr (Expr (..))
import Pretty (Horizontally (..), IsLines, horizontally)
import qualified Pretty

prettyExpr :: IsLines lines => (a -> lines) -> Expr a -> lines
prettyExpr prettyVar expr =
  case expr of
    Name name -> Pretty.line name
    Ctor name args ->
      horizontally $
        Pretty.line name
          <> ( if null args
                then mempty
                else
                  Pretty.line "("
                    <> Pretty.sepBy
                      (fmap (prettyExpr (Horizontally . prettyVar)) args)
                      (Pretty.line ", ")
                    <> Pretty.line ")"
             )
    Var var -> prettyVar var
    Lam{} ->
      error "TODO: prettyExpr: Lam"
    App{} ->
      error "TODO: prettyExpr: App"
    Yield{} ->
      error "TODO: prettyExpr: Yield"
    For{} ->
      error "TODO: prettyExpr: For"
    Where{} ->
      error "TODO: prettyExpr: Where"
    GroupBy{} ->
      error "TODO: prettyExpr: GroupBy"
    Record{} ->
      error "TODO: prettyExpr: Record"
    Dot{} ->
      error "TODO: prettyExpr: Dot"
    Splat{} ->
      error "TODO: prettyExpr: Splat"
    Int i -> Pretty.line . Text.pack $ show i
    Bool b -> Pretty.line $ if b then "true" else "false"
    String s -> Pretty.line . Text.pack $ show s
    Equals a b ->
      horizontally $
        prettyExpr (Horizontally . prettyVar) a
          <> Pretty.line " == "
          <> prettyExpr (Horizontally . prettyVar) b