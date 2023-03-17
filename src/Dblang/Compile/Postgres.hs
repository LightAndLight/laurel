{-# LANGUAGE OverloadedRecordDot #-}

module Dblang.Compile.Postgres (compileDefinition, compileQuery) where

import Bound (fromScope)
import Bound.Var (unvar)
import Data.Foldable (foldr')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Dblang.Definition (Definition)
import Dblang.Expr (Expr)
import qualified Dblang.Expr as Expr
import Dblang.Type (Type)
import qualified Dblang.Type as Type

data VarOrigin
  = TableVar
  | ColumnVar

data VarInfo = VarInfo {name :: Text, type_ :: Type, origin :: VarOrigin}

compileDefinition :: Definition -> Builder
compileDefinition = error "TODO: compileDefinition"

commaSep :: Vector Builder -> Builder
commaSep items =
  case Vector.uncons items of
    Nothing -> mempty
    Just (item, items') ->
      item <> foldMap (", " <>) items'

parens :: Builder -> Builder
parens x = "(" <> x <> ")"

queryParens :: (Expr a -> Builder) -> Expr a -> Builder
queryParens f expr =
  case expr of
    Expr.Name{} -> f expr
    Expr.App{} -> f expr
    _ -> parens $ f expr

data TableExpr = TableExpr {value :: Builder, freeVars :: HashSet Text}

data Select
  = Select {expr :: Builder, from :: TableExpr, froms :: [From]}
  | Yield {expr :: Builder}

data From = CrossJoin {lateral :: Bool, tableExpr :: TableExpr}

printSelect :: Select -> Builder
printSelect select =
  case select of
    Select{expr, from, froms} -> "SELECT " <> expr <> " FROM " <> from.value <> foldMap ((" " <>) . printFrom) froms
    Yield{expr} -> "VALUES " <> parens expr

printFrom :: From -> Builder
printFrom from =
  case from of
    CrossJoin{lateral, tableExpr} -> "CROSS JOIN " <> (if lateral then "LATERAL " else "") <> tableExpr.value

isReferencedBy :: Text -> TableExpr -> Bool
isReferencedBy name tableExpr =
  name `HashSet.member` tableExpr.freeVars

-- | Compile an `Expr` of type `Relation a`
compileRelation :: (a -> VarInfo) -> Expr a -> Select
compileRelation varInfo expr =
  case expr of
    Expr.Var{} ->
      error "TODO: compileRelation Var"
    -- name : Relation a
    Expr.Name{} ->
      error "TODO: compileRelation Name"
    -- f x_1 x_2 ... x_n : Relation a
    Expr.App{} ->
      error "TODO: compileRelation App"
    -- r.x : Relation b
    Expr.Dot{} ->
      error "TODO: compileRelation Dot"
    Expr.Yield value ->
      Yield{expr = compileSelectList varInfo value}
    Expr.From relation name type_ rest ->
      -- relation : Relation a
      let relation' =
            TableExpr
              { value = queryParens (compileQuery varInfo) relation <> " AS " <> Builder.fromText name
              , freeVars = foldr' (HashSet.insert . (.name) . varInfo) mempty relation
              }
       in case compileRelation (unvar (\() -> VarInfo{name, type_, origin = TableVar}) varInfo) (fromScope rest) of
            Yield{expr = yieldExpr} ->
              Select{expr = yieldExpr, from = relation', froms = []}
            Select{expr = selectExpr, from, froms} ->
              Select
                { expr = selectExpr
                , from = relation'
                , froms = CrossJoin{lateral = name `isReferencedBy` from, tableExpr = from} : froms
                }
    Expr.Lam{} ->
      error "compileRelation: impossible Lam"
    Expr.Splat{} ->
      error "compileRelation: impossible Splat"
    Expr.Record{} ->
      error "compileRelation: impossible Splat"

compileQuery :: (a -> VarInfo) -> Expr a -> Builder
compileQuery varInfo expr =
  case expr of
    Expr.Var{} ->
      error "TODO: compileExpr Var"
    Expr.Name name ->
      Builder.fromText name
    Expr.Lam{} ->
      error "TODO: compileExpr Lam"
    Expr.Yield{} ->
      printSelect $ compileRelation varInfo expr
    Expr.From{} ->
      printSelect $ compileRelation varInfo expr
    Expr.App{} ->
      compileExpr varInfo expr
    Expr.Dot{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)
    Expr.Splat{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)
    Expr.Record{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)

compileSelectList :: (a -> VarInfo) -> Expr a -> Builder
compileSelectList varInfo expr =
  case expr of
    Expr.Name name ->
      Builder.fromText name
    Expr.Var var ->
      let info = varInfo var
       in case info.type_ of
            Type.App (Type.Name "Record") _ ->
              Builder.fromText info.name <> ".*"
            _ ->
              Builder.fromText info.name
    Expr.Lam{} ->
      error "TODO: compileSelectList Lam"
    Expr.Yield{} ->
      error "TODO: compileSelectList Yield"
    Expr.From{} ->
      error "TODO: compileSelectList From"
    Expr.Splat expr' names ->
      commaSep (fmap (\name -> compileExpr varInfo expr' <> "." <> Builder.fromText name <> " AS " <> Builder.fromText name) names)
    Expr.Record fields ->
      commaSep (fmap (\(name, value) -> compileExpr varInfo value <> " AS " <> Builder.fromText name) fields)
    Expr.App type_ _ _ ->
      case type_ of
        Type.App (Type.Name "Record") _ ->
          parens (compileExpr varInfo expr) <> ".*"
        _ ->
          compileExpr varInfo expr <> " AS it"
    Expr.Dot type_ _ _ ->
      case type_ of
        Type.App (Type.Name "Record") _ ->
          parens (compileExpr varInfo expr) <> ".*"
        _ ->
          compileExpr varInfo expr <> " AS it"

compileExpr :: (a -> VarInfo) -> Expr a -> Builder
compileExpr varInfo expr =
  case expr of
    Expr.Var var ->
      Builder.fromText (varInfo var).name
    Expr.Name name ->
      Builder.fromText name
    Expr.Lam{} ->
      error "TODO: compileExpr Lam"
    Expr.Yield{} ->
      error "TODO: compileExpr Yield"
    Expr.From{} ->
      error "TODO: compileExpr From"
    Expr.App _ function args ->
      case function of
        Expr.Name name ->
          Builder.fromText name <> "(" <> commaSep (fmap (compileExpr varInfo) args) <> ")"
        _ ->
          error "TODO: compileExpr App"
    Expr.Dot _ expr' name ->
      ( case expr' of
          Expr.Name{} ->
            id
          Expr.Var var ->
            case (varInfo var).origin of
              TableVar ->
                id
              ColumnVar ->
                parens
          _ ->
            parens
      )
        (compileExpr varInfo expr')
        <> "."
        <> Builder.fromText name
    Expr.Splat expr' names ->
      "ROW" <> parens (commaSep (fmap (\name -> compileExpr varInfo expr' <> "." <> Builder.fromText name) names))
    Expr.Record fields ->
      "ROW" <> parens (commaSep (fmap (\(_name, value) -> compileExpr varInfo value) fields))