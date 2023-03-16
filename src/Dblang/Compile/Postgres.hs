{-# OPTIONS_GHC -Wno-unused-imports #-}

module Dblang.Compile.Postgres (compileDefinition, compileQuery) where

import Bound (instantiate1)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Dblang.Definition (Definition)
import Dblang.Expr (Expr)
import qualified Dblang.Expr as Expr

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

queryParens :: (Expr Void -> Builder) -> Expr Void -> Builder
queryParens f expr =
  case expr of
    Expr.Name{} -> f expr
    _ -> parens $ f expr

data Select
  = Select {expr :: Builder, from :: Builder, froms :: [From]}
  | Yield {expr :: Builder}

data From = CrossJoin Builder

printSelect :: Select -> Builder
printSelect select =
  case select of
    Select{expr, from, froms} -> "SELECT " <> expr <> " FROM " <> from <> foldMap ((" " <>) . printFrom) froms
    Yield{expr} -> "SELECT " <> expr <> " FROM (VALUES (null))"

printFrom :: From -> Builder
printFrom from =
  case from of
    CrossJoin value -> "CROSS JOIN " <> value

-- | Compile an `Expr` of type `Relation a`
compileRelation :: Expr Void -> Select
compileRelation expr =
  case expr of
    Expr.Var name ->
      absurd name
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
      Yield{expr = compileSelectList value}
    Expr.From relation rest ->
      -- relation : Relation a
      -- rest : a -> Relation b
      case rest of
        Expr.Var name ->
          absurd name
        -- name : a -> Relation b
        Expr.Name{} ->
          error "TODO: compileRelation/From: Name"
        -- r.x : a -> Relation b
        Expr.Dot{} ->
          error "TODO: compileRelation/From: Dot"
        -- f x_1 x_2 ... x_n : a -> Relation b
        Expr.App{} ->
          error "TODO: compileRelation/From: App"
        -- (\x -> y) : a -> Relation b
        Expr.Lam name body ->
          let relation' = queryParens compileQuery relation <> " AS " <> Builder.fromText name
           in case compileRelation (instantiate1 (Expr.Name name) body) of
                Yield{expr = yieldExpr} -> Select{expr = yieldExpr, from = relation', froms = []}
                Select{expr = selectExpr, from, froms} -> Select{expr = selectExpr, from = relation', froms = CrossJoin from : froms}
        -- yield : Relation ?
        Expr.Yield{} ->
          error "compileRelation/From: impossible Yield"
        -- from a b : Relation ?
        Expr.From{} ->
          error "compileRelation/From: impossible From"
        Expr.Splat{} ->
          error "compileRelation/From: impossible Splat"
        Expr.Record{} ->
          error "compileRelation/From: impossible Splat"
    Expr.Lam{} ->
      error "compileRelation: impossible Lam"
    Expr.Splat{} ->
      error "compileRelation: impossible Splat"
    Expr.Record{} ->
      error "compileRelation: impossible Splat"

compileQuery :: Expr Void -> Builder
compileQuery expr =
  case expr of
    Expr.Var name ->
      absurd name
    Expr.Name name ->
      Builder.fromText name
    Expr.Lam name body ->
      error "TODO: compileExpr Lam" name body
    Expr.Yield{} ->
      printSelect $ compileRelation expr
    Expr.From{} ->
      printSelect $ compileRelation expr
    Expr.App{} ->
      "SELECT "
        <> compileSelectList expr
        <> " FROM (VALUES (null))"
    Expr.Dot{} ->
      "SELECT "
        <> compileSelectList expr
        <> " FROM (VALUES (null))"
    Expr.Splat{} ->
      "SELECT "
        <> compileSelectList expr
        <> " FROM (VALUES (null))"
    Expr.Record{} ->
      "SELECT "
        <> compileSelectList expr
        <> " FROM (VALUES (null))"

compileSelectList :: Expr Void -> Builder
compileSelectList expr =
  case expr of
    Expr.Var name ->
      absurd name
    Expr.Name name ->
      Builder.fromText name
    Expr.Lam{} ->
      error "TODO: compileSelectList Lam"
    Expr.Yield{} ->
      error "TODO: compileSelectList Yield"
    Expr.From{} ->
      error "TODO: compileSelectList From"
    Expr.App function args ->
      Builder.fromText function <> "(" <> commaSep (fmap compileExpr args) <> ")" <> " AS it"
    Expr.Dot expr' name ->
      compileExpr expr' <> "." <> Builder.fromText name <> " AS it"
    Expr.Splat expr' names ->
      commaSep (fmap (\name -> compileExpr expr' <> "." <> Builder.fromText name <> " AS " <> Builder.fromText name) names)
    Expr.Record fields ->
      commaSep (fmap (\(name, value) -> compileExpr value <> " AS " <> Builder.fromText name) fields)

compileExpr :: Expr Void -> Builder
compileExpr expr =
  case expr of
    Expr.Var name ->
      absurd name
    Expr.Name name ->
      Builder.fromText name
    Expr.Lam{} ->
      error "TODO: compileExpr Lam"
    Expr.Yield{} ->
      error "TODO: compileExpr Yield"
    Expr.From{} ->
      error "TODO: compileExpr From"
    Expr.App function args ->
      Builder.fromText function <> "(" <> commaSep (fmap compileExpr args) <> ")"
    Expr.Dot expr' name ->
      compileExpr expr' <> "." <> Builder.fromText name
    Expr.Splat expr' names ->
      "ROW" <> parens (commaSep (fmap (\name -> compileExpr expr' <> "." <> Builder.fromText name) names))
    Expr.Record fields ->
      "ROW" <> parens (commaSep (fmap (\(_name, value) -> compileExpr value) fields))