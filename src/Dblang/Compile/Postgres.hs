{-# LANGUAGE OverloadedRecordDot #-}

module Dblang.Compile.Postgres (compileDefinition, compileQuery) where

import Bound (fromScope)
import Bound.Var (unvar)
import Data.Bifunctor (first)
import Data.Foldable (foldr')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
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

data TableExpr = TableExpr {expr :: Expr VarInfo, name :: Text, freeVars :: HashSet Text}

data Select
  = Select {expr :: Builder, from :: TableExpr, froms :: [From], wheres :: [Expr VarInfo]}
  | Yield {fieldNames :: Vector Text, expr :: Builder, wheres :: [Expr VarInfo]}

data From
  = CrossJoin {lateral :: Bool, tableExpr :: TableExpr}
  | InnerJoin {lateral :: Bool, tableExpr :: TableExpr, on :: NonEmpty (Expr VarInfo)}

-- | `CROSS JOIN t` with a `WHERE` condition `c` is equivalent to an `INNER JOIN t ON c`.
promoteCrossJoins :: Select -> Select
promoteCrossJoins select =
  case select of
    Select{expr, from, froms, wheres} ->
      let (froms', wheres') = go (HashSet.singleton from.name) froms wheres
       in Select{expr, from, froms = froms', wheres = wheres'}
    Yield{} -> select
 where
  go :: HashSet Text -> [From] -> [Expr VarInfo] -> ([From], [Expr VarInfo])
  go _ [] wheres = ([], wheres)
  go scope (from : froms) wheres =
    case from of
      CrossJoin{lateral, tableExpr} ->
        let newScope = HashSet.insert tableExpr.name scope
            (matched, unmatched) = List.partition (all (\varInfo -> varInfo.name `HashSet.member` newScope)) wheres
         in case NonEmpty.nonEmpty matched of
              Nothing -> first (from :) (go newScope froms unmatched)
              Just on -> first (InnerJoin{lateral, tableExpr, on} :) (go newScope froms unmatched)
      InnerJoin{lateral, tableExpr, on} ->
        let newScope = HashSet.insert tableExpr.name scope
            (matched, unmatched) = List.partition (all (\varInfo -> varInfo.name `HashSet.member` newScope)) wheres
         in case NonEmpty.nonEmpty matched of
              Nothing -> first (from :) (go newScope froms unmatched)
              Just on' -> first (InnerJoin{lateral, tableExpr, on = on <> on'} :) (go newScope froms unmatched)

compileSelect :: Select -> Builder
compileSelect select =
  case promoteCrossJoins select of
    Select{expr, from, froms, wheres} ->
      "SELECT "
        <> expr
        <> " FROM "
        <> queryParens (compileQuery id) from.expr
        <> " AS "
        <> Builder.fromText from.name
        <> foldMap ((" " <>) . printFrom) froms
        <> case wheres of
          [] -> mempty
          where_ : wheres' ->
            " WHERE " <> compileExpr id where_ <> foldMap ((" AND " <>) . compileExpr id) wheres'
    Yield{fieldNames, expr, wheres} ->
      "SELECT "
        <> commaSep (fmap Builder.fromText fieldNames)
        <> " FROM "
        <> parens
          ( "VALUES "
              <> parens expr
          )
        <> " AS _"
        <> ( case wheres of
              [] ->
                mempty
              where_ : wheres' ->
                " WHERE "
                  <> compileExpr id where_
                  <> foldMap ((" AND " <>) . compileExpr id) wheres'
           )

printFrom :: From -> Builder
printFrom from =
  case from of
    CrossJoin{lateral, tableExpr} ->
      "CROSS JOIN "
        <> (if lateral then "LATERAL " else "")
        <> queryParens (compileQuery id) tableExpr.expr
        <> " AS "
        <> Builder.fromText tableExpr.name
    InnerJoin{lateral, tableExpr, on} ->
      "INNER JOIN "
        <> (if lateral then "LATERAL " else "")
        <> queryParens (compileQuery id) tableExpr.expr
        <> " AS "
        <> Builder.fromText tableExpr.name
        <> " ON "
        <> compileExpr id (NonEmpty.head on)
        <> foldMap ((" AND " <>) . compileExpr id) (NonEmpty.tail on)

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
      Yield
        { fieldNames =
            case Type.matchRecord $ Expr.typeOf ((.type_) . varInfo) expr of
              Nothing -> ["it"]
              Just fields -> fmap fst fields
        , expr = compileSelectList varInfo value
        , wheres = []
        }
    Expr.From relation name type_ rest ->
      -- relation : Relation a
      let relation' =
            TableExpr
              { expr = fmap varInfo relation
              , name
              , freeVars = foldr' (HashSet.insert . (.name) . varInfo) mempty relation
              }
       in case compileRelation (unvar (\() -> VarInfo{name, type_, origin = TableVar}) varInfo) (fromScope rest) of
            Yield{expr = yieldExpr, wheres} ->
              Select{expr = yieldExpr, from = relation', froms = [], wheres}
            Select{expr = selectExpr, from, froms, wheres} ->
              Select
                { expr = selectExpr
                , from = relation'
                , froms = CrossJoin{lateral = name `isReferencedBy` from, tableExpr = from} : froms
                , wheres
                }
    Expr.Filter name type_ condition relation ->
      -- condition : (a -> Bool)
      -- relation : Relation a
      Select
        { expr = Builder.fromText name <> ".*"
        , from =
            TableExpr
              { expr = fmap varInfo relation
              , name
              , freeVars = foldr' (HashSet.insert . (.name) . varInfo) mempty relation
              }
        , froms = []
        , wheres =
            [ fmap
                (unvar (\() -> VarInfo{name, type_, origin = TableVar}) varInfo)
                (fromScope condition)
            ]
        }
    Expr.When condition relation ->
      -- condition : (a -> Bool)
      -- relation : Relation a
      case compileRelation varInfo relation of
        Yield{fieldNames, expr = yieldExpr, wheres} ->
          Yield{fieldNames, expr = yieldExpr, wheres = fmap varInfo condition : wheres}
        Select{expr = selectExpr, from, froms, wheres} ->
          Select{expr = selectExpr, from, froms, wheres = fmap varInfo condition : wheres}
    Expr.Lam{} ->
      error "compileRelation: impossible Lam"
    Expr.Splat{} ->
      error "compileRelation: impossible Splat"
    Expr.Record{} ->
      error "compileRelation: impossible Record"
    Expr.Equals{} ->
      error "compileRelation: impossible Record"
    Expr.Int{} ->
      error "compileRelation: impossible Record"

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
      compileSelect $ compileRelation varInfo expr
    Expr.From{} ->
      compileSelect $ compileRelation varInfo expr
    Expr.Filter{} ->
      compileSelect $ compileRelation varInfo expr
    Expr.When{} ->
      compileSelect $ compileRelation varInfo expr
    Expr.App{} ->
      compileExpr varInfo expr
    Expr.Dot{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)
    Expr.Splat{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)
    Expr.Record{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)
    Expr.Equals{} ->
      "VALUES " <> parens (compileSelectList varInfo expr)
    Expr.Int{} ->
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
    Expr.Filter{} ->
      error "TODO: compileSelectList Filter"
    Expr.When{} ->
      error "TODO: compileSelectList When"
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
    Expr.Equals{} ->
      parens (compileExpr varInfo expr) <> " AS it"
    Expr.Int{} ->
      parens (compileExpr varInfo expr) <> " AS it"

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
    Expr.Filter{} ->
      error "TODO: compileExpr Filter"
    Expr.When{} ->
      error "TODO: compileExpr When"
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
    Expr.Int i ->
      Builder.fromString $ show i
    Expr.Equals a b ->
      compileExpr varInfo a <> " = " <> compileExpr varInfo b