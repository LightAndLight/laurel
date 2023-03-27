{-# LANGUAGE OverloadedRecordDot #-}

module Dblang.Compile.Postgres (compileDefinition, compileQuery, compileCommand) where

import Bound (fromScope)
import Bound.Var (unvar)
import Data.Bifunctor (first)
import Data.Foldable (foldl', foldr')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Dblang.Command (Command (..))
import Dblang.Definition (Definition)
import qualified Dblang.Definition as Definition
import Dblang.Definition.Constraint (Constraint (..))
import Dblang.Definition.Table (Table (..))
import Dblang.Expr (Expr)
import qualified Dblang.Expr as Expr
import Dblang.Type (Type)
import qualified Dblang.Type as Type

data VarOrigin
  = TableVar
  | ColumnVar

data VarInfo = VarInfo {name :: Text, type_ :: Type, origin :: VarOrigin}

compileDefinition :: Definition -> Builder
compileDefinition definition =
  case definition of
    Definition.Table Table{name, types, inFields = _, outFields, constraints} ->
      let (defaults, constraints') = Vector.mapMaybeM compileConstraint constraints
       in "CREATE TABLE "
            <> Builder.fromText name
            <> " ("
            <> sepByList
              ( ("\n" <>)
                  <$> foldr
                    ( (:)
                        . uncurry
                          ( compileField
                              (foldl' (\acc (typeName, typeValue) -> HashMap.insert typeName typeValue acc) mempty types)
                              defaults
                          )
                    )
                    (Vector.toList constraints')
                    outFields
              )
              ","
            <> "\n)"
 where
  compileConstraint :: Constraint -> (HashMap Text (Expr Void), Maybe Builder)
  compileConstraint constraint =
    case constraint of
      Default name value -> (HashMap.singleton name value, Nothing)
      Key fields -> (mempty, Just $ "UNIQUE " <> parens (commaSep $ Builder.fromText <$> fields))
      PrimaryKey fields -> (mempty, Just $ "PRIMARY KEY " <> parens (commaSep $ Builder.fromText <$> fields))

  compileField :: HashMap Text Type -> HashMap Text (Expr Void) -> Text -> Type -> Builder
  compileField typeDefinitions defaults name type_ =
    Builder.fromText name
      <> " "
      <> compileType (Type.replaceDefinitions typeDefinitions type_)
      <> " NOT NULL"
      <> foldMap ((" DEFAULT " <>) . compileExpr absurd) (HashMap.lookup name defaults)

  compileType :: Type -> Builder
  compileType type_ =
    case type_ of
      Type.Name name ->
        case name of
          "Int" -> "INT"
          "Bool" -> "BOOL"
          "String" -> "TEXT"
          _ ->
            error $ "compileType: invalid type " <> show type_
      Type.App (Type.Name "Record") _rows ->
        error "TODO: compileType: Record"
      Type.App (Type.Name "Relation") a ->
        compileType a <> "[]"
      _ ->
        error $ "compileType: invalid type " <> show type_

sepBy :: Vector Builder -> Builder -> Builder
sepBy items sep =
  case Vector.uncons items of
    Nothing -> mempty
    Just (item, items') ->
      item <> foldMap (sep <>) items'

sepByList :: [Builder] -> Builder -> Builder
sepByList items sep =
  case items of
    [] -> mempty
    item : items' ->
      item <> foldMap (sep <>) items'

commaSep :: Vector Builder -> Builder
commaSep items = items `sepBy` ", "

parens :: Builder -> Builder
parens x = "(" <> x <> ")"

queryParens :: (Expr a -> Builder) -> Expr a -> Builder
queryParens f expr =
  case expr of
    Expr.Name{} -> f expr
    Expr.App{} -> f expr
    _ -> parens $ f expr

data TableExpr = TableExpr {expr :: Expr VarInfo, alias :: Maybe Text, freeVars :: HashSet Text}

data StarOr a = Star | NotStar a

data Select
  = Select {expr :: StarOr (Expr VarInfo), from :: TableExpr, froms :: [From], wheres :: [Expr VarInfo]}
  | Yield {fieldNames :: Vector Text, value :: Expr VarInfo, wheres :: [Expr VarInfo]}

data From
  = CrossJoin {lateral :: Bool, tableExpr :: TableExpr}
  | InnerJoin {lateral :: Bool, tableExpr :: TableExpr, on :: NonEmpty (Expr VarInfo)}

-- | `CROSS JOIN t` with a `WHERE` condition `c` is equivalent to an `INNER JOIN t ON c`.
promoteCrossJoins :: Select -> Select
promoteCrossJoins select =
  case select of
    Select{expr, from, froms, wheres} ->
      let (froms', wheres') = go (foldMap HashSet.singleton from.alias) froms wheres
       in Select{expr, from, froms = froms', wheres = wheres'}
    Yield{} -> select
 where
  go :: HashSet Text -> [From] -> [Expr VarInfo] -> ([From], [Expr VarInfo])
  go _ [] wheres = ([], wheres)
  go scope (from : froms) wheres =
    case from of
      CrossJoin{lateral, tableExpr} ->
        let newScope = maybe id HashSet.insert tableExpr.alias scope
            (matched, unmatched) = List.partition (all (\varInfo -> varInfo.name `HashSet.member` newScope)) wheres
         in case NonEmpty.nonEmpty matched of
              Nothing -> first (from :) (go newScope froms unmatched)
              Just on -> first (InnerJoin{lateral, tableExpr, on} :) (go newScope froms unmatched)
      InnerJoin{lateral, tableExpr, on} ->
        let newScope = maybe id HashSet.insert tableExpr.alias scope
            (matched, unmatched) = List.partition (all (\varInfo -> varInfo.name `HashSet.member` newScope)) wheres
         in case NonEmpty.nonEmpty matched of
              Nothing -> first (from :) (go newScope froms unmatched)
              Just on' -> first (InnerJoin{lateral, tableExpr, on = on <> on'} :) (go newScope froms unmatched)

compileSelect :: Select -> Builder
compileSelect select =
  case promoteCrossJoins select of
    Select{expr, from, froms, wheres} ->
      "SELECT "
        <> ( case expr of
              Star -> "*"
              NotStar expr' -> compileSelectList id expr'
           )
        <> " FROM "
        <> compileFrom id from.expr
        <> ( case from.alias of
              Nothing -> mempty
              Just alias ->
                " AS " <> Builder.fromText alias
           )
        <> foldMap ((" " <>) . printFrom) froms
        <> case wheres of
          [] -> mempty
          where_ : wheres' ->
            " WHERE " <> compileExpr id where_ <> foldMap ((" AND " <>) . compileExpr id) wheres'
    Yield{fieldNames, value, wheres} ->
      "SELECT * FROM "
        <> parens
          ( "VALUES "
              <> compileTuple id value
          )
        <> " AS _"
        <> parens (commaSep (fmap Builder.fromText fieldNames))
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
        <> compileFrom id tableExpr.expr
        <> ( case tableExpr.alias of
              Nothing -> mempty
              Just alias ->
                " AS " <> Builder.fromText alias
           )
    InnerJoin{lateral, tableExpr, on} ->
      "INNER JOIN "
        <> (if lateral then "LATERAL " else "")
        <> compileFrom id tableExpr.expr
        <> ( case tableExpr.alias of
              Nothing ->
                mempty
              Just alias ->
                " AS " <> Builder.fromText alias
           )
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
    Expr.Dot _ (Expr.Name "tables") tableName ->
      Select{expr = Star, from = TableExpr{expr = Expr.Name tableName, alias = Nothing, freeVars = [tableName]}, froms = [], wheres = []}
    -- r.x : Relation b
    Expr.Dot{} ->
      error "TODO: compileRelation Dot"
    Expr.Yield value ->
      Yield
        { fieldNames =
            case Type.matchRecord $ Expr.typeOf ((.type_) . varInfo) value of
              Just fields | not (null fields) -> fmap fst fields
              _ -> ["it"]
        , value = fmap varInfo value
        , wheres = []
        }
    Expr.For name type_ relation rest ->
      -- relation : Relation a
      let relation' =
            TableExpr
              { expr = fmap varInfo relation
              , alias = Just name
              , freeVars = foldr' (HashSet.insert . (.name) . varInfo) mempty relation
              }
       in case compileRelation (unvar (\() -> VarInfo{name, type_, origin = TableVar}) varInfo) (fromScope rest) of
            Yield{value, wheres} ->
              Select{expr = NotStar value, from = relation', froms = [], wheres}
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
        { expr = undefined -- Builder.fromText name <> ".*"
        , from =
            TableExpr
              { expr = fmap varInfo relation
              , alias = Just name
              , freeVars = foldr' (HashSet.insert . (.name) . varInfo) mempty relation
              }
        , froms = []
        , wheres =
            [ fmap
                (unvar (\() -> VarInfo{name, type_, origin = TableVar}) varInfo)
                (fromScope condition)
            ]
        }
    Expr.Where condition relation ->
      -- condition : (a -> Bool)
      -- relation : Relation a
      case compileRelation varInfo relation of
        Yield{fieldNames, value, wheres} ->
          Yield{fieldNames, value, wheres = fmap varInfo condition : wheres}
        Select{expr = selectExpr, from, froms, wheres} ->
          Select{expr = selectExpr, from, froms, wheres = fmap varInfo condition : wheres}
    Expr.Lam{} ->
      error "compileRelation: impossible Lam"
    Expr.Splat{} ->
      error "compileRelation: impossible Splat"
    Expr.Record{} ->
      error "compileRelation: impossible Record"
    Expr.Equals{} ->
      error "compileRelation: impossible Equals"
    Expr.Int{} ->
      error "compileRelation: impossible Int"
    Expr.Bool{} ->
      error "compileRelation: impossible Bool"
    Expr.String{} ->
      error "compileRelation: impossible String"

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
    Expr.For{} ->
      compileSelect $ compileRelation varInfo expr
    Expr.Filter{} ->
      compileSelect $ compileRelation varInfo expr
    Expr.Where{} ->
      compileSelect $ compileRelation varInfo expr
    Expr.App{} ->
      compileExpr varInfo expr
    Expr.Splat{} ->
      "VALUES " <> compileTuple varInfo expr
    Expr.Record{} ->
      "VALUES " <> compileTuple varInfo expr
    Expr.Dot (Type.App (Type.Name "Relation") _) (Expr.Name "tables") _ ->
      compileSelect $ compileRelation varInfo expr
    Expr.Dot{} ->
      "VALUES " <> parens (compileExpr varInfo expr)
    Expr.Equals{} ->
      "VALUES " <> parens (compileExpr varInfo expr)
    Expr.Int{} ->
      "VALUES " <> parens (compileExpr varInfo expr)
    Expr.Bool{} ->
      "VALUES " <> parens (compileExpr varInfo expr)
    Expr.String{} ->
      "VALUES " <> parens (compileExpr varInfo expr)

compileFrom :: (a -> VarInfo) -> Expr a -> Builder
compileFrom varInfo expr =
  case expr of
    Expr.Dot (Type.App (Type.Name "Relation") _) (Expr.Name "tables") tableName ->
      Builder.fromText tableName
    _ ->
      queryParens (compileQuery varInfo) expr

compileSelectList :: (a -> VarInfo) -> Expr a -> Builder
compileSelectList varInfo expr =
  case expr of
    Expr.Name name ->
      Builder.fromText name
    Expr.Var var
      | let info = varInfo var ->
          case info.type_ of
            Type.App (Type.Name "Record") _ ->
              Builder.fromText info.name <> ".*"
            Type.Unknown{} ->
              error "variable has unknown type"
            _ ->
              Builder.fromText info.name
    Expr.Lam{} ->
      error "TODO: compileSelectList Lam"
    Expr.Yield{} ->
      error "TODO: compileSelectList Yield"
    Expr.For{} ->
      error "TODO: compileSelectList For"
    Expr.Filter{} ->
      error "TODO: compileSelectList Filter"
    Expr.Where{} ->
      error "TODO: compileSelectList Where"
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
    Expr.Bool{} ->
      parens (compileExpr varInfo expr) <> " AS it"
    Expr.String{} ->
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
    Expr.For{} ->
      error "TODO: compileExpr For"
    Expr.Filter{} ->
      error "TODO: compileExpr Filter"
    Expr.Where{} ->
      error "TODO: compileExpr Where"
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
    Expr.Bool b ->
      if b then "TRUE" else "FALSE"
    Expr.String s ->
      "'" <> foldMap (\c -> if c == '\'' then "''" else Builder.fromText (Text.singleton c)) (Text.unpack s) <> "'"
    Expr.Equals a b ->
      compileExpr varInfo a <> " = " <> compileExpr varInfo b

compileTuple :: (a -> VarInfo) -> Expr a -> Builder
compileTuple varInfo expr =
  parens $
    case expr of
      Expr.Splat expr' names ->
        if null names
          then "ROW()"
          else commaSep (fmap (\name -> compileExpr varInfo expr' <> "." <> Builder.fromText name) names)
      Expr.Record fields ->
        if null fields
          then "ROW()"
          else commaSep (fmap (\(_name, value) -> compileExpr varInfo value) fields)
      _ ->
        compileExpr varInfo expr

compileCommand :: Command -> Builder
compileCommand command =
  case command of
    Eval{value} ->
      compileQuery absurd value
    Insert{table, value} ->
      "INSERT INTO "
        <> Builder.fromText table.name
        <> parens (commaSep $ fmap (Builder.fromText . fst) table.inFields)
        <> " VALUES "
        <> compileTuple absurd value