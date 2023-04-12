{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Dblang.Run.CSV (eval, Error (..), pretty) where

import Bound (fromScope)
import Bound.Scope (mapScope)
import Bound.Var (unvar)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Csv as Csv
import Data.Foldable (foldl', foldrM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (absurd)
import qualified Dblang.Definition as Definition
import Dblang.Definition.Table (Table (..))
import Dblang.Expr (Expr (..))
import qualified Dblang.Parse as Parse
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Dblang.Typecheck (checkExpr, runTypecheck)
import qualified Dblang.Typecheck as Typecheck
import Dblang.Value (Value)
import qualified Dblang.Value as Value
import qualified Dblang.Value.Pretty as Value.Pretty
import Streaming.Chars.Text (StreamText (..))
import qualified System.FilePath as FilePath
import Text.Parser.Combinators (eof)
import Text.Sage (ParseError, parse)

data Error
  = ParseError ParseError
  | TypeError Typecheck.Error
  | CsvError String
  deriving (Eq, Show)

valueEq :: Applicative m => Value -> Value -> m Value
valueEq v1 v2 =
  case (v1, v2) of
    (Value.Relation a, Value.Relation b) ->
      pure . Value.Bool $ a == b
    (Value.Record a, Value.Record b) ->
      pure . Value.Bool $ a == b
    (Value.Int a, Value.Int b) ->
      pure . Value.Bool $ a == b
    (Value.Bool a, Value.Bool b) ->
      pure . Value.Bool $ a == b
    (Value.String a, Value.String b) ->
      pure . Value.Bool $ a == b
    (Value.Unit, Value.Unit) ->
      pure $ Value.Bool True
    _ ->
      pure $ Value.Bool False

runExpr :: MonadIO m => HashMap Text Value -> (a -> Value) -> Expr a -> m Value
runExpr context varValue expr =
  case expr of
    Name name ->
      case HashMap.lookup name context of
        Just value -> pure value
        Nothing -> error $ "name " <> show name <> " not in scope"
    Var var ->
      pure $ varValue var
    Lam _ body ->
      pure $ Value.Lam 1 (mapScope (\() -> 0) varValue body)
    App _ function args -> do
      function' <- runExpr context varValue function
      case function' of
        Value.Lam argCount body -> do
          when (argCount > length args)
            . error
            $ "lam applied to too many args, expected " <> show argCount <> ", got " <> show (length args)
          args' <- traverse (runExpr context varValue) args
          runExpr context (unvar (args' Vector.!) id) (fromScope body)
        _ -> error $ "expected lam, got " <> show function'
    Yield value -> do
      value' <- runExpr context varValue value
      pure . Value.Relation $ Value.fromVector [value']
    For _ _ collection body -> do
      collection' <- runExpr context varValue collection
      case collection' of
        Value.Relation values -> do
          values' <- traverse (\value -> runExpr context (unvar (\() -> value) varValue) $ fromScope body) values
          pure . Value.Relation $
            foldl'
              ( \acc value' ->
                  case value' of
                    Value.Relation values'' -> acc <> values''
                    _ -> error $ "expected relation, got " <> show value'
              )
              mempty
              values'
        _ -> error $ "expected relation, got " <> show collection'
    Where condition rest -> do
      condition' <- runExpr context varValue condition
      case condition' of
        Value.Bool b ->
          if b
            then runExpr context varValue rest
            else pure $ Value.Relation mempty
        _ -> error $ "expected bool, got " <> show condition'
    GroupBy _ _ collection projection -> do
      collection' <- runExpr context varValue collection
      case collection' of
        Value.Relation values -> do
          projection' <- runExpr context varValue projection
          case projection' of
            Value.Lam _ projection'' -> do
              Value.Map . fmap Value.Relation
                <$> foldrM
                  ( \value rest -> do
                      key <- runExpr context (unvar ([value] Vector.!) id) $ fromScope projection''
                      pure $ HashMap.insertWith (\new old -> old <> new) key (Value.fromVector [value]) rest
                  )
                  mempty
                  values
            _ ->
              error $ "expected function, got " <> show projection'
        _ ->
          error $ "expected relation, got " <> show collection'
    Dot _ record field -> do
      record' <- runExpr context varValue record
      case record' of
        Value.Record fields ->
          case HashMap.lookup field fields of
            Just value -> pure value
            _ -> error $ "record " <> show record' <> " missing field " <> show field
        _ -> error $ "expected record, got " <> show record'
    Splat record fields -> do
      record' <- runExpr context varValue record
      case record' of
        Value.Record actualFields -> do
          fields' <- for fields $ \field ->
            case HashMap.lookup field actualFields of
              Just value -> pure (field, value)
              _ -> error $ "record " <> show record' <> " missing field " <> show field
          pure . Value.Record $ foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty fields'
        _ -> error $ "expected record, got " <> show record'
    Record fields -> do
      fields' <- (traverse . traverse) (runExpr context varValue) fields
      pure . Value.Record $ foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty fields'
    Int i ->
      pure $ Value.Int i
    Bool b ->
      pure $ Value.Bool b
    String s ->
      pure $ Value.String s
    Equals a b -> do
      a' <- runExpr context varValue a
      b' <- runExpr context varValue b
      valueEq a' b'

{- | Run a query against a set of CSV files.

A table definition is inferred for each file. The table's name is the file's basename, and
the table contains one `String` field for each column in the CSV.

Example:

```
# Inferred from `example.csv`.
#
# $ head -n 2 example.csv
# col1,col2,col3
# value1,value2,value3

table example {
  col1 : String,
  col2 : String,
  col3 : String
}
```
-}
eval ::
  MonadIO m =>
  Vector FilePath ->
  Text ->
  m (Either Error (Value, Type))
eval files input =
  runExceptT $ do
    (tablesType, tablesValue) <- do
      (definitions, context) <- fmap Vector.unzip . for files $ \file -> do
        contents <- liftIO $ ByteString.Lazy.readFile file

        table :: Vector (Vector Text) <- either (throwError . CsvError) pure $ Csv.decode Csv.NoHeader contents
        (types, body) <- case Vector.uncons table of
          Nothing ->
            error $ "table " <> show file <> " missing header"
          Just (header, body) ->
            pure (fmap (,Type.Name "String") header, body)

        let name = Text.pack $ FilePath.takeBaseName file

        pure
          ( Definition.Table
              Table
                { name
                , types
                , inFields = types
                , outFields = types
                , constraints = mempty
                }
          ,
            ( name
            , Value.Relation . Value.fromVector $
                let typesLength = length types
                 in fmap
                      ( \row ->
                          let rowLength = length row
                           in Value.Record
                                . foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty
                                $ Vector.zipWith
                                  (\(field, _ty) value -> (field, Value.String value))
                                  types
                                  (if rowLength < typesLength then row <> Vector.replicate (typesLength - rowLength) "" else row)
                      )
                      body
            )
          )

      pure
        ( Type.record $
            Vector.mapMaybe
              ( \case
                  Definition.Table Table{name, outFields} -> Just (name, Type.App (Type.Name "Relation") $ Type.record outFields)
              )
              definitions
        , Value.Record $ foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty context
        )

    syntax <-
      either (throwError . ParseError) pure $
        parse (Parse.expr Syntax.Name <* eof) (StreamText input)

    (core, ty) <-
      either (throwError . TypeError) pure . runTypecheck $ do
        ty <- Typecheck.unknown
        core <- checkExpr (HashMap.singleton "tables" tablesType) absurd syntax ty
        (,)
          <$> Typecheck.zonkExpr core
          <*> Typecheck.zonk ty

    value <- runExpr (HashMap.singleton "tables" tablesValue) absurd core
    pure (value, ty)

pretty :: MonadIO m => Either Error (Value, Type) -> m ()
pretty result =
  liftIO $
    case result of
      Left err ->
        print err
      Right (value, ty) ->
        Text.IO.putStrLn . Value.Pretty.unlines $ Value.Pretty.pretty value ty