{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Laurel.Postgres.Run (mkRun, eval, define, assume, run, PostgresError (..)) where

import Control.Applicative (many, some, (<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl', foldlM)
import Data.Functor.Contravariant.Divisible (divided)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Hasql.Connection (Connection)
import Hasql.Decoders (Result, Row)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Decoders as Decoder
import Hasql.Encoders (noParams)
import qualified Hasql.Encoders as Encoder
import qualified Hasql.Encoders as Postgres (noParams)
import Hasql.Session (QueryError)
import qualified Hasql.Session as Postgres (run, sql, statement)
import Hasql.Statement (Statement (..))
import Hasql.Statement as Postgres (Statement (..), refineResult)
import qualified Laurel.Command as Command
import Laurel.Definition (Definition)
import qualified Laurel.Definition as Definition
import Laurel.Definition.Constraint (Constraint)
import qualified Laurel.Definition.Constraint as Constraint
import Laurel.Definition.Table (Table (..))
import qualified Laurel.Parse as Parse
import Laurel.Postgres.Compile (compileCommand, compileDefinition, compileQuery)
import Laurel.Run (Run (..), RunError)
import qualified Laurel.Run as RunError (RunError (..))
import qualified Laurel.Syntax as Syntax
import Laurel.Type (Type)
import qualified Laurel.Type as Type
import Laurel.Typecheck (checkDefinition, checkExpr, runTypecheck)
import qualified Laurel.Typecheck as Typecheck
import Laurel.Value (Value (..))
import qualified Laurel.Value as Value
import Streaming.Chars (Chars)
import Streaming.Chars.Text (StreamText (..))
import Text.Parser.Char (anyChar, char, notChar, string)
import Text.Parser.Combinators (between, eof)
import Text.Parser.Token (decimal)
import Text.Sage (Parser, parse)

columnDefaultParser :: Chars s => Parser s (Either String (Syntax.Expr Void))
columnDefaultParser =
  pure . Syntax.Int . fromIntegral @Integer @Int <$> decimal
    <|> ( \content type_ ->
            case type_ of
              "text" -> pure . Syntax.String $ Text.pack content
              _ -> Left type_
        )
      <$> between (char '\'') (char '\'') (many $ notChar '\'' <|> char '\\' *> (char '\\' <|> char '\''))
      <* string "::"
      <*> some anyChar

mkRun :: MonadIO m => Connection -> IO (Run m)
mkRun conn = do
  result <- flip Postgres.run conn $ do
    tables :: Vector Text <-
      Postgres.statement () $
        Postgres.Statement
          "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"
          Postgres.noParams
          (Decoder.rowVector . Decoder.column $ Decoder.nonNullable Decoder.text)
          False

    for tables $ \table -> do
      columnInfo <-
        Postgres.statement table
          . Postgres.refineResult
            ( traverse $ \(columnName, columnType, columnNullable, columnDefault) -> do
                columnType' <- case columnType of
                  "integer" -> pure $ Type.Name "Int"
                  "text" -> pure $ Type.Name "String"
                  "boolean" -> pure $ Type.Name "Bool"
                  "USER-DEFINED" ->
                    {-
                    Use pg_type, pg_enum and company: https://stackoverflow.com/a/9540711/2884502
                    -}
                    error $ "TODO: handle user-defined types for " <> Text.unpack table <> "." <> Text.unpack columnName
                  _ -> Left $ "unknown column type: " <> columnType

                columnNullable' <-
                  case columnNullable of
                    "YES" -> pure True
                    "NO" -> pure False
                    _ -> Left $ "unknown column nullability: " <> columnNullable

                pure
                  ( columnName
                  , (if columnNullable' then Type.App (Type.Name "Optional") else id) columnType'
                  , columnNullable'
                  , columnDefault
                  )
            )
          $ Postgres.Statement
            "SELECT column_name, data_type, is_nullable, column_default FROM information_schema.columns WHERE table_schema = 'public' AND table_name = $1"
            (Encoder.param $ Encoder.nonNullable Encoder.text)
            ( Decoder.rowVector $
                (,,,)
                  <$> Decoder.column (Decoder.nonNullable Decoder.text)
                  <*> Decoder.column (Decoder.nonNullable Decoder.text)
                  <*> Decoder.column (Decoder.nonNullable Decoder.text)
                  <*> Decoder.column (Decoder.nullable Decoder.text)
            )
            True

      constraintInfo <-
        Postgres.statement table $
          Postgres.Statement
            "SELECT constraint_name, constraint_type FROM information_schema.table_constraints WHERE table_schema = 'public' AND table_name = $1"
            (Encoder.param $ Encoder.nonNullable Encoder.text)
            ( Decoder.rowVector $
                (,)
                  <$> Decoder.column (Decoder.nonNullable Decoder.text)
                  <*> Decoder.column (Decoder.nonNullable Decoder.text)
            )
            True

      constraints :: Vector Constraint <- flip Vector.mapMaybeM constraintInfo $ \(constraintName, constraintType) -> do
        constraintColumns <-
          Postgres.statement (table, constraintName) $
            Postgres.Statement
              "SELECT column_name FROM information_schema.constraint_column_usage WHERE table_schema = 'public' AND table_name = $1 AND constraint_name = $2"
              ( Encoder.param (Encoder.nonNullable Encoder.text)
                  `divided` Encoder.param (Encoder.nonNullable Encoder.text)
              )
              (Decoder.rowVector . Decoder.column $ Decoder.nonNullable Decoder.text)
              True

        case constraintType of
          "PRIMARY KEY" ->
            pure . Just $ Constraint.PrimaryKey constraintColumns
          "UNIQUE" ->
            pure . Just $ Constraint.Key constraintColumns
          "CHECK" -> do
            liftIO . putStrLn $ "TODO: handle CHECK constraint " <> show constraintName
            pure Nothing
          "FOREIGN KEY" -> do
            liftIO . putStrLn $ "TODO: handle FOREIGN KEY constraint " <> show constraintName
            pure Nothing
          _ ->
            error $ "unknown constraint type: " <> show constraintType <> " in table " <> show table

      defaultConstraints :: Vector Constraint <-
        Vector.mapMaybeM
          ( \(columnName, columnType, columnNullable, columnDefault) ->
              traverse
                ( \input -> case parse (columnDefaultParser <* eof) (StreamText input) of
                    Left err ->
                      error $ "error parsing column default " <> show input <> ": " <> show err
                    Right result ->
                      case result of
                        Left err ->
                          error $ "unknown column default value: " <> show err
                        Right syntax ->
                          case runTypecheck $ checkExpr mempty absurd (if columnNullable then Syntax.Ctor "Some" [syntax] else syntax) columnType of
                            Left err ->
                              error $ show err
                            Right core ->
                              pure $ Constraint.Default columnName core
                )
                columnDefault
          )
          columnInfo

      pure $
        Definition.Table
          Table
            { name = table
            , types =
                ( \(columnName, columnType, _columnNullable, _columnDefault) ->
                    ( columnName
                    , columnType
                    )
                )
                  <$> columnInfo
            , inFields =
                ( \(columnName, columnType, _columnNullable, columnDefault) ->
                    ( columnName
                    , ( if Maybe.isJust columnDefault
                          then Type.App (Type.Name "Optional")
                          else id
                      )
                        columnType
                    )
                )
                  <$> columnInfo
            , outFields =
                ( \(columnName, columnType, _columnNullable, _columnDefault) ->
                    ( columnName
                    , columnType
                    )
                )
                  <$> columnInfo
            , constraints =
                constraints
                  <> defaultConstraints
            }

  case result of
    Left err ->
      error $ show err
    Right definitions ->
      pure
        Run
          { eval = eval conn definitions
          , typeOf = typeOf definitions
          , definitions = pure definitions
          }

maybeToValue :: Maybe Value -> Value
maybeToValue = maybe (Ctor "None" []) (\arg -> Ctor "Some" [arg])

nullableColumn ::
  Either
    (Decoder.NullableOrNot Decoder.Value (Maybe Value))
    (Decoder.NullableOrNot Decoder.Value Value) ->
  Row Value
nullableColumn = either (fmap maybeToValue . Decode.column) Decode.column

resultDecoder :: Type -> Result Value
resultDecoder ty =
  case ty of
    Type.App (Type.Name "Relation") a ->
      Relation . Value.fromVector <$> Decode.rowVector (rowDecoder a)
    Type.App (Type.App (Type.Name "Map") k) v ->
      Map . foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty
        <$> Decode.rowVector
          ((,) <$> nullableColumn (valueDecoder k) <*> nullableColumn (valueDecoder v))
    _ ->
      Decode.singleRow (rowDecoder ty)

rowDecoder :: Type -> Row Value
rowDecoder ty =
  case ty of
    Type.App (Type.Name "Record") rows
      | (fields, Nothing) <- Type.matchRow rows ->
          Record
            <$> foldlM
              ( \acc (fieldName, fieldType) -> do
                  value <- nullableColumn $ valueDecoder fieldType
                  pure $ HashMap.insert fieldName value acc
              )
              mempty
              fields
    _ ->
      nullableColumn $ valueDecoder ty

valueDecoder :: Type -> Either (Decode.NullableOrNot Decode.Value (Maybe Value)) (Decode.NullableOrNot Decode.Value Value)
valueDecoder ty =
  case ty of
    Type.Name "Int" ->
      Right . Decode.nonNullable $
        Int . fromIntegral <$> Decode.int8
    Type.Name "Bool" ->
      Right . Decode.nonNullable $
        Bool <$> Decode.bool
    Type.Name "String" ->
      Right . Decode.nonNullable $
        String <$> Decode.text
    Type.App (Type.Name "Record") rows
      | (fields, Nothing) <- Type.matchRow rows ->
          Right . Decode.nonNullable $
            Record
              <$> Decode.composite
                ( foldlM
                    ( \acc (fieldName, fieldType) -> do
                        value <- Decode.field . Decode.nonNullable $ innerValueDecoder fieldType
                        pure $ HashMap.insert fieldName value acc
                    )
                    mempty
                    fields
                )
    Type.App (Type.Name "List") a ->
      Right . Decode.nonNullable $
        Decode.array (Decode.element . Decode.nonNullable $ innerValueDecoder a)
    Type.App (Type.Name "Relation") a ->
      Right . Decode.nonNullable $
        Decode.array (Decode.element . Decode.nonNullable $ innerValueDecoder a)
    Type.App (Type.Name "Optional") a ->
      Left . Decode.nullable $ innerValueDecoder a
    _ ->
      error $ "valueDecoder: invalid type " <> show ty

innerValueDecoder :: Type -> Decode.Value Value
innerValueDecoder ty =
  case ty of
    Type.Name "Int" ->
      Int . fromIntegral <$> Decode.int8
    Type.Name "Bool" ->
      Bool <$> Decode.bool
    Type.Name "String" ->
      String <$> Decode.text
    Type.App (Type.Name "Record") rows
      | (fields, Nothing) <- Type.matchRow rows ->
          Record
            <$> Decode.composite
              ( foldlM
                  ( \acc (fieldName, fieldType) -> do
                      value <- Decode.field . Decode.nonNullable $ innerValueDecoder fieldType
                      pure $ HashMap.insert fieldName value acc
                  )
                  mempty
                  fields
              )
    Type.App (Type.Name "List") a ->
      Decode.array (Decode.element . Decode.nonNullable $ innerValueDecoder a)
    Type.App (Type.Name "Relation") a ->
      Decode.array (Decode.element . Decode.nonNullable $ innerValueDecoder a)
    _ ->
      error $ "innerValueDecoder: invalid type " <> show ty

data PostgresError
  = QueryError QueryError
  deriving (Eq, Show)

eval ::
  MonadIO m =>
  Connection ->
  Vector Definition ->
  Text ->
  m (Either (RunError PostgresError) (Value, Type))
eval conn definitions input =
  runExceptT $ do
    let tablesType =
          Type.record $
            Vector.mapMaybe
              ( \case
                  Definition.Table Table{name, outFields} -> Just (name, Type.App (Type.Name "Relation") $ Type.record outFields)
              )
              definitions

    syntax <-
      either (throwError . RunError.ParseError) pure $
        parse (Parse.expr Syntax.Name) (StreamText input)

    (core, ty) <-
      either (throwError . RunError.TypeError) pure . runTypecheck $ do
        ty <- Typecheck.unknown
        core <- checkExpr (HashMap.singleton "tables" tablesType) absurd syntax ty
        (,)
          <$> Typecheck.zonkExpr core
          <*> Typecheck.zonk ty

    let query = compileQuery absurd core
    liftIO . putStrLn $ "query: " <> show query

    queryResult <-
      liftIO $
        Postgres.run
          ( Postgres.statement
              ()
              ( Statement
                  (Text.Encoding.encodeUtf8 $ Text.Lazy.toStrict $ Builder.toLazyText query)
                  noParams
                  (resultDecoder ty)
                  False
              )
          )
          conn

    value <- either (throwError . RunError.OtherError . QueryError) pure queryResult
    pure (value, ty)

define :: MonadIO m => Connection -> Text -> m (Either (RunError PostgresError) Definition)
define conn input =
  runExceptT $ do
    syntax <-
      either (throwError . RunError.ParseError) pure $
        parse Parse.definition (StreamText input)

    core <-
      either (throwError . RunError.TypeError) pure . runTypecheck $
        checkDefinition syntax

    let query = compileDefinition core
    liftIO . putStrLn $ "query: " <> show query

    queryResult <-
      liftIO $
        Postgres.run
          ( Postgres.sql
              . Text.Encoding.encodeUtf8
              . Text.Lazy.toStrict
              $ Builder.toLazyText query
          )
          conn

    either (throwError . RunError.OtherError . QueryError) pure queryResult

    pure core

assume :: MonadIO m => Text -> m (Either (RunError PostgresError) Definition)
assume input =
  runExceptT $ do
    syntax <-
      either (throwError . RunError.ParseError) pure $
        parse Parse.definition (StreamText input)

    core <-
      either (throwError . RunError.TypeError) pure . runTypecheck $
        checkDefinition syntax

    let query = compileDefinition core
    liftIO . putStrLn $ "query: " <> show query

    pure core

run ::
  MonadIO m =>
  Connection ->
  Vector Definition ->
  Text ->
  m (Either (RunError PostgresError) Value)
run conn definitions input =
  runExceptT $ do
    syntax <-
      either (throwError . RunError.ParseError) pure $
        parse Parse.command (StreamText input)

    command <-
      either (throwError . RunError.TypeError) pure . runTypecheck $
        Typecheck.checkCommand definitions syntax

    let query = compileCommand command
    liftIO . putStrLn $ "query: " <> show query

    queryResult <-
      liftIO $
        Postgres.run
          ( Postgres.statement
              ()
              ( Statement
                  (Text.Encoding.encodeUtf8 $ Text.Lazy.toStrict $ Builder.toLazyText query)
                  noParams
                  (maybe (Value.Unit <$ Decode.noResult) resultDecoder $ Command.result command)
                  False
              )
          )
          conn

    either (throwError . RunError.OtherError . QueryError) pure queryResult

typeOf ::
  MonadIO m =>
  Vector Definition ->
  Text ->
  m (Either (RunError PostgresError) Type)
typeOf definitions input =
  runExceptT $ do
    let tablesType =
          Type.record $
            Vector.mapMaybe
              ( \case
                  Definition.Table Table{name, outFields} -> Just (name, Type.App (Type.Name "Relation") $ Type.record outFields)
              )
              definitions

    syntax <-
      either (throwError . RunError.ParseError) pure $
        parse (Parse.expr Syntax.Name) (StreamText input)

    either (throwError . RunError.TypeError) pure . runTypecheck $ do
      ty <- Typecheck.unknown
      _core <- checkExpr (HashMap.singleton "tables" tablesType) absurd syntax ty
      Typecheck.zonk ty