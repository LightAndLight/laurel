{-# LANGUAGE LambdaCase #-}

module Dblang.Postgres.Run (mkRun, eval, define, assume, run, PostgresError (..)) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl', foldlM)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (absurd)
import qualified Dblang.Command as Command
import Dblang.Definition (Definition)
import qualified Dblang.Definition as Definition
import Dblang.Definition.Table (Table (..))
import qualified Dblang.Parse as Parse
import Dblang.Postgres.Compile (compileCommand, compileDefinition, compileQuery)
import Dblang.Run (Run (..), RunError)
import qualified Dblang.Run as RunError (RunError (..))
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Dblang.Typecheck (checkDefinition, checkExpr, runTypecheck)
import qualified Dblang.Typecheck as Typecheck
import Dblang.Value (Value (..))
import qualified Dblang.Value as Value
import Hasql.Connection (Connection)
import Hasql.Decoders (Result, Row)
import qualified Hasql.Decoders as Decode
import Hasql.Encoders (noParams)
import Hasql.Session (QueryError)
import qualified Hasql.Session as Postgres
import Hasql.Statement (Statement (..))
import Streaming.Chars.Text (StreamText (..))
import Text.Sage (parse)

mkRun :: MonadIO m => Connection -> Vector Definition -> Run m
mkRun conn definitions = Run{eval = eval conn definitions}

resultDecoder :: Type -> Result Value
resultDecoder ty =
  case ty of
    Type.App (Type.Name "Relation") a ->
      Relation . Value.fromVector <$> Decode.rowVector (rowDecoder a)
    Type.App (Type.App (Type.Name "Map") k) v ->
      Map . foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty
        <$> Decode.rowVector
          ((,) <$> Decode.column (valueDecoder k) <*> Decode.column (valueDecoder v))
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
                  value <- Decode.column $ valueDecoder fieldType
                  pure $ HashMap.insert fieldName value acc
              )
              mempty
              fields
    _ ->
      Decode.column $ valueDecoder ty

valueDecoder :: Type -> Decode.NullableOrNot Decode.Value Value
valueDecoder ty =
  Decode.nonNullable $
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
                        value <- Decode.field $ valueDecoder fieldType
                        pure $ HashMap.insert fieldName value acc
                    )
                    mempty
                    fields
                )
      Type.App (Type.Name "Relation") a ->
        Decode.array (Decode.element (valueDecoder a))
      _ ->
        error $ "valueDecoder: invalid type " <> show ty

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