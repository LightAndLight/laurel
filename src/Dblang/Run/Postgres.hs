module Dblang.Run.Postgres (runRelation, Error (..)) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldrM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import Data.Void (absurd)
import Dblang.Compile.Postgres (compileQuery)
import qualified Dblang.Parse as Parse
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Dblang.Typecheck (checkExpr, runTypecheck)
import qualified Dblang.Typecheck as Typecheck
import Hasql.Connection (Connection)
import Hasql.Decoders (Result, Row)
import qualified Hasql.Decoders as Decode
import Hasql.Encoders (noParams)
import Hasql.Session (QueryError)
import qualified Hasql.Session as Postgres
import Hasql.Statement (Statement (..))
import Streaming.Chars.Text (StreamText (..))
import Text.Sage (ParseError, parse)

newtype Multiset a = Multiset (Vector a)
  deriving (Eq, Show)

data Value
  = Relation (Multiset Value)
  | Record (HashMap Text Value)
  | Int Int
  | Bool Bool
  deriving (Eq, Show)

resultDecoder :: Type -> Result Value
resultDecoder ty =
  case ty of
    Type.App (Type.Name "Relation") a ->
      Relation . Multiset <$> Decode.rowVector (rowDecoder a)
    _ ->
      error $ "resultDecoder: invalid type " <> show ty

rowDecoder :: Type -> Row Value
rowDecoder ty =
  case ty of
    Type.App (Type.Name "Record") rows
      | (fields, Nothing) <- Type.matchRow rows ->
          Record
            <$> foldrM
              ( \(fieldName, fieldType) rest -> do
                  value <- valueDecoder fieldType
                  pure $ HashMap.insert fieldName value rest
              )
              mempty
              fields
    Type.Name "Int" ->
      Int . fromIntegral <$> Decode.column (Decode.nonNullable Decode.int8)
    Type.Name "Bool" ->
      Bool <$> Decode.column (Decode.nonNullable Decode.bool)
    _ ->
      error $ "rowDecoder: invalid type " <> show ty

valueDecoder :: Type -> Row Value
valueDecoder ty =
  case ty of
    Type.Name "Int" ->
      Int . fromIntegral <$> Decode.column (Decode.nonNullable Decode.int8)
    Type.Name "Bool" ->
      Bool <$> Decode.column (Decode.nonNullable Decode.bool)
    _ ->
      error $ "valueDecoder: invalid type " <> show ty

data Error
  = ParseError ParseError
  | TypeError Typecheck.Error
  | QueryError QueryError
  deriving (Eq, Show)

runRelation :: MonadIO m => Connection -> HashMap Text Type -> Text -> m (Either Error Value)
runRelation conn nameTypes input =
  runExceptT $ do
    syntax <-
      either (throwError . ParseError) pure $
        parse (Parse.expr Syntax.Name) (StreamText input)

    (core, ty) <-
      either (throwError . TypeError) pure . runTypecheck $ do
        ty <- Type.App (Type.Name "Relation") <$> Typecheck.unknown
        core <- checkExpr nameTypes absurd syntax ty
        ty' <- Typecheck.zonk ty
        pure (core, ty')

    let query = compileQuery absurd core
    liftIO . putStrLn $ "info: " <> show query

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

    either (throwError . QueryError) pure queryResult