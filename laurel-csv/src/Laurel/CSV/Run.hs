{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Laurel.CSV.Run (mkRun, eval, CsvError (..), pretty) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Csv as Csv
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Laurel.Definition (Definition)
import qualified Laurel.Definition as Definition
import Laurel.Definition.Table (Table (..))
import qualified Laurel.Eval
import Laurel.Run (Run (..), RunError)
import qualified Laurel.Run as RunError (RunError (..))
import qualified Laurel.Syntax as Syntax
import Laurel.Type (Type)
import qualified Laurel.Type as Type
import Laurel.Typecheck (checkExpr, runTypecheck)
import qualified Laurel.Typecheck as Typecheck
import Laurel.Value (Value)
import qualified Laurel.Value as Value
import qualified Laurel.Value.Pretty as Value.Pretty
import qualified Pretty
import qualified System.FilePath as FilePath

data CsvError
  = CsvError String
  deriving (Eq, Show)

mkRun :: MonadIO m => Vector FilePath -> IO (Either (RunError CsvError) (Run m))
mkRun files =
  runExceptT $ do
    (definitions, context) <- fmap Vector.unzip . for files $ \file -> do
      contents <- liftIO $ ByteString.Lazy.readFile file

      table :: Vector (Vector Text) <- either (throwError . RunError.OtherError . CsvError) pure $ Csv.decode Csv.NoHeader contents
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
      Run
        { eval = eval definitions context
        , typeOf = typeOf definitions
        , definitions = pure definitions
        }

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
  Vector Definition ->
  Vector (Text, Value) ->
  Syntax.Expr Void ->
  m (Either (RunError CsvError) (Value, Type))
eval definitions context syntax =
  runExceptT $ do
    let tablesType =
          Type.record $
            Vector.mapMaybe
              ( \case
                  Definition.Table Table{name, outFields} -> Just (name, Type.App (Type.Name "Relation") $ Type.record outFields)
              )
              definitions

    let tablesValue =
          Value.Record $ foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty context

    (core, ty) <-
      either (throwError . RunError.TypeError) pure . runTypecheck $ do
        ty <- Typecheck.unknown
        core <- checkExpr (HashMap.singleton "tables" tablesType) absurd syntax ty
        (,)
          <$> Typecheck.zonkExpr core
          <*> Typecheck.zonk ty

    value <- Laurel.Eval.eval (HashMap.singleton "tables" tablesValue) absurd core
    pure (value, ty)

pretty :: MonadIO m => Either (RunError CsvError) (Value, Type) -> m ()
pretty result =
  liftIO $
    case result of
      Left err ->
        print err
      Right (value, ty) ->
        Text.IO.putStrLn . Pretty.unlines $ Value.Pretty.pretty value ty

typeOf ::
  MonadIO m =>
  Vector Definition ->
  Syntax.Expr Void ->
  m (Either (RunError CsvError) Type)
typeOf definitions syntax =
  runExceptT $ do
    let tablesType =
          Type.record $
            Vector.mapMaybe
              ( \case
                  Definition.Table Table{name, outFields} -> Just (name, Type.App (Type.Name "Relation") $ Type.record outFields)
              )
              definitions

    either (throwError . RunError.TypeError) pure . runTypecheck $ do
      ty <- Typecheck.unknown
      _core <- checkExpr (HashMap.singleton "tables" tablesType) absurd syntax ty
      Typecheck.zonk ty