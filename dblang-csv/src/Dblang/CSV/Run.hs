{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Dblang.CSV.Run (eval, Error (..), pretty) where

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
import Data.Void (absurd)
import qualified Dblang.Definition as Definition
import Dblang.Definition.Table (Table (..))
import qualified Dblang.Eval
import qualified Dblang.Parse as Parse
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Dblang.Typecheck (checkExpr, runTypecheck)
import qualified Dblang.Typecheck as Typecheck
import Dblang.Value (Value)
import qualified Dblang.Value as Value
import qualified Dblang.Value.Pretty as Value.Pretty
import qualified Pretty
import Streaming.Chars.Text (StreamText (..))
import qualified System.FilePath as FilePath
import Text.Parser.Combinators (eof)
import Text.Sage (ParseError, parse)

data Error
  = ParseError ParseError
  | TypeError Typecheck.Error
  | CsvError String
  deriving (Eq, Show)

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

    value <- Dblang.Eval.eval (HashMap.singleton "tables" tablesValue) absurd core
    pure (value, ty)

pretty :: MonadIO m => Either Error (Value, Type) -> m ()
pretty result =
  liftIO $
    case result of
      Left err ->
        print err
      Right (value, ty) ->
        Text.IO.putStrLn . Pretty.unlines $ Value.Pretty.pretty value ty