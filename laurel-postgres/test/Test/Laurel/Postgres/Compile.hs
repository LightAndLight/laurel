module Test.Laurel.Postgres.Compile (spec) where

import Bound (Var (..), toScope)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import Data.Void (Void, absurd)
import Laurel.Definition (Definition)
import Laurel.Expr (Expr (..))
import qualified Laurel.Parse as Parse
import Laurel.Postgres.Compile (compileCommand, compileDefinition, compileQuery)
import qualified Laurel.Type as Type
import qualified Laurel.Typecheck as Typecheck
import Streaming.Chars.Text (StreamText (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Sage (parse)

parseAndCheckDefinition :: Monad m => Text -> m Definition
parseAndCheckDefinition input = do
  syntax <- case parse Parse.definition (StreamText input) of
    Left err ->
      error $ show err
    Right syntax ->
      pure syntax

  case Typecheck.runTypecheck (Typecheck.checkDefinition syntax) of
    Left err ->
      error $ show err
    Right definition ->
      pure definition

spec :: Spec
spec = do
  describe "Laurel.Compile.Postgres" $ do
    describe "compileQuery" $ do
      let
        testCases :: [(String, Expr Void, Text)]
        testCases =
          [
            ( "from people (\\(person : Person) -> yield (person.name : String))"
            , For "person" (Type.Name "Person") (Name "people") . toScope $
                Yield (Dot (Type.Name "String") (Var (B ())) "name")
            , "SELECT person.name AS it FROM people AS person"
            )
          ,
            ( "from people (\\(person1 : Person) -> from people (\\(person2 : Person) -> yield { name1 = person1.name : String, name2 = person2.name : String }))"
            , For "person1" (Type.Name "Person") (Name "people") . toScope $
                For "person2" (Type.Name "Person") (Name "people") . toScope $
                  Yield (Record [("name1", Dot (Type.Name "String") (Var (F $ B ())) "name"), ("name2", Dot (Type.Name "String") (Var $ B ()) "name")])
            , "SELECT person1.name AS name1, person2.name AS name2 FROM people AS person1 CROSS JOIN people AS person2"
            )
          , let tRecordZInt = Type.record [("z", Type.Name "Int")]
             in ( "from people (\\(x : Person) -> from (test x : Relation { z : Int }) (\\(y : { z : Int }) -> yield y))"
                , For "x" (Type.Name "Person") (Name "people") . toScope $
                    For "y" (Type.record [("z", Type.Name "Int")]) (App (Type.App (Type.Name "Relation") tRecordZInt) (Name "test") [Var $ B ()]) . toScope $
                      Yield (Var $ B ())
                , "SELECT y.* FROM people AS x CROSS JOIN LATERAL test(x) AS y"
                )
          , let tRecordZInt = Type.record [("z", Type.Name "Int")]
             in ( "from people (\\(x : Person) -> from (test x : Relation { z : Int }) (\\(y : { z : Int }) -> yield (y.z : Int)))"
                , For "x" (Type.Name "Person") (Name "people") . toScope $
                    For "y" tRecordZInt (App (Type.App (Type.Name "Relation") tRecordZInt) (Name "test") [Var $ B ()]) . toScope $
                      Yield (Dot (Type.Name "Int") (Var $ B ()) "z")
                , "SELECT y.z AS it FROM people AS x CROSS JOIN LATERAL test(x) AS y"
                )
          ,
            ( "from people (\\(person : Person) -> when (person.age == 25) (yield person.name))"
            , For
                "person"
                (Type.Name "Person")
                (Name "people")
                . toScope
                $ Where (Equals (Dot (Type.Name "Int") (Var $ B ()) "age") (Int 25)) (Yield (Dot (Type.Name "String") (Var (B ())) "name"))
            , "SELECT person.name AS it FROM people AS person WHERE person.age = 25"
            )
          ,
            ( "from people (\\(person1 : Person) -> from people (\\(person2 : Person) -> when (person1.id == person2.id) (yield person1)))"
            , For
                "person1"
                (Type.Name "Person")
                (Name "people")
                . toScope
                $ For
                  "person2"
                  (Type.Name "Person")
                  (Name "people")
                  . toScope
                $ Where (Equals (Dot (Type.Name "Int") (Var $ F $ B ()) "id") (Dot (Type.Name "Int") (Var $ B ()) "id")) (Yield $ Var $ F $ B ())
            , "SELECT person1 FROM people AS person1 INNER JOIN people AS person2 ON person1.id = person2.id"
            )
          ]
      for_ testCases $ \(label, input, expected) ->
        it ("compiles \"" <> label <> "\"") $ do
          Text.Lazy.toStrict (Builder.toLazyText $ compileQuery absurd input) `shouldBe` expected

    describe "compileDefinition" $ do
      let
        testCases :: [(Text, Text)]
        testCases =
          [
            ( "table people { type Id = Int, id : Id, name : String, age : Int }"
            , Text.intercalate
                "\n"
                [ "CREATE TABLE people ("
                , "id INT NOT NULL,"
                , "name TEXT NOT NULL,"
                , "age INT NOT NULL"
                , ")"
                ]
            )
          ,
            ( "table people { type Id = Int, id : Id, name : String, age : Int [Default(0)] }"
            , Text.intercalate
                "\n"
                [ "CREATE TABLE people ("
                , "id INT NOT NULL,"
                , "name TEXT NOT NULL,"
                , "age INT NOT NULL DEFAULT 0"
                , ")"
                ]
            )
          ,
            ( "table people { type Id = Int, id : Id [Key], name : String, age : Int [Default(0)] }"
            , Text.intercalate
                "\n"
                [ "CREATE TABLE people ("
                , "id INT NOT NULL,"
                , "name TEXT NOT NULL,"
                , "age INT NOT NULL DEFAULT 0,"
                , "UNIQUE (id)"
                , ")"
                ]
            )
          ,
            ( "table people { type Id = Int, id : Id [Key], name : String, age : Int [Default(0)], Key(name, age) }"
            , Text.intercalate
                "\n"
                [ "CREATE TABLE people ("
                , "id INT NOT NULL,"
                , "name TEXT NOT NULL,"
                , "age INT NOT NULL DEFAULT 0,"
                , "UNIQUE (id),"
                , "UNIQUE (name, age)"
                , ")"
                ]
            )
          ,
            ( "table people { type Id = Int, id : Id [PrimaryKey], name : String, age : Int [Default(0)], Key(name, age) }"
            , Text.intercalate
                "\n"
                [ "CREATE TABLE people ("
                , "id INT NOT NULL,"
                , "name TEXT NOT NULL,"
                , "age INT NOT NULL DEFAULT 0,"
                , "PRIMARY KEY (id),"
                , "UNIQUE (name, age)"
                , ")"
                ]
            )
          ]
      for_ testCases $ \(input, expected) ->
        it ("compiles " <> show input) $ do
          definition <- parseAndCheckDefinition input
          Text.Lazy.toStrict (Builder.toLazyText $ compileDefinition definition) `shouldBe` expected

    describe "compileCommand" $ do
      let peopleString = "table people { type Id = Int, id : Int [PrimaryKey], name : String, age : Int }"
      people <- parseAndCheckDefinition peopleString

      describe ("given " <> show peopleString) $ do
        let
          definitions :: Vector Definition
          definitions = [people]

          testCases :: [(Text, Text)]
          testCases =
            [
              ( ":insert people { id = 1, name = \"Harry Dresden\", age = 36 }"
              , "INSERT INTO people(id, name, age) VALUES (1, 'Harry Dresden', 36)"
              )
            ]
        for_ testCases $ \(input, expected) ->
          it ("compiles " <> show input) $ do
            syntax <- case parse Parse.command (StreamText input) of
              Left err ->
                error $ show err
              Right syntax ->
                pure syntax

            command <-
              case Typecheck.runTypecheck (Typecheck.checkCommand definitions syntax) of
                Left err ->
                  error $ show err
                Right command ->
                  pure command

            Text.Lazy.toStrict (Builder.toLazyText $ compileCommand command) `shouldBe` expected