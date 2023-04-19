{-# LANGUAGE OverloadedRecordDot #-}

module Test.Laurel.Typecheck (spec) where

import Bound (Var (..), toScope)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void, absurd)
import Laurel.Command (Command (..))
import Laurel.Definition (Definition)
import qualified Laurel.Definition as Definition
import Laurel.Definition.Constraint (Constraint (..))
import Laurel.Definition.Table (Table (..))
import Laurel.Expr (Expr (..))
import qualified Laurel.Parse as Parse
import qualified Laurel.Syntax as Syntax
import Laurel.Type (Type)
import qualified Laurel.Type as Type
import Laurel.Typecheck (Error (..), checkCommand, checkDefinition, checkExpr, runTypecheck)
import qualified Laurel.Typecheck as Typecheck
import Streaming.Chars.Text (StreamText (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
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
  describe "Laurel.Typecheck" $ do
    describe "checkExpr" $ do
      let
        testCases :: [(Text, Type, Either Typecheck.Error (Expr Void))]
        testCases =
          [
            ( "{ a = 1, b = true }"
            , Type.record [("a", Type.Name "Int"), ("b", Type.Name "Bool")]
            , Right $ Record [("a", Int 1), ("b", Bool True)]
            )
          ,
            ( "{ a = 1, b = true }"
            , Type.record [("b", Type.Name "Bool"), ("a", Type.Name "Int")]
            , Right $ Record [("a", Int 1), ("b", Bool True)]
            )
          ,
            ( "{ a = 1, b = true }"
            , Type.record [("b", Type.Name "Int"), ("a", Type.Name "Int")]
            , Left Typecheck.TypeMismatch{expected = Type.Name "Int", actual = Type.Name "Bool"}
            )
          ,
            ( "{ a = 1, b = true }"
            , Type.record [("a", Type.Name "Int")]
            , Left Typecheck.TypeMismatch{expected = Type.RNil, actual = Type.RCons "b" (Type.Unknown 4) (Type.Unknown 5)}
            )
          ]
      for_ testCases $ \(input, inputTy, output) -> it
        ( "checking "
            <> show input
            <> " has type "
            <> show inputTy
            <> " "
            <> either (const "fails") (const "succeeds") output
        )
        $ do
          case Text.Sage.parse (Parse.expr Syntax.Name) (StreamText input) of
            Left err ->
              expectationFailure $ show err
            Right expr ->
              runTypecheck (checkExpr mempty absurd expr inputTy) `shouldBe` output

    describe "checkDefinition" $ do
      let
        testCases :: [(Text, Either Typecheck.Error Definition)]
        testCases =
          [
            ( "table people { type Id = Int, id : Id, name : String, age : Int }"
            , Right $
                Definition.Table
                  Table
                    { name = "people"
                    , types = [("Id", Type.Name "Int")]
                    , inFields =
                        [ ("id", Type.Name "Id")
                        , ("name", Type.Name "String")
                        , ("age", Type.Name "Int")
                        ]
                    , outFields =
                        [ ("id", Type.Name "Id")
                        , ("name", Type.Name "String")
                        , ("age", Type.Name "Int")
                        ]
                    , constraints = []
                    }
            )
          ,
            ( "table people { type Id = Int, id : Id [Default(0)], name : String, age : Int }"
            , Right $
                Definition.Table
                  Table
                    { name = "people"
                    , types = [("Id", Type.Name "Int")]
                    , inFields =
                        [ ("id", Type.App (Type.Name "Optional") (Type.Name "Id"))
                        , ("name", Type.Name "String")
                        , ("age", Type.Name "Int")
                        ]
                    , outFields =
                        [ ("id", Type.Name "Id")
                        , ("name", Type.Name "String")
                        , ("age", Type.Name "Int")
                        ]
                    , constraints = [Default "id" $ Int 0]
                    }
            )
          ,
            ( "table people { type Id = Int, id : Id [Default(true)], name : String, age : Int }"
            , Left TypeMismatch{expected = Type.Name "Int", actual = Type.Name "Bool"}
            )
          ]
      for_ testCases $ \(input, output) -> it
        ( "checking definition "
            <> show input
            <> " "
            <> either (const "fails") (const "succeeds") output
        )
        $ do
          case Text.Sage.parse Parse.definition (StreamText input) of
            Left err ->
              expectationFailure $ show err
            Right definition ->
              runTypecheck (checkDefinition definition) `shouldBe` output

    describe "checkCommand" $ do
      let peopleString = "table people { type Id = Int, id : Int [PrimaryKey], name : String, age : Int }"
      peopleDefinition <- parseAndCheckDefinition peopleString
      peopleTable <-
        case peopleDefinition of
          Definition.Table table -> pure table

      describe ("given " <> show peopleString) $ do
        let
          definitions :: Vector Definition
          definitions = [peopleDefinition]

          testCases :: [(Text, Either Typecheck.Error Command)]
          testCases =
            [
              ( ":insert people { id = 1, name = \"Harry Dresden\", age = 36 }"
              , Right
                  Insert
                    { table = peopleTable
                    , value = Record [("id", Int 1), ("name", String "Harry Dresden"), ("age", Int 36)]
                    }
              )
            ,
              ( ":insert people { id = 1, name = \"Harry Dresden\", age = true }"
              , Left TypeMismatch{expected = Type.Name "Int", actual = Type.Name "Bool"}
              )
            ,
              ( ":eval for person in tables.people where person.id == 1 yield person"
              , Right
                  Eval
                    { value =
                        For
                          "person"
                          (Type.record peopleTable.outFields)
                          ( Dot
                              (Type.App (Type.Name "Relation") (Type.record peopleTable.outFields))
                              (Name "tables")
                              "people"
                          )
                          (toScope $ Where (Equals (Dot (Type.Name "Int") (Var $ B ()) "id") (Int 1)) $ Yield $ Var $ B ())
                    , type_ = Type.App (Type.Name "Relation") (Type.record peopleTable.outFields)
                    }
              )
            ]

        for_ testCases $ \(input, output) -> it
          ( "checking command "
              <> show input
              <> " "
              <> either (const "fails") (const "succeeds") output
          )
          $ do
            case Text.Sage.parse Parse.command (StreamText input) of
              Left err ->
                expectationFailure $ show err
              Right command ->
                runTypecheck (checkCommand definitions command) `shouldBe` output