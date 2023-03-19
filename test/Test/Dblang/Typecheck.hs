module Test.Dblang.Typecheck (spec) where

import Data.Foldable (for_)
import Data.Text (Text)
import Data.Void (Void, absurd)
import Dblang.Definition (Constraint (..), Definition (..))
import Dblang.Expr (Expr (..))
import qualified Dblang.Parse as Parse
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Dblang.Typecheck (Error (..), checkDefinition, checkExpr, runTypecheck)
import qualified Dblang.Typecheck as Typecheck
import Streaming.Chars.Text (StreamText (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Sage (parse)

spec :: Spec
spec = do
  describe "Dblang.Typecheck" $ do
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