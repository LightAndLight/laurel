module Test.Dblang.Compile.Postgres (spec) where

import Bound (Var (..), toScope)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Void (Void, absurd)
import Dblang.Compile.Postgres (compileQuery)
import Dblang.Expr (Expr (..))
import qualified Dblang.Type as Type
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Dblang.Compile.Postgres" $ do
    describe "compileQuery" $ do
      let
        testCases :: [(String, Expr Void, Text)]
        testCases =
          [
            ( "from people (\\(person : Person) -> yield (person.name : String))"
            , From (Name "people") "person" (Type.Name "Person") . toScope $
                Yield (Dot (Type.Name "String") (Var (B ())) "name")
            , "SELECT person.name AS it FROM people AS person"
            )
          ,
            ( "from people (\\(person1 : Person) -> from people (\\(person2 : Person) -> yield { name1 = person1.name : String, name2 = person2.name : String }))"
            , From (Name "people") "person1" (Type.Name "Person") . toScope $
                From (Name "people") "person2" (Type.Name "Person") . toScope $
                  Yield (Record [("name1", Dot (Type.Name "String") (Var (F $ B ())) "name"), ("name2", Dot (Type.Name "String") (Var $ B ()) "name")])
            , "SELECT person1.name AS name1, person2.name AS name2 FROM people AS person1 CROSS JOIN people AS person2"
            )
          , let tRecordZInt = Type.record [("z", Type.Name "Int")]
             in ( "from people (\\(x : Person) -> from (test x : Relation { z : Int }) (\\(y : { z : Int }) -> yield y))"
                , From (Name "people") "x" (Type.Name "Person") . toScope $
                    From (App (Type.App (Type.Name "Relation") tRecordZInt) (Name "test") [Var $ B ()]) "y" (Type.record [("z", Type.Name "Int")]) . toScope $
                      Yield (Var $ B ())
                , "SELECT y.* FROM people AS x CROSS JOIN LATERAL test(x) AS y"
                )
          , let tRecordZInt = Type.record [("z", Type.Name "Int")]
             in ( "from people (\\(x : Person) -> from (test x : Relation { z : Int }) (\\(y : { z : Int }) -> yield (y.z : Int)))"
                , From (Name "people") "x" (Type.Name "Person") . toScope $
                    From (App (Type.App (Type.Name "Relation") tRecordZInt) (Name "test") [Var $ B ()]) "y" tRecordZInt . toScope $
                      Yield (Dot (Type.Name "Int") (Var $ B ()) "z")
                , "SELECT y.z AS it FROM people AS x CROSS JOIN LATERAL test(x) AS y"
                )
          ]
      for_ testCases $ \(label, input, expected) ->
        it ("compiles \"" <> label <> "\"") $ do
          Text.Lazy.toStrict (Builder.toLazyText $ compileQuery absurd input) `shouldBe` expected