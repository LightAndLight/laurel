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