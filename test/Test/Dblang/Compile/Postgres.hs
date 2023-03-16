module Test.Dblang.Compile.Postgres (spec) where

import Bound (Var (..), toScope)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Void (Void)
import Dblang.Compile.Postgres (compileQuery)
import Dblang.Expr (Expr (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Dblang.Compile.Postgres" $ do
    describe "compileQuery" $ do
      let
        testCases :: [(String, Expr Void, Text)]
        testCases =
          [
            ( "from people (\\person -> yield person.name)"
            , From (Name "people") (Lam "person" (toScope (Yield $ Dot (Var (B ())) "name")))
            , "SELECT person.name AS it FROM people AS person"
            )
          ,
            ( "from people (\\person1 -> from people (\\person2 -> yield { name1 = person1.name, name2 = person2.name }))"
            , From (Name "people") (Lam "person1" (toScope (From (Name "people") (Lam "person2" (toScope (Yield (Record [("name1", Dot (Var (F $ B ())) "name"), ("name2", Dot (Var $ B ()) "name")])))))))
            , "SELECT person1.name AS name1, person2.name AS name2 FROM people AS person1 CROSS JOIN people AS person2"
            )
          ]
      for_ testCases $ \(label, input, expected) ->
        it ("compiles \"" <> label <> "\"") $ do
          Text.Lazy.toStrict (Builder.toLazyText $ compileQuery input) `shouldBe` expected