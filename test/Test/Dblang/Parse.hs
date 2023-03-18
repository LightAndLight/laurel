module Test.Dblang.Parse (spec) where

import Bound (Var (..), toScope)
import Data.Void (Void)
import Dblang.Parse (expr)
import Dblang.Syntax (Expr (..))
import Streaming.Chars.Text (StreamText (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Sage (parse)

spec :: Spec
spec = do
  describe "Dblang.Parse" $ do
    describe "expr" $ do
      it "works" $ do
        parse (expr Name) (StreamText "for a in as for b in bs where a.id == b.id yield { a, b }")
          `shouldBe` Right
            ( ( For "a" (Name "as") . toScope $
                  For "b" (Name "bs") . toScope $
                    Where (Equals (Dot (Var $ F $ B ()) "id") (Dot (Var $ B ()) "id")) $
                      Yield (Record [("a", Var $ F $ B ()), ("b", Var $ B ())])
              ) ::
                Expr Void
            )