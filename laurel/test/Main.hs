module Main (main) where

import qualified Test.Laurel.Parse
import qualified Test.Laurel.Typecheck
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Test.Laurel.Parse.spec
    Test.Laurel.Typecheck.spec
