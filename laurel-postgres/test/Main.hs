module Main (main) where

import qualified Test.Laurel.Postgres.Compile
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Test.Laurel.Postgres.Compile.spec
