module Main (main) where

import qualified Test.Dblang.Postgres.Compile
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Test.Dblang.Postgres.Compile.spec
