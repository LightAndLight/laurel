module Main (main) where

import qualified Test.Dblang.Compile.Postgres
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Test.Dblang.Compile.Postgres.spec
