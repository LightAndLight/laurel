module Main where

import Control.Applicative ((<**>))
import qualified Options.Applicative as Options
import qualified Repl
import qualified System.IO

data Cli
  = Repl
  deriving (Eq, Show)

cliParser :: Options.Parser Cli
cliParser =
  Options.subparser $
    Options.command "repl" (Options.info (pure Repl <**> Options.helper) Options.fullDesc)

main :: IO ()
main = do
  cli <- Options.execParser $ Options.info (cliParser <**> Options.helper) Options.fullDesc
  case cli of
    Repl ->
      Repl.run System.IO.stdin System.IO.stdout
