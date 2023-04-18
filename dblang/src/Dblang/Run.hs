{-# LANGUAGE ExistentialQuantification #-}

module Dblang.Run (Run (..), RunError (..)) where

import Data.Text (Text)
import Dblang.Type (Type)
import qualified Dblang.Typecheck as Typecheck
import Dblang.Value (Value)
import Text.Sage (ParseError)

data RunError e
  = ParseError ParseError
  | TypeError Typecheck.Error
  | OtherError !e
  deriving (Eq, Show)

data Run m = forall e.
  Show e =>
  Run
  { eval :: Text -> m (Either (RunError e) (Value, Type))
  , typeOf :: Text -> m (Either (RunError e) Type)
  }