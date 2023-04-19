{-# LANGUAGE ExistentialQuantification #-}

module Laurel.Run (Run (..), RunError (..)) where

import Data.Text (Text)
import Data.Vector (Vector)
import Laurel.Definition (Definition)
import Laurel.Type (Type)
import qualified Laurel.Typecheck as Typecheck
import Laurel.Value (Value)
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
  , definitions :: m (Vector Definition)
  }