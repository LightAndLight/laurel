{-# LANGUAGE ExistentialQuantification #-}

module Laurel.Run (Run (..), RunError (..)) where

import Data.Vector (Vector)
import Data.Void (Void)
import Laurel.Definition (Definition)
import qualified Laurel.Syntax as Syntax
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
  { eval :: Syntax.Expr Void -> m (Either (RunError e) (Value, Type))
  , typeOf :: Syntax.Expr Void -> m (Either (RunError e) Type)
  , definitions :: m (Vector Definition)
  }