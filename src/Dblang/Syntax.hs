{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Dblang.Syntax (Expr (..)) where

import Bound (Scope, (>>>=))
import qualified Control.Monad
import Data.Eq.Deriving (deriveEq1)
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Show.Deriving (deriveShow1)

data Expr a
  = Name Text
  | Var a
  | Lam Text (Scope () Expr a)
  | App (Expr a) (Expr a)
  | -- | `r.x`
    Dot (Expr a) Text
  | -- | `r.{ x, y, z }`
    Splat (Expr a) (Vector Text)
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad Expr where
  Name name >>= _ = Name name
  Var a >>= f = f a
  Lam name body >>= f = Lam name (body >>>= f)
  App a b >>= f = App (a >>= f) (b >>= f)
  Dot a b >>= f = Dot (a >>= f) b
  Splat a b >>= f = Splat (a >>= f) b

deriveEq1 ''Expr
deriveShow1 ''Expr

deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)