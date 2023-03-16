{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Dblang.Expr (Expr (..)) where

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
  | -- | `f x`
    App Text (Vector (Expr a))
  | Yield (Expr a)
  | From (Expr a) (Expr a)
  | -- | `r.x`
    Dot (Expr a) Text
  | -- | `r.{ x, y, z }`
    Splat (Expr a) (Vector Text)
  | Record (Vector (Text, Expr a))
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad Expr where
  Name name >>= _ = Name name
  Var a >>= f = f a
  Lam name body >>= f = Lam name (body >>>= f)
  App a b >>= f = App a (fmap (>>= f) b)
  Yield a >>= f = Yield (a >>= f)
  From a b >>= f = From (a >>= f) (b >>= f)
  Dot a b >>= f = Dot (a >>= f) b
  Splat a b >>= f = Splat (a >>= f) b
  Record a >>= f = Record ((fmap . fmap) (>>= f) a)

deriveEq1 ''Expr
deriveShow1 ''Expr

deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)