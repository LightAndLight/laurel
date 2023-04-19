{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Laurel.Syntax (
  Definition (..),
  TableItem (..),
  Constraint (..),
  Expr (..),
  Command (..),
) where

import Bound (Scope, (>>>=))
import qualified Control.Monad
import Data.Eq.Deriving (deriveEq1)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)
import Laurel.Type (Type)
import Text.Show.Deriving (deriveShow1)

data Expr a
  = Name Text
  | Var a
  | Ctor Text (Vector (Expr a))
  | Lam Text (Scope () Expr a)
  | App (Expr a) (Expr a)
  | -- | `r.x`
    Dot (Expr a) Text
  | -- | `r.{ x, y, z }`
    Splat (Expr a) (Vector Text)
  | -- | `for a in as rest`
    For Text (Expr a) (Scope () Expr a)
  | -- | `where b rest`
    Where (Expr a) (Expr a)
  | -- | `yield a`
    Yield (Expr a)
  | -- | `a group by b`
    GroupBy (Expr a) (Expr a)
  | Record (Vector (Text, Expr a))
  | Equals (Expr a) (Expr a)
  | Int Int
  | Bool Bool
  | String Text
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad Expr where
  Name name >>= _ = Name name
  Var a >>= f = f a
  Ctor name arg >>= f = Ctor name (fmap (>>= f) arg)
  Lam name body >>= f = Lam name (body >>>= f)
  App a b >>= f = App (a >>= f) (b >>= f)
  Dot a b >>= f = Dot (a >>= f) b
  Splat a b >>= f = Splat (a >>= f) b
  For name a b >>= f = For name (a >>= f) (b >>>= f)
  Where a b >>= f = Where (a >>= f) (b >>= f)
  Yield a >>= f = Yield (a >>= f)
  GroupBy a b >>= f = GroupBy (a >>= f) (b >>= f)
  Record fields >>= f = Record $ (fmap . fmap) (>>= f) fields
  Equals a b >>= f = Equals (a >>= f) (b >>= f)
  Int i >>= _ = Int i
  Bool b >>= _ = Bool b
  String s >>= _ = String s

deriveEq1 ''Expr
deriveShow1 ''Expr

deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)

data Definition = Table {name :: Text, items :: Vector TableItem}
  deriving (Eq, Show)

data TableItem
  = -- | `type X = Y`
    Type {name :: Text, value :: Type}
  | -- | `x : Y [constraints]`
    Field {name :: Text, type_ :: Type, constraints :: Vector Constraint}
  | -- | `Name(arg_1, arg_2, ..., arg_n)`
    Constraint Constraint
  deriving (Eq, Show)

data Constraint = MkConstraint {name :: Text, arguments :: Vector (Expr Void)}
  deriving (Eq, Show)

data Command
  = Eval {value :: Expr Void}
  | Insert {table :: Text, value :: Expr Void}
  deriving (Eq, Show)