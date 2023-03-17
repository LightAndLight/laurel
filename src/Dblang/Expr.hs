{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Dblang.Expr (Expr (..), typeOf) where

import Bound (Scope, (>>>=))
import qualified Control.Monad
import Data.Eq.Deriving (deriveEq1)
import Data.Text (Text)
import Data.Vector (Vector)
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Text.Show.Deriving (deriveShow1)

data Expr a
  = Name Text
  | Var a
  | Lam Text (Scope () Expr a)
  | -- | `f x`
    App Type (Expr a) (Vector (Expr a))
  | Yield (Expr a)
  | From (Expr a) Text Type (Scope () Expr a)
  | Filter Text Type (Scope () Expr a) (Expr a)
  | When (Expr a) (Expr a)
  | -- | `r.x`
    Dot Type (Expr a) Text
  | -- | `r.{ x, y, z }`
    Splat (Expr a) (Vector Text)
  | Record (Vector (Text, Expr a))
  | Int Int
  | Equals (Expr a) (Expr a)
  deriving (Functor, Foldable, Traversable)

instance Applicative Expr where
  pure = Var
  (<*>) = Control.Monad.ap

instance Monad Expr where
  Name name >>= _ = Name name
  Var a >>= f = f a
  Lam name body >>= f = Lam name (body >>>= f)
  App t a b >>= f = App t (a >>= f) (fmap (>>= f) b)
  Yield a >>= f = Yield (a >>= f)
  From a name ty b >>= f = From (a >>= f) name ty (b >>>= f)
  Filter name ty a b >>= f = Filter name ty (a >>>= f) (b >>= f)
  When a b >>= f = When (a >>= f) (b >>= f)
  Dot t a b >>= f = Dot t (a >>= f) b
  Splat a b >>= f = Splat (a >>= f) b
  Record a >>= f = Record ((fmap . fmap) (>>= f) a)
  Int i >>= _ = Int i
  Equals a b >>= f = Equals (a >>= f) (b >>= f)

deriveEq1 ''Expr
deriveShow1 ''Expr

deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)

typeOf :: (a -> Type) -> Expr a -> Type
typeOf varType expr =
  case expr of
    Name{} ->
      error "TODO: typeOf: Name"
    Var var ->
      varType var
    Lam{} ->
      error "TODO: typeOf: Lam"
    App{} ->
      error "TODO: typeOf: App"
    Yield a ->
      Type.App (Type.Name "Relation") (typeOf varType a)
    From{} ->
      error "TODO: typeOf: From"
    Filter{} ->
      error "TODO: typeOf: Filter"
    When{} ->
      error "TODO: typeOf: When"
    Dot{} ->
      error "TODO: typeOf: Dot"
    Splat{} ->
      error "TODO: typeOf: Splat"
    Record{} ->
      error "TODO: typeOf: Record"
    Int{} ->
      Type.Name "Int"
    Equals{} ->
      Type.Name "Bool"