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
  | -- | `for a in as rest`
    For Text Type (Expr a) (Scope () Expr a)
  | Where (Expr a) (Expr a)
  | -- | `r.x`
    Dot Type (Expr a) Text
  | -- | `r.{ x, y, z }`
    Splat (Expr a) (Vector Text)
  | Record (Vector (Text, Expr a))
  | Int Int
  | Bool Bool
  | String Text
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
  For name ty a b >>= f = For name ty (a >>= f) (b >>>= f)
  Where a b >>= f = Where (a >>= f) (b >>= f)
  Dot t a b >>= f = Dot t (a >>= f) b
  Splat a b >>= f = Splat (a >>= f) b
  Record a >>= f = Record ((fmap . fmap) (>>= f) a)
  Int i >>= _ = Int i
  Bool b >>= _ = Bool b
  String s >>= _ = String s
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
    For{} ->
      error "TODO: typeOf: For"
    Where{} ->
      error "TODO: typeOf: Where"
    Dot{} ->
      error "TODO: typeOf: Dot"
    Splat{} ->
      error "TODO: typeOf: Splat"
    Record fields ->
      Type.App (Type.Name "Record") $ foldr (\(field, value) -> Type.RCons field (typeOf varType value)) Type.RNil fields
    Int{} ->
      Type.Name "Int"
    Bool{} ->
      Type.Name "Bool"
    String{} ->
      Type.Name "String"
    Equals{} ->
      Type.Name "Bool"