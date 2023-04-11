{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dblang.Value (Multiset, fromVector, Value (..)) where

import Bound (Scope)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)
import Dblang.Expr (Expr)

newtype Multiset a = Multiset (Vector a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid)

fromVector :: Vector a -> Multiset a
fromVector = Multiset

data Value
  = Relation (Multiset Value)
  | Record (HashMap Text Value)
  | Int Int
  | Bool Bool
  | String Text
  | Unit
  | Lam Int (Scope Int Expr Value)
  deriving (Eq, Show)