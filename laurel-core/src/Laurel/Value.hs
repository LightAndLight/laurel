{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Laurel.Value (Multiset, fromVector, toVector, Value (..)) where

import Bound (Scope)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Laurel.Expr (Expr)

-- instance Hashable a => Hashable (Vector a)
import Data.Vector.Instances ()

newtype Multiset a = Multiset (Vector a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid, Hashable)

fromVector :: Vector a -> Multiset a
fromVector = Multiset

toVector :: Multiset a -> Vector a
toVector (Multiset a) = a

data Value
  = Relation (Multiset Value)
  | Map (HashMap Value Value)
  | Record (HashMap Text Value)
  | Int Int
  | Bool Bool
  | String Text
  | Unit
  | Lam Int (Scope Int Expr Value)
  | Ctor Text (Vector Value)
  | List (Vector Value)
  deriving (Eq, Show, Generic)

instance Hashable Value