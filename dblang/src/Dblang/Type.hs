{-# LANGUAGE DeriveGeneric #-}

module Dblang.Type (
  Type (..),
  arrow,
  matchArrow,
  record,
  matchRecord,
  matchRow,
  replaceDefinitions,
) where

import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

data Type
  = Name Text
  | App Type Type
  | RCons Text Type Type
  | RNil
  | Unknown Int
  deriving (Eq, Show, Generic)

instance Hashable Type

arrow :: Type -> Type -> Type
arrow a = App (App (Name "Arrow") a)

matchArrow :: Type -> Maybe (Type, Type)
matchArrow ty =
  case ty of
    App (App (Name "Arrow") a) b -> Just (a, b)
    _ -> Nothing

record :: Vector (Text, Type) -> Type
record = App (Name "Record") . foldr (uncurry RCons) RNil

matchRecord :: Type -> Maybe (Vector (Text, Type))
matchRecord type_ =
  case type_ of
    App (Name "Record") fields -> fmap Vector.fromList (go fields)
    _ -> Nothing
 where
  go :: Type -> Maybe [(Text, Type)]
  go fields =
    case fields of
      RCons name ty fields' -> ((name, ty) :) <$> go fields'
      RNil -> Just []
      _ -> Nothing

matchRow :: Type -> (Vector (Text, Type), Maybe Type)
matchRow = first Vector.fromList . go
 where
  go fields =
    case fields of
      RCons name ty rest -> first ((name, ty) :) (go rest)
      RNil -> ([], Nothing)
      ty -> ([], Just ty)

replaceDefinitions :: HashMap Text Type -> Type -> Type
replaceDefinitions typeDefinitions type_ =
  case type_ of
    Name typeName ->
      maybe type_ (replaceDefinitions typeDefinitions) (HashMap.lookup typeName typeDefinitions)
    App a b ->
      App (replaceDefinitions typeDefinitions a) (replaceDefinitions typeDefinitions b)
    RCons a b c ->
      RCons a (replaceDefinitions typeDefinitions b) (replaceDefinitions typeDefinitions c)
    RNil ->
      type_
    Unknown{} ->
      type_