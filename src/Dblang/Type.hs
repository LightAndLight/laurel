module Dblang.Type (Type (..), arrow, matchArrow, record, matchRecord, matchRow) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Type
  = Name Text
  | App Type Type
  | RCons Text Type Type
  | RNil
  | Unknown Int
  deriving (Eq, Show)

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

matchRow :: Type -> Maybe (Vector (Text, Type), Maybe Type)
matchRow type_ =
  case type_ of
    RCons{} -> Just $ first Vector.fromList (go type_)
    RNil{} -> Just $ first Vector.fromList (go type_)
    _ -> Nothing
 where
  go :: Type -> ([(Text, Type)], Maybe Type)
  go fields =
    case fields of
      RCons name ty rest -> first ((name, ty) :) (go rest)
      RNil -> ([], Nothing)
      ty -> ([], Just ty)