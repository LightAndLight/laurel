module Dblang.Type (Type (..), record, matchRecord) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Type
  = Name Text
  | App Type Type
  | RCons Text Type Type
  | RNil
  deriving (Eq, Show)

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