module Dblang.Type (Type (..)) where

import Data.Text (Text)

data Type
  = Name Text
  | App Type Type
  deriving (Eq, Show)