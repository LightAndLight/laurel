{-# LANGUAGE OverloadedRecordDot #-}

module Laurel.Definition.Table (Table (..), inputType) where

import Data.Text (Text)
import Data.Vector (Vector)
import Laurel.Definition.Constraint (Constraint)
import Laurel.Type (Type)
import qualified Laurel.Type as Type

data Table = Table
  { name :: Text
  , types :: Vector (Text, Type)
  , inFields :: Vector (Text, Type)
  , outFields :: Vector (Text, Type)
  , constraints :: Vector Constraint
  }
  deriving (Eq, Show)

inputType :: Table -> Type
inputType table = Type.record table.inFields