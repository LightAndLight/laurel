module Laurel.Definition (Definition (..)) where

import Laurel.Definition.Table (Table)

data Definition = Table Table
  deriving (Eq, Show)