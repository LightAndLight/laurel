module Dblang.Definition (Definition (..)) where

import Dblang.Definition.Table (Table)

data Definition = Table Table
  deriving (Eq, Show)