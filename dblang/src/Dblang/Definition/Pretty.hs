module Dblang.Definition.Pretty (prettyDefinition) where

import Dblang.Definition (Definition (..))
import Dblang.Definition.Table.Pretty (prettyTable)
import Pretty (IsLines)

prettyDefinition :: IsLines lines => Definition -> lines
prettyDefinition definition =
  case definition of
    Table table ->
      prettyTable table