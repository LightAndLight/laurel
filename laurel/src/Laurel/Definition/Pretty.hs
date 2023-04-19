module Laurel.Definition.Pretty (prettyDefinition) where

import Laurel.Definition (Definition (..))
import Laurel.Definition.Table.Pretty (prettyTable)
import Pretty (IsLines)

prettyDefinition :: IsLines lines => Definition -> lines
prettyDefinition definition =
  case definition of
    Table table ->
      prettyTable table