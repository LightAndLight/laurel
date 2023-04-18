{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dblang.Definition.Table.Pretty (prettyTable) where

import Data.Bifunctor (first)
import Data.Foldable (fold, foldr')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (absurd)
import Dblang.Definition.Constraint (Constraint)
import qualified Dblang.Definition.Constraint as Constraint
import Dblang.Definition.Table (Table (..))
import Dblang.Expr.Pretty (prettyExpr)
import Dblang.Value.Pretty (prettyType)
import Pretty (IsLines, horizontally, vertically)
import qualified Pretty

{-# ANN prettyTable ("HLint: ignore Avoid lambda" :: String) #-}
prettyTable :: IsLines lines => Table -> lines
prettyTable Table{name, types, constraints} =
  vertically . fold @[] $
    [ horizontally $ Pretty.line "table " <> Pretty.line name <> Pretty.line " {"
    , Pretty.indent 2 . Pretty.endToEnd . flip Pretty.sepBy (Pretty.line "," <> Pretty.newline) $
        fmap
          ( \(columnName, columnType) ->
              Pretty.hfold @[] $
                [ Pretty.line columnName
                , Pretty.line " : "
                , Pretty.line $ prettyType columnType
                , case HashMap.lookup columnName singleFieldConstraints of
                    Nothing ->
                      mempty
                    Just fieldConstraints ->
                      horizontally $
                        Pretty.line " ["
                          <> Pretty.sepBy
                            ( fmap
                                ( \case
                                    Constraint.PrimaryKey{} ->
                                      Pretty.line "PrimaryKey"
                                    Constraint.Key{} ->
                                      Pretty.line "Key"
                                    Constraint.Default _field value ->
                                      Pretty.line "Default(" <> prettyExpr absurd value <> Pretty.line ")"
                                )
                                fieldConstraints
                            )
                            (Pretty.line ", ")
                          <> Pretty.line "]"
                ]
          )
          types
          <> ( if null multiFieldConstraints
                then []
                else
                  fmap
                    ( \case
                        Constraint.PrimaryKey fields ->
                          horizontally $ Pretty.line "PrimaryKey" <> Pretty.line "(" <> Pretty.sepBy (fmap Pretty.line fields) (Pretty.line ", ") <> Pretty.line ")"
                        Constraint.Key fields ->
                          horizontally $ Pretty.line "Key" <> Pretty.line "(" <> Pretty.sepBy (fmap Pretty.line fields) (Pretty.line ", ") <> Pretty.line ")"
                        Constraint.Default{} ->
                          -- `Default` is always a single-field constraint
                          undefined
                    )
                    multiFieldConstraints
             )
    , Pretty.line "}"
    ]
 where
  singleFieldConstraints :: HashMap Text [Constraint]
  multiFieldConstraints :: Vector Constraint
  (singleFieldConstraints, multiFieldConstraints) =
    first (foldr' (\(field, constraint) rest -> HashMap.insertWith (\new old -> old <> new) field [constraint] rest) mempty) $
      Vector.partitionWith
        ( \constraint -> case constraint of
            Constraint.PrimaryKey fields ->
              case fields of
                [field] -> Left (field, constraint)
                _ -> Right constraint
            Constraint.Key fields ->
              case fields of
                [field] -> Left (field, constraint)
                _ -> Right constraint
            Constraint.Default field _value ->
              Left (field, constraint)
        )
        constraints