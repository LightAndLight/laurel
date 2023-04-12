{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dblang.Value.Pretty (pretty, prettyType, Table (..), prettyTable) where

import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Dblang.Value (Value)
import qualified Dblang.Value as Value

prettyType :: Type -> Text
prettyType ty
  | Just (a, b) <- Type.matchArrow ty =
      arrowParens prettyType a <> " -> " <> prettyType b
 where
  arrowParens f a =
    case Type.matchArrow a of
      Just{} -> "(" <> f a <> ")"
      Nothing -> f a
prettyType ty
  | Just rows <- Type.matchRecord ty =
      "{"
        <> ( case Vector.uncons rows of
              Nothing -> ""
              Just ((fieldName, fieldTy), rest) ->
                " "
                  <> fieldName
                  <> " : "
                  <> prettyType fieldTy
                  <> foldMap (\(fieldName', fieldType') -> ", " <> fieldName' <> " : " <> prettyType fieldType') rest
                  <> " "
           )
        <> "}"
prettyType ty =
  case ty of
    Type.Name name -> name
    Type.App a b ->
      prettyType a
        <> " "
        <> appParens prettyType b
     where
      appParens f x =
        case x of
          Type.App{} ->
            "(" <> f x <> ")"
          _ ->
            f x
    Type.RCons{} -> undefined
    Type.RNil{} -> undefined
    Type.Unknown n -> Text.pack $ "?" <> show n

pretty :: Value -> Type -> Text
pretty value ty =
  case value of
    Value.Relation values ->
      case ty of
        Type.App (Type.Name "Relation") itemType ->
          case Type.matchRecord itemType of
            Just tyFields ->
              prettyTable
                Table
                  { header = fmap (\(fieldName, fieldType) -> fieldName <> " : " <> prettyType fieldType) tyFields
                  , body =
                      ( \case
                          Value.Record fields ->
                            fmap
                              ( \(fieldName, fieldType) ->
                                  pretty
                                    ( Maybe.fromMaybe
                                        (error $ "field " <> show fieldName <> " missing from " <> show fields)
                                        (HashMap.lookup fieldName fields)
                                    )
                                    fieldType
                              )
                              tyFields
                          value' -> error $ "expected record, got " <> show value'
                      )
                        <$> Value.toVector values
                  }
            Nothing ->
              prettyTable
                Table
                  { header = ["_ : " <> prettyType itemType]
                  , body = (\value' -> [pretty value' itemType]) <$> Value.toVector values
                  }
        _ ->
          error $ "invalid type for Relation: " <> show ty
    Value.Map values ->
      case ty of
        Type.App (Type.App (Type.Name "Map") keyType) valueType ->
          prettyTable
            Table
              { header =
                  ["key : " <> prettyType keyType, "value : " <> prettyType valueType]
              , body =
                  Vector.fromList
                    . fmap (\(k, v) -> [pretty k keyType, pretty v valueType])
                    $ HashMap.toList values
              }
        _ ->
          error $ "invalid type for Map: " <> show ty
    Value.Record fields ->
      case Type.matchRecord ty of
        Just tyFields ->
          "{"
            <> ( case Vector.uncons tyFields of
                  Nothing -> ""
                  Just ((fieldName, fieldType), rest) ->
                    prettyField fields fieldName fieldType
                      <> foldMap (\(fieldName', fieldType') -> ", " <> prettyField fields fieldName' fieldType') rest
               )
            <> "}"
        Nothing ->
          error $ "invalid type for Map: " <> show ty
    Value.Int i -> Text.pack $ show i
    Value.Bool b -> if b then "true" else "false"
    Value.String s -> s
    Value.Unit -> "()"
    Value.Lam{} -> "<function>"
 where
  prettyField fields fieldName fieldType =
    fieldName
      <> " = "
      <> pretty
        ( Maybe.fromMaybe
            (error $ "field " <> show fieldName <> " not found in " <> show fields)
            (HashMap.lookup fieldName fields)
        )
        fieldType

{-
╭────────────────┬───────────────┬───────────╮
│ id : people.Id │ name : String | age : Int │
├────────────────┼───────────────┼───────────┤
│ 1              │ Joe Bloe      │ 22        │
│ 2              │ John Doe      │ 5         │
│ 3              │ Jane Doe      │ 17        │
│ 4              │ Li Wang       │ 62        │
│ 5              │ Alan Turing   │ 79        │
│ 6              │ Alonzo Church │ 23        │
│ 7              │ Plato         │ 34        │
╰────────────────┴───────────────┴───────────╯
-}
(╭), (╰), (╯), (│), (─), (┴), (╮), (┬), (┼), (├), (┤) :: Text
(╭) = "╭"
(╰) = "╰"
(╮) = "╮"
(╯) = "╯"
(│) = "│"
(─) = "─"
(┬) = "┬"
(┼) = "┼"
(┴) = "┴"
(├) = "├"
(┤) = "┤"

data Table = Table {header :: Vector Text, body :: Vector (Vector Text)}

sepBy :: Monoid m => Vector m -> m -> m
sepBy ms sep =
  case Vector.uncons ms of
    Nothing ->
      mempty
    Just (m, rest) ->
      m <> foldMap (sep <>) rest

rightPad :: Int -> Text -> Text -> Text
rightPad width padding input =
  input <> Text.replicate (max 0 (width - inputLength)) padding
 where
  inputLength = Text.length input

{-# HLINT ignore "Use map" #-}
prettyTable :: Table -> Text
prettyTable table =
  Text.unlines $
    [ prettyBorder (╭) (┬) (╮)
    , prettyRow table.header
    , prettyBorder (├) (┼) (┤)
    ]
      <> foldr (\row rest -> prettyRow row : rest) [] table.body
      <> [prettyBorder (╰) (┴) (╯)]
 where
  -- The width of each column's content. Each column is as wide as its widest content.
  columnContentWidths :: Vector Int
  columnContentWidths =
    foldl'
      ( \widths row ->
          if length widths == length row
            then
              Vector.zipWith
                (\width content -> max width (Text.length content))
                widths
                row
            else
              error $
                "incorrect number of fields in row. expected "
                  <> show (length widths)
                  <> ", got "
                  <> show (length row)
      )
      (fmap Text.length table.header)
      table.body

  prettyBorder :: Text -> Text -> Text -> Text
  prettyBorder left middle right =
    left
      <> sepBy (fmap (\width -> (─) <> Text.replicate width (─) <> (─)) columnContentWidths) middle
      <> right

  prettyRow :: Vector Text -> Text
  prettyRow row =
    (│)
      <> sepBy
        ( Vector.zipWith
            (\width content -> " " <> rightPad width " " content <> " ")
            columnContentWidths
            row
        )
        (│)
      <> (│)