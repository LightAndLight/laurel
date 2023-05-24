{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Laurel.Value.Pretty (
  pretty,
  prettyType,
  Table (..),
  prettyTable,
) where

import Data.Foldable (fold, foldl')
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Semigroup (stimesMonoid)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Laurel.Type (Type)
import qualified Laurel.Type as Type
import Laurel.Value (Value)
import qualified Laurel.Value as Value
import Pretty (Horizontally (..), IsLines, Vertically (..), horizontally)
import qualified Pretty
import Prelude hiding (unlines)

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
      appParens f x
        | Just{} <- Type.matchRecord x =
            f x
      appParens f x =
        case x of
          Type.App{} ->
            "(" <> f x <> ")"
          _ ->
            f x
    Type.RCons{} -> undefined
    Type.RNil{} -> undefined
    Type.Unknown n -> Text.pack $ "?" <> show n

isTable :: Type -> Bool
isTable ty =
  case ty of
    Type.App (Type.Name "Relation") a | Just{} <- Type.matchRecord a -> True
    Type.App (Type.App (Type.Name "Map") _) _ -> True
    _ -> False

pretty :: IsLines lines => Value -> Type -> lines
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
    Value.List values ->
      case ty of
        Type.App (Type.Name "List") itemType ->
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
                        <$> values
                  }
            Nothing ->
              prettyTable
                Table
                  { header = ["_ : " <> prettyType itemType]
                  , body = (\value' -> [pretty value' itemType]) <$> values
                  }
        _ ->
          error $ "invalid type for List: " <> show ty
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
          if any (\(_, fieldType) -> isTable fieldType) tyFields
            then
              Pretty.vfold @[]
                [ Pretty.line "{"
                , case Vector.unsnoc tyFields of
                    Nothing ->
                      -- impossible, because if tyFields was empty then we'd be in the `else` branch
                      undefined
                    Just (rest, (fieldName, fieldType)) ->
                      Pretty.indent 2 $
                        Pretty.vertically
                          ( foldMap
                              ( \(fieldName', fieldType') ->
                                  prettyField fields fieldName' fieldType' `Pretty.append` Pretty.line ","
                              )
                              rest
                          )
                          `Pretty.vcat` prettyField fields fieldName fieldType
                , Pretty.line "}"
                ]
            else
              Pretty.hfold @[]
                [ Pretty.line "{"
                , if null tyFields then Pretty.empty else Pretty.line " "
                , Pretty.horizontally $
                    Pretty.sepBy
                      (fmap (uncurry (prettyField fields)) tyFields)
                      (Pretty.line ", ")
                , if null tyFields then Pretty.empty else Pretty.line " "
                , Pretty.line "}"
                ]
        Nothing ->
          error $ "invalid type for Record: " <> show ty
    Value.Int i ->
      Pretty.line . Text.pack $ show i
    Value.Bool b ->
      Pretty.line $ if b then "true" else "false"
    Value.String s ->
      Pretty.line s
    Value.Unit ->
      Pretty.line "()"
    Value.Lam{} ->
      Pretty.line "<function>"
    Value.Ctor name args ->
      horizontally $
        Pretty.line name
          <> ( case ty of
                Type.App (Type.Name "Optional") argTy
                  | "Some" <- name
                  , [arg] <- args ->
                      Pretty.line "("
                        <> pretty arg argTy
                        <> Pretty.line ")"
                  | "None" <- name
                  , [] <- args ->
                      mempty
                _ ->
                  error $ "invalid type for Ctor: " <> show ty
             )
 where
  prettyField :: IsLines lines => HashMap Text Value -> Text -> Type -> lines
  prettyField fields fieldName fieldType =
    Pretty.line (fieldName <> " = ")
      & if isTable fieldType
        then
          ( `Pretty.vcat`
              Pretty.indent
                2
                ( pretty
                    ( Maybe.fromMaybe
                        (error $ "field " <> show fieldName <> " not found in " <> show fields)
                        (HashMap.lookup fieldName fields)
                    )
                    fieldType
                )
          )
        else
          ( `Pretty.hcat`
              pretty
                ( Maybe.fromMaybe
                    (error $ "field " <> show fieldName <> " not found in " <> show fields)
                    (HashMap.lookup fieldName fields)
                )
                fieldType
          )

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

data Table lines = Table {header :: Vector Text, body :: Vector (Vector lines)}

prettyTable :: IsLines lines => Table lines -> lines
prettyTable table =
  Pretty.vertically . fold @[] $
    [ prettyBorder (╭) (┬) (╮)
    , prettyRow $ Vector.zipWith (\a b -> (a, Pretty.line b)) columnContentWidths table.header
    , prettyBorder (├) (┼) (┤)
    , foldMap @Vector (Vertically . prettyRow . Vector.zip columnContentWidths) table.body
    , prettyBorder (╰) (┴) (╯)
    ]
 where
  -- The width of each column's content. Each column is as wide as its widest content.
  columnContentWidths :: Vector Int
  columnContentWidths =
    foldl'
      ( \widths row ->
          if length widths == length row
            then
              Vector.zipWith
                (\width content -> max width (Pretty.width content))
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

  prettyBorder :: IsLines lines => Text -> Text -> Text -> lines
  prettyBorder left middle right =
    horizontally . fold @[] $
      [ Pretty.line left
      , Pretty.sepBy
          (fmap (\width -> Pretty.line $ (─) <> Text.replicate width (─) <> (─)) columnContentWidths)
          (Pretty.line middle)
      , Pretty.line right
      ]

prettyRow :: IsLines lines => Vector (Int, lines) -> lines
prettyRow row =
  horizontally . fold @[] $
    [ Pretty.vertically $ stimesMonoid rowHeight (Pretty.line $ (│) <> " ")
    , Pretty.sepBy
        ( fmap
            (\(width, content) -> Pretty.rightPad width ' ' $ Pretty.downPad rowHeight "" $ Horizontally content)
            row
        )
        (Pretty.vertically $ stimesMonoid rowHeight (Pretty.line $ " " <> (│) <> " "))
    , Pretty.vertically $ stimesMonoid rowHeight (Pretty.line $ " " <> (│))
    ]
 where
  rowHeight :: Int
  rowHeight = foldl' max 0 $ fmap (Pretty.height . snd) row