{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dblang.Value.Pretty (
  pretty,
  prettyType,
  Table (..),
  prettyTable,
  Lines,
  unlines,
  line,
  Vertically (..),
  vertically,
  Horizontally (..),
  horizontally,
) where

import Data.Foldable (foldl')
import Data.Function ((&))
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

newtype Lines = Lines [Text]

unlines :: Lines -> Text
unlines (Lines ls) = Text.unlines ls

newtype Horizontally = Horizontally Lines

horizontally :: Horizontally -> Lines
horizontally (Horizontally ls) = ls

instance Semigroup Horizontally where
  Horizontally a <> Horizontally b = Horizontally (hcat a b)

instance Monoid Horizontally where
  mempty = Horizontally empty

newtype Vertically = Vertically Lines

vertically :: Vertically -> Lines
vertically (Vertically ls) = ls

instance Semigroup Vertically where
  Vertically a <> Vertically b = Vertically (vcat a b)

instance Monoid Vertically where
  mempty = Vertically empty

empty :: Lines
empty = Lines []

vcat :: Lines -> Lines -> Lines
vcat (Lines a) (Lines b) = Lines (a <> b)

vtimes :: Int -> Lines -> Lines
vtimes 0 _ = empty
vtimes n x = x `vcat` vtimes (n - 1) x

hcat :: Lines -> Lines -> Lines
hcat (Lines a) (Lines b) =
  Lines
    . take (max (length a) (length b))
    $ zipWith (<>) (a <> repeat "") (b <> repeat "")

line :: Text -> Lines
line = Lines . pure

linesWidth :: Lines -> Int
linesWidth (Lines ls) = foldl' max 0 (fmap Text.length ls)

linesHeight :: Lines -> Int
linesHeight (Lines ls) = length ls

rightPadLines :: Int -> Text -> Lines -> Lines
rightPadLines width padding (Lines ls) =
  Lines $
    fmap
      ( \l ->
          let lLength = Text.length l
           in l <> Text.replicate (max 0 (width - lLength)) padding
      )
      ls

downPadLines :: Int -> Text -> Lines -> Lines
downPadLines height padding (Lines ls) =
  Lines $ ls <> replicate (max 0 (height - length ls)) padding

indent :: Int -> Lines -> Lines
indent n (Lines ls) = Lines $ fmap (Text.replicate n " " <>) ls

append :: Lines -> Text -> Lines
append (Lines ls) value = Lines $ go ls
 where
  go [] = []
  go [x] = [x <> value]
  go (x : xs@(_ : _)) = x : go xs

isTable :: Type -> Bool
isTable ty =
  case ty of
    Type.App (Type.Name "Relation") a | Just{} <- Type.matchRecord a -> True
    Type.App (Type.App (Type.Name "Map") _) _ -> True
    _ -> False

pretty :: Value -> Type -> Lines
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
          if any (\(_, fieldType) -> isTable fieldType) tyFields
            then
              line "{"
                `vcat` indent
                  2
                  ( case Vector.unsnoc tyFields of
                      Nothing ->
                        -- impossible, because if tyFields was empty then we'd be in the
                        -- horizontal case
                        undefined
                      Just (rest, (fieldName, fieldType)) ->
                        vertically
                          ( foldMap
                              ( \(fieldName', fieldType') ->
                                  Vertically $
                                    prettyField fields fieldName' fieldType' `append` ","
                              )
                              rest
                          )
                          `vcat` prettyField fields fieldName fieldType
                  )
                `vcat` line "}"
            else
              line "{"
                `hcat` (if null tyFields then empty else line " ")
                `hcat` horizontally (sepBy (fmap (Horizontally . uncurry (prettyField fields)) tyFields) (Horizontally $ line ", "))
                `hcat` (if null tyFields then empty else line " ")
                `hcat` line "}"
        Nothing ->
          error $ "invalid type for Map: " <> show ty
    Value.Int i -> line . Text.pack $ show i
    Value.Bool b -> line $ if b then "true" else "false"
    Value.String s -> line s
    Value.Unit -> line "()"
    Value.Lam{} -> line "<function>"
 where
  prettyField fields fieldName fieldType =
    line (fieldName <> " = ")
      & if isTable fieldType
        then
          ( `vcat`
              indent
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
          ( `hcat`
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

data Table = Table {header :: Vector Text, body :: Vector (Vector Lines)}

sepBy :: Monoid m => Vector m -> m -> m
sepBy ms sep =
  case Vector.uncons ms of
    Nothing ->
      mempty
    Just (m, rest) ->
      m <> foldMap (sep <>) rest

{-# HLINT ignore "Use map" #-}
prettyTable :: Table -> Lines
prettyTable table =
  line (prettyBorder (╭) (┬) (╮))
    `vcat` prettyRow (fmap line table.header)
    `vcat` line (prettyBorder (├) (┼) (┤))
    `vcat` foldr (\row rest -> prettyRow row `vcat` rest) empty table.body
    `vcat` line (prettyBorder (╰) (┴) (╯))
 where
  -- The width of each column's content. Each column is as wide as its widest content.
  columnContentWidths :: Vector Int
  columnContentWidths =
    foldl'
      ( \widths row ->
          if length widths == length row
            then
              Vector.zipWith
                (\width content -> max width (linesWidth content))
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

  prettyRow :: Vector Lines -> Lines
  prettyRow row =
    vtimes rowHeight (line $ (│) <> " ")
      `hcat` horizontally
        ( sepBy
            ( Horizontally
                <$> Vector.zipWith
                  (\width content -> rightPadLines width " " $ downPadLines rowHeight "" content)
                  columnContentWidths
                  row
            )
            (Horizontally $ vtimes rowHeight (line $ " " <> (│) <> " "))
        )
      `hcat` vtimes rowHeight (line $ " " <> (│))
   where
    rowHeight :: Int
    rowHeight = foldl' max 0 (fmap linesHeight row)