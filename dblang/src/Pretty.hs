{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pretty (
  Lines,
  unlines,
  IsLines (..),
  indent,
  leftPad,
  rightPad,
  leftSquare,
  rightSquare,
  downPad,
  append,
  sepBy,

  -- * Horizontal composition
  Horizontally (..),
  horizontally,
  hfold,

  -- * Vertical composition
  Vertically (..),
  vertically,
  vfold,
) where

import Data.Foldable (foldl', toList)
import Data.Semigroup (stimesMonoid)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (lines, unlines)

newtype Lines = Lines {value :: [Text]}

unlines :: Lines -> Text
unlines (Lines ls) = Text.intercalate "\n" ls

class IsLines lines where
  width :: lines -> Int
  height :: lines -> Int

  -- | `width empty == 0`
  --   `height empty == 0`
  empty :: lines

  -- | `width (line l) == Text.length l`
  --   `height (line l) == 1`
  --   `line (a <> b) == line a `hcat` line b`
  line :: Text -> lines

  -- | `width (hcat a b) == width a + width b`
  --   `height (hcat a b) == max (height a) (height b)`
  --   `hcat empty a == a`
  --   `hcat a empty == a`
  hcat :: lines -> lines -> lines

  -- | `width (vcat a b) == max (width a) (width b)`
  --   `height (vcat a b) == height a + height b`
  --   `vcat empty a == a`
  --   `vcat a empty == a`
  vcat :: lines -> lines -> lines

  -- | `mapLines id x = x`
  --   `mapLines f (mapLines g x) = mapLines (f . g) x`
  mapLines :: (Text -> Text) -> lines -> lines

  -- | `substLines line x = x`
  --   `substLines f (substLines g x) = substLines (substLines f . g) x`
  substLines :: (Text -> lines) -> lines -> lines

  -- | `mapSlice offset length id x = x`
  --   `mapSlice offset length f (mapSlice offset length g x) = mapSlice offset length (f . g) x`
  --   `mapSlice 0 (height x) f x = f x
  --   ```
  --   mapSlice offset length1 f (mapSlice (offset + length1) length2 f x) =
  --     mapSlice offset (length1 + length2) f x
  --   ```
  mapSlice :: Int -> Int -> (lines -> lines) -> lines -> lines

instance IsString Lines where
  fromString = Lines . Text.lines . Text.pack

instance IsLines Lines where
  empty :: Lines
  empty = Lines []

  line :: Text -> Lines
  line = Lines . pure

  width :: Lines -> Int
  width (Lines ls) = foldl' max 0 (fmap Text.length ls)

  height :: Lines -> Int
  height (Lines ls) = length ls

  mapLines :: (Text -> Text) -> Lines -> Lines
  mapLines f (Lines ls) = Lines (fmap f ls)

  substLines :: (Text -> Lines) -> Lines -> Lines
  substLines f (Lines ls) = Lines (ls >>= \x -> let Lines ls' = f x in ls')

  hcat :: Lines -> Lines -> Lines
  hcat a b = Lines $ go a.value b.value
   where
    aWidth = width a
    pad x = x <> Text.replicate (max 0 (aWidth - Text.length x)) " "

    go [] [] = []
    go [] (y : ys) = let defaultLine = pad "" in (defaultLine <>) <$> (y : ys)
    go (x : xs) [] = pad x : xs
    go (x : xs) (y : ys) = (pad x <> y) : go xs ys

  vcat :: Lines -> Lines -> Lines
  vcat (Lines a) (Lines b) = Lines (a <> b)

  mapSlice :: Int -> Int -> (Lines -> Lines) -> Lines -> Lines
  mapSlice offset len f a = Lines $ go offset a.value
   where
    go 0 ls =
      let (prefix, suffix) = splitAt len ls
       in if length prefix < len
            then error $ "offset " <> show offset <> " + length " <> show len <> " was greater than " <> show (height a)
            else (f $ Lines prefix).value <> suffix
    go _ [] = error $ "offset " <> show offset <> " was greater than " <> show (height a)
    go n (l : ls) = l : go (n - 1) ls

append :: IsLines lines => lines -> Text -> lines
append ls value = mapSlice (height ls - 1) 1 (`hcat` line value) ls

indent :: IsLines lines => Int -> lines -> lines
indent n lines =
  vertically (stimesMonoid (height lines) (line (Text.replicate n " ")))
    `hcat` lines

-- | `pad (\padding line -> ...) desiredWidth paddingChar lines`
hpad :: IsLines lines => (forall m. Monoid m => m -> m -> m) -> Int -> Char -> lines -> lines
hpad placement width_ padding =
  mapLines (\line_ -> placement (Text.replicate (max 0 (width_ - Text.length line_)) (Text.singleton padding)) line_)

{-# ANN rightPad ("HLint: ignore Avoid lambda" :: String) #-}
rightPad :: IsLines lines => Int -> Char -> lines -> lines
rightPad = hpad (\padding line_ -> line_ <> padding)

{-# ANN leftPad ("HLint: ignore Avoid lambda" :: String) #-}
leftPad :: IsLines lines => Int -> Char -> lines -> lines
leftPad = hpad (\padding line_ -> padding <> line_)

rightSquare :: IsLines lines => lines -> lines
rightSquare l = rightPad (width l) ' ' l

leftSquare :: IsLines lines => lines -> lines
leftSquare l = leftPad (width l) ' ' l

downPad :: IsLines lines => Int -> Text -> lines -> lines
downPad height_ padding lines =
  lines
    `vcat` vertically (stimesMonoid (max 0 (height_ - height lines)) (line padding))

newtype Horizontally lines = Horizontally lines
  deriving (IsLines, IsString)

horizontally :: Horizontally lines -> lines
horizontally (Horizontally ls) = ls

-- | `hfold = horizontally . foldMap Horizontally`
hfold :: (Foldable f, IsLines lines) => f lines -> lines
hfold = horizontally . foldMap Horizontally

instance IsLines lines => Semigroup (Horizontally lines) where
  Horizontally a <> Horizontally b = Horizontally (hcat a b)

instance IsLines lines => Monoid (Horizontally lines) where
  mempty = Horizontally empty

newtype Vertically lines = Vertically lines
  deriving (IsLines, IsString)

vertically :: Vertically lines -> lines
vertically (Vertically ls) = ls

-- | `vfold = vorizontally . foldMap Vorizontally`
vfold :: (Foldable f, IsLines lines) => f lines -> lines
vfold = vertically . foldMap Vertically

instance IsLines lines => Semigroup (Vertically lines) where
  Vertically a <> Vertically b = Vertically (vcat a b)

instance IsLines lines => Monoid (Vertically lines) where
  mempty = Vertically empty

sepBy :: (Foldable f, Monoid m) => f m -> m -> m
sepBy ms sep =
  case toList ms of
    [] ->
      mempty
    m : rest ->
      m <> foldMap (sep <>) rest