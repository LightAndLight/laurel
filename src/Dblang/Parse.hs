{-# LANGUAGE TupleSections #-}

module Dblang.Parse (expr) where

import Bound (Var (..), toScope)
import Control.Applicative (many, optional, (<|>))
import Data.Text (Text)
import qualified Data.Vector as Vector
import Dblang.Syntax (Expr (..))
import Streaming.Chars (Chars)
import Text.Parser.Char (CharParsing, letter, lower)
import Text.Parser.Token (braces, commaSep, parens, symbol, symbolic)
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Highlight
import Text.Sage (Parser)

idStyle :: CharParsing m => Text.Parser.Token.IdentifierStyle m
idStyle =
  Text.Parser.Token.IdentifierStyle
    { _styleName = "identifier"
    , _styleStart = lower
    , _styleLetter = letter
    , _styleReserved = ["for", "where", "in", "yield"]
    , _styleHighlight = Text.Parser.Token.Highlight.Identifier
    , _styleReservedHighlight = Text.Parser.Token.Highlight.ReservedIdentifier
    }

ident :: Chars s => Parser s Text
ident = Text.Parser.Token.ident idStyle

keyword :: Chars s => Text -> Parser s ()
keyword = Text.Parser.Token.reserveText idStyle

expr :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
expr toVar =
  ( do
      _ <- symbolic '\\'
      name <- ident
      _ <- symbol "->"
      body <- expr (\name' -> if name' == name then Var (B ()) else fmap F (toVar name'))
      pure $ Lam name (toScope body)
  )
    <|> ( do
            keyword "for"
            name <- ident
            keyword "in"
            collection <- expr toVar
            body <- expr (\name' -> if name' == name then Var (B ()) else fmap F (toVar name'))
            pure $ For name collection (toScope body)
        )
    <|> Where <$ keyword "where" <*> expr toVar <*> expr toVar
    <|> Yield <$ keyword "yield" <*> expr toVar
    <|> exprEquals toVar

exprEquals :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprEquals toVar = (\l -> maybe l (Equals l)) <$> exprApp toVar <*> optional (symbol "==" *> exprApp toVar)

exprApp :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprApp toVar = foldl App <$> exprDot toVar <*> many (exprDot toVar)

exprDot :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprDot toVar = foldl Dot <$> exprAtom toVar <*> many (symbolic '.' *> ident)

exprAtom :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprAtom toVar =
  toVar <$> ident
    <|> braces
      ( Record . Vector.fromList
          <$> commaSep
            ( (\field -> maybe (field, toVar field) (field,))
                <$> ident
                <*> optional (symbolic '=' *> expr toVar)
            )
      )
    <|> parens (expr toVar)