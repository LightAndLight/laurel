{-# LANGUAGE TupleSections #-}

module Dblang.Parse (
  definition,
  expr,
  command,
) where

import Bound (Var (..), toScope)
import Control.Applicative (many, optional, (<|>))
import Data.Foldable (fold)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Vector as Vector
import Dblang.Syntax (Command (..), Constraint (..), Definition (..), Expr (..), TableItem (..))
import Dblang.Type (Type)
import qualified Dblang.Type as Type
import Streaming.Chars (Chars)
import Text.Parser.Char (CharParsing, alphaNum, lower, upper)
import Text.Parser.Token (braces, brackets, comma, commaSep, integer, parens, stringLiteral, symbol, symbolic)
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Highlight
import Text.Sage (Parser)

idStyle :: CharParsing m => Text.Parser.Token.IdentifierStyle m
idStyle =
  Text.Parser.Token.IdentifierStyle
    { _styleName = "identifier"
    , _styleStart = lower
    , _styleLetter = alphaNum
    , _styleReserved = ["for", "where", "in", "yield", "true", "false", "table"]
    , _styleHighlight = Text.Parser.Token.Highlight.Identifier
    , _styleReservedHighlight = Text.Parser.Token.Highlight.ReservedIdentifier
    }

constructorStyle :: CharParsing m => Text.Parser.Token.IdentifierStyle m
constructorStyle =
  Text.Parser.Token.IdentifierStyle
    { _styleName = "constructor"
    , _styleStart = upper
    , _styleLetter = alphaNum
    , _styleReserved = []
    , _styleHighlight = Text.Parser.Token.Highlight.Constructor
    , _styleReservedHighlight = Text.Parser.Token.Highlight.ReservedConstructor
    }

ident :: Chars s => Parser s Text
ident = Text.Parser.Token.ident idStyle

constructor :: Chars s => Parser s Text
constructor = Text.Parser.Token.ident constructorStyle

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
    <|> Int . fromIntegral <$> integer
    <|> Bool True <$ keyword "true"
    <|> Bool False <$ keyword "false"
    <|> String <$> stringLiteral
    <|> braces
      ( Record . Vector.fromList
          <$> commaSep
            ( (\field -> maybe (field, toVar field) (field,))
                <$> ident
                <*> optional (symbolic '=' *> expr toVar)
            )
      )
    <|> parens (expr toVar)

definition :: Chars s => Parser s Definition
definition =
  Table <$ keyword "table" <*> ident <*> braces (Vector.fromList <$> commaSep tableItem)

tableItem :: Chars s => Parser s TableItem
tableItem =
  Type <$ keyword "type" <*> constructor <* symbolic '=' <*> type_
    <|> (\name ty mConstraints -> Field name ty (fold mConstraints))
      <$> ident
      <* symbolic ':'
      <*> type_
      <*> optional (brackets $ Vector.fromList <$> commaSep constraint)
    <|> Constraint <$> constraint

typeAtom :: Chars s => Parser s Type
typeAtom =
  Type.Name <$> constructor
    <|> Type.Name <$> ident
    <|> Type.App (Type.Name "Record") <$> braces recordItems
 where
  recordItems :: Chars s => Parser s Type
  recordItems =
    (\name ty -> Type.RCons name ty . Maybe.fromMaybe Type.RNil)
      <$> ident
      <* symbolic ':'
      <*> type_
      <*> optional (comma *> recordItems)
      <|> pure Type.RNil

type_ :: Chars s => Parser s Type
type_ =
  foldl Type.App <$> typeAtom <*> many typeAtom

constraint :: Chars s => Parser s Constraint
constraint =
  (\name -> MkConstraint name . Maybe.fromMaybe [])
    <$> constructor
    <*> optional (parens (Vector.fromList <$> commaSep (expr Name)))

command :: Chars s => Parser s Command
command =
  Eval <$ symbol ":eval" <*> expr Name
    <|> Insert <$ symbol ":insert" <*> ident <*> exprAtom Name