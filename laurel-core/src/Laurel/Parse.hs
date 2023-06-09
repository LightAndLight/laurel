{-# LANGUAGE TupleSections #-}

module Laurel.Parse (
  definition,
  expr,
  exprAtom,
  command,
) where

import Bound (Var (..), toScope)
import Control.Applicative (many, optional, some, (<|>))
import Data.Foldable (fold)
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Laurel.Syntax (Command (..), Constraint (..), Definition (..), Expr, TableItem (..))
import qualified Laurel.Syntax as Expr (Expr (..))
import Laurel.Type (Type)
import qualified Laurel.Type as Type
import Streaming.Chars (Chars)
import Text.Parser.Char (CharParsing, alphaNum, char, lower, notChar, upper)
import Text.Parser.Combinators (between, (<?>))
import Text.Parser.Token (braces, brackets, comma, commaSep, integer, parens, stringLiteral, symbol, symbolic)
import qualified Text.Parser.Token
import qualified Text.Parser.Token.Highlight
import Text.Sage (Parser)

idStyle :: CharParsing m => Text.Parser.Token.IdentifierStyle m
idStyle =
  Text.Parser.Token.IdentifierStyle
    { _styleName = "identifier"
    , _styleStart = lower <|> char '_'
    , _styleLetter = alphaNum <|> char '_'
    , _styleReserved = ["for", "where", "in", "yield", "group", "by", "true", "false", "table"]
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
ident =
  Text.Parser.Token.ident idStyle
    <|> between
      (symbolic '`')
      (symbolic '`')
      ( fmap Text.pack . some $
          notChar '`' <|> char '\\' *> (char '\\' <|> char '`')
      )

constructor :: Chars s => Parser s Text
constructor = Text.Parser.Token.ident constructorStyle

keyword :: Chars s => Text -> Parser s ()
keyword = Text.Parser.Token.reserveText idStyle

expr :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
expr toVar =
  exprLam toVar
    <|> exprGroupBy toVar

exprLam :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprLam toVar = do
  _ <- symbolic '\\'
  name <- ident
  _ <- symbol "->"
  body <- expr (\name' -> if name' == name then Expr.Var (B ()) else fmap F (toVar name'))
  pure $ Expr.Lam name (toScope body)

data Operator = Equals
  deriving (Eq, Show)

operator :: Chars s => Parser s Operator
operator =
  Equals <$ symbol "=="

exprGroupBy :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprGroupBy toVar =
  foldl
    Expr.GroupBy
    <$> exprQuery toVar
    <*> many (((keyword "group" *> keyword "by") <?> "group by") *> exprOperator toVar)

exprQuery :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprQuery toVar =
  exprWhere toVar
    <|> exprYield toVar
    <|> exprFor toVar
    <|> exprOperator toVar

exprWhere :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprWhere toVar =
  Expr.Where <$ keyword "where" <*> expr toVar <*> exprQuery toVar

exprYield :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprYield toVar =
  Expr.Yield <$ keyword "yield" <*> exprDot toVar

exprFor :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprFor toVar = do
  keyword "for"
  name <- ident
  keyword "in"
  collection <- expr toVar
  body <- exprQuery (\name' -> if name' == name then Expr.Var (B ()) else fmap F (toVar name'))
  pure $ Expr.For name collection (toScope body)

exprOperator :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprOperator toVar =
  foldl
    ( \l' (op, r) ->
        case op of
          Equals -> Expr.Equals l' r
    )
    <$> exprApp toVar
    <*> many ((,) <$> operator <*> exprApp toVar)

exprApp :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprApp toVar = foldl Expr.App <$> exprDot toVar <*> many (exprDot toVar)

exprDot :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprDot toVar =
  foldl (&)
    <$> exprAtom toVar
    <*> many
      ( symbolic '.'
          *> ( flip Expr.Dot <$> ident
                <|> flip Expr.Splat <$> braces (Vector.fromList <$> commaSep ident)
             )
      )

exprAtom :: Chars s => (Text -> Expr a) -> Parser s (Expr a)
exprAtom toVar =
  toVar <$> ident
    <|> Expr.Int . fromIntegral <$> integer
    <|> Expr.Bool True <$ keyword "true"
    <|> Expr.Bool False <$ keyword "false"
    <|> Expr.String <$> stringLiteral
    <|> braces
      ( Expr.Record . Vector.fromList
          <$> commaSep
            ( (\field -> maybe (field, toVar field) (field,))
                <$> ident
                <*> optional (symbolic '=' *> expr toVar)
            )
      )
    <|> brackets (Expr.List . Vector.fromList <$> commaSep (expr toVar))
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
    <*> optional (parens (Vector.fromList <$> commaSep (expr Expr.Name)))

command :: Chars s => Parser s Command
command =
  Eval <$ symbol ":eval" <*> expr Expr.Name
    <|> Insert <$ symbol ":insert" <*> ident <*> exprAtom Expr.Name