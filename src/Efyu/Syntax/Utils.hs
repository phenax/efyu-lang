module Efyu.Syntax.Utils where

import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Efyu.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

class Parsable a where
  parser :: MParser () -> MParser a

withOptionalParens :: MParser a -> MParser a
withOptionalParens comb =
  withParens (lexeme comb) <|> comb

withParens :: MParser a -> MParser a
withParens = between (char '(') (char ')')

type MParser = Parsec Void String

-- Consume spaces and tabs
(sc, scnl) =
  ( L.space (void $ oneOf " \t") lineComment blockComment,
    L.space (void spaceChar) lineComment blockComment
  )
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockCommentNested "{-" "-}"

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol :: String -> MParser String
symbol = L.symbol sc

integer :: MParser Integer
integer = int <|> L.signed sc int
  where
    int = read <$> some digitChar

float :: MParser Double
float = fl <|> L.signed sc fl
  where
    fl = do
      n <- digitChar `someTill` char '.'
      dec <- fromMaybe "0" <$> optional (some digitChar)
      pure . read $ n ++ "." ++ dec

reservedKeywords :: [String]
reservedKeywords = ["let", "in", "if", "then", "else", "type", "alias", "case", "of"]

varIdentifier :: MParser (IdentifierName 'VarName)
varIdentifier = IdentifierName <$> lowerIdentifier

polyTypeIdentifier :: MParser (IdentifierName 'PolyTypeName)
polyTypeIdentifier = IdentifierName <$> lowerIdentifier

typeIdentifier :: MParser (IdentifierName 'TypeName)
typeIdentifier = IdentifierName <$> upperIdentifier

constructorIdentifier :: MParser (IdentifierName 'ConstructorName)
constructorIdentifier = IdentifierName <$> upperIdentifier

lowerIdentifier :: MParser String
lowerIdentifier = do
  f <- lowerChar
  rest <- many (alphaNumChar <|> oneOf "_'?")
  let name = f : rest
  if name `elem` reservedKeywords
    then unexpected $ Label (f :| rest)
    else pure name

upperIdentifier :: MParser String
upperIdentifier = do
  f <- upperChar
  rest <- many (alphaNumChar <|> oneOf "_'?")
  let name = f : rest
  if name `elem` reservedKeywords
    then unexpected $ Label (f :| rest)
    else pure name

insideQuotes :: MParser String
insideQuotes = char '"' >> manyTill L.charLiteral (char '"')

withLineFold :: (MParser () -> MParser u) -> MParser u
withLineFold = L.lineFold scnl

nonIndented :: MParser a -> MParser a
nonIndented = L.nonIndented scnl

indentLevel :: MParser (Pos, MParser ())
indentLevel = (,scnl) <$> L.indentLevel

indentGt :: (Pos, MParser ()) -> MParser ()
indentGt (pos, sp) =
  L.indentGuard scnl GT pos >> sp

indentGtEq :: (Pos, MParser ()) -> MParser ()
indentGtEq p@(pos, sp) =
  try (indentGt p) <|> (L.indentGuard scnl EQ pos >> sp)

argListP :: MParser e -> MParser () -> MParser [e]
argListP argP sp = argListParser []
  where
    argListParser ls = do
      optn <- optional . try $ sp >> argP
      sc
      case optn of
        Nothing -> pure ls
        Just p -> argListParser $ ls ++ [p]

---
