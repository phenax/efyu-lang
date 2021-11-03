module Efyu.Syntax.Expr where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type MParser = Parsec Void String

-- Consume comments

-- Consume spaces and tabs
(sc, scnl) =
  ( L.space (void $ oneOf " \t") lineComment blockComment,
    L.space (void spaceChar) lineComment blockComment
  )
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

lexeme :: MParser a -> MParser a
lexeme = L.lexeme sc

symbol = L.symbol sc

integer :: MParser Integer
integer = L.signed sc (lexeme L.decimal)

float :: MParser Float
float = L.float

indentBlock = L.indentBlock scnl

lineFold = L.lineFold scnl

pComplexItem :: MParser (String, [String])
pComplexItem = indentBlock $ do
  header <- lexeme $ some (alphaNumChar <|> char '-')
  pure (L.IndentMany Nothing (pure . (header,)) pLineFold)

pLineFold :: MParser String
pLineFold = lineFold $ \indents -> do
  xs <- some (alphaNumChar <|> char '-') `sepBy1` try indents
  sc
  pure . unwords $ xs

exprParser = pComplexItem <* eof
