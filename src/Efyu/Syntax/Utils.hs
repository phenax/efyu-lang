module Efyu.Syntax.Utils where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- withOptionalParens :: EfyuParser u a -> EfyuParser u a
-- withOptionalParens comb =
--   between (char '(') (char ')') comb <|> comb

type MParser = Parsec Void String

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
integer = int <|> L.signed sc int
  where
    int = read <$> some digitChar

float :: MParser Float
float = fl <|> L.signed sc fl
  where
    fl = do
      n <- digitChar `someTill` char '.'
      dec <- fromMaybe "0" <$> optional (some digitChar)
      pure . read $ n ++ "." ++ dec

indentBlock = L.indentBlock scnl

insideQuotes :: MParser String
insideQuotes = char '"' >> manyTill L.charLiteral (char '"')

lineFold = L.lineFold scnl
