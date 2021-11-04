module Efyu.Syntax.Block where

import Data.List (foldl')
import Efyu.Syntax.Expression
import Efyu.Syntax.Syntax
import Efyu.Syntax.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Block
  = Module String [Block]
  | Def Identifier Expression
  deriving (Show, Eq)

-- TODO: Write tests
-- TODO: Re-use defineFnP in let bindings
defineFnP :: MParser Block
defineFnP = withLineFold $ \sp -> do
  name <- identifier
  params <- many (L.lexeme sp parameter)
  sp
  char '='
  body <- L.lexeme sp expressionP
  optional (char ';')
  let func = foldl' (flip Lambda) body params
  pure $ Def name func

definitionListP :: (MParser Block -> MParser Block) -> MParser [Block]
definitionListP pre = defP
  where
    defP = pre p `sepBy` many newline
    p = defineFnP

-- moduleP :: MParser Block
-- moduleP = withLineFold $ \sp -> do -- TODO: Use indent block
--   name <- symbol "module" >> lexeme identifier <* symbol "where"
--   Module name <$> definitionListP sp

blockP :: String -> MParser Block
blockP name = Module name <$> definitionListP nonIndented
