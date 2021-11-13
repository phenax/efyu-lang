module Efyu.Syntax.Block where

import Efyu.Syntax.Expression
import Efyu.Syntax.Utils
import Efyu.Types
import Text.Megaparsec
import Text.Megaparsec.Char

-- import qualified Text.Megaparsec.Char.Lexer as L

data Block
  = Module String [Block]
  | Def Definition
  deriving (Show, Eq)

defineFnP :: MParser Block
defineFnP = Def <$> definitionP

definitionListP :: (MParser Block -> MParser Block) -> MParser [Block]
definitionListP pre = (pre p `sepBy` many newline) <* scnl
  where
    p = defineFnP

-- moduleP :: MParser Block
-- moduleP = withLineFold $ \sp -> do -- TODO: Use indent block
--   name <- symbol "module" >> lexeme identifier <* symbol "where"
--   Module name <$> definitionListP sp

blockP :: String -> MParser Block
blockP name = Module name <$> definitionListP nonIndented
