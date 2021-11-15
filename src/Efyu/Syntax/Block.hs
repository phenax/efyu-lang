module Efyu.Syntax.Block where

import Efyu.Syntax.Expression
import Efyu.Syntax.TypeAnnotations
import Efyu.Syntax.Utils
import Efyu.Types
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

data Block
  = Module String [Block]
  | Def Definition
  | TypeAliasDef (IdentifierName 'TypeName) Type
  deriving (Show, Eq)

defineFnP :: MParser Block
defineFnP = Def <$> definitionP

typeAliasP :: MParser Block
typeAliasP = withLineFold $ \sp -> do
  L.symbol sp "type" >> L.symbol sp "alias"
  name <- typeIdentifier <* sp <* L.symbol sp "="
  -- TODO: forall poly vars
  ty <- typeP sp
  pure $ TypeAliasDef name ty

blockDeclrP :: (MParser Block -> MParser Block) -> MParser [Block]
blockDeclrP pre = (pre p `sepBy` scnl) <* scnl
  where
    p = (typeAliasP <* scnl) <|> try (defineFnP <* scnl) <?> "<declaration>"

blockP :: String -> MParser Block
blockP name = Module name <$> blockDeclrP nonIndented
