module Efyu.Syntax.Block where

import Data.Foldable (foldr')
import Efyu.Syntax.Expression
import Efyu.Syntax.TypeAnnotations
import Efyu.Syntax.Utils
import Efyu.Types
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

data Block
  = Module String [Block]
  | Def Definition
  | TypeDef (IdentifierName 'TypeName) Type
  deriving (Show, Eq)

defineFnP :: MParser Block
defineFnP = Def <$> definitionP

typeAliasP :: MParser Block
typeAliasP = withLineFold $ \sp -> do
  L.symbol sp "type"
  name <- typeIdentifier <* sp
  args <- many (polyTypeIdentifier <* sp)

  ty <- L.symbol sp "=" >> typeP sp

  let tyScoped = foldr' TScope ty args
  pure $ TypeDef name tyScoped

blockDeclrP :: (MParser Block -> MParser Block) -> MParser [Block]
blockDeclrP pre = (pre p `sepBy` scnl) <* scnl
  where
    p = (typeAliasP <* scnl) <|> try (defineFnP <* scnl) <?> "<declaration>"

blockP :: String -> MParser Block
blockP name = Module name <$> blockDeclrP nonIndented
