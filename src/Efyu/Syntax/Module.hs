module Efyu.Syntax.Module where

import Data.Foldable (foldr')
import Efyu.Syntax.Expression ()
import Efyu.Syntax.TypeAnnotations
import Efyu.Syntax.Utils
import Efyu.Types
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

instance Parsable [Block] where
  parser sp = nonIndented $ (nonIndented p `sepBy` sp) <* sp
    where
      p = (dataDeclrP <* sp) <|> (typeAliasP <* sp) <|> try (defineFnP <* sp) <?> "<declaration>"

      defineFnP = Def <$> parser scnl

      dataDeclrP :: MParser Block
      dataDeclrP = withLineFold $ \sp' -> do
        L.symbol sp' "data"
        name <- typeIdentifier <* sp'
        args <- many (polyTypeIdentifier <* sp')
        ty <- L.symbol sp' "=" >> tSumTypeP sp'
        let tyScoped = foldr' TScope ty args
        pure $ TypeDef name tyScoped

      typeAliasP :: MParser Block
      typeAliasP = withLineFold $ \sp' -> do
        L.symbol sp' "type"
        name <- typeIdentifier <* sp'
        args <- many (polyTypeIdentifier <* sp')
        ty <- L.symbol sp' "=" >> parser sp'
        let tyScoped = foldr' TScope ty args
        pure $ TypeDef name tyScoped

moduleP :: String -> MParser Module
moduleP name = Module name <$> parser scnl
