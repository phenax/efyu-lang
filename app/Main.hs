module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Efyu.Syntax.Parse (parseFile)
import Efyu.Types.Infer (checkModule, runTI)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Pretty.Simple (pPrint)

main = do
  ast <- parseFile "./examples/scratchpad.fu"
  case ast of
    Left e -> putStrLn . errorBundlePretty $ e
    Right r -> do
      res <- liftIO . runTI . checkModule $ r
      case res of
        Left e -> putStrLn e
        Right _ -> pPrint r
