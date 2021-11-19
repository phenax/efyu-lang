module Efyu where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Efyu.Errors (CompilerError (ParseError))
import Efyu.Syntax.Block (Block)
import Efyu.Syntax.Parse
import Efyu.TypeChecker.Infer

typeCheckFile :: String -> IO (Either CompilerError Block)
typeCheckFile file = do
  ast <- parseFile file
  case ast of
    Left e -> pure . Left $ ParseError e
    Right r -> do
      res <- liftIO . runTI . checkModule $ r
      pure $ case res of
        Left e -> Left e
        Right _ -> Right r
