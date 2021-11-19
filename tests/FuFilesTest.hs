module FuFilesTest where

import Control.Monad (forM_, void)
import Data.List (isSuffixOf)
import Efyu (typeCheckFile)
import System.Directory
import Test.Hspec

testDirPath = "./tests/test-files/"

getFuFiles = map (testDirPath ++) . filter (isSuffixOf ".fu") <$> getDirectoryContents testDirPath

tests = describe "test-files/*.fu" $ do
  files <- runIO getFuFiles
  forM_ files $ \file -> do
    it ("should type check " ++ file) $ do
      fmap void (typeCheckFile file) `shouldReturn` Right ()
