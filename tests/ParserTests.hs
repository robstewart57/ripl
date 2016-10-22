
module Main where

import Control.Monad
import Control.Applicative
import Test.Tasty -- (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit as TU
import System.Directory
import System.FilePath
import RiplFrontend
import ErrM

main = do

  riplSrcFiles <- filter isRegularFileOrDirectory <$> getDirectoryContents "examples"

  defaultMain
       (testGroup "ripl tests"
        [ -- RDF graph API tests
          riplParsingTests riplSrcFiles
        ])

isRegularFileOrDirectory :: FilePath -> Bool
isRegularFileOrDirectory f = f /= "." && f /= ".."

riplParsingTests :: [FilePath] -> TestTree
riplParsingTests exampleSourceFiles = do
  testGroup "parsing files in examples/ directory" tests
   where
    tests :: [TestTree]
    tests =
     map
      (\srcFile -> do
         TU.testCase ("parsing " ++ show srcFile) (do
          src <- readFile ("examples/" ++ srcFile)
          case parseTree src of
            Bad s -> assertFailure ("Unable to parse " ++ show srcFile ++ ". Reason:\n" ++ show s)
            Ok tree -> assertString ""))
      exampleSourceFiles
