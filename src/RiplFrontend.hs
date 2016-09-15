{-# LANGUAGE DeriveDataTypeable #-}

module RiplFrontend where

import LexRIPL
import ParRIPL
import AbsRIPL
import ErrM
import System.Console.CmdArgs
import Control.Monad
import PrintRIPL
import System.Directory
import System.FilePath
import qualified Data.Text as T
import Types
import Compiler (compile)


parseProgram opts prog = do
  case include opts of
         Nothing -> return prog
         Just includeCLI -> do
           let includedFilenamesCLI = semiColonDelimited includeCLI
           inlineIncludes pProgram prog includedFilenamesCLI

-- Note: I'm not keen on how numFrames has leaked into this function.
--       Consider refactoring the generation of the Parameters.cal
--       file into the `main` function.
frontend :: String
         -> Int
         -> (CalProject, Dimension, Int, [String])
frontend s numFrames =
  let parsed = parseTree s
  in case parsed of
       Bad s -> error (show parsed ++ "\n" ++ show s)
       Ok tree -> compile tree (fromIntegral numFrames)

parseTree :: String -> Err Program
parseTree s =
  let ts = myLLexer s
  in pProgram ts
-- case pProgram ts of
--        Bad s -> error (show ts ++ "\n" ++ show s)
--        Ok tree -> tree

type ParseFun a = [Token] -> Err a

myLLexer :: String -> [Token]
myLLexer = myLexer

data RiplcOptions = RiplcOptions
  { cal :: Bool
  , sac :: Bool
  , fifo :: Int
  , frames :: Int
  , out_directory :: Maybe String
  , include :: Maybe String
  , riplProgram :: String
  } deriving (Data, Typeable, Show, Eq)

riplcOpts :: RiplcOptions
riplcOpts =
  RiplcOptions
  { cal = def &= help "compiles RIPL programs to CAL, a dataflow IR"
  , fifo = def &= help "specify the default FIFO depth for all dataflow wires"
  , frames =
      def &=
      help "specify the number of frames to process (-1 means infinite frames)"
  , sac = def &= help "Compiles RIPL to the array-based SAC"
  , out_directory = def &= help "Directory to save CAL actor network project"
  , include = def &= help "semi colon delimited included RIPL library files"
  , riplProgram = "" &= argPos 0 &= typ "RIPL_SOURCE"
  }

semiColonDelimited :: String -> [String]
semiColonDelimited = map T.unpack . T.splitOn (T.pack ";") . T.pack

includeFiles :: Program -> [String]
includeFiles (ProgramC includes _ _ _ _) =
  map (\(IncludeFile (Ident s)) -> s ++ ".ripl") includes

inlineIncludes :: ParseFun Program -> String -> [String] -> IO String
inlineIncludes p s includedFilesCommandLine =
  let ts = myLLexer s
  in case p ts of
       Bad s -> error (show ts ++ "\n" ++ show s)
       Ok tree -> do
         let includes = includeFiles tree
         includeSource <-
           concat <$>
           forM
             includes
             (\fname -> readCorrectFile includedFilesCommandLine fname)
         return $ includeSource ++ printTree (progNoIncludeStatements tree)

putStrV :: Int -> String -> IO ()
putStrV v s =
  if v > 1
    then putStrLn s
    else return ()

progNoIncludeStatements :: Program -> Program
progNoIncludeStatements (ProgramC _ functions imRead stmts dataOut) =
  ProgramC [] functions imRead stmts dataOut

readCorrectFile :: [String] -> String -> IO String
readCorrectFile [] fname =
  error ("unable to find file " ++ show fname ++ " from -I command")
readCorrectFile (x:xs) fname =
  if takeFileName x == fname
    then readFile x >>= \s -> return (s ++ "\n")
    else readCorrectFile xs fname

