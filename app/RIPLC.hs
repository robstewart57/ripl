{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.FilePath


import qualified PrintCAL as C

import Compiler (compile)
import Types
import XDFNetwork
import Utils
import SkeletonTemplates.YUVToStream

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import System.IO (stdout, stderr, hSetBuffering, BufferMode(..))
import System.Console.CmdArgs
import System.Directory
import System.Environment
import PrintRIPL
import AbsRIPL
import RiplFrontend

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  opts <- getOpts
  prog <- readFile (riplProgram opts)
  let defaultFifo = 4194304 -- 2 ^ 22
  let defaultNumFrames = 1
  let globalFifoDepth =
        if fifo opts == 0
          then defaultFifo
          else fifo opts
  let numFrames =
        if frames opts == 0
          then defaultNumFrames
          else frames opts
  fullProgram <- parseProgram opts prog
  -- fullProgram <-
  --   do case include opts of
  --        Nothing -> return prog
  --        Just includeCLI -> do
  --          let includedFilenamesCLI = semiColonDelimited includeCLI
  --          inlineIncludes pProgram prog includedFilenamesCLI
  let (calProject, outImageDim, outImageBitWidth, unusedActors,(inputColour,outputColour)) =
        frontend fullProgram numFrames
  let action
        | cal opts = do
          let calProjectDir =
                if (isJust (out_directory opts))
                  then fromJust (out_directory opts)
                  else "cal-out/Test/"
          createDirectoryIfMissing True calProjectDir
          createDirectoryIfMissing True (calProjectDir ++ "src/cal")
          createDirectoryIfMissing True (calProjectDir ++ "src/std/headers")
          createDirectoryIfMissing True (calProjectDir ++ "src/std/stdio")
          createDirectoryIfMissing True (calProjectDir ++ "src/xdf")
          createDirectoryIfMissing True (calProjectDir ++ "bin")
          -- print actors to .cal files
          let actorsInNetwork = actors calProject
          forM_
            actorsInNetwork
            (\actor ->
               case actor of
                 RiplActor {} ->
                   writeFile
                     (calProjectDir ++
                      (pkgToDir (package actor)) ++ (actorName actor) ++ ".cal")
                     (C.printTree (actorAST actor))
                 RiplUnit {} ->
                   writeFile
                     (calProjectDir ++
                      (pkgToDir (package actor)) ++ (unitName actor) ++ ".cal")
                     (C.printTree (unitAST actor))
                 IncludeActor {} -> return () {- already written in include/ -}
             )
          -- print program and IO network to .xdf file
          writeFile
            (calProjectDir ++ "src/xdf/ProgNetwork.xdf")
            (xmlFromProgramConnections
               calProject
               unusedActors
               globalFifoDepth
               outImageBitWidth
               inputColour outputColour
            )
          writeFile
            (calProjectDir ++ "src/xdf/TopNetwork.xdf")
            (xmlFromTopLevelIOConnections inputColour outputColour)
          riplDir <- fromMaybe "" <$> lookupEnv "RIPL_PATH"
          copyFile
            (riplDir ++ "eclipse-files/.classpath")
            (calProjectDir ++ ".classpath")
          let projName =
                let (_, fname) = splitFileName (riplProgram opts)
                    s = (takeWhile (/= '.') fname)
                in map
                     (\c ->
                        if c == '-'
                          then '_'
                          else c)
                     s
          writeFile (calProjectDir ++ ".project") (mkEclipsePrjFile projName)
          -- include boiler plate I/O actors
          copyFile
            (riplDir ++ "include/std/stdio/Source.cal")
            (calProjectDir ++ "src/std/stdio/Source.cal")
          copyFile
            (riplDir ++ "include/std/stdio/FileReader.cal")
            (calProjectDir ++ "src/std/stdio/FileReader.cal")
          case inputColour of
            Chan1 ->
              copyFile
              (riplDir ++ "include/std/stdio/StreamToGrey.cal")
              (calProjectDir ++ "src/std/stdio/StreamToGrey.cal")
            Chan3 -> do
              copyFile
                (riplDir ++ "include/std/stdio/StreamToYUV3Ports.cal")
                (calProjectDir ++ "src/std/stdio/StreamToYUV3Ports.cal")
              copyFile
                (riplDir ++ "include/std/stdio/YUVToRGB.cal")
                (calProjectDir ++ "src/std/stdio/YUVToRGB.cal")
          -- the following actor will be useful when output of an application network is a YUV stream.
          copyFile
            (riplDir ++ "include/std/stdio/Writer.cal")
            (calProjectDir ++ "src/std/stdio/Writer.cal")
          copyFile
            (riplDir ++ "include/std/stdio/EndOfStream.cal")
            (calProjectDir ++ "src/std/stdio/EndOfStream.cal")

          case outputColour of
            Chan3 -> do
              copyFile
                (riplDir ++ "include/std/stdio/RGBToYUV.cal")
                (calProjectDir ++ "src/std/stdio/RGBToYUV.cal")
              copyFile
                (riplDir ++ "include/std/stdio/YUVToStream.cal")
                (calProjectDir ++ "src/std/stdio/YUVToStream.cal")
            Chan1 ->
              writeFile
              (calProjectDir ++ "src/std/stdio/YToStream.cal")
              (actorCodeYUVToStream 32)


        | otherwise =
          error
            "you must choose either latex or CAL generation with -i or -c respectively"
  putStrLn $ "compiling " ++ (riplProgram opts) ++ " ..."
  action
  putStrLn ""
  putStrLn ("Out image dimension: " ++ show (outImageDim :: Dimension))
  putStrLn $ (riplProgram opts) ++ " compiled."


getOpts :: IO RiplcOptions
getOpts =
  cmdArgs $
  riplcOpts &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT) &=
  help _PROGRAM_ABOUT &=
  helpArg [explicit, name "help", name "h"] &=
  program _PROGRAM_NAME

_PROGRAM_NAME = "riplc"

_PROGRAM_INFO = _PROGRAM_NAME

_PROGRAM_ABOUT = unlines $ ["The RIPL compiler", ""] ++ logo

_COPYRIGHT = "(C) Rathlin Project 2016"

logo :: [String]
logo =
  [ "______  _____ _____  _"
  , "|  __ \\ _   _|  __ \\| |"
  , "| |__) | | | | |__) | |"
  , "|  _  /  | | |  ___/| |"
  , "| | \\ \\ _| |_| |    | |____"
  , "|_|  \\_\\_____|_|    |______|"
  ]

mkEclipsePrjFile :: String -> String
mkEclipsePrjFile projName =
  unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , " <projectDescription>"
    , " <name>" ++ projName ++ "_ripl</name>"
    , " <comment></comment>"
    , " <projects>"
    , " </projects>"
    , " <buildSpec>"
    , "   <buildCommand>"
    , "      <name>org.eclipse.xtext.ui.shared.xtextBuilder</name>"
    , "      <arguments>"
    , "      </arguments>"
    , "   </buildCommand>"
    , " </buildSpec>"
    , " <natures>"
    , "  <nature>net.sf.orcc.core.nature</nature>"
    , "    <nature>org.eclipse.xtext.ui.shared.xtextNature</nature>"
    , "    <nature>org.eclipse.jdt.core.javanature</nature>"
    , "  </natures>"
    , " </projectDescription>"
    ]
