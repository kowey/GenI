-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

-- This standalone program is a counterpart to geniextract.

-- We have a little bit of sneakiness here: the sentence keyword
-- is optional, so we exploit this fact to visually distinguish
-- between expected test cases (sentence), and results produced
-- by the generator.  If you read the resulting test suite in
-- GenI, both are interpreted as expected output.

module Main (main) where

import NLP.GenI.Btypes
import NLP.GenI.General (basename, comparing, (///), ePutStrLn, readFile', toAlphaNum)
import NLP.GenI.GeniParsers(geniSemanticInput, geniDerivations)
import NLP.GenI.GeniShow (GeniShow(geniShow))

import Data.List (sortBy, intersperse)
import qualified Data.Map as Map
import Data.Maybe(catMaybes)
import System.Directory
import System.Environment
import System.Exit(exitFailure)
import System.IO
import Text.ParserCombinators.Parsec

import System.Console.GetOpt


main :: IO ()
main =
 do (Settings { testDir = eDir, responseDir = rDir, showTraces = showT }) <- readArgv
    cases     <- readSubDirsWith readExtracted eDir
    responses <- readSubDirsWith readResponses rDir
    let responseMap = Map.fromList responses
        showCase = geniShow.getExtra
        -- delete the traces unless the showTrace flag is specified
        tweakTraces = if showT then id else map (\(t,_) -> (t,[]))
        getExtra c = case Map.lookup (tcName c) responseMap of
                        Nothing -> c
                        Just rs -> c { tcOutputs = tweakTraces rs }
        sortAlphaNum = sortBy (comparing $ toAlphaNum.tcName)
    putStrLn . unlines . intersperse comment . map showCase . sortAlphaNum $ cases
 where
  comment =
     "\n% ------------------------------------------------------------------------\n"

  readSubDirsWith r d =
    do subdirs <- getDirectoryContents d
       catMaybes `fmap` mapM (r d) subdirs

-- ---------------------------------------------------------------------
-- command line arguments
-- ---------------------------------------------------------------------

data Settings = Settings
       { testDir     :: FilePath
       , responseDir :: FilePath
       , showTraces  :: Bool
       }

emptySettings :: Settings
emptySettings = Settings { testDir = "", responseDir = "", showTraces = False }

options :: [OptDescr (Settings -> Settings)]
options =
 [ Option ['t']  ["traces"]      (NoArg $ \s -> s { showTraces = True }) "output trace information"
 , Option []     ["tests"]       (ReqArg (\f s -> s { testDir = f })     "DIR") "tests found in DIR"
 , Option []     ["responses"]   (ReqArg (\f s -> s { responseDir = f }) "DIR") "responses found in DIR"
 ]

readArgv :: IO Settings
readArgv =
  do pname <- getProgName
     argv  <- getArgs
     case getOpt Permute options argv of
      (os,_,[]  )
        | notSet testDir          -> help pname ["Must specify a tests directory"]
        | notSet responseDir      -> help pname ["Must specify a responses directory"]
        | otherwise               -> return settings
        where
         notSet x = null (x settings)
         settings = foldr ($) emptySettings os
      (_,_,errs)  -> help pname errs
  where
   help pname errs =
     ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: " ++ pname ++ " [OPTION...]"


-- ---------------------------------------------------------------------
-- reading in the input files
-- ---------------------------------------------------------------------

readExtracted :: FilePath -> FilePath -> IO (Maybe TestCase)
readExtracted parentdir subdir =
 do semanticsE <- doesFileExist semanticsF
    sentencesE <- doesFileExist sentencesF
    if semanticsE && sentencesE
       then do sentences <- lines `fmap` readFile' sentencesF
               semantics <- getParse =<< parseFromFile geniSemanticInput semanticsF
               return . Just $ TestCase
                 { tcName = basename subdir
                 , tcSemString = ""
                 , tcSem  = semantics
                 , tcExpected = sentences
                 , tcOutputs = []
                 }
       else return Nothing
 where
  semanticsF = parentdir /// subdir /// "semantics"
  sentencesF = parentdir /// subdir /// "sentences"

type Result = (String, [String])

readResponses :: FilePath -> FilePath -> IO (Maybe (String, [Result]))
readResponses parentdir subdir =
 do overgensE <- doesFileExist overgensF
    if overgensE
       then do os <- getParse =<< parseFromFile geniDerivations overgensF
               return . Just $ (subdir, os)
       else return Nothing
 where
  overgensF = parentdir /// subdir /// "derivations"

getParse :: (Show a) => Either a b -> IO b
getParse = either exitShowing return

exitShowing :: (Show a) => a -> IO b
exitShowing err=
 do let err_ = show err
    ePutStrLn err_
    exitFailure


