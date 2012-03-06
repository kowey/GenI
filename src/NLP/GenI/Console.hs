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

-- | The console user interface including batch processing on entire
--   test suites.

{-# LANGUAGE DeriveDataTypeable #-}
module NLP.GenI.Console(consoleGeni) where

import Control.Applicative ( pure, (<$>) )
import Control.Monad
import Data.IORef(readIORef, modifyIORef)
import Data.List ( partition )
import Data.Maybe ( fromMaybe, isJust )
import Data.Time ( getCurrentTime, formatTime )
import Data.Typeable
import System.Log.Logger
import System.Locale ( defaultTimeLocale, iso8601DateFormat )
import System.Directory( createDirectoryIfMissing, getTemporaryDirectory )
import System.Exit ( exitWith, exitFailure, ExitCode(..) )
import System.FilePath ( (</>), takeFileName )
import System.Timeout ( timeout )

import NLP.GenI.General
  ( ePutStr, ePutStrLn,
  )
import NLP.GenI
import NLP.GenI.GeniShow
import NLP.GenI.Configuration
  ( Params
  , BatchDirFlg(..), DumpDerivationFlg(..), EarlyDeathFlg(..)
  , MetricsFlg(..), RankingConstraintsFlg(..)
  , TestCaseFlg(..), TestSuiteFlg(..), TestInstructionsFlg(..)
  , TimeoutFlg(..),  VerboseModeFlg(..)
  , hasFlagP, getListFlagP, getFlagP, setFlagP
  , builderType , BuilderType(..)
  )
import NLP.GenI.General ( mkLogname )
import NLP.GenI.Semantics ( SemInput )
import NLP.GenI.Simple.SimpleBuilder
import NLP.GenI.Statistics ( Statistics )
import NLP.GenI.TestSuite ( TestCase(..) )
import NLP.GenI.Warnings

import Text.JSON
import Text.JSON.Pretty ( render, pp_value )

consoleGeni :: ProgStateRef -> IO()
consoleGeni pstRef = do
  pst <- readIORef pstRef
  loadEverything pstRef
  let job = runInstructions pstRef
  case getFlagP TimeoutFlg (pa pst) of
    Nothing -> job
    Just t  -> do
     status <- timeout (fromIntegral t * 1000000) job
     case status of
        Just () -> return ()
        Nothing -> do ePutStrLn $ "GenI timed out after " ++ show t ++ "s"
                      exitWith (ExitFailure 2)

-- | Runs the tests specified in our instructions list.
--   We assume that the grammar and lexicon are already
--   loaded into the monadic state.
--   If batch processing is enabled, save the results to the batch output
--   directory with one subdirectory per suite and per case within that suite.
runInstructions :: ProgStateRef -> IO ()
runInstructions pstRef =
  do pst <- readIORef pstRef
     let config = pa pst
     batchDir <- case getFlagP BatchDirFlg config of
                   Nothing  -> do
                     t   <- getTemporaryDirectory
                     utc <- fmtTime <$> getCurrentTime
                     return (t </> "geni-" ++ utc)
                   Just bdir -> return bdir
     runBatch batchDir
     unless (hasFlagP BatchDirFlg config) $ do
       ePutStr $ unlines [ ""
                         , "Results saved to directory " ++ batchDir
                         , "To save results in a different directory, use the --batchdir flag"
                         ]
  where
  fmtTime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H%M"))
  runBatch bdir =
    do config <- pa `fmap` readIORef pstRef
       mapM_ (runSuite bdir) $ getListFlagP TestInstructionsFlg config
  runSuite bdir next@(file, _) =
    do suite  <- loadNextSuite pstRef next
       -- we assume the that the suites have unique filenames
       let bsubdir = bdir </> takeFileName file
       createDirectoryIfMissing True bsubdir
       if any (null . tcName) suite
          then    fail $ "Can't do batch processing. The test suite " ++ file ++ " has cases with no name."
          else do ePutStrLn "Batch processing mode"
                  debugM logname (show $ length suite)
                  mapM_ (runCase bsubdir) suite
  runCase bdir (TestCase { tcName = n, tcSem = s }) =
   do config <- pa `fmap` readIORef pstRef
      let verbose = hasFlagP VerboseModeFlg config
          earlyDeath = hasFlagP EarlyDeathFlg config
      when verbose $
        ePutStrLn "======================================================"
      (res , _) <- runOnSemInput pstRef (PartOfSuite n bdir) s
      let (goodres, badres) = partition isSuccess res
          badresSuf = if null badres then "" else " (" ++ show (length badres) ++ " errors)"
      ePutStrLn $ " " ++ n ++ " - " ++ show (length goodres) ++ " results" ++ badresSuf
      when (null res && earlyDeath) $ do
        ePutStrLn $ "Exiting early because test case " ++ n ++ " failed."
        exitFailure

-- | Used in processing instructions files. Each instruction consists of a
--   suite file and a list of test case names from that file
--
--   See <http://projects.haskell.org/GenI/manual/command-line.html> for
--   how testsuite, testcase, and instructions are expected to interact
loadNextSuite :: ProgStateRef -> (FilePath, Maybe [String]) -> IO [TestCase]
loadNextSuite pstRef (file, mtcs) = do
    debugM logname $ "Loading next test suite: " ++ file
    debugM logname $ "Test case filter: " ++ maybe "none" (\xs -> show (length xs) ++ " items") mtcs
    modifyIORef pstRef $ \p -> p { pa = setFlagP TestSuiteFlg file (pa p) } -- yucky statefulness! :-(
    config <- pa `fmap` readIORef pstRef
    let mspecific = getFlagP TestCaseFlg config
    debugM logname $ "Test case to pick out: " ++ fromMaybe "none"  mspecific
    fullsuite <- loadTestSuite pstRef
    return (filterSuite mtcs mspecific fullsuite)
  where
    filterSuite _         (Just c) suite = filter (\t -> tcName t == c) suite
    filterSuite Nothing   Nothing  suite = suite
    filterSuite (Just cs) Nothing  suite = filter (\t -> tcName t `elem` cs) suite

data RunAs = Standalone  FilePath FilePath
           | PartOfSuite String FilePath

-- | Runs a case in the test suite.  If the user does not specify any test
--   cases, we run the first one.  If the user specifies a non-existing
--   test case we raise an error.
runOnSemInput :: ProgStateRef
              -> RunAs
              -> SemInput
              -> IO ([GeniResult], Statistics)
runOnSemInput pstRef args semInput =
  do modifyIORef pstRef (resetLocal semInput)
     pst <- readIORef pstRef
     let config = pa pst
     (results, stats) <- case builderType config of
                            SimpleBuilder -> helper simpleBuilder_2p
                            SimpleOnePhaseBuilder -> helper simpleBuilder_1p
     warningsOut <- (warnings . local) `fmap` readIORef pstRef
     writeResults pst args semInput results stats warningsOut
     return (results, stats)
  where
    helper builder =
      do (results, stats, _) <- runGeni pstRef builder
         return (results, stats)

writeResults :: ProgState -> RunAs -> SemInput -> [GeniResult] -> Statistics -> GeniWarnings -> IO ()
writeResults pst args semInput results stats warningsOut = do
     -- create output directory as needed
     case args of
       PartOfSuite n f -> createDirectoryIfMissing True (f </> n)
       _               -> return ()
     -- print responses
     if dump
        then writeResponses $ ppJSON results
        else writeResponses $ unlines . concatMap (fromResult formatResponses) $ results
     -- print out statistical data (if available)
     when (isJust $ getFlagP MetricsFlg config) $
        writeStats (ppJSON stats)
     -- print any warnings we picked up along the way
     unless (null (fromGeniWarnings warningsOut)) $ do
        ePutStr $ "Warnings:\n" ++ formatWarnings warningsOut
        writeBatchFile "warnings" $ unlines . map show . reverse
                                  $ fromGeniWarnings warningsOut
     -- other outputs when run in batch mode
     writeBatchFile "semantics"  $ geniShowSemInput semInput ""
     writeBatchFile "derivations"$ ppJSON results
  where
    config      = pa pst
    dump        = hasFlagP DumpDerivationFlg config
    -- do we print ranking information and all that other jazz?
    formatResponses = if hasFlagP RankingConstraintsFlg config
                         then pure . prettyResult pst
                         else grRealisations
    formatWarnings = unlines . map (" - " ++)
                   . concatMap showGeniWarning
                   . reverse . fromGeniWarnings . sortWarnings
    --
    writeBatchFile key = case args of
        Standalone _  _ -> const (return ())
        PartOfSuite n f -> writeFile (f </> n </> key)
    writeResponses = case args of
        Standalone "" _ -> putStrLn
        Standalone f  _ -> writeFile f
        PartOfSuite _ _ -> writeBatchFile "responses"
    writeStats = case args of
        Standalone _ "" -> putStrLn
        Standalone _ f  -> writeFile f
        PartOfSuite _ _ -> writeBatchFile "stats"
    --
    fromResult :: (GeniSuccess -> [String]) -> GeniResult -> [String]
    fromResult _ (GError errs) = [ show errs ]
    fromResult f (GSuccess x)  = f x

ppJSON :: JSON a => a -> String
ppJSON = render . pp_value . showJSON

-- ----------------------------------------------------------------------
-- Odds and ends
-- ----------------------------------------------------------------------

data MNAME = MNAME deriving Typeable
logname :: String
logname = mkLogname MNAME
