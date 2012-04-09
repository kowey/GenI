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
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.Console(consoleGeni) where

import Control.Applicative ( pure, (<$>) )
import Control.Monad
import Data.IORef(readIORef, modifyIORef)
import Data.List ( find, partition )
import Data.Maybe ( fromMaybe, isJust )
import Data.Text ( Text )
import Data.Time ( getCurrentTime, formatTime )
import Data.Typeable
import System.Log.Logger
import System.Locale ( defaultTimeLocale, iso8601DateFormat )
import System.Directory( createDirectoryIfMissing, getTemporaryDirectory )
import System.Exit ( exitWith, exitFailure, ExitCode(..) )
import System.FilePath ( (</>), takeFileName )
import System.IO ( stderr )
import System.Timeout ( timeout )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import NLP.GenI.General
  ( ePutStr, ePutStrLn,
  )
import NLP.GenI
import NLP.GenI.Configuration
  ( Params
  , BatchDirFlg(..), DumpDerivationFlg(..), EarlyDeathFlg(..)
  , MetricsFlg(..), RankingConstraintsFlg(..)
  , TestCaseFlg(..), TestSuiteFlg(..), TestInstructionsFlg(..)
  , FromStdinFlg(..), OutputFileFlg(..), StatsFileFlg(..)
  , TimeoutFlg(..),  VerboseModeFlg(..)
  , hasFlagP, getListFlagP, getFlagP, setFlagP
  , builderType , BuilderType(..)
  )
import NLP.GenI.General ( mkLogname )
import NLP.GenI.Pretty
import NLP.GenI.Semantics ( SemInput )
import NLP.GenI.Simple.SimpleBuilder
import NLP.GenI.TestSuite ( TestCase(..) )

import Text.JSON
import Text.JSON.Pretty ( render, pp_value )

consoleGeni :: ProgStateRef -> IO()
consoleGeni pstRef = do
    config <- pa <$> readIORef pstRef
    loadEverything pstRef
    let job | hasFlagP FromStdinFlg config           = runStdinTestCase pstRef
            | hasFlagP BatchDirFlg config            = runInstructions pstRef -- even if there is a testcase
            | Just tc <- getFlagP TestCaseFlg config = runSpecificTestCase pstRef tc
            | otherwise                              = runInstructions pstRef
    case getFlagP TimeoutFlg config of
      Nothing -> job
      Just t  -> withGeniTimeOut t job

withGeniTimeOut :: Int -- ^ seconds
                -> IO ()
                -> IO ()
withGeniTimeOut t job = do
    status <- timeout (fromIntegral t * 1000000) job
    case status of
      Just () -> return ()
      Nothing -> do
          ePutStrLn $ "GenI timed out after " ++ show t ++ "s"
          exitWith (ExitFailure 2)

-- | Run GenI without reading any test suites, just grab semantics from stdin
runStdinTestCase :: ProgStateRef -> IO ()
runStdinTestCase pstRef = do
    config   <- pa <$> readIORef pstRef
    mSemInput <- parseSemInput <$> getContents
    case mSemInput of
      Left err ->
           fail $ "I didn't understand the semantics you gave me: " ++ show err
      Right semInput ->
           runOnSemInput pstRef (runAsStandalone config) semInput >> return ()

-- | Run a test case with the specified name
runSpecificTestCase :: ProgStateRef -> String -> IO ()
runSpecificTestCase pstRef cname = do
    config <- pa <$> readIORef pstRef
    fullsuite <- loadTestSuite pstRef
    case find (\x -> tcName x == cname) fullsuite  of
      Nothing -> fail ("No such test case: " ++ cname)
      Just s  -> runOnSemInput pstRef (runAsStandalone config) (tcSem s) >> return ()

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
                  mapM_ (runCase bsubdir) suite
  runCase bdir (TestCase { tcName = n, tcSem = s }) =
   do config <- pa `fmap` readIORef pstRef
      let verbose = hasFlagP VerboseModeFlg config
          earlyDeath = hasFlagP EarlyDeathFlg config
      when verbose $
        ePutStrLn "======================================================"
      gresults <- runOnSemInput pstRef (PartOfSuite n bdir) s
      let res = grResults gresults
          (goodres, badres) = partition isSuccess (grResults gresults)
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

runAsStandalone :: Params -> RunAs
runAsStandalone config =
    Standalone (fromMaybe "" $ getFlagP OutputFileFlg config)
               (fromMaybe "" $ getFlagP StatsFileFlg config)

-- | Runs a case in the test suite.  If the user does not specify any test
--   cases, we run the first one.  If the user specifies a non-existing
--   test case we raise an error.
runOnSemInput :: ProgStateRef
              -> RunAs
              -> SemInput
              -> IO GeniResults
runOnSemInput pstRef args semInput = do
    pst <- readIORef pstRef
    case builderType (pa pst) of
             SimpleBuilder         -> helper pst simpleBuilder_2p
             SimpleOnePhaseBuilder -> helper pst simpleBuilder_1p
  where
    helper pst builder = do
         (res,_) <- runGeni pstRef semInput builder
         writeResults pst args semInput res
         return res

-- | Not just the global warnings but the ones local to each response too
allWarnings :: GeniResults -> [Text]
allWarnings res = concat $ grGlobalWarnings res
                         : [ grWarnings s | GSuccess s <- grResults res ]

writeResults :: ProgState -> RunAs -> SemInput -> GeniResults -> IO ()
writeResults pst args semInput gresults = do
    -- create output directory as needed
    case args of
      PartOfSuite n f -> createDirectoryIfMissing True (f </> n)
      _               -> return ()
    -- print responses
    if dump
       then writeResponses $ ppJSON results
       else writeResponses $ T.unlines . concatMap (fromResult formatResponses) $ results
    -- print out statistical data (if available)
    when (isJust $ getFlagP MetricsFlg config) $
       writeStats (ppJSON stats)
    -- print any warnings we picked up along the way
    unless (null warnings) $ do
       T.hPutStrLn stderr $ "Warnings:\n" <> formatWarnings warnings
       writeBatchFile "warnings" $ T.unlines warnings
    -- other outputs when run in batch mode
    writeBatchFile "semantics"  $ pretty semInput
    writeBatchFile "derivations"$ ppJSON results
  where
    results     = grResults    gresults
    warnings    = allWarnings  gresults
    stats       = grStatistics gresults
    config      = pa pst
    dump        = hasFlagP DumpDerivationFlg config
    -- do we print ranking information and all that other jazz?
    formatResponses = if hasFlagP RankingConstraintsFlg config
                         then pure . prettyResult pst
                         else grRealisations
    formatWarnings = T.unlines . map (" - " <>)
    --
    writeBatchFile key = case args of
        Standalone _  _ -> const (return ())
        PartOfSuite n f -> T.writeFile (f </> n </> key)
    writeResponses = case args of
        Standalone "" _ -> T.putStrLn
        Standalone f  _ -> T.writeFile f
        PartOfSuite _ _ -> writeBatchFile "responses"
    writeStats = case args of
        Standalone _ "" -> T.putStrLn
        Standalone _ f  -> T.writeFile f
        PartOfSuite _ _ -> writeBatchFile "stats"
    --
    fromResult :: (GeniSuccess -> [Text]) -> GeniResult -> [Text]
    fromResult _ (GError errs) = [ pretty errs ]
    fromResult f (GSuccess x)  = f x

-- | TODO: If somebody puts together a render function that emits Data.Text
--   we should just use that instead
ppJSON :: JSON a => a -> Text
ppJSON = T.pack . render . pp_value . showJSON

-- ----------------------------------------------------------------------
-- Odds and ends
-- ----------------------------------------------------------------------

data MNAME = MNAME deriving Typeable
logname :: String
logname = mkLogname MNAME
