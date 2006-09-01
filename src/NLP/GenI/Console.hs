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

module NLP.GenI.Console(consoleGeni, runTestCaseOnly) where

import Control.Monad ( when, unless )
import Data.IORef(readIORef, modifyIORef)
import Data.List(find)
import Data.Maybe ( isJust, fromMaybe )
import System.Directory(createDirectoryIfMissing)
import System.Exit ( exitFailure )
import Test.HUnit.Text (runTestTT)
import qualified Test.HUnit.Base as H
import Test.HUnit.Base ((@?))

import NLP.GenI.Btypes
   ( SemInput, showSem,
   , TestCase(tcSem, tcName, tcExpected)
   )
import qualified NLP.GenI.Btypes as G
import NLP.GenI.General
  ( ePutStrLn, withTimeout, exitTimeout, (///)
  , fst3
  )
import NLP.GenI.Geni
import NLP.GenI.Configuration
  ( Params
  , DisableGuiFlg(..), BatchDirFlg(..), EarlyDeathFlg(..), OutputFileFlg(..)
  , MetricsFlg(..), RegressionTestModeFlg(..), StatsFileFlg(..)
  , TestCaseFlg(..), TimeoutFlg(..),  VerboseModeFlg(..)
  , hasFlagP, getFlagP,
  , builderType , BuilderType(..),
  )
import qualified NLP.GenI.Builder as B
import NLP.GenI.CkyEarley.CkyBuilder
import NLP.GenI.Simple.SimpleBuilder
import Statistics ( showFinalStats, Statistics )

consoleGeni :: ProgStateRef -> IO()
consoleGeni pstRef = do
  pst <- readIORef pstRef
  let config = pa pst
  unless (hasFlagP DisableGuiFlg config) $
    ePutStrLn "GUI not available"
  --
  loadGrammar pstRef
  case getFlagP TimeoutFlg (pa pst) of
    Nothing -> runSuite pstRef
    Just t  -> withTimeout t (timeoutErr t) $ runSuite pstRef
  where
   timeoutErr t = do ePutStrLn $ "GenI timed out after " ++ (show t) ++ "s"
                     exitTimeout

-- | Runs a test suite.
--   We assume that the grammar and target semantics are already
--   loaded into the monadic state.
--   If batch processing is enabled, save the results to the batch output
--   directory with one subdirectory per case.
runSuite :: ProgStateRef -> IO ()
runSuite pstRef =
  do pst <- readIORef pstRef
     let suite  = tsuite pst
         config = pa pst
         verbose = hasFlagP VerboseModeFlg config
         earlyDeath = hasFlagP EarlyDeathFlg config
     if hasFlagP RegressionTestModeFlg config
        then runRegressionSuite pstRef >> return ()
        else case getFlagP BatchDirFlg config of
              Nothing   -> runTestCaseOnly pstRef >> return ()
              Just bdir -> runBatch earlyDeath verbose bdir suite
  where
  runBatch earlyDeath verbose bdir suite =
    if any null $ map tcName suite
    then    ePutStrLn "Can't do batch processing. The test suite has cases with no name."
    else do ePutStrLn "Batch processing mode"
            mapM_ (runCase earlyDeath verbose bdir) suite
  runCase earlyDeath verbose bdir (G.TestCase n _ s _) =
   do when verbose $
        ePutStrLn "======================================================"
      (res , _) <- runOnSemInput pstRef (PartOfSuite n bdir) s
      ePutStrLn $ " " ++ n ++ " - " ++ (show $ length res) ++ " results"
      when (null res && earlyDeath) $ do
        ePutStrLn $ "Exiting early because test case " ++ n ++ " failed."
        exitFailure

-- | Run a test suite, but in HUnit regression testing mode,
--   treating each GenI test case as an HUnit test.  Obviously
--   we need a test suite, grammar, etc as input
runRegressionSuite :: ProgStateRef -> IO (H.Counts)
runRegressionSuite pstRef =
 do pst <- readIORef pstRef
    tests <- (mapM toTest) . tsuite $ pst
    runTestTT . (H.TestList) . concat $ tests
 where
  toTest :: G.TestCase -> IO [H.Test] -- ^ GenI test case to HUnit Tests
  toTest tc = -- run the case, and return a test case for each expected result
   do (res , _) <- runOnSemInput pstRef InRegressionTest (tcSem tc)
      let name = tcName tc
          semStr = showSem . fst3 . tcSem $ tc
          mainMsg  = "for " ++ semStr ++ ",  got no results"
          mainCase = H.TestLabel name
            $ H.TestCase $ (not.null $ res) @? mainMsg
          subMsg e = "for " ++ semStr ++ ", failed to get (" ++ e ++ ")"
          subCase e = H.TestLabel name
            $ H.TestCase $ (e `elem` res) @? subMsg e
      return $ (mainCase :) $ map subCase (tcExpected tc)

-- | Run the specified test case, or failing that, the first test
--   case in the suite
runTestCaseOnly :: ProgStateRef -> IO ([String], Statistics)
runTestCaseOnly pstRef =
 do pst <- readIORef pstRef
    let config     = pa pst
        pstOutfile = fromMaybe "" $ getFlagP OutputFileFlg config
        sFile      = fromMaybe "" $ getFlagP StatsFileFlg  config
    semInput <- case getFlagP TestCaseFlg config of
                   Nothing -> getFirstCase pst
                   Just c  -> findCase pst c
    runOnSemInput pstRef (Standalone pstOutfile sFile) semInput
 where
  getFirstCase pst =
    case tsuite pst of
    []    -> fail "Test suite is empty."
    (c:_) -> return $ tcSem c
  findCase pst theCase =
    case find (\x -> tcName x == theCase) (tsuite pst) of
    Nothing -> fail ("No such test case: " ++ theCase)
    Just s  -> return $ tcSem s

data RunAs = Standalone  FilePath FilePath
           | PartOfSuite String FilePath
           | InRegressionTest

-- | Runs a case in the test suite.  If the user does not specify any test
--   cases, we run the first one.  If the user specifies a non-existing
--   test case we raise an error.
runOnSemInput :: ProgStateRef
              -> RunAs
              -> SemInput
              -> IO ([String], Statistics)
runOnSemInput pstRef args semInput =
  do modifyIORef pstRef (\x -> x{ts = semInput})
     pst <- readIORef pstRef
     let config = pa pst
     (sentences, stats) <- case builderType config of
                            NullBuilder   -> helper B.nullBuilder
                            SimpleBuilder -> helper simpleBuilder_2p
                            SimpleOnePhaseBuilder -> helper simpleBuilder_1p
                            CkyBuilder    -> helper ckyBuilder
                            EarleyBuilder -> helper earleyBuilder
     -- create directory if need be
     case args of
       PartOfSuite n f -> createDirectoryIfMissing False (f///n)
       _               -> return ()
     let oPutStrLn = case args of
                     Standalone "" _ -> putStrLn
                     Standalone f  _ -> writeFile f
                     PartOfSuite n f -> writeFile $ f /// n /// "responses"
                     InRegressionTest -> const $ return ()
         soPutStrLn = case args of
                     Standalone _ "" -> putStrLn
                     Standalone _ f  -> writeFile f
                     PartOfSuite n f -> writeFile $ f /// n /// "stats"
                     InRegressionTest -> const $ return ()
     oPutStrLn (unlines sentences)
     -- print out statistical data (if available)
     when (isJust $ getFlagP MetricsFlg config) $
       do soPutStrLn $ "begin stats\n" ++ showFinalStats stats ++ "end"
     return (sentences, stats)
  where
    helper :: B.Builder st it Params -> IO ([String], Statistics)
    helper builder =
      do (sentences, stats, _) <- runGeni pstRef builder
         return (sentences, stats)
