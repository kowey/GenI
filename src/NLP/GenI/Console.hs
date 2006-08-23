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

import NLP.GenI.Btypes( SemInput )
import NLP.GenI.General
  ( ePutStrLn, withTimeout, exitTimeout, (///)
  , fst3, thd3,
  )
import NLP.GenI.Geni
import NLP.GenI.Configuration
  ( Params
  , DisableGuiFlg(..), BatchDirFlg(..), OutputFileFlg(..)
  , MetricsFlg(..), StatsFileFlg(..)
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
  when (hasFlagP VerboseModeFlg config) $
    ePutStrLn "======================================================"
  --
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
     case getFlagP BatchDirFlg (pa pst) of
       Nothing   -> runTestCaseOnly pstRef >> return ()
       Just bdir ->
          if any null $ map fst3 suite
             then ePutStrLn "Can't do batch processing. The test suite has cases with no name."
             else do ePutStrLn "Batch processing mode"
                     mapM_ (runCase bdir) suite
 where
  runCase bdir (n,_,s) =
   do (res , _) <- runOnSemInput pstRef (PartOfSuite n bdir) s
      ePutStrLn $ " " ++ n ++ " - " ++ (show $ length res) ++ " results"

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
    (c:_) -> return $ thd3 c
  findCase pst theCase =
    case find (\x -> fst3 x == theCase) (tsuite pst) of
    Nothing      -> fail ("No such test case: " ++ theCase)
    Just (_,_,s) -> return s

data RunAs = Standalone  FilePath FilePath
           | PartOfSuite String FilePath

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
         soPutStrLn = case args of
                     Standalone _ "" -> putStrLn
                     Standalone _ f  -> writeFile f
                     PartOfSuite n f -> writeFile $ f /// n /// "stats"
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
