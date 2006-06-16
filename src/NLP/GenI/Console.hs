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

module NLP.GenI.Console(consoleGeni) where

import Data.List(find)
import Control.Monad(when)
import Data.IORef(readIORef, modifyIORef)
import System.Directory(createDirectoryIfMissing)

import NLP.GenI.Btypes( SemInput )
import NLP.GenI.General(ePutStrLn, withTimeout, exitTimeout)
import NLP.GenI.Geni
import NLP.GenI.Configuration
  ( Params(batchDir), isGraphical, outputFile, statsFile, metricsParam, timeoutSecs
  , builderType
  , BuilderType(..))
import qualified NLP.GenI.Builder as B
import NLP.GenI.CkyEarley.CkyBuilder
import NLP.GenI.Simple.SimpleBuilder
import Statistics ( showFinalStats, Statistics )

consoleGeni :: ProgStateRef -> IO()
consoleGeni pstRef = do
  pst <- readIORef pstRef
  when (isGraphical $ pa pst) $ do
    ePutStrLn "GUI not available"
  --
  loadGrammar pstRef
  ePutStrLn "======================================================"
  --
  case timeoutSecs $ pa pst of
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
     let bdir   = batchDir $ pa pst
         suite  = tsuite pst
     if null bdir
        then runTestCaseOnly pstRef
        else if any null $ map fst suite
             then ePutStrLn "Can't do batch processing. The test suite has cases with no name."
             else do mapM (\ (n,s) -> runOnSemInput pstRef (PartOfSuite n bdir) s) suite
                     return ()

-- | Run the specified test case, or failing that, the first test
--   case in the suite
runTestCaseOnly :: ProgStateRef -> IO ()
runTestCaseOnly pstRef =
 do pst <- readIORef pstRef
    let config     = pa pst
        pstCase    = tcase pst
        pstOutfile = outputFile config
        sFile      = statsFile config
    semInput <- if null pstCase
                   then getFirstCase pst
                   else findCase pst pstCase
    runOnSemInput pstRef (Standalone pstOutfile sFile) semInput
 where
  getFirstCase pst =
    case tsuite pst of
    []    -> fail "Test suite is empty."
    (c:_) -> return $ snd c
  findCase pst theCase =
    case find (\x -> fst x == theCase) (tsuite pst) of
    Nothing    -> fail ("No such test case: " ++ theCase)
    Just (_,s) -> return s

data RunAs = Standalone  FilePath FilePath
           | PartOfSuite String FilePath

-- | Runs a case in the test suite.  If the user does not specify any test
--   cases, we run the first one.  If the user specifies a non-existing
--   test case we raise an error.
runOnSemInput :: ProgStateRef
              -> RunAs
              -> SemInput
              -> IO ()
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
     when (not . null . metricsParam $ config) $
       do soPutStrLn $ "begin stats\n" ++ showFinalStats stats ++ "end"
  where
    helper :: B.Builder st it Params -> IO ([String], Statistics)
    helper builder =
      do (sentences, stats, _) <- runGeni pstRef builder
         return (sentences, stats)

-- from darcs (move to general)
(///) :: FilePath -> FilePath -> FilePath
""///b = b
a///"" = a
a///b  = a ++ "/" ++ b
