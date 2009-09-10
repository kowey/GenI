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

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad
import Data.IORef(readIORef, modifyIORef)
import Data.List(find, nub, sort)
import Data.Maybe ( isJust, fromMaybe )
import System.Directory(createDirectoryIfMissing)
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )

import NLP.GenI.Btypes
   ( SemInput, showSem
   , TestCase(tcSem, tcName, tcExpected)
   )
import qualified NLP.GenI.Btypes as G
import NLP.GenI.General
  ( ePutStrLn, withTimeout, exitTimeout
  , fst3,
  )
import NLP.GenI.Geni
import NLP.GenI.Configuration
  ( Params
  , BatchDirFlg(..), EarlyDeathFlg(..), FromStdinFlg(..), OutputFileFlg(..)
  , MetricsFlg(..), RegressionTestModeFlg(..), StatsFileFlg(..)
  , TestCaseFlg(..), TimeoutFlg(..),  VerboseModeFlg(..)
  , hasFlagP, getFlagP
  , builderType , BuilderType(..)
  )
import qualified NLP.GenI.Builder as B
import NLP.GenI.Simple.SimpleBuilder
import NLP.GenI.Statistics ( showFinalStats, Statistics )
import NLP.GenI.Tags ( DerivationStep(..) )

import Text.JSON
import Text.JSON.Pretty ( render, pp_value )

consoleGeni :: ProgStateRef -> IO()
consoleGeni pstRef = do
  pst <- readIORef pstRef
  loadEverything pstRef
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
     case getFlagP BatchDirFlg config of
       Nothing   -> runTestCaseOnly pstRef >> return ()
       Just bdir -> runBatch earlyDeath verbose bdir suite
  where
  runBatch earlyDeath verbose bdir suite =
    if any null $ map tcName suite
    then    ePutStrLn "Can't do batch processing. The test suite has cases with no name."
    else do ePutStrLn "Batch processing mode"
            mapM_ (runCase earlyDeath verbose bdir) suite
  runCase earlyDeath verbose bdir (G.TestCase { tcName = n, tcSem = s }) =
   do when verbose $
        ePutStrLn "======================================================"
      (res , _) <- runOnSemInput pstRef (PartOfSuite n bdir) s
      ePutStrLn $ " " ++ n ++ " - " ++ (show $ length res) ++ " results"
      when (null res && earlyDeath) $ do
        ePutStrLn $ "Exiting early because test case " ++ n ++ " failed."
        exitFailure

-- | Run the specified test case, or failing that, the first test
--   case in the suite
runTestCaseOnly :: ProgStateRef -> IO ([GeniResult], Statistics)
runTestCaseOnly pstRef =
 do pst <- readIORef pstRef
    let config     = pa pst
        pstOutfile = fromMaybe "" $ getFlagP OutputFileFlg config
        sFile      = fromMaybe "" $ getFlagP StatsFileFlg  config
    semInput <- case getFlagP TestCaseFlg config of
                   Nothing -> if hasFlagP FromStdinFlg config
                                 then do getContents >>= loadTargetSemStr pstRef
                                         ts `fmap` readIORef pstRef
                                 else getFirstCase pst
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

-- | Runs a case in the test suite.  If the user does not specify any test
--   cases, we run the first one.  If the user specifies a non-existing
--   test case we raise an error.
runOnSemInput :: ProgStateRef
              -> RunAs
              -> SemInput
              -> IO ([GeniResult], Statistics)
runOnSemInput pstRef args semInput =
  do modifyIORef pstRef (\x -> x{ts = semInput})
     pst <- readIORef pstRef
     let config = pa pst
     (results', stats) <- case builderType config of
                            NullBuilder   -> helper B.nullBuilder
                            SimpleBuilder -> helper simpleBuilder_2p
                            SimpleOnePhaseBuilder -> helper simpleBuilder_1p
     let results = sort results'
     -- create directory if need be
     case args of
       PartOfSuite n f -> createDirectoryIfMissing False (f </> n)
       _               -> return ()
     let oWrite = case args of
                     Standalone "" _ -> putStrLn
                     Standalone f  _ -> writeFile f
                     PartOfSuite n f -> writeFile $ f </> n </> "responses"
         doWrite = case args of
                     Standalone _  _ -> const (return ())
                     PartOfSuite n f -> writeFile $ f </> n </> "derivations"
         soWrite = case args of
                     Standalone _ "" -> putStrLn
                     Standalone _ f  -> writeFile f
                     PartOfSuite n f -> writeFile $ f </> n </> "stats"
     oWrite . unlines . map fst $ results
     doWrite . ppJSON $ map (toNiceResult pst) results
     -- print out statistical data (if available)
     when (isJust $ getFlagP MetricsFlg config) $ soWrite (ppJSON stats)
     return (results, stats)
  where
    ppJSON :: JSON a => a -> String
    ppJSON = render . pp_value . showJSON 
    helper builder =
      do (results, stats, _) <- runGeni pstRef builder
         return (results, stats)

toNiceResult pst (s,d) =
 NiceResult { nrSentence     = s
            , nrDerivation   = d
            , nrLexSelection = map (\x -> NiceLexSel x (getTraces pst x))
                                (lexicalSelection d)
            }

data NiceResult = NiceResult
 { nrSentence     :: String
 , nrDerivation   :: B.Derivation
 , nrLexSelection :: [ NiceLexSel ]
 }

data NiceLexSel = NiceLexSel
 { nlTree  :: String
 , nlTrace :: [String]
 }

instance JSON NiceResult where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       NiceResult <$> field "sentence"
                  <*> field "derivation"
                  <*> field "lexical-selection"
 showJSON nr =
     JSObject . toJSObject $ [ ("sentence", showJSON $ nrSentence nr)
                             , ("derivation", showJSONs $ nrDerivation nr)
                             , ("lexical-selection", showJSONs $ nrLexSelection nr)
                             ]

instance JSON NiceLexSel where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       NiceLexSel <$> field "lex-item"
                  <*> field "trace"
 showJSON x =
     JSObject . toJSObject $ [ ("lex-item", showJSON  $ nlTree x)
                             , ("trace",    showJSONs $ nlTrace x)
                             ]

lexicalSelection :: B.Derivation -> [String]
lexicalSelection = sort . nub . concatMap (\d -> [dsChild d, dsParent d])
