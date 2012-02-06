-- GenI surface realiser
-- Copyright (C) 2009 Eric Kow
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

-- | Just regression testing of suites
--   This can be seen as regression testing of GenI
--   and also of grammars using GenI

module NLP.GenI.Regression (mkSuite) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Either
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List(sort)
import System.FilePath ((</>))
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2


import NLP.GenI.Btypes
   ( SemInput, showSem
   , TestCase(tcSem, tcName, tcExpected)
   )
import qualified NLP.GenI.Btypes as G
import NLP.GenI.General
  ( fst3,
  )
import NLP.GenI.Geni
import NLP.GenI.Configuration
  ( Params
  , builderType , BuilderType(..)
  , setFlagP, DisableGuiFlg(..)
  , processInstructions, treatArgs, optionsForStandardGenI
  )
import NLP.GenI.Simple.SimpleBuilder

mkSuite :: IO Test.Framework.Test
mkSuite = do
 goods <- sequence
  [ goodSuite "ej"        (usualArgs "examples/ej" [])
  , goodSuite "chatnoir"  (usualArgs "examples/chatnoir" [])
  , goodSuite "demo"      (usualArgs "examples/demo" [])
  , goodSuite "promettre" (usualArgs "examples/promettre" ["--opts=pol"])
  , goodSuite "artificial" (usualArgs "examples/artificial" [])
  , badSuite  "artificial (bad)" (usualArgsBad "examples/artificial" [])
  ]
 return $ testGroup "Functional tests (coarse grained)" goods

usualArgs :: FilePath -> [String] -> [String]
usualArgs p args =
  [ "-m", p </> "macros"
  , "-l", p </> "lexicon"
  , "-s", p </> "suite"
  ] ++ args

usualArgsBad :: FilePath -> [String] -> [String]
usualArgsBad p args =
  [ "-m", p </> "macros"
  , "-l", p </> "lexicon"
  , "-s", p </> "suite-bad"
  ] ++ args

noGui = setFlagP DisableGuiFlg () 

goodSuite = genSuite goodSuiteCase
badSuite  = genSuite badSuiteCase

genSuite mkCase name xs = do
  confArgs <- processInstructions =<< treatArgs optionsForStandardGenI xs
  let pst = emptyProgState (noGui confArgs)
  pstRef <- newIORef pst
  loadEverything pstRef
  suite <- tsuite <$> readIORef pstRef
  return . testGroup name $ map (mkCase pstRef) suite


-- goodSuiteCase :: ProgStateRef -> G.TestCase -> TestCase
goodSuiteCase pstRef tc = testCase (tcName tc) $ do
  res <- runOnSemInput pstRef (tcSem tc)
  let sentences = map lemmaSentenceString (successes res)
      name = tcName tc
      semStr = showSem . fst3 . tcSem $ tc
      mainMsg  = "for " ++ semStr ++ ",  got no results"
  assertBool "got result" (not (null sentences))
  forM_ (tcExpected tc) $ \e ->
      assertBool ("got result: " ++ e) (e `elem` sentences)

badSuiteCase pstRef tc = testCase (tcName tc) $ do
  res <- runOnSemInput pstRef (tcSem tc)
  let sentences = map lemmaSentenceString (successes res)
  assertBool "no results" (null sentences)

runOnSemInput :: ProgStateRef -> SemInput -> IO [GeniResult]
runOnSemInput pstRef semInput =
  do modifyIORef pstRef (resetLocal semInput)
     pst <- readIORef pstRef
     let config = pa pst
         go = case builderType config of
                SimpleBuilder -> helper simpleBuilder_2p
                SimpleOnePhaseBuilder -> helper simpleBuilder_1p
     sort `fmap` go
  where
    helper builder = fst3 `fmap` runGeni pstRef builder

successes :: [GeniResult] -> [GeniSuccess]
successes xs = [ s | GSuccess s <- xs ]
