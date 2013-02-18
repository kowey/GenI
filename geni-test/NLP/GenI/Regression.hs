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

{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.Regression (mkSuite) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Trans.Error
import Data.Either
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List(sort)
import System.FilePath ((</>))
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import qualified Data.Text as T

import NLP.GenI
import NLP.GenI.Configuration
import NLP.GenI.Console
import NLP.GenI.General ( fst3, )
import NLP.GenI.LexicalSelection ( CustomSem )
import NLP.GenI.Pretty
import NLP.GenI.Semantics ( SemInput )
import NLP.GenI.TestSuite ( TestCase(tcSem, tcName, tcExpected) )
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
  [ "-t", p </> "trees"
  , "-l", p </> "lexicon"
  , "-s", p </> "suite"
  ] ++ args

usualArgsBad :: FilePath -> [String] -> [String]
usualArgsBad p args =
  [ "-t", p </> "trees"
  , "-l", p </> "lexicon"
  , "-s", p </> "suite-bad"
  ] ++ args

noGui = setFlagP DisableGuiFlg () 

type TestMaker = ProgStateRef -> CustomSem SemInput -> TestCase SemInput -> Test.Framework.Test

goodSuite = genSuite goodSuiteCase
badSuite  = genSuite badSuiteCase

genSuite :: TestMaker -> String -> [String] -> IO Test.Framework.Test
genSuite mkCase name xs = do
    confArgs <- processInstructions =<< treatArgs optionsForStandardGenI xs
    let pst = emptyProgState (noGui confArgs)
    pstRef <- newIORef pst
    wrangler <- defaultCustomSem pst
    loadEverything pstRef wrangler
    suite <- case getListFlagP TestInstructionsFlg confArgs of
                 []  -> error "NLP.GenI.Regression: not expecting empty instructions"
                 [x] -> loadNextSuite pstRef wrangler x
                 _   -> error "NLP.GenI.Regression: not expecting multiple instructions"
    return . testGroup name $ map (mkCase pstRef wrangler) suite

goodSuiteCase :: TestMaker
goodSuiteCase pstRef wrangler tc = testCase (T.unpack (tcName tc)) $ do
    res <- runOnSemInput pstRef wrangler tc
    let sentences = map lemmaSentenceString (successes res)
        name = tcName tc
        semStr = prettyStr . fst3 . tcSem $ tc
        mainMsg  = "for " ++ semStr ++ ",  got no results"
    assertBool "got result" (not (null sentences))
    forM_ (tcExpected tc) $ \e ->
        assertBool ("got result: " ++ T.unpack e) (e `elem` sentences)

badSuiteCase :: TestMaker
badSuiteCase pstRef wrangler tc = testCase (T.unpack (tcName tc)) $ do
    res <- runOnSemInput pstRef wrangler tc
    let sentences = map lemmaSentenceString (successes res)
    assertBool "no results" (null sentences)

runOnSemInput :: ProgStateRef -> CustomSem SemInput -> TestCase SemInput -> IO [GeniResult]
runOnSemInput pstRef wrangler tc = do
    pst <- readIORef pstRef
    let config = pa pst
        go = case getBuilderType config of
               SimpleBuilder         -> helper pst simpleBuilder_2p
               SimpleOnePhaseBuilder -> helper pst simpleBuilder_1p
    sort `fmap` go
  where
    helper pst b = (grResults . simplifyResults) <$>
        (runErrorT $ runGeni pst wrangler b tc)

successes :: [GeniResult] -> [GeniSuccess]
successes xs = [ s | GSuccess s <- xs ]
