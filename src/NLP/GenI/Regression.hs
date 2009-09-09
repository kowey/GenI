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

module NLP.GenI.Regression (regressionGeni) where

import Control.Monad
import Data.IORef(readIORef, modifyIORef)
import Data.List(sort)
import Test.HUnit.Text (runTestTT)
import qualified Test.HUnit.Base as H
import Test.HUnit.Base ((@?))

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
  )
import qualified NLP.GenI.Builder as B
import NLP.GenI.Simple.SimpleBuilder

regressionGeni :: ProgStateRef -> IO ()
regressionGeni pstRef = do
 do pst <- readIORef pstRef
    loadEverything pstRef
    tests <- (mapM toTest) . tsuite $ pst
    runTestTT . (H.TestList) . concat $ tests
    return ()
 where
  toTest :: G.TestCase -> IO [H.Test] -- ^ GenI test case to HUnit Tests
  toTest tc = -- run the case, and return a test case for each expected result
   do res <- runOnSemInput pstRef (tcSem tc)
      let sentences = fst (unzip res)
          name = tcName tc
          semStr = showSem . fst3 . tcSem $ tc
          mainMsg  = "for " ++ semStr ++ ",  got no results"
          mainCase = H.TestLabel name
            $ H.TestCase $ (not.null $ sentences) @? mainMsg
          subMsg e = "for " ++ semStr ++ ", failed to get (" ++ e ++ ")"
          subCase e = H.TestLabel name
            $ H.TestCase $ (e `elem` sentences) @? subMsg e
      return $ (mainCase :) $ map subCase (tcExpected tc)

-- | Runs a case in the test suite.  If the user does not specify any test
--   cases, we run the first one.  If the user specifies a non-existing
--   test case we raise an error.
runOnSemInput :: ProgStateRef -> SemInput -> IO [GeniResult]
runOnSemInput pstRef semInput =
  do modifyIORef pstRef (\x -> x{ts = semInput})
     pst <- readIORef pstRef
     let config = pa pst
         go = case builderType config of
                NullBuilder   -> helper B.nullBuilder
                SimpleBuilder -> helper simpleBuilder_2p
                SimpleOnePhaseBuilder -> helper simpleBuilder_1p
     sort `fmap` go
  where
    helper builder = fst3 `fmap` runGeni pstRef builder
