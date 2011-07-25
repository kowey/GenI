-- GenI surface realiser
-- Copyright (C) 2011 SRI
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Polarity where

import Control.Monad ( forM_ )
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.FeatureStructures
import NLP.GenI.GeniVal
import NLP.GenI.Polarity
import NLP.GenI.Polarity.Internal
import NLP.GenI.PolarityTypes
import NLP.GenI.Tags

import NLP.GenI.Test.Tags hiding ( suite )

deriving instance Eq   PolarityDetectionResult
deriving instance Show PolarityDetectionResult

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.Polarity"
  [ testGroup "detecting values"
      [ testCase "simple example" testDetectPolarityForAttr
      , testCase "simple example (neg)"     testDetectPolarityForAttrNeg
      , testCase "simple example (disj)"    testDetectPolarityForAttrDisj
      , testCase "simple example (var)"     testDetectPolarityForAttrFree
      , testCase "restricted example (detects)" testDetectedRestrictedPolarity
      , testCase "restricted example (filters)" testDetectedRestrictedPolarityFilter
      ]
  , testGroup "detection on trees"
      [ testCase "silly TagElem"       testDetectPolarityForSillyTagElem
      , testCase "silly TagElem (aux)" testDetectPolarityForSillyTagElemAux
      ]
  ]

testDetectPolarityForAttr :: Assertion 
testDetectPolarityForAttr =
  assertEqual ""
     (foundFoo 1)
     (detectPolarity 1 simpleFoo emptyFeatStruct (barAvAnd fooAv))

testDetectPolarityForAttrNeg :: Assertion 
testDetectPolarityForAttrNeg =
  assertEqual ""
     (foundFoo (-1))
     (detectPolarity (-1) simpleFoo emptyFeatStruct (barAvAnd fooAv))

testDetectPolarityForAttrDisj :: Assertion
testDetectPolarityForAttrDisj = do
  assertEqual ""
     (tweak expected)
     (tweak $ detectPolarity (-1) simpleFoo emptyFeatStruct (barAvAnd fooDisjAv))
 where
  tweak (PD_Just xs) = PD_Just (sort xs)
  tweak pd = pd
  expected = PD_Just $ map (\x -> (PolarityKeyAv "foo" x, (-1,0))) ["vfoo", "vfoo2"]

testDetectPolarityForAttrFree :: Assertion
testDetectPolarityForAttrFree =
  forM_ [ AvPair "fo"   "x"
        , AvPair "foo"  (mkGVarNone "X")
        , AvPair "foo"  mkGAnon
        ] $ \av ->
    assertEqual "simple detection on missing/free"
       unconstrainedFoo
       (detectPolarity 1 simpleFoo emptyFeatStruct (barAvAnd av))

testDetectedRestrictedPolarity :: Assertion
testDetectedRestrictedPolarity =
  assertEqual "restricted detection (detects)"
     (foundFoo 1)
     (detectPolarity 1 (restrictedFoo "x") ffs fs) 
  where
   ffs = catAvAnd "x" fooAv
   fs  = barAvAnd fooAv

testDetectedRestrictedPolarityFilter :: Assertion
testDetectedRestrictedPolarityFilter =
  assertEqual "restricted detection (filters)"
     PD_Nothing
     (detectPolarity 1 (restrictedFoo "x") ffs fs) 
  where
   ffs = catAvAnd "y" fooAv
   fs  = barAvAnd fooAv

-- ----------------------------------------------------------------------
-- TagElem
-- ----------------------------------------------------------------------

testDetectPolarityForSillyTagElem :: Assertion
testDetectPolarityForSillyTagElem = do
   forM_ [ PolarityKeyAv "idx" "rbad"
         , PolarityKeyAv "idx" "sbad"
         ] $ \k -> assertEqual "ignored" Nothing $ Map.lookup k pols
   --
   forM_ [ PolarityKeyAv "cat" "a"
         , PolarityKeyAv "idx" "r"
         ] $ \k -> assertEqual "root" (Just (1,1)) $ Map.lookup k pols

   forM_ [ PolarityKeyAv "cat" "b"
         , PolarityKeyAv "idx" "s"
         ] $ \k -> assertEqual "root" (Just (-2,-2)) $ Map.lookup k pols
  where
   pols  = tpolarities (detectPols attrs sillyTagElem)
   attrs = Set.fromList [ SimplePolarityAttr "cat", SimplePolarityAttr "idx" ]


testDetectPolarityForSillyTagElemAux :: Assertion
testDetectPolarityForSillyTagElemAux = do
   forM_ [ PolarityKeyAv "cat" "a"
         , PolarityKeyAv "idx" "r"
         , PolarityKeyAv "idx" "f"
         ] $ \k -> assertEqual "ignored" Nothing $ Map.lookup k pols
   assertEqual "subst" (Just (-1,-1))
     $ Map.lookup (PolarityKeyAv "cat" "b") pols
   assertEqual "subst" (Just (-1,-1))
     $ Map.lookup (PolarityKeyAv "idx" "s") pols
  where
   pols  = tpolarities (detectPols attrs sillyTagElemAux)
   attrs = Set.fromList $ map SimplePolarityAttr [ "cat", "idx" ]


-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

simpleFoo = SimplePolarityAttr "foo"
restrictedFoo c = RestrictedPolarityAttr c "foo"

foundFoo i = PD_Just [(PolarityKeyAv "foo" "vfoo", (i,i))]
unconstrainedFoo = PD_Unconstrained ("foo", (0,1))

catAvAnd c x = mkFeatStruct [ x, AvPair "cat" (mkGConstNone c) ]
barAvAnd x = mkFeatStruct [ x, barAv ]

foAv  = AvPair "fo" "vfo"
fooAv = AvPair "foo" "vfoo"
fooDisjAv = AvPair "foo" (mkGConst "vfoo" ["vfoo2"])
barAv = AvPair "bar" "vbar"
