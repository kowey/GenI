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
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Polarity where

import Data.List
import Data.Maybe

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

deriving instance Eq   PolarityDetectionResult
deriving instance Show PolarityDetectionResult

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.Polarity"
  [ testGroup "detecting values"
      [ testCase "simple example" test_detectPolarityForAttr
      , testCase "simple example (neg)"     test_detectPolarityForAttrNeg
      , testCase "simple example (disj)"    test_detectPolarityForAttrDisj
      , testCase "simple example (false +)" test_detectPolarityForAttrMissing
      , testCase "restricted example (detects)" test_detectRestrictedPolarity
      , testCase "restricted example (filters)" test_detectRestrictedPolarityFilter
      ]
  ]

test_detectPolarityForAttr :: Assertion 
test_detectPolarityForAttr =
  assertEqual ""
     (foundFoo 1)
     (detectPolarity 1 simpleFoo emptyFeatStruct (barAvAnd fooAv))

test_detectPolarityForAttrNeg :: Assertion 
test_detectPolarityForAttrNeg =
  assertEqual ""
     (foundFoo (-1))
     (detectPolarity (-1) simpleFoo emptyFeatStruct (barAvAnd fooAv))

test_detectPolarityForAttrDisj :: Assertion
test_detectPolarityForAttrDisj = do
  assertEqual ""
     (tweak expected)
     (tweak $ detectPolarity (-1) simpleFoo emptyFeatStruct (barAvAnd fooDisjAv))
 where
  tweak (PD_Just xs) = PD_Just (sort xs)
  tweak pd = pd
  expected = PD_Just $ map (\x -> (PolarityKeyAv "foo" x, (-1,0))) ["vfoo", "vfoo2"]

test_detectPolarityForAttrMissing :: Assertion
test_detectPolarityForAttrMissing =
  assertEqual "simple detection (no false +)"
     unconstrainedFoo
     (detectPolarity 1 simpleFoo emptyFeatStruct (barAvAnd foAv))

test_detectPolarityForAttrVar :: Assertion
test_detectPolarityForAttrVar =
  assertEqual "simple detection (variable)"
     unconstrainedFoo
     (detectPolarity 1 simpleFoo emptyFeatStruct (barAvAnd foAv))

test_detectRestrictedPolarity :: Assertion
test_detectRestrictedPolarity = do
  assertEqual "restricted detection (detects)"
     (foundFoo 1)
     (detectPolarity 1 (restrictedFoo "x") ffs fs) 
  where
   ffs = catAvAnd "x" fooAv
   fs  = barAvAnd fooAv

test_detectRestrictedPolarityFilter :: Assertion
test_detectRestrictedPolarityFilter = do
  assertEqual "restricted detection (filters)"
     PD_Nothing
     (detectPolarity 1 (restrictedFoo "x") ffs fs) 
  where
   ffs = catAvAnd "y" fooAv
   fs  = barAvAnd fooAv

simpleFoo = SimplePolarityAttr "foo"
restrictedFoo c = RestrictedPolarityAttr c "foo"

foundFoo i = PD_Just [(PolarityKeyAv "foo" "vfoo", (i,i))]
unconstrainedFoo = PD_Unconstrained ("foo", (0,1))

catAvAnd c x = mkFeatStruct [ x, AvPair "cat" (mkGConstNone c) ]
barAvAnd x = mkFeatStruct [ x, barAv ]

foAv  = AvPair "fo" (mkGConstNone "vfo")
fooAv = AvPair "foo" (mkGConstNone "vfoo")
fooDisjAv = AvPair "foo" (mkGConst "vfoo" ["vfoo2"])
barAv = AvPair "bar" (mkGConstNone "vbar")
