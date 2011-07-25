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
      ]
  ]

test_detectPolarityForAttr :: Assertion 
test_detectPolarityForAttr =
  assertEqual ""
     (PD_Just [(PolarityKeyAv "foo" "vfoo", (1,1))])
     (detectPolarity 1 simpleFoo emptyFeatStruct (barAvAnd fooAv))

test_detectPolarityForAttrNeg :: Assertion 
test_detectPolarityForAttrNeg =
  assertEqual ""
     (PD_Just [(PolarityKeyAv "foo" "vfoo", (-1,-1))])
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

-- test_detectRestrictedPolarity


simpleFoo = SimplePolarityAttr "foo"

unconstrainedFoo = PD_Unconstrained ("foo", (0,1))

barAvAnd x = mkFeatStruct [ x, barAv ]

foAv  = AvPair "fo" (mkGConstNone "vfo")
fooAv = AvPair "foo" (mkGConstNone "vfoo")
fooDisjAv = AvPair "foo" (mkGConst "vfoo" ["vfoo2"])
barAv = AvPair "bar" (mkGConstNone "vbar")
