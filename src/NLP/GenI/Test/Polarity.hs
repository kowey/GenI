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
     (detectPolarityForAttr 1 "foo" [ fooAv, barAv ])

test_detectPolarityForAttrNeg :: Assertion 
test_detectPolarityForAttrNeg =
  assertEqual ""
     (PD_Just [(PolarityKeyAv "foo" "vfoo", (-1,-1))])
     (detectPolarityForAttr (-1) "foo" [ fooAv, barAv ])

test_detectPolarityForAttrDisj :: Assertion
test_detectPolarityForAttrDisj = do
  assertEqual ""
     (tweak expected)
     (tweak $ detectPolarityForAttr (-1) "foo" [ fooDisjAv, barAv ])
 where
  tweak (PD_Just xs) = PD_Just (sort xs)
  tweak pd = pd
  expected = PD_Just $ map (\x -> (PolarityKeyAv "foo" x, (-1,0))) ["vfoo", "vfoo2"]

test_detectPolarityForAttrMissing :: Assertion
test_detectPolarityForAttrMissing =
  assertEqual "simple detection (no false +)"
     unconstrainedFoo
     (detectPolarityForAttr 1 "foo" [ foAv, barAv ])

test_detectPolarityForAttrVar :: Assertion
test_detectPolarityForAttrVar =
  assertEqual "simple detection (variable)"
     unconstrainedFoo
     (detectPolarityForAttr 1 "foo" [ foAv, barAv ])


unconstrainedFoo = PD_Unconstrained ("foo", (0,1))


foAv  = AvPair "fo" (mkGConst "vfo" [])
fooAv = AvPair "foo" (mkGConst "vfoo" [])
fooDisjAv = AvPair "foo" (mkGConst "vfoo" ["vfoo2"])
barAv = AvPair "bar" (mkGConst "vbar" [])
