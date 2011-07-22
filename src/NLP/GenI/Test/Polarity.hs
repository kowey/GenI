{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Polarity where

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
      , testCase "simple example (false +)" test_detectPolarityForAttrFP
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

test_detectPolarityForAttrFP :: Assertion
test_detectPolarityForAttrFP =
  assertEqual "simple detection (no false +)"
     PD_Nothing
     (detectPolarityForAttr 1 "foo" [ foAv, barAv ])


foAv  = AvPair "fo" (mkGConst "vfo" [])
fooAv = AvPair "foo" (mkGConst "vfoo" [])
barAv = AvPair "bar" (mkGConst "vbar" [])
