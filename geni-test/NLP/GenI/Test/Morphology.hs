{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Morphology where

import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.Test.GeniVal ( gTestStrings )
import NLP.GenI.FeatureStructures
import NLP.GenI.GeniParsers ( geniLanguageDef )
import NLP.GenI.Morphology
import Text.JSON

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.Morphology"
  [ testCase "JSON parsing" testMorphOutputJson
  ]

deriving instance Show MorphOutput

testMorphOutputJson = do
  assertEqual "" (Ok sansWarnings) (decode "[\"john loves mary\", \"mary is loved by john\"]")
  assertEqual "" (Ok sansWarnings) (decode "{\"realisations\":[\"john loves mary\", \"mary is loved by john\"]}")
  assertEqual "" (Ok withWarnings) (decode "{\"warnings\":[\"foo\",\"bar\"],\"realisations\":[\"john loves mary\", \"mary is loved by john\"]}")
 where
   sansWarnings = MorphOutput []            sentences
   withWarnings = MorphOutput ["foo","bar"] sentences
   sentences = ["john loves mary", "mary is loved by john"]
