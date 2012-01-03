{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.GeniParsers where

import Control.Monad ( liftM2 )

import Data.List
import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.FeatureStructures
import NLP.GenI.General
import NLP.GenI.GeniVal
import NLP.GenI.GeniParsers
import NLP.GenI.Semantics
import NLP.GenI.Test.FeatureStructures ()
import NLP.GenI.Test.GeniVal ()

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.GeniParsers"
  [ testGroup "GeniVal"
      [ testCase     "empty constraints illegal"  testEmptyConstraintsIllegal
      , testCase     "show evil value"            testEvilGeniVal
      , testCase     "non-alphanum"               testNonAlphanum
      ]
  , testGroup "Semantics"
      [ testCase     "example"                    testRecogniseSem
      ]
  , testGroup "meta test mainly"
      [ testProperty "GeniVal" propParseableGeniVal
      ]
  , testGroup "round trips"
      [ testProperty "GeniVal" propRoundTripGeniVal
      , testProperty "FS"      propRoundTripFeats
      , testProperty "Sem"     propRoundTripSem
      ]
  ]

testEvilGeniVal :: Assertion
testEvilGeniVal =
 assertEqual "show GeniVal"
    (intercalate "|"
            [ quotemarks "\\\"?\\\\/"
            , quotemarks "\\\\"
            , "x"
            , quotemarks "|"
            ])
    (show (mkGConst "\"?\\/" [ "|", "x", "\\" ]))
 where
  quotemarks x = "\"" ++ x ++ "\""

testEmptyConstraintsIllegal :: Assertion
testEmptyConstraintsIllegal = do
 assertBool "empty constraints" (isLeft (testParse geniValue "?X/"))
 assertBool "empty disjunction piece" (isLeft (testParse geniValue "x|"))

testNonAlphanum :: Assertion
testNonAlphanum = do
 assertBool "dash"       (isRight (testParse geniValue "x-y-z"))
 assertBool "underscore" (isRight (testParse geniValue "x_y_z"))
 assertBool "blah"       (isRight (testParse geniValue "instance-of"))

testRecogniseSem :: Assertion
testRecogniseSem = do
 assertBool "example"    (isRight (testParse geniSemanticInput "semantics:[L24474:instance-of(Activate101114 Activate) L24475:object(Activate101114 GDP-Bound-G-Protein101111 sg3) L24476:instance-of(GDP-Bound-G-Protein101111 GDP-Bound-G-Protein)]"))

propParseableGeniVal g =
  isRight (testParse geniValue (show (g :: GeniVal)))

-- propRoundTripGeniVal :: GeniVal -> Property
propRoundTripGeniVal g =
 case testParse geniValue (show g) of
   Left  e  -> False
   Right g2 -> g2 == g

propRoundTripFeats g =
 case testParse geniFeats (showFlist g) of
   Left  e  -> False
   Right g2 -> g2 == g

propRoundTripSem g =
 case testParse geniSemanticInput semStr of
   Left  e  -> False
   Right g2@(x,_,_) -> first3 (map anonhandle) g2 == (g, [], [])
 where
   semStr = "semantics: " ++ showSem g
   anonhandle lit@(Literal h p xs) =
     case gConstraints h of
       Just [c] | isInternalHandle c -> Literal mkGAnon p xs
       _                             -> lit

testParse p = runParser (tillEof p) () ""

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False
