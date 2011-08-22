{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.GeniParsers where

import Control.Monad ( liftM2 )

import Data.List ( intercalate )
import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.FeatureStructures
import NLP.GenI.GeniVal
import NLP.GenI.GeniParsers
import NLP.GenI.Test.FeatureStructures ()
import NLP.GenI.Test.GeniVal ()

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.GeniParsers"
  [ testGroup "GeniVal"
      [ testCase     "empty constraints illegal"  testEmptyConstraintsIllegal
      , testCase     "show evil value"            testEvilGeniVal
      ]
  , testGroup "meta test mainly"
      [ testProperty "GeniVal" propParseableGeniVal
      ]
  , testGroup "round trips"
      [ testProperty "GeniVal" propRoundTripGeniVal
      , testProperty "FS"      propRoundTripFeats
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


testParse p = runParser (tillEof p) () ""

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False
