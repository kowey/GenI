{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Parser where

import Control.Monad ( liftM2 )

import Control.Applicative ( (<$>) )
import Data.FullList hiding ( (++) )
import Data.List
import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.FeatureStructures
import NLP.GenI.Lexicon ( ILexEntry(..) )
import NLP.GenI.General
import NLP.GenI.GeniShow
import NLP.GenI.GeniVal
import NLP.GenI.Parser
import NLP.GenI.Pretty
import NLP.GenI.Semantics
import NLP.GenI.Test.FeatureStructures ()
import NLP.GenI.Test.Lexicon ()
import NLP.GenI.Test.GeniVal ()
import NLP.GenI.Test.Semantics ()

instance Show (AvPair GeniVal) where
    show = geniShow

instance Show ILexEntry where
    show = geniShow


suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.Parser"
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
      , testProperty "ILexEntry" propRoundTripILexEntry
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
    (show (mkGConst ("\"?\\/" !: [ "|", "x", "\\" ])))
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

propRoundTripFeats :: Flist GeniVal -> Bool
propRoundTripFeats g =
 case testParse geniFeats (geniShow g) of
   Left  e  -> False
   Right g2 -> g2 == g

propRoundTripSem g =
 case testParse geniSemanticInput semStr of
   Left  e  -> False
   Right g2@(x,_,_) -> first3 (map anonhandle) g2 == (g, [], [])
 where
   semStr = "semantics: " ++ geniShow g

propRoundTripILexEntry x_ =
 whenFail (putStrLn $ "----\n" ++ s  ++ "\n---\n" ++ show p) $
 case p of
   Left  e  -> False
   Right x2 -> x2 == [x]
 where
  x = sortEntry x_
  s = geniShow [x]
  p = map nosempols <$> testParse geniLexicon s
  sortEntry l = l { iinterface = sortFlist (iinterface l)
                  , ifilters   = sortFlist (ifilters l)
                  , iequations = sortFlist (iequations l)
                  , isemantics = sortSem (isemantics l)
                  }
  nosempols l = l { isempols = [] }

anonhandle lit@(Literal h p xs) =
     case singletonVal h of
       Just c | isInternalHandle c -> Literal mkGAnon p xs
       _                           -> lit

testParse p = runParser (tillEof p) () ""

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False
