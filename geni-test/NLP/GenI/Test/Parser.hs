{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Parser where

import Control.Monad ( liftM2 )
import Control.Applicative ( (<$>) )
import Data.FullList hiding ( (++) )
import Data.List
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.FeatureStructure
import NLP.GenI.Lexicon ( LexEntry(..) )
import NLP.GenI.General
import NLP.GenI.GeniShow
import NLP.GenI.GeniVal
import NLP.GenI.Parser
import NLP.GenI.Pretty
import NLP.GenI.Semantics
import NLP.GenI.TestSuite
import NLP.GenI.Test.FeatureStructure ()
import NLP.GenI.Test.Lexicon ()
import NLP.GenI.Test.GeniVal ()
import NLP.GenI.Test.Semantics ()

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
      , testProperty "LexEntry" propRoundTripLexEntry
      ]
  , tSuite
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

tSuite :: Test.Framework.Test
tSuite = testGroup "test suites"
    [ testCase "silly/simple" $ assertBool "" $
        isRight (testParse geniTestSuite sillySuite)
    , testCase "comma" $ assertEqual ""
        (Right ["v,runs"])
        (map tcName <$> testParse geniTestSuite commaSuite)
    ]

sillySuite :: Text
sillySuite = T.unlines
    [ "v_runs"
    , "semantics: [ name(s1 a vincent) run(e a) ]"
    ]

commaSuite :: Text
commaSuite = T.unlines
    [ "\"v,runs\""
    , "semantics: [ name(s1 a vincent) run(e a) ]"
    ]

propParseableGeniVal g =
    isRight (testParse geniValue (geniShowText (g :: GeniVal)))

propRoundTripGeniVal :: GeniVal -> Bool
propRoundTripGeniVal g =
   case testParse geniValue (geniShowText g) of
       Left  e  -> False
       Right g2 -> g2 == g

propRoundTripFeats :: Flist GeniVal -> Bool
propRoundTripFeats g =
   case testParse geniFeats (geniShowText g) of
       Left  e  -> False
       Right g2 -> g2 == g

propRoundTripSem g =
 case testParse geniSemanticInput semStr of
   Left  e  -> False
   Right g2@(x,_,_) -> first3 (map anonhandle) g2 == (g, [], [])
 where
   semStr = geniKeyword "semantics" $ geniShowText g

propRoundTripLexEntry x_ =
    whenFail (T.putStrLn $ "----\n" <> s <> "\n---\n" <> T.pack (show p)) $
    case p of
        Left  e  -> False
        Right x2 -> x2 == [x]
  where
    x = nosempols $ sortEntry x_
    s = geniShowText [x]
    p = map nosempols <$> testParse geniLexicon s
    sortEntry l = l
        { iinterface = sortFlist (iinterface l)
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

instance Eq ParseError where
   e1 == e2 = show e1 == show e2

isLeft (Left _) = True
isLeft _ = False

isRight (Right _) = True
isRight _ = False
