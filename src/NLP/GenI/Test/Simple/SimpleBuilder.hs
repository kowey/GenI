module NLP.GenI.Test.Simple.SimpleBuilder ( suite ) where

import Data.Maybe (isJust, isNothing)
import Data.Tree

import NLP.GenI.Simple.SimpleBuilder

import NLP.GenI.Btypes
  ( AvPair(..)
  , GNode(..)
  , GType(Foot,Other)
  )
import NLP.GenI.GeniVal ( GeniVal, mkGConst )

import NLP.GenI.Tags (
             toTagSite,
            )

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

suite :: Test.Framework.Test
suite = testGroup "simple builder"
 [ testAdjunction
 ]

testAdjunction :: Test.Framework.Test
testAdjunction =
  testGroup "adjunction"
   [ testCase "canAdjoin pos" $ assertBool "" $ isJust    $ testCanAdjoin ttGoodAux (toTagSite ttAdjNode)
   , testCase "canAdjoin neg" $ assertBool "" $ isNothing $ testCanAdjoin ttBadAux  (toTagSite ttAdjNode)
   , testCase "iapplyAdjNode pos" $ assertBool "" $ isJust    $ testIapplyAdjNode True ttGoodAux ttAdj
   , testCase "iapplyAdjNode neg" $ assertBool "" $ isNothing $ testIapplyAdjNode True ttBadAux  ttAdj
   , testCase "iapplyAdjNode neg" $ assertBool "" $ isNothing $ testIapplyAdjNode True ttAdj ttBadAux
   ]

-- testing
ttAdj :: SimpleItem
ttAdj =
  ttEmptySimpleItem { siId       = 0
                    , siRoot_    = r
                    , siAdjnodes = [r] }
  where
   r = gnname ttAdjNode

ttGoodAux :: SimpleItem
ttGoodAux =
  ttEmptySimpleItem { siId   = 0
                    , siSemantics = 1
                    , siRoot_ = gnname ttFootTop
                    , siFoot_ = Just (gnname ttFootBot) }

ttBadAux :: SimpleItem
ttBadAux =
  ttEmptySimpleItem { siId   = 0
                    , siSemantics = 1
                    , siRoot_ = gnname ttFootBot
                    , siFoot_ = Just (gnname ttFootTop) }

ttEmptySimpleItem :: SimpleItem
ttEmptySimpleItem
  = SimpleItem { siId           = 0  -- must set
               , siSubstnodes   = [] -- must set
               , siAdjnodes     = [] -- must set
               , siSemantics    = 0  -- must set
               , siPolpaths     = 1
               , siDerived      = Node "" []
               , siRoot_        = gnname ttEmptyNode
               , siFoot_        = Nothing
               , siNodes        = [ ttEmptyNode, ttAdjNode, ttFootTop, ttFootBot ]
               , siPendingTb    = []
               , siDerivation   = []
               , siGuiStuff     = testEmptySimpleGuiItem
               }
 where

ttEmptyNode :: GNode GeniVal
ttEmptyNode = GN { gnname = "empty"
                 , gup   = [ ttCat ttA_ ]
                 , gdown = [ ttCat ttA_ ]
                 , ganchor = False
                 , glexeme = []
                 , gtype   = Other
                 , gorigin = "test"
                 , gaconstr = False
                 }

ttAdjNode :: GNode GeniVal
ttAdjNode = ttEmptyNode { gnname = "testing-adjsite"
                        , gup    = [ ttCat ttA_, ttDet ttPlus_ ]
                        , gdown  = [ ttCat ttA_, ttDet ttMinus_ ]
                        }

ttFootTop :: GNode GeniVal
ttFootTop = ttEmptyNode { gnname = "testing-foot-top"
                        , gup    = [ ttCat ttA_, ttDet ttPlus_ ]
                        , gdown  = [ ttCat ttA_, ttDet ttPlus_ ]
                        , gtype  = Foot
                        }

ttFootBot :: GNode GeniVal
ttFootBot = ttEmptyNode { gnname = "testing-foot-bot"
                        , gup    = [ ttCat ttA_, ttDet ttMinus_ ]
                        , gdown  = [ ttCat ttA_, ttDet ttMinus_ ]
                        , gtype  = Foot
                        }

ttCat, ttDet  :: GeniVal -> AvPair GeniVal
ttCat = AvPair "cat"
ttDet = AvPair "det"

ttA_, ttPlus_, ttMinus_ :: GeniVal
ttA_     = mkGConst "a" []
ttPlus_  = mkGConst "+" []
ttMinus_ = mkGConst "-" []
