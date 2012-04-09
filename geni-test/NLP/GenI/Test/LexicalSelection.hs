-- Copyright: 2012 Eric Kow (BSD3)
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
module NLP.GenI.Test.LexicalSelection where

import Control.Applicative
import Control.Arrow ( (***) )
import Control.Monad ( liftM, liftM2 )
import Control.Monad.Writer
import GHC.Exts ( IsString(..) )
import Data.FullList ( (!:) )
import Data.Maybe (isJust)
import Data.Text ( Text )
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T

import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.QuickCheck.Arbitrary
import Test.SmallCheck.Series
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.GeniVal
import NLP.GenI.Test.FeatureStructure
import NLP.GenI.Test.General ( fullListOf )
import NLP.GenI.Test.GeniVal ( arbitrary1, arbitraryText1, gTestStrings )
import NLP.GenI.Test.Semantics () -- instance Arbitrary
import NLP.GenI.Test.TreeSchema
import NLP.GenI.FeatureStructure
import NLP.GenI.LexicalSelection
import NLP.GenI.LexicalSelection.Types
import NLP.GenI.Parser ( geniLanguageDef )
import NLP.GenI.TreeSchema
import Text.ParserCombinators.Parsec.Token ( reservedNames )

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.LexicalSelection"
  [ t_parsePathEq
  , t_maybeEnrichBy
  ]

t_parsePathEq :: Test.Framework.Test
t_parsePathEq = testGroup "parsePathEq"
  [ tc (PeqInterface "foo")                "interface.foo"
  , tc (PeqJust $ PeqFeat "foo" Top "bar") "foo.bar"
  , testProperty "roundtrip" prop_parsePathEq_roundTrip
  ]
 where
  tc res str = testCase str $ assertEqual "" res (fst . runWriter . parsePathEq $ T.pack str)

t_maybeEnrichBy = testGroup "maybeEnrinchBy"
  [ tc (Just tB1) (PeqFeat "r"      Bottom "x", "y")
  , tc (Just tB2) (PeqFeat "r"      Top    "x", "y")
  , tc (Just tC)  (PeqFeat "anchor" Bottom "z", "a1")
  , tc (Just tD)  (PeqLex  "b1", "quux")
  , tc Nothing    (PeqLex  "b1", mkGVarNone "X")
  ]
 where
  tc res eq@(eqLhs, v) =
      testCase ((T.unpack . showPathEqLhs $ PeqJust eqLhs) ++ ":" ++ show v)
      $ assertEqual "" res (fmap fst $ maybeEnrichBy t eq)
  --
  tB1 = t { tree = mkT rB1 s1  l  s2 }
  tB2 = t { tree = mkT rB2 s1  l  s2 }
  tC  = t { tree = mkT r   s1  lC s2 }
  tD  = t { tree = mkT r   s1D l  s2 }

  rB1 = r { gdown = [ idxAv "r", AvPair "x" "y" ] }
  rB2 = r { gup   = [ catAv "a", idxAv "rbad", AvPair "x" "y" ] }
  lC  = l { gdown = [ AvPair "z" "a1" ] }
  s1D = s1 { glexeme = [ "quux" ] }
  --
  t = TT { params  = []
         , pfamily = "fam"
         , pidname = "nom"
         , pinterface = [ AvPair "z" (mkGVarNone "Z") ]
         , psemantics = Nothing
         , ptype  = Initial
         , ptrace = []
         , tree   = mkT r s1 l s2
         }
  mkT xr xs1 xl xs2 = toSchemaNode <$> Node xr [ Node xs1 [], Node xl [], Node xs2 [] ]
  r = emptyGN { gnname = "r"
              , gup    = [ catAv "a", idxAv "rbad" ]
              , gdown  = [ idxAv "r" ] }
  s x = emptyGN
    { gnname = x
    , gup    = [ catAv "b", idxAv "s" ]
    , gdown  = [ idxAv "sbad" ]
    , gtype  = Subs
    }
  s1  = s "b1"
  s2  = s "b2"
  l = sillyLexNode

-- | There are some assumptions here baked into the Arbitrary instances:
--    - non-empty names
--    - no dots in paths
prop_parsePathEq_roundTrip :: PathEqLhs -> Bool
prop_parsePathEq_roundTrip eq = eq == eq2
 where
  str = showPathEqLhs eq
  eq2 = fst . runWriter . parsePathEq $ str

instance Arbitrary TopBottom where
  arbitrary = elements [ Top, Bottom ]

instance Arbitrary NodePathEqLhs where
  arbitrary = PeqFeat <$> nodeName <*> arbitrary <*> attribute
   where
    nodeName  = dotless <$> arbitraryText1
    attribute = dotless <$> arbitraryText1

dotless :: Text -> Text
dotless t | t == "."  = "x"
          | otherwise = T.filter (/= '.') t

instance Arbitrary PathEqLhs where
  arbitrary = oneof [ (PeqInterface . dotless) <$> arbitraryText1
                    , (PeqUnknown   . dotless) <$> arbitraryText1
                    , PeqJust      <$> arbitrary
                    ]
