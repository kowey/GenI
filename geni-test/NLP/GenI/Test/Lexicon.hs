{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.Lexicon where

import Control.Applicative
import Control.Arrow ( (***) )
import Control.Monad ( liftM, liftM2 )
import GHC.Exts ( IsString(..) )
import Data.FullList ( (!:) )
import Data.Maybe (isJust)
import Data.Text ( Text )
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
import NLP.GenI.Test.GeniVal ( arbitrary1, gTestStrings )
import NLP.GenI.Test.Semantics () -- instance Arbitrary
import NLP.GenI.FeatureStructure
import NLP.GenI.Lexicon
import NLP.GenI.Parser ( geniLanguageDef )
import Text.ParserCombinators.Parsec.Token ( reservedNames )

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.Lexicon"
  [
  ]

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

instance Arbitrary ILexEntry where
  arbitrary =
    mkILexEntry
        <$> fullListOf nonEmptyStr
        <*> nonEmptyStr
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
     where
      nonEmptyStr :: Gen Text
      nonEmptyStr = elements gTestStrings
  shrink l = do
    mkILexEntry
        <$> pure (iword l)
        <*> pure (ifamname l)
        <*> shrinkList shrink (iparams l)
        <*> shrinkList shrink (iinterface l)
        <*> shrinkList shrink (ifilters l)
        <*> shrinkList shrink (iequations l)
        <*> shrinkList shrink (isemantics l)
