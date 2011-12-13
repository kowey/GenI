{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.FeatureStructures where

import Control.Monad ( liftM, liftM2 )
import GHC.Exts ( IsString(..) )
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.Test.GeniVal ( gTestStrings )
import NLP.GenI.FeatureStructures
import NLP.GenI.GeniParsers ( geniLanguageDef )
import Text.ParserCombinators.Parsec.Token ( reservedNames )

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.FeatureStructures"
  [
  ]

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

instance Arbitrary v => Arbitrary (AvPair v) where
  arbitrary = liftM2 AvPair arbitraryAtt arbitrary

arbitraryAtt =
  T.pack `liftM` elements (reservedNames geniLanguageDef ++ gTestStrings)

