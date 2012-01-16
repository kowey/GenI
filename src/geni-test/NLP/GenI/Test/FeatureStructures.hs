{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.FeatureStructures where

import Control.Arrow ( (***) )
import Control.Monad ( liftM, liftM2 )
import GHC.Exts ( IsString(..) )
import Data.Maybe (isJust)
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
import NLP.GenI.Test.GeniVal ( gTestStrings, shrinkText )
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
  shrink x = do
    v <- shrink (avVal x)
    a <- shrinkText (avAtt x)
    return (AvPair a v)

arbitraryAtt =
  T.pack `liftM` elements (reservedNames geniLanguageDef ++ gTestStrings)

shrinkFeatStruct :: Arbitrary a => FeatStruct a -> [FeatStruct a]
shrinkFeatStruct = fmap Map.fromList
                 . shrinkList shrinkPair
                 . Map.toList
 where
  shrinkPair (t, v) = do
    t2 <- shrinkText t
    v2 <- shrink v
    return (t2, v2)

-- via derive
instance (Serial a) => Serial (AvPair a) where
        series = cons2 AvPair
        coseries rs d
          = [\ t ->
               case t of
                   AvPair x1 x2 -> t0 x1 x2
             | t0 <- alts2 rs d]
