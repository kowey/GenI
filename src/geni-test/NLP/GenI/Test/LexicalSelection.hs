-- Copyright: 2012 Eric Kow (BSD3)
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
import NLP.GenI.Test.FeatureStructures
import NLP.GenI.Test.General ( fullListOf )
import NLP.GenI.Test.GeniVal ( arbitrary1, gTestStrings )
import NLP.GenI.Test.Semantics () -- instance Arbitrary
import NLP.GenI.FeatureStructures
import NLP.GenI.LexicalSelection
import NLP.GenI.GeniParsers ( geniLanguageDef )
import Text.ParserCombinators.Parsec.Token ( reservedNames )

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.LexicalSelection"
  [ t_parsePathEq
  ]

deriving instance Show PathEqLhs
deriving instance Show NodePathEqLhs
deriving instance Show TopBottom

t_parsePathEq :: Test.Framework.Test
t_parsePathEq = testGroup "parsePathEq"
  [ tc (PeqInterface "foo")                "interface.foo"
  , tc (PeqJust $ PeqNode "foo" Top "bar") "foo.bar"
  ]
 where
  tc res str = testCase str $ assertEqual "" res (fst . runWriter . parsePathEq $ T.pack str)
