{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.SmallCheck.GeniVal (suite) where

import Control.Applicative
import Data.Char
import Data.FullList
import Data.List ( nub )
import GHC.Exts ( IsString(..) )
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Map as Map
import Test.HUnit
import Test.SmallCheck hiding ( (==>) )
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series

import NLP.GenI.GeniVal

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.GeniVal (SC)"
  [
    testProperty "unify self" prop_unify_self
  ]

-- | Unifying something with itself should always succeed
prop_unify_self :: GeniVal -> Bool
prop_unify_self x_ =
   case unify [x] [x] of
     Nothing  -> False
     Just unf -> fst unf == [x]
 where
   x = finaliseVars "" x_


-- ----------------------------------------------------------------------
-- serial
-- ----------------------------------------------------------------------

instance Serial GeniVal where
  series = cons2 GeniVal

instance Serial T.Text where
  series = cons1 T.pack

instance (Serial a) => Serial (FullList a) where
  series = cons2 cons
    where
      cons :: a -> [a] -> FullList a
      cons = (!:)
