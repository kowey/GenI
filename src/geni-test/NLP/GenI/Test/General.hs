module NLP.GenI.Test.General where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( liftM2 )
import Data.Char
import Data.FullList
import Data.List ( nub, isPrefixOf )
import GHC.Exts ( IsString(..) )
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Map as Map
import Test.HUnit
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.QuickCheck hiding (collect, Failure, Property, (==>))
import Test.QuickCheck.Arbitrary
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.SmallCheck
-- import NLP.GenI.Test.SmallCheck.GeniVal as SC
import NLP.GenI.GeniVal
-- import NLP.GenI.Test.SmallCheck.General

-- ----------------------------------------------------------------------
-- Arbitrary
-- ----------------------------------------------------------------------

fullListOf :: Gen a -> Gen (FullList a)
fullListOf g = (!:) <$> g <*> listOf g

-- ----------------------------------------------------------------------
-- Serial
-- ----------------------------------------------------------------------

instance Serial T.Text where
  series = cons1 T.pack

instance (Serial a) => Serial (FullList a) where
  series = cons2 cons
    where
      cons :: a -> [a] -> FullList a
      cons = (!:)
