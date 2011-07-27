{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.GeniVal where

import Control.Monad ( liftM2 )
import GHC.Exts ( IsString(..) )
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import NLP.GenI.GeniVal

suite :: Test.Framework.Test
suite =
 testGroup "NLP.GenI.GeniVal"
  [ testGroup "alphaconvert"
      [ testCase "simple example" test_alphaconvert_simple
      , testProperty "constraints are subset of original" prop_alphaconvert_subset
      , testProperty "idempotent sans suffix" prop_alphaconvert_idempotent ]
  , testGroup "unification"
      [ testProperty "self" prop_unify_self
      , testProperty "anonymous variables" prop_unify_anon
      , testProperty "symmetry" prop_unify_sym
      , testBackPropagation
      ]
  , testGroup "subsumption"
     [ testProperty "subsumeOne reflexive"
         (\x -> qc_not_empty_GVar x ==> tt_subsumes x x)
     , testProperty "subsumeOne antisymmetric" prop_subsume_antisymmetric
     , testProperty "subsumeOne transitive"    prop_subsume_transitive
     ]
  ]

test_alphaconvert_simple :: Assertion
test_alphaconvert_simple =
  assertEqual "" [v1n2, v1n2] $ alphaConvert "" [v1, v2]
 where
  v1 = mkGVar "X" (Just ["x","y"])
  v2 = mkGVar "X" (Just ["y","z"])
  v1n2 = mkGVar "X" (Just ["y"])

prop_alphaconvert_idempotent :: [GeniVal] -> Bool
prop_alphaconvert_idempotent xs =
  alphaConvert "" xs2 == xs2
 where
  xs2 = alphaConvert "" xs

prop_alphaconvert_subset :: [GeniVal] -> Bool
prop_alphaconvert_subset gs =
  and $ zipWith csubset gs2 gs
 where
  gs2 = alphaConvert "" gs
  csubset x y = csubsetH (gConstraints x) (gConstraints y)
  csubsetH Nothing Nothing     = True
  csubsetH Nothing (Just _)    = False
  csubsetH (Just _) Nothing    = True
  csubsetH (Just xs) (Just ys) = all (`elem` ys) xs

-- | Unifying something with itself should always succeed
prop_unify_self :: [GeniVal] -> Property
prop_unify_self x_ =
 all qc_not_empty_GVar x ==>
   case unify x x of
     Nothing  -> False
     Just unf -> fst unf == x
 where
   x = alphaConvert "" x_

-- | Unifying something with only anonymous variables should succeed and return
--   the same result.
prop_unify_anon :: [GeniVal] -> Bool
prop_unify_anon x =
  case unify x y of
    Nothing  -> False
    Just unf -> fst unf == x
  where --
    y  = replicate (length x) mkGAnon

-- | Unification should be symmetrical.  We can't guarantee these if there
--   are cases where there are variables in the same place on both sides, so we
--   normalise the sides so that this doesn't happen.
prop_unify_sym :: [GeniVal] -> [GeniVal] -> Property
prop_unify_sym x_ y_ =
  let (TestPair x y) = alphaConvert "" (TestPair x_ y_)
      u1 = (unify x y) :: Maybe ([GeniVal],Subst)
      u2 = unify y x
  in all qc_not_empty_GVar x && all qc_not_empty_GVar y ==> u1 == u2

-- | Unifying something with the empty list should always succeed
prop_unify_empty :: [GeniVal] -> Bool
prop_unify_empty x = isJust (unify x [])

prop_subsume_antisymmetric :: GeniVal -> GeniVal -> Property
prop_subsume_antisymmetric x_ y_ =
 all qc_not_empty_GVar [ x, y ] && tt_subsumes x y ==>
   x `tt_equiv` y || not (tt_subsumes y x)
 where
   (x, y) = case alphaConvert "" [ x_, y_ ] of
             [n1,n2] -> (n1,n2)
             _ -> error "huh? alphaConvert length mismatch"

prop_subsume_transitive :: GeniVal -> GeniVal -> GeniVal -> Property
prop_subsume_transitive x_ y_ z_ =
 all qc_not_empty_GVar [ x, y, z ] && tt_subsumes x y && tt_subsumes y z ==>
   tt_subsumes x z
 where
   (x, y, z) = case alphaConvert "" [ x_, y_, z_ ] of
                [n1,n2,n3] -> (n1,n2,n3)
                _ -> error "huh? alphaConvert length mismatch"

unificationSuccesful :: UnificationResult -> Bool
unificationSuccesful Failure = False
unificationSuccesful _ = True

tt_subsumes :: GeniVal -> GeniVal -> Bool
tt_subsumes x y = unificationSuccesful (subsumeOne x y)

tt_equiv :: GeniVal -> GeniVal -> Bool
tt_equiv (GeniVal _ xc) (GeniVal _ yc) =
 case (xc, yc) of
   (Just xs, Just ys) -> xs == ys
   (Just _, Nothing)  -> False
   (Nothing, Just _)  -> False
   (Nothing, Nothing) -> True

testBackPropagation :: Test.Framework.Test
testBackPropagation =
  testGroup "back propagation"
   [ testCase "unify left/right" $ assertEqual "" expected $ unify left right
   , testCase "unify right/left" $ assertEqual "" expected $ unify right left
   ]
 where
  n = 3
  cx = mkGConst "X" []
  leftStrs = map show [1..n]
  left  = map (flip mkGVar Nothing) leftStrs
  right = drop 1 left ++ [cx]
  expected = Just (expectedResult, expectedSubst)
  expectedResult = replicate n cx
  expectedSubst  = Map.fromList $ zip leftStrs (repeat cx)

qc_not_empty_GVar :: GeniVal -> Bool
qc_not_empty_GVar (GeniVal (Just _) (Just [])) = False
qc_not_empty_GVar _ = True

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

-- for more convenient testing
instance IsString GeniVal where
  fromString = mkGConstNone

-- Definition of Arbitrary GeniVal for QuickCheck
newtype GTestString = GTestString String
newtype GTestString2 = GTestString2 String
data TestPair = TestPair [GeniVal] [GeniVal]

instance Collectable TestPair where
  collect (TestPair x y) = collect x . collect y

instance DescendGeniVal TestPair where
  descendGeniVal f (TestPair x y) = TestPair (descendGeniVal f x) (descendGeniVal f y)

fromGTestString :: GTestString -> String
fromGTestString (GTestString s) = s

fromGTestString2 :: GTestString2 -> String
fromGTestString2 (GTestString2 s) = s

instance Arbitrary GTestString where
  arbitrary =
    oneof $ map (return . GTestString) $
    [ "a", "apple" , "b", "banana", "c", "carrot", "d", "durian"
    , "e", "eggplant", "f", "fennel" , "g", "grape" ]

instance Arbitrary GTestString2 where
  arbitrary =
    oneof $ map (return . GTestString2) $
    [ "X", "Y", "Z", "H", "I", "J", "P", "Q", "R", "S", "T", "U"  ]

instance Arbitrary GeniVal where
  arbitrary = oneof [ arbitraryGConst, arbitraryGVar, return mkGAnon ]

arbitraryGConst :: Gen GeniVal
arbitraryGConst = liftM2 mkGConst (fromGTestString `fmap` arbitrary)
                                  (map fromGTestString `fmap` arbitrary)

arbitraryGVar :: Gen GeniVal
arbitraryGVar = liftM2 mkGVar (fromGTestString2 `fmap` arbitrary)
                              (fmap (map fromGTestString . fromList1) `fmap` arbitrary)

data List1 a = List1 { fromList1 :: [a] }

instance Arbitrary a => Arbitrary (List1 a) where
  arbitrary = List1 `fmap` arbitrary1

arbitrary1 :: Arbitrary a => Gen [a]
arbitrary1 = sized (\n -> choose (1,n+1) >>= vector)
