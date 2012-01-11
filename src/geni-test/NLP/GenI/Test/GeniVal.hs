{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Test.GeniVal where

import Control.Applicative ( (<$>) )
import Control.Monad ( liftM2 )
import Data.Char
import GHC.Exts ( IsString(..) )
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Map as Map
import Test.HUnit
import Test.QuickCheck hiding (collect, Failure)
import Test.QuickCheck.Arbitrary
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import NLP.GenI.Test.SmallCheck.GeniVal as SC
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
  , SC.suite
  ]

test_alphaconvert_simple :: Assertion
test_alphaconvert_simple =
  assertEqual "" [v1n2, v1n2] $ finaliseVars "" [v1, v2]
 where
  v1 = mkGVar "X" (Just ["x","y"])
  v2 = mkGVar "X" (Just ["y","z"])
  v1n2 = mkGVar "X" (Just ["y"])

prop_alphaconvert_idempotent :: [GeniVal] -> Bool
prop_alphaconvert_idempotent xs =
  finaliseVars "" xs2 == xs2
 where
  xs2 = finaliseVars "" xs

prop_alphaconvert_subset :: [GeniVal] -> Bool
prop_alphaconvert_subset gs_ =
  and $ zipWith csubset gs2 gs
 where
  gs  = gs_ ++ gs_ -- duplicate gs_ so that we don't trigger singleton
                   -- anonymisation
  gs2 = finaliseVars "" gs
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
   x = finaliseVars "" x_

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
  let (TestPair x y) = finaliseVars "" (TestPair x_ y_)
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
   (x, y) = case finaliseVars "" [ x_, y_ ] of
             [n1,n2] -> (n1,n2)
             _ -> error "huh? finaliseVars length mismatch"

prop_subsume_transitive :: GeniVal -> GeniVal -> GeniVal -> Property
prop_subsume_transitive x_ y_ z_ =
 all qc_not_empty_GVar [ x, y, z ] && tt_subsumes x y && tt_subsumes y z ==>
   tt_subsumes x z
 where
   (x, y, z) = case finaliseVars "" [ x_, y_, z_ ] of
                [n1,n2,n3] -> (n1,n2,n3)
                _ -> error "huh? finaliseVars length mismatch"

unificationSuccesful :: UnificationResult -> Bool
unificationSuccesful Failure = False
unificationSuccesful _ = True

tt_subsumes :: GeniVal -> GeniVal -> Bool
tt_subsumes x y = unificationSuccesful (subsumeOne x y)

tt_equiv :: GeniVal -> GeniVal -> Bool
tt_equiv (gConstraints -> xc) (gConstraints -> yc) =
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
  fromString = mkGConstNone . T.pack

-- Definition of Arbitrary GeniVal for QuickCheck
newtype PrintString = PrintString { fromPrintString :: String }
newtype GTestString = GTestString { fromGTestString :: String }
newtype GTestString2 = GTestString2 { fromGTestString2 :: String }
data TestPair = TestPair [GeniVal] [GeniVal]

instance Collectable TestPair where
  collect (TestPair x y) = collect x . collect y

instance DescendGeniVal TestPair where
  descendGeniVal f (TestPair x y) = TestPair (descendGeniVal f x) (descendGeniVal f y)

instance Arbitrary PrintString where
  arbitrary = PrintString `fmap` (listOf1 (arbitrary `suchThat` isPrint))

instance Arbitrary GTestString where
  arbitrary = GTestString `fmap` elements gTestStrings

gTestStrings :: [String]
gTestStrings =
    [ "a", "apple" , "b", "banana", "c", "carrot", "d", "durian"
    , "e", "eggplant", "f", "fennel" , "g", "grape" ]

instance Arbitrary GTestString2 where
  arbitrary =
    oneof $ map (return . GTestString2) $
    [ "X", "Y", "Z", "H", "I", "J", "P", "Q", "R", "S", "T", "U"  ]

instance Arbitrary GeniVal where
  arbitrary = oneof [ arbitraryGConst, arbitraryGVar, return mkGAnon ]
  shrink g  = do
    label       <- shrink (gLabel g)
    constraints <- shrinkMaybe (shrinkList shrinkText) (gConstraints g)
    return $ g { gLabel       = label
               , gConstraints = constraints
               }

shrinkText :: T.Text -> [T.Text]
shrinkText = map T.pack . shrinkList2 shrink . T.unpack

shrinkMaybe :: (a -> [a]) -> Maybe a -> [Maybe a]
shrinkMaybe shr Nothing  = []
shrinkMaybe shr (Just x) = Nothing : map Just (shr x)

arbitraryGConst :: Gen GeniVal
arbitraryGConst = liftM2 mkGConst (T.pack . fromPrintString <$> arbitrary)
                                  (map (T.pack . fromPrintString) <$> arbitrary)

arbitraryGVar :: Gen GeniVal
arbitraryGVar = liftM2 mkGVar (fromGTestString2 `fmap` arbitrary)
                              (fmap (map (T.pack . fromPrintString) . fromList1) `fmap` arbitrary)


-- | a small subset of GeniVal for some more elaborate tests
newtype GeniValLite = GeniValLite { fromGeniValLite :: GeniVal }

instance Arbitrary GeniValLite where
  arbitrary = GeniValLite `fmap`
                oneof [ liftM2 mkGConst astr (listOf astr)
                      , liftM2 mkGVar (fromGTestString2 `fmap` arbitrary) (maybeOf (listOf1 astr))
                      , return mkGAnon
                      ]
    where
     astr   = T.pack . fromGTestString <$> arbitrary

instance Show GeniValLite where
  show = show . fromGeniValLite

instance Collectable GeniValLite where
  collect = collect . fromGeniValLite

instance DescendGeniVal GeniValLite where
  descendGeniVal f (GeniValLite g) = GeniValLite (descendGeniVal f g)

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = frequency [(1, return Nothing), (3, fmap Just g)]  -- stolen from instance Arbitrary

data List1 a = List1 { fromList1 :: [a] }

instance Arbitrary a => Arbitrary (List1 a) where
  arbitrary = List1 `fmap` arbitrary1

arbitrary1 :: Arbitrary a => Gen [a]
arbitrary1 = listOf1 arbitrary

-- ----------------------------------------------------------------------
-- shrinkList 2
-- ----------------------------------------------------------------------

-- | This is more aggressive than shrinkList in the sense that when
--   shrinkList is faced with a 1000 elements, it first tries removing
--   1000, 500, 250.. elements
--
--   This version instead goes down to
--   1000, 998, 994, 984, 968.., 512 (and then resumes the basic behaviour)
shrinkList2 :: (a -> [a]) -> [a] -> [[a]]
shrinkList2 shr xs =
      concat [ removes k n xs | k <- ks ]
   ++ shrinkOne xs
 where
  n      = length xs
  ks     = newks ++ drop 2 oldks
  newks  = [ n - (2 ^ p) | p <- [0 .. log2_n - 1] ]
  oldks  = [ k | k <- takeWhile (>0) (iterate (`div`2) n) ]
  log2_n = floor . logBase 2 . fromIntegral $ n
  
  shrinkOne []     = []
  shrinkOne (x:xs) = [ x':xs | x'  <- shr x ]
                  ++ [ x:xs' | xs' <- shrinkOne xs ] 
  
  removes k n xs
      | k > n     = []
      | null xs2  = [[]]
      | otherwise = xs2 : map (xs1 ++) (removes k (n-k) xs2)
   where
      xs1 = take k xs
      xs2 = drop k xs
-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

instance Serial GeniVal where
  series   =  cons0 mkGAnon
           \/ cons1 mkGConst
           \/ cons2 mkGVar
  -- meh, I'd rather be forced to pattern match in case the type of GeniVal
  -- changes in the future, but I also don't want to import the constructor
  coseries rs d = [ \g -> f (gLabel g) (gConstraints g) | f <- alts2 rs d ]
