{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}
module NLP.GenI.Test.Semantics ( suite ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first )
import Data.Maybe ( isJust, maybeToList )
import qualified Data.Map as Map

import NLP.GenI.Semantics
import NLP.GenI.GeniVal
import NLP.GenI.Test.GeniVal hiding (  suite )

import Test.HUnit
-- import Test.QuickCheck hiding (collect)
import Test.QuickCheck ( suchThat )
import Test.QuickCheck.Arbitrary
import Test.SmallCheck
import qualified Test.SmallCheck as SmallCheck
import Test.SmallCheck.Series
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.SmallCheck
-- import Test.Framework.Providers.QuickCheck2

-- ----------------------------------------------------------------------
-- Testing
-- ----------------------------------------------------------------------

suite :: Test.Framework.Test
suite = testGroup "NLP.GenI.Semantics"
 [ testGroup "subsumeLiteral"
     [ testProperty "reflexive"     prop_subsumePred_reflexive
     , testProperty "antisymmetric" prop_subsumePred_antisymmetric
     ]
 , testGroup "subsumeSem"
     [ testProperty "reflexive"    prop_subsumeSem_reflexive
     , testProperty "only return matching portion" prop_subsumeSem_length
     , testCase "works 1"  $ assertBool "" $ not . null $ sem1 `subsumeSem` sem2
     , testCase "works 2"  $ assertBool "" $ not . null $ sem1 `subsumeSem` (sem2 ++ sem2)
     , testCase "distinct" $ assertBool "" $ null $ (sem1 ++ sem1) `subsumeSem` sem2
     ]
 , testGroup "unifySem"
     [ testCase "works x"    $ assertMatchSem [ sem_x  ] $ unifySem sem1 sem_x
     , testCase "works xy"   $ assertMatchSem [ sem_xy ] $ unifySem sem_x sem_y
     , testCase "works xV"   $ assertMatchSem [ sem_xy, sem_xy ] $ unifySem sem1 sem_xy
     ]
{-
     [ testProperty "reflexive"     prop_unifyPred_reflexive
     , testProperty "antisymmetric" prop_unifyPred_antisymmetric
     ]
-}
 ]
 where
  assertMatchSem sems xs = assertEqual "" (map sortSem sems) $ map fst xs
  sem1  = [ lit1 ]
  sem2  = sem_x
  sem_x  = [ lit_x ]
  sem_y  = [ lit_y ]
  sem_xy = [ lit_x, lit_y ]
  lit1 =  Literal (mkGConstNone "a") (mkGConstNone "apple") [mkGVarNone "A"]
  lit_x = Literal (mkGConstNone "a") (mkGConstNone "apple") [mkGConstNone "x"]
  lit_y = Literal (mkGConstNone "a") (mkGConstNone "apple") [mkGConstNone "y"]

prop_subsumeSem_length :: [GTestLiteral] -> [GTestLiteral] -> SmallCheck.Property
prop_subsumeSem_length lits1 lits2 =
  not (null sboth) ==>
    all (\x -> length (fst x) == s1_len) sboth
 where
  sboth = s1 `subsumeSem` s2
  s1_len = length s1
  s1 = map fromGTestLiteral lits1
  s2 = map fromGTestLiteral lits2

prop_subsumeSem_reflexive :: [GTestLiteral] -> Bool
prop_subsumeSem_reflexive lits = not . null $ s `subsumeSem` s
 where
  s = map fromGTestLiteral lits

prop_subsumePred_reflexive :: GTestLiteral -> Bool
prop_subsumePred_reflexive pr = s `ttSubsumeLiteral` s
 where
  s = fromGTestLiteral pr

prop_subsumePred_antisymmetric :: SubsumedPair GTestLiteral -> Bool
prop_subsumePred_antisymmetric (SubsumedPair x_ y_) =
   x `tt_literal_equiv` y || not (y `ttSubsumeLiteral` x)
 where
   x = fromGTestLiteral x_
   y = fromGTestLiteral y_

class Subsumable a where
  subsume :: a -> a -> [(a, Subst)]

instance Subsumable GeniVal where
  subsume x y = fromUnificationResult (x `subsumeOne` y)

instance Subsumable GeniValLite where
  subsume x y = map (first GeniValLite) . fromUnificationResult
              $ fromGeniValLite x `subsumeOne` fromGeniValLite y

instance Subsumable Literal where
  subsume x y = maybeToList (x `subsumeLiteral` y)

instance Subsumable GTestLiteral where
  subsume x y = map (first tp) . maybeToList
              $ fromGTestLiteral x `subsumeLiteral` fromGTestLiteral y
   where
    tp (Literal x y z) = GTestLiteral x y z

instance (Arbitrary a, Show a, Subsumable a) => Show (SubsumedPair a) where
  show (SubsumedPair x y) = show (x,y)

data (Subsumable a, Arbitrary a) => SubsumedPair a = SubsumedPair a a

unzipSubsumedPair :: (Subsumable a, Arbitrary a) => [SubsumedPair a] -> ([a],[a])
unzipSubsumedPair = unzip . map helper
  where
   helper (SubsumedPair x y) = (x,y)

instance (Show a, DescendGeniVal a, Collectable a, Subsumable a, Arbitrary a) => Arbitrary (SubsumedPair a) where
  arbitrary = do
    x <-  finaliseVars "-1" `fmap` arbitrary
    y <- (finaliseVars "-2" `fmap` arbitrary) `suchThat` (\y -> not (null (x `subsume` y)))
    return (SubsumedPair x y)

instance Arbitrary (SubsumedPair GTestLiteral) where
  arbitrary = do
    (SubsumedPair h1 h2) <- arbitrary
    (SubsumedPair p1 p2) <- arbitrary
    (args1, args2) <- unzipSubsumedPair `fmap` arbitrary
    return $ SubsumedPair (mkPred h1 p1 args1) (mkPred h2 p2 args2)
   where
    mkPred x y zs = GTestLiteral (fromGeniValLite x)
                              (fromGeniValLite y)
                              (map fromGeniValLite zs)

isSuccess :: UnificationResult -> Bool
isSuccess NLP.GenI.GeniVal.Failure = False
isSuccess (SuccessSans _)     = True
isSuccess (SuccessRep  _ _)   = True
isSuccess (SuccessRep2 _ _ _) = True

fromUnificationResult :: UnificationResult -> [(GeniVal,Subst)]
fromUnificationResult NLP.GenI.GeniVal.Failure = []
fromUnificationResult (SuccessSans g)  = [(g, Map.empty)]
fromUnificationResult (SuccessRep v g) = [(g, Map.fromList [(v,g)])]
fromUnificationResult (SuccessRep2 v1 v2 g) = [(g, Map.fromList [(v1,g),(v2,g)])]

ttSubsumeLiteral :: Literal -> Literal -> Bool
ttSubsumeLiteral x y = isJust (subsumeLiteral x y)

tt_literal_equiv :: Literal -> Literal -> Bool
tt_literal_equiv (Literal h1 p1 as1) (Literal h2 p2 as2) =
  and $ zipWith tt_equiv (h1 : p1 : as1) (h2 : p2 : as2)

fromGTestLiteral :: GTestLiteral -> Literal
fromGTestLiteral (GTestLiteral h r as) = Literal h r as

data GTestLiteral = GTestLiteral GeniVal GeniVal [GeniVal]

instance Arbitrary Literal where
  arbitrary = Literal <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
  shrink l = Literal <$> shrink (lHandle l)
                     <*> shrink (lPredicate l)
                     <*> shrinkList2 shrink (lArgs l)

instance Show GTestLiteral where
  show = showLiteral . fromGTestLiteral

instance Arbitrary GTestLiteral where
 arbitrary =
  do handle <- arbitraryGConst
     rel  <- arbitrary
     args <- arbitrary
     return $ GTestLiteral handle (fromGeniValLite rel) (map fromGeniValLite args)

-- ----------------------------------------------------------------------
-- SmallCheck
-- ----------------------------------------------------------------------

instance Serial Literal where
        series = cons3 Literal
        coseries rs d
          = [\ t ->
               case t of
                   Literal x1 x2 x3 -> t0 x1 x2 x3
             | t0 <- alts3 rs d]

{-!
deriving instance (Arbitrary a, Serial a) => Serial (SubsumedPair a)
deriving instance Serial GTestLiteral
!-}
-- GENERATED START

 
instance (Arbitrary a, Subsumable a, Serial a) => Serial (SubsumedPair a) where
        series = cons2 SubsumedPair
        coseries rs d
          = [\ t ->
               case t of
                   SubsumedPair x1 x2 -> t0 x1 x2
             | t0 <- alts2 rs d]

 
instance Serial GTestLiteral where
        series = cons3 GTestLiteral
        coseries rs d
          = [\ t ->
               case t of
                   GTestLiteral x1 x2 x3 -> t0 x1 x2 x3
             | t0 <- alts3 rs d]
-- GENERATED STOP
