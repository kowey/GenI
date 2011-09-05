{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}
module NLP.GenI.Test.Semantics ( suite ) where

import Control.Arrow ( first )
import Data.Maybe ( isJust, maybeToList )
import qualified Data.Map as Map

import NLP.GenI.Semantics
import NLP.GenI.GeniVal
import NLP.GenI.Test.GeniVal hiding (  suite )

import Test.HUnit
import Test.QuickCheck hiding (collect)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

-- ----------------------------------------------------------------------
-- Testing
-- ----------------------------------------------------------------------

suite :: Test.Framework.Test
suite = testGroup "NLP.GenI.Semantics"
 [ testGroup "subsumePred"
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
  lit1 = (mkGConst "a" [], mkGConst "apple" [], [mkGVar "A" Nothing])
  lit_x = (mkGConst "a" [], mkGConst "apple" [], [mkGConst "x" []])
  lit_y = (mkGConst "a" [], mkGConst "apple" [], [mkGConst "y" []])

prop_subsumeSem_length :: [GTestPred] -> [GTestPred] -> Property
prop_subsumeSem_length lits1 lits2 =
  all qc_not_empty_GVar_Pred s1 && all qc_not_empty_GVar_Pred s2 && not (null sboth) ==>
    all (\x -> length (fst x) == s1_len) sboth
 where
  sboth = s1 `subsumeSem` s2
  s1_len = length s1
  s1 = alphaConvert "-1" $ map fromGTestPred lits1
  s2 = alphaConvert "-2" $ map fromGTestPred lits2

prop_subsumeSem_reflexive :: [GTestPred] -> Property
prop_subsumeSem_reflexive lits =
  not (null s) && all qc_not_empty_GVar_Pred s ==> not . null $ s `subsumeSem` s
 where
  s = alphaConvert "" $ map fromGTestPred lits

prop_subsumePred_reflexive :: GTestPred -> Property
prop_subsumePred_reflexive pr =
  qc_not_empty_GVar_Pred s ==> s `tt_subsumePred` s
 where
  s = alphaConvert "" (fromGTestPred pr)

prop_subsumePred_antisymmetric :: SubsumedPair GTestPred -> Bool
prop_subsumePred_antisymmetric (SubsumedPair x_ y_) =
   x `tt_pred_equiv` y || not (y `tt_subsumePred` x)
 where
   x = fromGTestPred x_
   y = fromGTestPred y_

class Subsumable a where
  subsume :: a -> a -> [(a, Subst)]

instance Subsumable GeniVal where
  subsume x y = fromUnificationResult (x `subsumeOne` y)

instance Subsumable GeniValLite where
  subsume x y = map (first GeniValLite) . fromUnificationResult
              $ fromGeniValLite x `subsumeOne` fromGeniValLite y

instance Subsumable Pred where
  subsume x y = maybeToList (x `subsumePred` y)

instance Subsumable GTestPred where
  subsume x y = map (first tp) . maybeToList
              $ fromGTestPred x `subsumePred` fromGTestPred y
   where
    tp (x,y,z) = GTestPred x y z

instance (Arbitrary a, Show a, Subsumable a) => Show (SubsumedPair a) where
  show (SubsumedPair x y) = show (x,y)

data (Subsumable a, Arbitrary a) => SubsumedPair a = SubsumedPair a a

unzipSubsumedPair :: (Subsumable a, Arbitrary a) => [SubsumedPair a] -> ([a],[a])
unzipSubsumedPair = unzip . map helper
  where
   helper (SubsumedPair x y) = (x,y)

instance (Show a, DescendGeniVal a, Collectable a, Subsumable a, Arbitrary a) => Arbitrary (SubsumedPair a) where
  arbitrary = do
    x <-  alphaConvert "-1" `fmap` arbitrary
    y <- (alphaConvert "-2" `fmap` arbitrary) `suchThat` (\y -> not (null (x `subsume` y)))
    return (SubsumedPair x y)

instance Arbitrary (SubsumedPair GTestPred) where
  arbitrary = do
    (SubsumedPair h1 h2) <- arbitrary
    (SubsumedPair p1 p2) <- arbitrary
    (args1, args2) <- unzipSubsumedPair `fmap` arbitrary
    return $ SubsumedPair (mkPred h1 p1 args1) (mkPred h2 p2 args2)
   where
    mkPred x y zs = GTestPred (fromGeniValLite x)
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

tt_subsumePred :: Pred -> Pred -> Bool
tt_subsumePred x y = isJust (subsumePred x y)

tt_pred_equiv :: (GeniVal, GeniVal, [GeniVal]) -> (GeniVal, GeniVal, [GeniVal]) -> Bool
tt_pred_equiv (h1,p1,as1) (h2,p2,as2) =
  and $ zipWith tt_equiv (h1 : p1 : as1) (h2 : p2 : as2)

qc_not_empty_GVar_Pred :: Pred -> Bool
qc_not_empty_GVar_Pred (h,r,as) = all qc_not_empty_GVar (h:r:as)

fromGTestPred :: GTestPred -> (GeniVal, GeniVal, [GeniVal])
fromGTestPred (GTestPred h r as) = (h,r,as)

data GTestPred = GTestPred GeniVal GeniVal [GeniVal]

instance Show GTestPred where
  show = showPred . fromGTestPred

instance Arbitrary GTestPred where
 arbitrary =
  do handle <- arbitraryGConst
     rel  <- arbitrary
     args <- arbitrary
     return $ GTestPred handle rel args
