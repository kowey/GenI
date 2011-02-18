module NLP.GenI.Test.Semantics ( suite ) where

import Data.Maybe ( isJust )

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

prop_subsumePred_antisymmetric :: GTestPred -> GTestPred -> Property
prop_subsumePred_antisymmetric x_ y_ =
 all qc_not_empty_GVar_Pred [ x, y ] && x `tt_subsumePred` y ==>
   x `tt_pred_equiv` y || not (y `tt_subsumePred` x)
 where
   x = alphaConvert "-1" (fromGTestPred x_)
   y = alphaConvert "-2" (fromGTestPred y_)

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
