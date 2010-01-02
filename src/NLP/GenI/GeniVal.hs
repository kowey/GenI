-- GenI surface realiser
-- Copyright (C) 2009 Eric Kow
-- Copyright (C) 2005 Carlos Areces
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# LANGUAGE OverlappingInstances, FlexibleInstances, DeriveDataTypeable #-}
module NLP.GenI.GeniVal where

-- import Debug.Trace -- for test stuff
import Control.Arrow (first, (***))
import Control.Monad (liftM, liftM2)
import Data.List
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit
import Test.QuickCheck hiding (collect)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck

import Data.Generics.PlateDirect

import Control.Parallel.Strategies

import NLP.GenI.General (geniBug)

-- | constant : no label, just constraints
--   variable : label, with or without constraints
--   anonymous : no label, no constraints
data GeniVal = GeniVal { gLabel       :: Maybe String
                       , gConstraints :: Maybe [String]
                       }
  deriving (Eq,Ord, Data, Typeable)

-- | 'mkGConst' @x []@ creates a single constant.  'mkGConst' @x xs@
--   creates an atomic disjunction.  It makes no difference which of the values
--   you supply for @x@ and @xs@ as they will be sorted and nubed anyway.  We
--   divide these only to enforce a non-empty list.
mkGConst :: String   -- ^ one value
         -> [String] -- ^ any additional values (atomic disjunction)
         -> GeniVal
mkGConst x xs = GeniVal Nothing (Just . sort . nub $ x:xs)
mkGVar x mxs  = GeniVal (Just x) ((sort . nub) `fmap` mxs)
mkGAnon       = GeniVal Nothing Nothing

instance Uniplate GeniVal where
  uniplate x = (Zero, \Zero -> x)

instance Show GeniVal where
  show (GeniVal Nothing Nothing)    = "?_"
  show (GeniVal Nothing (Just cs))  = concat (intersperse "|" cs)
  show (GeniVal (Just l) Nothing)   = '?':l
  show (GeniVal (Just l) (Just cs)) = '?':concat (l : "/" : intersperse "|" cs)

isConst :: GeniVal -> Bool
isConst = isNothing . gLabel

isVar :: GeniVal -> Bool
isVar = isJust . gConstraints

isAnon :: GeniVal -> Bool
isAnon (GeniVal Nothing Nothing) = True
isAnon _     = False

-- ----------------------------------------------------------------------
-- Helper types
-- ----------------------------------------------------------------------

type Subst = Map.Map String GeniVal

-- ----------------------------------------------------------------------
-- Unification and subsumption
-- ----------------------------------------------------------------------

-- | 'unify' performs unification on two lists of 'GeniVal'.  If
--   unification succeeds, it returns @Just (r,s)@ where \verb!r! is
--   the result of unification and \verb!s! is a list of substitutions that
--   this unification results in.
unify :: Monad m => [GeniVal] -> [GeniVal] -> m ([GeniVal], Subst)
unify = unifyHelper unifyOne

-- | @l1 `allSubsume` l2@ returns the result of @l1 `unify` l2@ if
--   doing a simultaneous traversal of both lists, each item in
--   @l1@ subsumes the corresponding item in @l2@
allSubsume :: Monad m => [GeniVal] -> [GeniVal] -> m ([GeniVal], Subst)
allSubsume = unifyHelper subsumeOne

unifyHelper :: Monad m
            => (GeniVal -> GeniVal -> UnificationResult)
            -> [GeniVal]
            -> [GeniVal]
            -> m ([GeniVal], Subst)
unifyHelper f ll1 ll2 = repropagate `liftM` helper ll1 ll2
 where
  repropagate (xs, sub) = (replace sub xs, sub)
  helper [] l2 = return (l2, Map.empty)
  helper l1 [] = return (l1, Map.empty)
  helper (h1:t1) (h2:t2) =
    case f h1 h2 of
    Failure -> fail $ "unification failure between " ++ show h1 ++ " and " ++ show h2
    SuccessRep v g -> prepend `liftM` helper t1b t2b
                      where
                       s   = (v,g)
                       t1b = replaceOne s t1
                       t2b = replaceOne s t2
                       prepend = (g:) *** prependToSubst s
    SuccessRep2 v1 v2 g -> prepend `liftM` helper t1b t2b
                      where
                       s1  = (v1,g)
                       s2  = (v2,g)
                       t1b = replaceOne s2 . replaceOne s1 $ t1
                       t2b = replaceOne s2 . replaceOne s1 $ t2
                       prepend = (g:) *** (prependToSubst s1 . prependToSubst s2)
    SuccessSans g  -> first (g:) `liftM` helper t1 t2

-- | Note that the first Subst is assumed to come chronologically
--   before the second one; so merging @{ X -> Y }@ and @{ Y -> 3 }@
--   should give us @{ X -> 3; Y -> 3 }@;
--
--   See 'prependToSubst' for a warning!
mergeSubst :: Subst -> Subst -> Subst
mergeSubst sm1 sm2 = Map.foldWithKey (curry prependToSubst) sm2 sm1

-- | Add to variable replacement to a 'Subst' that logical comes before
--   the other stuff in it.  So for example, if we have @Y -> foo@
--   and we want to insert @X -> Y@, we notice that, in fact, @Y@ has
--   already been replaced by @foo@, so we add @X -> foo@ instead
--
--   Note that it is undefined if you try to append something like
--   @Y -> foo@ to @Y -> bar@, because that would mean that unification
--   is broken
prependToSubst :: (String,GeniVal) -> Subst -> Subst
prependToSubst (v, gr@(GeniVal (Just r) _)) sm =
  case Map.lookup v sm of
    Just v2 -> geniBug . unlines $
                [ "prependToSubst: GenI just tried to prepend the substitution"
                , "  " ++ show (mkGVar v Nothing) ++ " -> " ++ show gr
                , "to one where where "
                , "  " ++ show (mkGVar v Nothing) ++ " -> " ++ show v2
                , "is slated to occur afterwards."
                , ""
                , "This could mean that either"
                , " (a) the core unification algorithm is broken"
                , " (b) we failed to propagate a value somewhere or"
                , " (c) we are attempting unification without renaming."
                ]
    Nothing -> Map.insert v gr2 sm
  where gr2 = fromMaybe gr $ Map.lookup r sm
prependToSubst (v, gr) sm = Map.insert v gr sm

-- ----------------------------------------------------------------------
-- Core unification
-- TODO: would continuation passing style make this more efficient?
-- ----------------------------------------------------------------------

data UnificationResult = SuccessSans GeniVal
                       | SuccessRep  String GeniVal
                       | SuccessRep2 String String GeniVal
                       | Failure

-- | See source code for details
--
--   Note that we assume that it's acceptable to generate new
--   variable names by appending an 'x' to them; this assumption
--   is only safe if the variables have gone through the function
--   'alphaConvertById' or have been pre-processed and rewritten
--   with some kind of common suffix to avoid an accidental match
unifyOne :: GeniVal -> GeniVal -> UnificationResult
unifyOne (GeniVal Nothing Nothing) g = SuccessSans g
unifyOne g (GeniVal Nothing Nothing) = SuccessSans g
unifyOne g1 g2 =
 case intersectConstraints gc1 gc2 of
   Nothing -> Failure
   Just cs -> case (gLabel g1, gLabel g2) of
                (Nothing, Nothing) -> SuccessSans  (GeniVal Nothing cs)
                (Nothing, Just v)  -> SuccessRep v (GeniVal Nothing cs)
                (Just v, Nothing)  -> SuccessRep v (GeniVal Nothing cs)
                (Just v1, Just v2) | v1 == v2 && gc1 /= gc2 -> geniBug $ "I just tried to unify variable with itself, but it has mismatching constraints: " ++ show g1 ++ " vs. "++ show g2
                                   | v1 == v2   -> SuccessSans g1
                                   | gc1 == gc2 -> let gv = GeniVal (Just (max v1 v2)) cs
                                                   in  SuccessRep (min v1 v2) gv
                                   | otherwise  -> let gv = GeniVal (Just (max v1 v2 ++ "-g")) cs
                                                   in  SuccessRep2 (min v1 v2) (max v1 v2) gv -- min/max stuff for symmetry
 where
  gc1 = gConstraints g1
  gc2 = gConstraints g2

intersectConstraints Nothing cs = Just cs
intersectConstraints cs Nothing = Just cs
intersectConstraints (Just v1) (Just v2) =
  case v1 `intersect` v2 of
    []   -> Nothing
    newV -> Just (Just newV)

-- ----------------------------------------------------------------------
-- Core subsumption
-- ----------------------------------------------------------------------

-- | 'subsumeOne' @x y@ returns the same result as @unifyOne x y@ if @x@
--   subsumes @y@ or 'Failure' otherwise
subsumeOne :: GeniVal -> GeniVal -> UnificationResult
subsumeOne g1@(GeniVal _ (Just cs1)) g2@ (GeniVal _ (Just cs2)) =
   if cs1 `subset` cs2 then unifyOne g1 g2 else Failure
 where
   subset x y = all (`elem` y) x
subsumeOne (GeniVal _ (Just cs1)) (GeniVal _ Nothing) = Failure
subsumeOne g1@(GeniVal _ Nothing) g2 = unifyOne g1 g2

-- ----------------------------------------------------------------------
-- Variable substitution
-- ----------------------------------------------------------------------

replace :: DescendGeniVal a => Subst -> a -> a
replace m | Map.null m = id
replace m = descendGeniVal (replaceMapG m)

replaceOne :: DescendGeniVal a => (String, GeniVal) -> a -> a
replaceOne = descendGeniVal . replaceOneG

-- | Here it is safe to say (X -> Y; Y -> Z) because this would be crushed
--   down into a final value of (X -> Z; Y -> Z)
replaceList :: DescendGeniVal a => [(String,GeniVal)] -> a -> a
replaceList = replace . foldl' update Map.empty
  where
   update m (s1,s2) = Map.insert s1 s2 $ Map.map (replaceOne (s1,s2)) m

replaceMapG :: Subst -> GeniVal -> GeniVal
replaceMapG m v@(GeniVal (Just v_) _) = {-# SCC "replaceMapG" #-} Map.findWithDefault v v_ m
replaceMapG _ v = {-# SCC "replaceMapG" #-} v

replaceOneG :: (String, GeniVal) -> GeniVal -> GeniVal
replaceOneG (s1, s2) (GeniVal (Just v_) _) | v_ == s1 = {-# SCC "replaceOneG" #-} s2
replaceOneG _ v = {-# SCC "replaceOneG" #-} v

-- ----------------------------------------------------------------------
-- Variable collection and renaming
-- ----------------------------------------------------------------------

type CollectedVar = (String, Maybe [String])

-- | A 'Collectable' is something which can return its variables as a set.
--   By variables, what I most had in mind was the GVar values in a
--   GeniVal.  This notion is probably not very useful outside the context of
--   alpha-conversion task, but it seems general enough that I'll keep it
--   around for a good bit, until either some use for it creeps up, or I find
--   a more general notion that I can transform this into.
class Collectable a where
  collect :: a -> Set.Set CollectedVar -> Set.Set CollectedVar

instance Collectable a => Collectable (Maybe a) where
  collect Nothing  s = s
  collect (Just x) s = collect x s

instance (Collectable a => Collectable [a]) where
  collect l s = foldr collect s l

instance Collectable GeniVal where
  collect (GeniVal (Just v) cs) s = Set.insert (v,cs) s
  collect _ s = s

-- | An Idable is something that can be mapped to a unique id.
--   You might consider using this to implement Ord, but I won't.
--   Note that the only use I have for this so far (20 dec 2005)
--  is in alpha-conversion.
class Idable a where
  idOf :: a -> Integer

-- 'alphaConvertById' appends a unique suffix to all variables in
-- an object.  This avoids us having to alpha convert all the time
-- and relies on the assumption finding that a unique suffix is
-- possible.
alphaConvertById :: (Collectable a, DescendGeniVal a, Idable a) => a -> a
alphaConvertById x = {-# SCC "alphaConvertById" #-}
  alphaConvert ('-' : (show . idOf $ x)) x

alphaConvert :: (Collectable a, DescendGeniVal a) => String -> a -> a
alphaConvert suffix x = {-# SCC "alphaConvert" #-}
  replace subst x
 where
  subst :: Subst
  subst = Map.mapWithKey convert vars
  vars  = Map.fromListWith isect . Set.elems $ collect x Set.empty
  isect x y = fromMaybe (Just []) $ intersectConstraints x y
  convert v = GeniVal (Just (v ++ suffix))

-- ----------------------------------------------------------------------
-- Genericity
-- ----------------------------------------------------------------------

class DescendGeniVal a where
  descendGeniVal :: (GeniVal -> GeniVal) -> a -> a

instance DescendGeniVal GeniVal where
  descendGeniVal f = f

instance (Functor f, DescendGeniVal a) => DescendGeniVal (f a) where
  descendGeniVal = fmap . descendGeniVal

-- ----------------------------------------------------------------------
-- Testing
-- ----------------------------------------------------------------------

testSuite :: Test.Framework.Test
testSuite =
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
  csubsetH (Just xs) Nothing   = True
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

prop_subsume_antisymmetric x_ y_ =
 all qc_not_empty_GVar [ x, y ] && tt_subsumes x y ==>
   x `tt_equiv` y || not (tt_subsumes y x)
 where
   (x, y) = case alphaConvert "" [ x_, y_ ] of
             [n1,n2] -> (n1,n2)
             _ -> error "huh? alphaConvert length mismatch"

prop_subsume_transitive x_ y_ z_ =
 all qc_not_empty_GVar [ x, y, z ] && tt_subsumes x y && tt_subsumes y z ==>
   tt_subsumes x z
 where
   (x, y, z) = case alphaConvert "" [ x_, y_, z_ ] of
                [n1,n2,n3] -> (n1,n2,n3)
                _ -> error "huh? alphaConvert length mismatch"

tt_subsumes x y =
  case subsumeOne x y of
    Failure -> False
    _       -> True

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
-- Testing
-- ----------------------------------------------------------------------

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
  coarbitrary = error "no implementation of coarbitrary for GTestString"

instance Arbitrary GTestString2 where
  arbitrary =
    oneof $ map (return . GTestString2) $
    [ "X", "Y", "Z", "H", "I", "J", "P", "Q", "R", "S", "T", "U"  ]
  coarbitrary = error "no implementation of coarbitrary for GTestString2"

instance Arbitrary GeniVal where
  arbitrary = oneof [ return mkGAnon
                    , liftM2 mkGVar (fromGTestString2 `fmap` arbitrary)
                                    (fmap (map fromGTestString) `fmap` arbitrary)
                    , liftM2 mkGConst (fromGTestString `fmap` arbitrary)
                                      (map fromGTestString `fmap` arbitrary)
                    ]
  coarbitrary = error "no implementation of coarbitrary for GeniVal"

arbitrary1 :: Arbitrary a => Gen [a]
arbitrary1 = sized (\n -> choose (1,n+1) >>= vector)
