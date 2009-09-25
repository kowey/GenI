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
import Control.Monad (liftM)
import Data.List
import Data.Maybe (fromMaybe, isJust)
import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as Map

import Test.HUnit
import Test.QuickCheck hiding (collect)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck

import Data.Generics.PlateDirect

import Control.Parallel.Strategies

import NLP.GenI.General (geniBug)

data GeniVal = GConst [String] -- ^ atomic disjunction - constant x | y | z
             | GVar   String   -- ^ variable
             | GAnon           -- ^ anonymous
  deriving (Eq,Ord, Data, Typeable)

instance Uniplate GeniVal where
  uniplate x = (Zero, \Zero -> x)

instance Show GeniVal where
  show (GConst x) = concat $ intersperse "|" x
  show (GVar x)   = '?':x
  show GAnon      = "?_"

isConst :: GeniVal -> Bool
isConst (GConst _) = True
isConst _ = False

isVar :: GeniVal -> Bool
isVar (GVar _) = True
isVar _        = False

isAnon :: GeniVal -> Bool
isAnon GAnon = True
isAnon _     = False

-- | (assumes that it's a GConst!)
fromGConst :: GeniVal -> [String]
fromGConst (GConst x) = x
fromGConst x = error ("fromGConst on " ++ show x)

-- | (assumes that it's a GVar!)
fromGVar :: GeniVal -> String
fromGVar (GVar x) = x
fromGVar x = error ("fromGVar on " ++ show x)

-- ----------------------------------------------------------------------
-- Helper types
-- ----------------------------------------------------------------------

type Subst = Map.Map String GeniVal

-- ----------------------------------------------------------------------
-- Unification
-- ----------------------------------------------------------------------

-- | 'unify' performs unification on two lists of 'GeniVal'.  If
--   unification succeeds, it returns @Just (r,s)@ where \verb!r! is
--   the result of unification and \verb!s! is a list of substitutions that
--   this unification results in.
unify :: Monad m => [GeniVal] -> [GeniVal] -> m ([GeniVal], Subst)
unify l1 l2 = repropagate `liftM` helper l1 l2
 where
  repropagate (xs, sub) = (replace sub xs, sub)
  helper [] l2 = return (l2, Map.empty)
  helper l1 [] = return (l1, Map.empty)
  helper (h1:t1) (h2:t2) =
    case unifyOne h1 h2 of
    Failure -> fail $ "unification failure between " ++ show h1 ++ " and " ++ show h2
    SuccessRep v g -> prepend `liftM` unify t1b t2b
                      where
                       s   = (v,g)
                       t1b = replaceOne s t1
                       t2b = replaceOne s t2
                       prepend = (g:) *** prependToSubst s
    SuccessSans g  -> first (g:) `liftM` unify t1 t2

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
prependToSubst (v, gr@(GVar r)) sm =
  case Map.lookup v sm of
    Just v2 -> geniBug . unlines $
                [ "prependToSubst: GenI just tried to assign a new value " ++ "(" ++ show gr ++ ")"
                , "to the unification variable " ++ show (GVar v) ++ ", which already has a "
                , "a value assigned to it (" ++ show v2 ++ ").  This could mean that either"
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
                       | Failure

-- | See source code for details
unifyOne :: GeniVal -> GeniVal -> UnificationResult
unifyOne g GAnon = SuccessSans g
unifyOne GAnon g = SuccessSans g
unifyOne (GVar v) gc@(GConst _) = SuccessRep v gc
unifyOne gc@(GConst _) (GVar v) = SuccessRep v gc
unifyOne (GConst v1) (GConst v2) =
  case v1 `intersect` v2 of
    []   -> Failure
    newV -> SuccessSans (GConst newV)
unifyOne x1@(GVar v1) (GVar v2) =
  if v1 == v2
     then SuccessSans x1
     else SuccessRep  v2  x1

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
replaceMapG m v@(GVar v_) = {-# SCC "replaceMapG" #-} Map.findWithDefault v v_ m
replaceMapG _ v = {-# SCC "replaceMapG" #-} v

replaceOneG :: (String, GeniVal) -> GeniVal -> GeniVal
replaceOneG (s1, s2) (GVar v_) | v_ == s1 = {-# SCC "replaceOneG" #-} s2
replaceOneG _ v = {-# SCC "replaceOneG" #-} v

-- ----------------------------------------------------------------------
-- Performance
-- ----------------------------------------------------------------------

instance NFData GeniVal
    where rnf (GConst x1) = rnf x1
          rnf (GVar x1) = rnf x1
          rnf (GAnon) = ()

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

testSuite = testGroup "unification"
 [ testProperty "self" prop_unify_sym
 , testProperty "anonymous variables" prop_unify_anon
 , testProperty "symmetry" prop_unify_sym
 , testBackPropagation
 ]

-- | Unifying something with itself should always succeed
prop_unify_self :: [GeniVal] -> Property
prop_unify_self x =
  (all qc_not_empty_GConst) x ==>
    case unify x x of
    Nothing  -> False
    Just unf -> fst unf == x

-- | Unifying something with only anonymous variables should succeed and return
--   the same result.
prop_unify_anon :: [GeniVal] -> Bool
prop_unify_anon x =
  case unify x y of
    Nothing  -> False
    Just unf -> fst unf == x
  where --
    y  = replicate (length x) GAnon

-- | Unification should be symmetrical.  We can't guarantee these if there
--   are cases where there are variables in the same place on both sides, so we
--   normalise the sides so that this doesn't happen.
prop_unify_sym :: [GeniVal] -> [GeniVal] -> Property
prop_unify_sym x y =
  let u1 = (unify x y) :: Maybe ([GeniVal],Subst)
      u2 = unify y x
      --
      notOverlap (GVar _, GVar _) = False
      notOverlap _ = True
  in (all qc_not_empty_GConst) x &&
     (all qc_not_empty_GConst) y &&
     all (notOverlap) (zip x y) ==> u1 == u2

testBackPropagation =
  testGroup "back propagation"
   [ testCase "unify left/right" $ assertEqual "" expected $ unify left right
   , testCase "unify right/left" $ assertEqual "" expected $ unify right left
   ]
 where
  n = 3
  cx = GConst ["X"]
  leftStrs = map show [1..n]
  left  = map GVar leftStrs
  right = drop 1 left ++ [cx]
  expected = Just (expectedResult, expectedSubst)
  expectedResult = replicate n cx
  expectedSubst  = Map.fromList $ zip leftStrs (repeat cx)

-- ----------------------------------------------------------------------
-- Testing
-- ----------------------------------------------------------------------

-- Definition of Arbitrary GeniVal for QuickCheck
newtype GTestString = GTestString String
newtype GTestString2 = GTestString2 String

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
  arbitrary = oneof [ return $ GAnon,
                      fmap (GVar . fromGTestString2) arbitrary,
                      fmap (GConst . nub . sort . map fromGTestString) arbitrary ]
  coarbitrary = error "no implementation of coarbitrary for GeniVal"

qc_not_empty_GConst :: GeniVal -> Bool
qc_not_empty_GConst (GConst []) = False
qc_not_empty_GConst _ = True
