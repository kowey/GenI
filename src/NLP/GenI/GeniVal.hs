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

{-# LANGUAGE OverlappingInstances, FlexibleInstances, TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module NLP.GenI.GeniVal where

-- import Debug.Trace -- for test stuff
import Control.Arrow (first, (***))
import Control.Monad (liftM)
import Data.List
import Data.Maybe (catMaybes, fromMaybe, isNothing, isJust)
import Data.Generics (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as Map

import Data.Text ( Text )
import qualified Data.Text as T

import Control.DeepSeq

import NLP.GenI.General (geniBug, quoteString, isGeniIdentLetter)

-- | constant : no label, just constraints
--   variable : label, with or without constraints
--   anonymous : no label, no constraints
data GeniVal = GeniVal { gLabel       :: Maybe String 
                       , gConstraints :: Maybe [Text]
                       }
  deriving (Eq,Ord, Data, Typeable)

-- | 'mkGConst' @x []@ creates a single constant.  'mkGConst' @x xs@
--   creates an atomic disjunction.  It makes no difference which of the values
--   you supply for @x@ and @xs@ as they will be sorted and nubed anyway.  We
--   divide these only to enforce a non-empty list.
mkGConst :: Text -- ^ one value
         -> [Text] -- ^ any additional values (atomic disjunction)
         -> GeniVal
mkGConst x xs  = GeniVal Nothing (Just . sort . nub $ x:xs)

mkGConstNone :: Text -> GeniVal
mkGConstNone x = mkGConst x []

mkGVar :: String -> Maybe [Text] -> GeniVal
mkGVar x mxs  = GeniVal (Just x) ((sort . nub) `fmap` mxs)

mkGVarNone :: String -> GeniVal
mkGVarNone x  = mkGVar x Nothing

mkGAnon :: GeniVal
mkGAnon       = GeniVal Nothing Nothing

instance Show GeniVal where
  show = showGeniVal

showGeniVal :: GeniVal -> String
showGeniVal gv =
  case gv of
   GeniVal Nothing Nothing    -> "?_"
   GeniVal Nothing (Just cs)  -> showConstraints cs
   GeniVal (Just l) Nothing   -> '?':l
   GeniVal (Just l) (Just cs) -> '?':concat [ l, "/", showConstraints cs ]
  where
   showConstraints = intercalate "|" . map (maybeQuote . T.unpack) -- FIXME push down
   maybeQuote "" = quoteString ""
   maybeQuote x | any naughty x = quoteString x
   maybeQuote x  = x
   naughty x = not (isGeniIdentLetter x) || x `elem` "_?/"

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

showSubst :: Subst -> String
showSubst = unwords . map sho . Map.toList
 where
  sho (v,s) = v ++ "<-" ++ show s

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
appendSubst :: Subst -> Subst -> Subst
appendSubst sm1 sm2 = Map.foldrWithKey (curry prependToSubst) sm2 sm1

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
--   'finaliseVarsById' or have been pre-processed and rewritten
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

intersectConstraints :: Eq a => Maybe [a] -> Maybe [a] -> Maybe (Maybe [a])
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
subsumeOne (GeniVal _ (Just _)) (GeniVal _ Nothing) = Failure
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

type CollectedVar = (String, Maybe [Text])

-- | A 'Collectable' is something which can return its variables as a
--   map from the variable to the number of times that variable occurs
--   in it.
--
--   Important invariant: if the variable does not occur, then it does
--   not appear in the map (ie. all counts must be >= 1 or the item
--   does not occur at all)
--
--   By variables, what I most had in mind was the GVar values in a
--   GeniVal.  This notion is probably not very useful outside the context of
--   alpha-conversion task, but it seems general enough that I'll keep it
--   around for a good bit, until either some use for it creeps up, or I find
--   a more general notion that I can transform this into.
class Collectable a where
  collect :: a -> Map.Map CollectedVar Int -> Map.Map CollectedVar Int

instance Collectable a => Collectable (Maybe a) where
  collect Nothing  s = s
  collect (Just x) s = collect x s

instance (Collectable a => Collectable [a]) where
  collect l s = foldr collect s l

instance Collectable GeniVal where
  collect (GeniVal (Just v) cs) s = Map.insertWith' (+) (v,cs) 1 s
  collect _ s = s

-- | An Idable is something that can be mapped to a unique id.
--   You might consider using this to implement Ord, but I won't.
--   Note that the only use I have for this so far (20 dec 2005)
--  is in alpha-conversion.
class Idable a where
  idOf :: a -> Integer

-- | Anonymise any variable that occurs only once in the object
anonymiseSingletons :: (Collectable a, DescendGeniVal a) => a -> a
anonymiseSingletons x =
   replace subst x
 where
   subst = Map.map (const mkGAnon) . Map.filter (== 1)
           -- merge counts for same var, different constraints
         . Map.fromListWith (+) . map (first fst) . Map.toList
         $ collect x Map.empty

-- 'finaliseVarsById' appends a unique suffix to all variables in
-- an object.  This avoids us having to alpha convert all the time
-- and relies on the assumption finding that a unique suffix is
-- possible.
finaliseVarsById :: (Collectable a, DescendGeniVal a, Idable a) => a -> a
finaliseVarsById x = {-# SCC "finaliseVarsById" #-}
  finaliseVars ('-' : (show . idOf $ x)) x

-- | 'finaliseVars' does the following:
--
--   * (if suffix is non-null) appends a suffix to all variable names
--     to ensure global uniqueness
--
--   * anonymises any singleton variables
--
--   * intersects constraints for for all variables within the same
--     object
finaliseVars :: (Collectable a, DescendGeniVal a) => String -> a -> a
finaliseVars suffix x = {-# SCC "finaliseVars" #-}
  replace subst (anonymiseSingletons x)
 where
  subst :: Subst
  subst = Map.mapWithKey convert vars
  vars  = Map.fromListWith isect . Map.keys $ collect x Map.empty
  isect xi yi = fromMaybe (Just []) -- unification failure => fully constrained
              $ intersectConstraints xi yi
  convert v = GeniVal (Just (v ++ suffix))

-- ----------------------------------------------------------------------
-- Fancy disjunction
-- ----------------------------------------------------------------------

crushOne :: [GeniVal] -> Maybe GeniVal
crushOne []   = Nothing
crushOne [gs] = Just gs
crushOne gs   =
  if any isNothing gcs
     then Nothing
     else case concat (catMaybes gcs) of
            []     -> Nothing
            (c:cs) -> Just (mkGConst c cs)
  where
   gcs = map gConstraints gs

crushList :: [[GeniVal]] -> Maybe [GeniVal]
crushList = mapM crushOne

-- ----------------------------------------------------------------------
-- Genericity
-- ----------------------------------------------------------------------

class DescendGeniVal a where
  descendGeniVal :: (GeniVal -> GeniVal) -> a -> a

instance DescendGeniVal GeniVal where
  descendGeniVal f = f

instance (Functor f, DescendGeniVal a) => DescendGeniVal (f a) where
  descendGeniVal = fmap . descendGeniVal

instance NFData GeniVal where
  rnf (GeniVal x y) = rnf x `seq` rnf y
