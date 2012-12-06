-- GenI surface realiser
-- Copyright (C) 2005-2009 Carlos Areces and Eric Kow
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | We use a flat semantics in GenI (bag of literals).
module NLP.GenI.Semantics where

import Control.Arrow ( first, (***), (&&&) )
import Control.Applicative ( (<$>) )
import Control.DeepSeq
import Control.Monad.Error
import Data.Binary
import Data.Function ( on )
import Data.Data
import Data.List ( nub, sortBy, delete, insert )
import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as T

import Control.Error

import NLP.GenI.FeatureStructure
import NLP.GenI.GeniShow
import NLP.GenI.General ( histogram )
import NLP.GenI.GeniVal
import NLP.GenI.Pretty

-- | A single semantic literal containing its handle, predicate, and arguments
--
--   This can be paramaterised on the kinds of variables it uses, for example,
--   'GeniVal' for a semantics that you might still want to do unification on
--   or 'Text' if it's supposed to be ground.
data Literal gv = Literal
    { -- | the handle can be seen as a special kind of argument; stored separately
      lHandle    :: gv
      -- |
    , lPredicate :: gv
      -- |
    , lArgs      :: [gv]
    }
 deriving (Eq, Data, Typeable)

instance Ord gv => Ord (Literal gv) where
  compare = compare `on` tucked
    where
      -- treat the handle as an argument
      tucked l = (lPredicate l, lHandle l : lArgs l)

-- | A semantics is just a set of literals.
type Sem = [Literal GeniVal]

-- | A literal and any constraints associated with it (semantic input)
type LitConstr = (Literal GeniVal, [Text])

-- | Semantics, index constraints, literal constraints
--
--   The intention here is that for @(sem, icons, lcons)@
--   @all (`elem` sem) lcons@
type SemInput  = (Sem,Flist GeniVal,[LitConstr])

instance Collectable a => Collectable (Literal a) where
  collect (Literal a b c) = collect a . collect b . collect c

-- | An empty literal, not sure you should really be using this
emptyLiteral :: Literal GeniVal
emptyLiteral = Literal mkGAnon mkGAnon []

-- ----------------------------------------------------------------------
-- * Utility functions
-- ----------------------------------------------------------------------

-- | Strip any index or literal constraints from an input.
--   Use with care.
removeConstraints :: SemInput -> SemInput
removeConstraints (x, _, _) = (x, [], [])

-- | Default sorting for a semantics
sortSem :: Ord a => [Literal a] -> [Literal a]
sortSem = sortBy compareOnLiteral

-- | Default comparison for a literal
compareOnLiteral :: Ord a => Literal a -> Literal a -> Ordering
compareOnLiteral = compare

-- | Sort primarily putting the ones with the most constants first
-- and secondarily by the number of instances a predicate occurs
-- (if plain string; atomic disjunction/vars treated as infinite)
sortByAmbiguity :: Sem -> Sem
sortByAmbiguity sem =
    sortBy (flip compare `on` criteria) sem
  where
    criteria  = (constants &&& ambiguity) -- this is reverse sorting
                                          -- so high numbers come first
    ambiguity l = fromMaybe 0 $ do -- Maybe
        p <- boringLiteral l
        negate <$> Map.lookup p (literalCount sem)
    literalCount = histogram . mapMaybe boringLiteral
    boringLiteral = singletonVal . lPredicate

-- | Anything that we would want to count the number constants in
--   (as opposed to variables)
class HasConstants a where
  -- | Number of constants
  constants :: a -> Int

instance HasConstants GeniVal where
  constants g = if isConst2 g then 1 else 0
   where
    isConst2 :: GeniVal -> Bool
    isConst2 x = isJust (gConstraints x) && isNothing (gLabel x)

instance HasConstants a => HasConstants [a] where
  constants = sum . map constants

instance HasConstants (Literal GeniVal) where
  constants (Literal h p args) = constants (h:p:args)

-- ----------------------------------------------------------------------
-- Traversal
-- ----------------------------------------------------------------------

instance DescendGeniVal a => DescendGeniVal (Literal a) where
  descendGeniVal s (Literal h n lp) = Literal (descendGeniVal s h)
                                              (descendGeniVal s n)
                                              (descendGeniVal s lp)

-- ----------------------------------------------------------------------
-- Pretty printing
-- ----------------------------------------------------------------------

instance Pretty Sem where
   pretty = geniShowText

instance GeniShow Sem where
   geniShowText = squares . T.unwords . map geniShowText

instance Pretty (Literal GeniVal) where
   pretty = geniShowText

instance GeniShow (Literal GeniVal) where
   geniShowText (Literal h p l) =
       mh `T.append` geniShowText p
          `T.append` (parens . T.unwords . map geniShowText $ l)
     where
       mh    = if hideh h then "" else geniShowText h `T.snoc` ':'
       hideh = maybe False isInternalHandle . singletonVal

instance Pretty SemInput where
    pretty = geniShowText

instance GeniShow SemInput where
    geniShowText = displaySemInput (T.unwords . map geniShowText)

instance GeniShow LitConstr where
    geniShowText (sem, []) = geniShowText sem
    geniShowText (sem, cs) = geniShowText sem <> squares (T.unwords cs)

-- | Helper for displaying or pretty printing a semantic input
--
--   This gives you a bit of control over how each literal is
--   displayed
displaySemInput :: ([LitConstr] -> Text) -> SemInput -> Text
displaySemInput dispLits (sem, icons, lcons) =
    -- CAREFUL: if you're modifying this, note that geniShowText
    -- can be affected
    T.intercalate "\n" . concat $
        [ [semStuff]
        , [ idxStuff | not (null icons) ]
        ]
      where
        semStuff = geniKeyword "semantics"
                 . squares . dispLits
                 $ map withConstraints sem
        idxStuff = geniKeyword "idxconstraints"
                 . squares
                 $ geniShowText icons
        withConstraints lit =
            (lit, concat [ cs | (p,cs) <- lcons, p == lit ])

-- | Is a handle generated by GenI. GenI lets you write literals without
--   a handle; in these cases a unique handle is generated and hidden
--   from the UI.
isInternalHandle :: Text -> Bool
isInternalHandle = ("genihandle" `T.isPrefixOf`)

-- ----------------------------------------------------------------------
-- * Subsumption
-- ----------------------------------------------------------------------

-- | @x `subsumeSem` y@ returns all the possible ways to unify
--   @x@ with some SUBSET of @y@ so that @x@ subsumes @y@.
--   If @x@ does NOT subsume @y@, we return the empty list.
subsumeSem :: Sem -> Sem -> [(Sem,Subst)]
subsumeSem x y | length x > length y = []
subsumeSem x y =
  map (first sortSem) $ subsumeSemH x y

-- | Helper for 'subsumeSem' traversal
subsumeSemH :: Sem -> Sem -> [(Sem,Subst)]
subsumeSemH [] [] = [ ([], Map.empty) ]
subsumeSemH _ []  = error "subsumeSemH: got longer list in front"
subsumeSemH []     _  = [ ([], Map.empty) ]
subsumeSemH (x:xs) ys = nub $
 do let attempts = zip ys $ map (hush . subsumeLiteral x) ys
    (y, Just (x2, subst)) <- attempts
    let next_xs = replace subst xs
        next_ys = replace subst $ delete y ys
        prepend = insert x2 *** appendSubst subst
    prepend `fmap` subsumeSemH next_xs next_ys

-- | @p1 `subsumeLiteral` p2@ is the unification of @p1@ and @p2@ if
--   both literals have the same arity, and the handles, predicates,
--   and arguments in @p1@ all subsume their counterparts in @p2@
subsumeLiteral :: MonadUnify m
               => Literal GeniVal
               -> Literal GeniVal
               -> m (Literal GeniVal, Subst)
subsumeLiteral l1@(Literal h1 p1 la1) l2@(Literal h2 p2 la2) =
  if length la1 == length la2
  then do let hpla1 = h1:p1:la1
              hpla2 = h2:p2:la2
          (hpla, sub) <- hpla1 `allSubsume` hpla2
          return (toLiteral hpla, sub)
  else throwError $ pretty l1 <+> "does not subsume" <+> pretty l2 <+>
                    "because they don't have the same arity"
 where
  toLiteral (h:p:xs) = Literal h p xs
  toLiteral _ = error "subsumeLiteral.toLiteral"

-- ----------------------------------------------------------------------
-- * Unification
-- ----------------------------------------------------------------------

-- | Return the list of minimal ways to unify two semantics, ie.
-- where any literals that are not the product of a succesful unification
-- really do not unify with anything else.
unifySem :: Sem -> Sem -> [(Sem,Subst)]
unifySem xs ys =
 map (first sortSem) $
 if length xs < length ys
    then unifySemH xs ys
    else unifySemH ys xs

-- | Helper traversal for 'unifySem'
unifySemH :: Sem -> Sem -> [(Sem,Subst)]
unifySemH [] [] = return ([], Map.empty)
unifySemH [] xs = return (xs, Map.empty)
unifySemH xs [] = error $ "unifySem: shorter list should always be in front: " ++ prettyStr xs
unifySemH (x:xs) ys = nub $ do -- list monad for Prolog-style backtracking.
 let attempts = zip ys $ map (hush . unifyLiteral x) ys
 if all (isNothing . snd) attempts
    then first (x:) `fmap` unifySemH xs ys -- only include x unmolested if no unification succeeds
    else do (y, Just (x2, subst)) <- attempts
            let next_xs = replace subst xs
                next_ys = replace subst $ delete y ys
                prepend = insert x2 *** appendSubst subst
            prepend `fmap` unifySemH next_xs next_ys

-- | Two literals unify if they have the same arity, and their
--   handles, predicates, and arguments also unify
unifyLiteral :: MonadUnify m
             => Literal GeniVal
             -> Literal GeniVal -> m (Literal GeniVal, Subst)
unifyLiteral l1@(Literal h1 p1 la1) l2@(Literal h2 p2 la2) =
  if length la1 == length la2
  then do let hpla1 = h1:p1:la1
              hpla2 = h2:p2:la2
          (hpla, sub) <- hpla1 `unify` hpla2
          return (toLiteral hpla, sub)
  else throwError $ pretty l1 <+> "does not unify with" <+> pretty l2 <+>
                    "because they don't have the same arity"
 where
  toLiteral (h:p:xs) = Literal h p xs
  toLiteral _ = error "unifyLiteral.toLiteral"

-- ----------------------------------------------------------------------
--
-- ----------------------------------------------------------------------

{-!
deriving instance NFData Literal 
deriving instance Binary Literal 
!-}

-- GENERATED START

 
instance NFData g => NFData (Literal g) where
        rnf (Literal x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` ()

 
instance Binary g => Binary (Literal g) where
        put (Literal x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (Literal x1 x2 x3)
-- GENERATED STOP
