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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Semantics where

import Control.Arrow ( first, (***), (&&&) )
import Control.Applicative ( (<$>) )
import Control.DeepSeq
import Data.Binary
import Data.Function ( on )
import Data.Data
import Data.List ( nub, sortBy, delete, insert )
import Data.Maybe ( isNothing, isJust, mapMaybe, fromMaybe )
import qualified Data.Map as Map
import Data.Text ( Text )
import qualified Data.Text as T

import NLP.GenI.FeatureStructures
import NLP.GenI.General ( histogram )
import NLP.GenI.GeniVal

-- handle, predicate, parameters
data Literal = Literal { lHandle    :: GeniVal
                       , lPredicate :: GeniVal
                       , lArgs      :: [GeniVal]
                       }
 deriving (Eq, Data, Typeable)

instance Ord Literal where
  compare = compare `on` tucked
    where
      -- treat the handle as an argument
      tucked l = (lPredicate l, lHandle l : lArgs l)

-- FIXME: I'm never too sure if this is a good idea or not
-- implementing show as pretty printing
instance Show Literal where
  show = showLiteral

type Sem = [Literal]
type LitConstr = (Literal, [String])
type SemInput  = (Sem,Flist GeniVal,[LitConstr])

instance Collectable Literal where
  collect (Literal a b c) = collect a . collect b . collect c

emptyLiteral :: Literal
emptyLiteral = Literal mkGAnon mkGAnon []

-- Utility functions

removeConstraints :: SemInput -> SemInput
removeConstraints (x, _, _) = (x, [], [])

-- | default sorting for a semantics
sortSem :: Sem -> Sem
sortSem = sortBy compareOnLiteral

compareOnLiteral :: Literal -> Literal -> Ordering
compareOnLiteral = compare

-- sort primarily putting the ones with the most constants first
-- and secondarily by the number of instances a predicate occurs
-- (if plain string; atomic disjunction/vars treated as infinite)
sortByAmbiguity :: Sem -> Sem
sortByAmbiguity sem = sortBy (flip compare `on` criteria) sem
 where
   criteria  = (constants &&& ambiguity) -- this is reverse sorting
                                         -- so high numbers come first
   ambiguity l = fromMaybe 0 $ do -- Maybe
                   p <- boringLiteral l
                   negate <$> Map.lookup p (literalCount sem)

class HasConstants a where
  constants :: a -> Int

instance HasConstants GeniVal where
  constants g = if isConst2 g then 1 else 0
   where
    isConst2 :: GeniVal -> Bool
    isConst2 x = isJust (gConstraints x) && isNothing (gLabel x)

instance HasConstants a => HasConstants [a] where
  constants = sum . map constants

instance HasConstants Literal where
  constants (Literal h p args) = constants (h:p:args)

literalCount :: [Literal] -> Map.Map Text Int
literalCount = histogram . mapMaybe boringLiteral

boringLiteral :: Literal -> Maybe Text
boringLiteral = singletonVal . lPredicate
    -- predicate with a straightfoward constant value
    -- exactly one constraint

-- | Given a Semantics, return the string with the proper keys
--   (propsymbol+arity) to access the agenda
toKeys :: Sem -> [String]
toKeys = map (\l -> show (lPredicate l) ++ show  (length $ lArgs l))

-- Traversal

instance DescendGeniVal Literal where
  descendGeniVal s (Literal h n lp) = Literal (descendGeniVal s h)
                                              (descendGeniVal s n)
                                              (descendGeniVal s lp)

-- Pretty printing

showSem :: Sem -> String
showSem l =
    "[" ++ (unwords $ map showLiteral l) ++ "]"

showLiteral :: Literal -> String
showLiteral (Literal h p l) = showh ++ show p ++ "(" ++ unwords (map show l) ++ ")"
  where
    hideh g = case singletonVal g of
                Just c -> isInternalHandle c
                _      -> False
    --
    showh = if hideh h then "" else (show h) ++ ":"

isInternalHandle :: Text -> Bool
isInternalHandle = ("genihandle" `T.isPrefixOf`)

-- ----------------------------------------------------------------------
-- Subsumption
-- ----------------------------------------------------------------------

-- | @x `subsumeSem` y@ returns all the possible ways to unify
--   @x@ with some SUBSET of @y@ so that @x@ subsumes @y@.
--   If @x@ does NOT subsume @y@, we return the empty list.
subsumeSem :: Sem -> Sem -> [(Sem,Subst)]
subsumeSem x y | length x > length y = []
subsumeSem x y =
  map (first sortSem) $ subsumeSemH x y

subsumeSemH :: Sem -> Sem -> [(Sem,Subst)]
subsumeSemH [] [] = [ ([], Map.empty) ]
subsumeSemH _ []  = error "subsumeSemH: got longer list in front"
subsumeSemH []     _  = [ ([], Map.empty) ]
subsumeSemH (x:xs) ys = nub $
 do let attempts = zip ys $ map (subsumeLiteral x) ys
    (y, Just (x2, subst)) <- attempts
    let next_xs = replace subst xs
        next_ys = replace subst $ delete y ys
        prepend = insert x2 *** appendSubst subst
    prepend `fmap` subsumeSemH next_xs next_ys

-- | @p1 `subsumeLiteral` p2@... FIXME
subsumeLiteral :: Literal -> Literal -> Maybe (Literal, Subst)
subsumeLiteral (Literal h1 p1 la1) (Literal h2 p2 la2) =
  if length la1 == length la2
  then do let hpla1 = h1:p1:la1
              hpla2 = h2:p2:la2
          (hpla, sub) <- hpla1 `allSubsume` hpla2
          return (toLiteral hpla, sub)
  else Nothing
 where
  toLiteral (h:p:xs) = Literal h p xs
  toLiteral _ = error "subsumeLiteral.toLiteral"

-- ----------------------------------------------------------------------
-- Unification
-- ----------------------------------------------------------------------

-- We return the list of minimal ways to unify two semantics.
-- By minimal, I mean that any literals that are not the product of a
-- succesful unification really do not unify with anything else.
unifySem :: Sem -> Sem -> [(Sem,Subst)]
unifySem xs ys =
 map (first sortSem) $
 if length xs < length ys
    then unifySemH xs ys
    else unifySemH ys xs

-- list monad for Prolog-style backtracking.
unifySemH :: Sem -> Sem -> [(Sem,Subst)]
unifySemH [] [] = return ([], Map.empty)
unifySemH [] xs = return (xs, Map.empty)
unifySemH xs [] = error $ "unifySem: shorter list should always be in front: " ++ showSem xs
unifySemH (x:xs) ys = nub $ do
 let attempts = zip ys $ map (unifyLiteral x) ys
 if all (isNothing . snd) attempts
    then first (x:) `fmap` unifySemH xs ys -- only include x unmolested if no unification succeeds
    else do (y, Just (x2, subst)) <- attempts
            let next_xs = replace subst xs
                next_ys = replace subst $ delete y ys
                prepend = insert x2 *** appendSubst subst
            prepend `fmap` unifySemH next_xs next_ys

unifyLiteral :: Literal -> Literal -> Maybe (Literal, Subst)
unifyLiteral (Literal h1 p1 la1) (Literal h2 p2 la2) =
  if length la1 == length la2
  then do let hpla1 = h1:p1:la1
              hpla2 = h2:p2:la2
          (hpla, sub) <- hpla1 `unify` hpla2
          return (toLiteral hpla, sub)
  else Nothing
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

 
instance NFData Literal where
        rnf (Literal x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` ()

 
instance Binary Literal where
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
