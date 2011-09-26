% GenI surface realiser
% Copyright (C) 2005-2009 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\chapter{Semantics}

\ignore{
\begin{code}
{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Semantics where

import Control.Arrow ( first, (***) )
import Data.Generics.PlateDirect
import Data.Function ( on )
import Data.List ( isPrefixOf, nub, sort, sortBy, delete, insert )
import Data.Maybe ( isNothing, isJust )
import qualified Data.Map as Map

import NLP.GenI.FeatureStructures
import NLP.GenI.GeniVal
\end{code}
}

\begin{code}
-- handle, predicate, parameters
type Pred = (GeniVal, GeniVal, [GeniVal])
type Sem = [Pred]
type LitConstr = (Pred, [String])
type SemInput  = (Sem,Flist GeniVal,[LitConstr])

instance Biplate Pred GeniVal where
  biplate (g1, g2, g3) = plate (,,) |* g1 |* g2 ||* g3

instance Biplate (Maybe Sem) GeniVal where
  biplate (Just s) = plate Just ||+ s
  biplate Nothing  = plate Nothing

-- Pred is what I had in mind here
instance ((Collectable a, Collectable b, Collectable c)
           => Collectable (a,b,c)) where
  collect (a,b,c) = collect a . collect b . collect c

emptyPred :: Pred
emptyPred = (mkGAnon,mkGAnon,[])
\end{code}

\section{Utility functions}

\begin{code}
removeConstraints :: SemInput -> SemInput
removeConstraints (x, _, _) = (x, [], [])

sortByMostConstants :: Sem -> Sem
sortByMostConstants = sortBy (flip compare `on` constants)

class HasConstants a where
  constants :: a -> Int

instance HasConstants GeniVal where
  constants g = if isConst2 g then 1 else 0
   where
    isConst2 :: GeniVal -> Bool
    isConst2 g = isJust (gConstraints g) && isNothing (gLabel g)

instance HasConstants a => HasConstants [a] where
  constants = sum . map constants

instance HasConstants Pred where
  constants (h, p, args) = constants (h:p:args)
\end{code}

\begin{code}
-- | Sort semantics first according to its predicate, and then to its handles.
sortSem :: Sem -> Sem
sortSem = sortBy (\(h1,p1,a1) (h2,p2,a2) -> compare (p1, h1:a1) (p2, h2:a2))

-- | Given a Semantics, return the string with the proper keys
--   (propsymbol+arity) to access the agenda
toKeys :: Sem -> [String]
toKeys l = map (\(_,prop,par) -> show prop ++ (show $ length par)) l
\end{code}

\subsection{Traversal}

A replacement on a predicate is just a replacement on its parameters

\begin{code}
instance DescendGeniVal Pred where
  descendGeniVal s (h, n, lp) = (descendGeniVal s h, descendGeniVal s n, descendGeniVal s lp)
\end{code}

\subsection{Pretty printing}

\begin{code}
showSem :: Sem -> String
showSem l =
    "[" ++ (unwords $ map showPred l) ++ "]"

showPred :: Pred -> String
showPred (h, p, l) = showh ++ show p ++ "(" ++ unwords (map show l) ++ ")"
  where
    hideh g = case gConstraints g of
                Just [c] -> "genihandle" `isPrefixOf` c
                _        -> False
    --
    showh = if (hideh h) then "" else (show h) ++ ":"
\end{code}

% ----------------------------------------------------------------------
\section{Subsumption}
% ----------------------------------------------------------------------

We say that $L \sqsubseteq I$ (or $L$ subsumes $I$; mnemonic: $L$ for lemma,
and $I$ for input semantics as an example use) if for each literal $l \in L$,
we can find a \emph{distinct} literal $i \in I$ such that $l \sqsubseteq i$
(no resuing literals in $I$).

Notes about the subsumeSem function:
\begin{enumerate}
\item We return multiple results because it's important to take into
      account the possibility that one semantics subsumes different
      subsets of an another semantics.  For example:
      \semexpr{name(?X,?Y)} subsumes two different parts of
      \semexpr{love(j,m), name(j,john), name(m,mary)}
\item You MUST propagate the substitutions
      throughout any objects that contain the semantics.
\item We return the unified semantics and not just the substitutions
      so that we know to do with anonymous variables.
\end{enumerate}

\begin{code}
-- | @x `subsumeSem` y@ returns all the possible ways to unify
--   @x@ with some SUBSET of @y@ so that @x@ subsumes @y@.
--   If @x@ does NOT subsume @y@, we return the empty list.
subsumeSem :: Sem -> Sem -> [(Sem,Subst)]
subsumeSem x y | length x > length y = []
subsumeSem x_ y_ =
  map (first sortSem) $ subsumeSemH x y
 where
  -- the sorting is just to ensure that we get results in the same order
  -- not sure if it's really needed
  x = sortByMostConstants x_
  y = sortByMostConstants y_

subsumeSemH :: Sem -> Sem -> [(Sem,Subst)]
subsumeSemH [] [] = [ ([], Map.empty) ]
subsumeSemH _ []  = error "subsumeSemH: got longer list in front"
subsumeSemH []     _  = [ ([], Map.empty) ]
subsumeSemH (x:xs) ys = nub $
 do let attempts = zip ys $ map (subsumePred x) ys
    (y, Just (x2, subst)) <- attempts
    let next_xs = replace subst xs
        next_ys = replace subst $ delete y ys
        prepend = insert x2 *** appendSubst subst
    prepend `fmap` subsumeSemH next_xs next_ys
\end{code}

As for literals $l$ and $i$, $l \sqsubseteq i$ if
\begin{enumerate}
\item For the corresponding relations $lr$ and $ir$, $lr \sqsubseteq ir$
\item $l$ and $i$ have the same arity
\item All arguments $l_n \sqsubseteq i_n$
\end{enumerate}

\begin{code}
-- | @p1 `subsumePred` p2@... FIXME
subsumePred :: Pred -> Pred -> Maybe (Pred, Subst)
subsumePred (h1, p1, la1) (h2, p2, la2) =
  if length la1 == length la2
  then do let hpla1 = h1:p1:la1
              hpla2 = h2:p2:la2
          (hpla, sub) <- hpla1 `allSubsume` hpla2
          return (toPred hpla, sub)
  else Nothing
 where
  toPred (h:p:xs) = (h, p, xs)
  toPred _ = error "subsumePred.toPred"
\end{code}

% ----------------------------------------------------------------------
\section{Unification}
% ----------------------------------------------------------------------

We say that $X \sqcup Y$ if...
TODO

\begin{code}
-- We return the list of minimal ways to unify two semantics.
-- By minimal, I mean that any literals that are not the product of a
-- succesful unification really do not unify with anything else.
unifySem :: Sem -> Sem -> [(Sem,Subst)]
unifySem xs_ ys_ = 
 map (first sortSem) $
 if length xs_ < length ys_
    then unifySemH xs ys
    else unifySemH ys xs
 where
  xs = sortByMostConstants xs_
  ys = sortByMostConstants ys_

-- list monad for Prolog-style backtracking.
unifySemH :: Sem -> Sem -> [(Sem,Subst)]
unifySemH [] [] = return ([], Map.empty)
unifySemH [] xs = return (xs, Map.empty)
unifySemH xs [] = error $ "unifySem: shorter list should always be in front: " ++ showSem xs
unifySemH (x:xs) ys = nub $ do
 let attempts = zip ys $ map (unifyPred x) ys
 if all (isNothing . snd) attempts
    then first (x:) `fmap` unifySemH xs ys -- only include x unmolested if no unification succeeds
    else do (y, Just (x2, subst)) <- attempts
            let next_xs = replace subst xs
                next_ys = replace subst $ delete y ys
                prepend = insert x2 *** appendSubst subst
            prepend `fmap` unifySemH next_xs next_ys

unifyPred :: Pred -> Pred -> Maybe (Pred, Subst)
unifyPred (h1, p1, la1) (h2, p2, la2) =
  if length la1 == length la2
  then do let hpla1 = h1:p1:la1
              hpla2 = h2:p2:la2
          (hpla, sub) <- hpla1 `unify` hpla2
          return (toPred hpla, sub)
  else Nothing
 where
  toPred (h:p:xs) = (h, p, xs)
  toPred _ = error "unifyPred.toPred"
\end{code}
