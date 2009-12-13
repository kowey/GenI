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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.Semantics where

import Data.Generics.PlateDirect
import Data.List ( isPrefixOf, nub, sort, sortBy )
import qualified Data.Map as Map

import NLP.GenI.FeatureStructures
import NLP.GenI.General(snd3)
import NLP.GenI.GeniVal

import Test.HUnit
import Test.QuickCheck hiding (collect)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
\end{code}
}

\begin{code}
-- handle, predicate, parameters
type Pred = (GeniVal, GeniVal, [GeniVal])
type Sem = [Pred]
type LitConstr = (Pred, [String])
type SemInput  = (Sem,Flist,[LitConstr])

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
-- | @lsem `subsumeSem` tsem@ returns the list of ways to unify
--   the two semantics such that @lsem@ subsumes @tsem@.  If
--   @lsem@ does NOT subsume @tsem@, we return the empty list.
subsumeSem :: Sem -> Sem -> [(Sem,Subst)]
subsumeSem lsem tsem =
  subsumeSemHelper ([],Map.empty) (revsort lsem) (revsort tsem)
 where
  revsort = reverse . sortSem

-- This is tricky because each substep returns multiple results.
-- We solicit the help of accumulators to keep things from getting confused.
subsumeSemHelper :: (Sem,Subst) -> Sem -> Sem -> [(Sem,Subst)]
subsumeSemHelper acc@([], subst) [] [] | Map.null subst  = [acc]
subsumeSemHelper acc [] _      = [acc]
subsumeSemHelper acc (hd:tl) tsem =
  let (accSem,accSub) = acc
      -- does the literal hd subsume the input semantics?
      pRes = hd `subsumePred` tsem
      -- toPred reconstructs the literal hd with new parameters p.
      -- The head of the list is taken to be the handle.
      toPred p = (head p, snd3 hd, tail p)
      -- next adds a result from predication subsumption to
      -- the accumulators and goes to the next recursive step
      next (p,s) = subsumeSemHelper acc2 tl2 tsem2
         where tl2   = replace s tl
               tsem2 = replace s tsem
               acc2  = (toPred p : accSem, mergeSubst accSub s)
  in concatMap next pRes
\end{code}

As for literals $l$ and $i$, $l \sqsubseteq i$ if
\begin{enumerate}
\item For the corresponding relations $lr$ and $ir$, $lr \sqsubseteq ir$
\item $l$ and $i$ have the same arity
\item All arguments $l_n \sqsubseteq i_n$
\end{enumerate}

\begin{code}
-- The first Sem s1 and second Sem s2 are the same when we start we circle on s2
-- looking for a match for Pred, and meanwhile we apply the partical substitutions
-- to s1.  Note: we treat the handle as if it were a parameter.
subsumePred :: Pred -> Sem -> [([GeniVal],Subst)]
subsumePred _ [] = []
subsumePred (pred2@(h2,p2,la2)) ((h1, p1, la1):l) =
    -- if we found the proper predicate
    if ((p1 == p2) && (length la1 == length la2))
    then let mrs  = unify (h1:la1) (h2:la2)
             next = pred2 `subsumePred` l
         in maybe next (:next) mrs
    else if (p1 < p2) -- note that the semantics have to be reversed!
         then []
         else pred2 `subsumePred` l
\end{code}

\ignore{
% ----------------------------------------------------------------------
% Testing
% ----------------------------------------------------------------------

\begin{code}
testSuite :: Test.Framework.Test
testSuite = testGroup "subsumption"
 [ testSubsumePred
 , testProperty "reflexive" prop_subsumption_reflexive
 ]

testSubsumePred :: Test.Framework.Test
testSubsumePred = testGroup "subsumePred"
 [ testProperty "reflexive" prop_subsumePred_reflexive ]

prop_subsumption_reflexive lits =
  not (null s) ==> not . null $ s `subsumeSem` s
 where
  s = map fromGTestPred lits

prop_subsumePred_reflexive pred =
  not . null $ s `subsumePred` [s]
 where
  s = fromGTestPred pred

fromGTestPred (GTestPred h r as) = (h,r,as)

data GTestPred = GTestPred GeniVal GeniVal [GeniVal]

instance Show GTestPred where
  show = showPred . fromGTestPred

instance Arbitrary GTestPred where
 arbitrary =
  do handle <- arbitrary
     rel  <- oneof [ fmap (GConst . nub . sort . map fromGTestString) arbitrary1 ]
     args <- arbitrary
     return $ GTestPred handle rel args
 coarbitrary =
  error "No instance of coarbitrary for GTestPred"
\end{code}
}
