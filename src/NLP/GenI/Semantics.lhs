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
\label{fn:subsumeSem}
% ----------------------------------------------------------------------

FIXME: comment fix

Given tsem the input semantics, and lsem the semantics of a potential
lexical candidate, returns a list of possible ways that the lexical
semantics could subsume the input semantics.  We return a pair with
the semantics that would result from unification\footnote{We need to
do this because there may be anonymous variables}, and the
substitutions that need to be propagated throughout the rest of the
lexical item later on.

Note: we return more than one possible substitution because s could be
different subsets of ts.  Consider, for example, \semexpr{love(j,m),
  name(j,john), name(m,mary)} and the candidate \semexpr{name(X,Y)}.

TODO WE ASSUME BOTH SEMANTICS ARE ORDERED and that the input semantics is
non-empty.

\begin{code}
subsumeSem :: Sem -> Sem -> [(Sem,Subst)]
subsumeSem tsem lsem =
  subsumeSemHelper ([],Map.empty) (reverse tsem) (reverse lsem)
\end{code}

This is tricky because each substep returns multiple results.  We solicit
the help of accumulators to keep things from getting confused.

\begin{code}
subsumeSemHelper :: (Sem,Subst) -> Sem -> Sem -> [(Sem,Subst)]
subsumeSemHelper _ [] _  =
  error "input semantics is empty in subsumeSemHelper"
subsumeSemHelper acc _ []      = [acc]
subsumeSemHelper acc tsem (hd:tl) =
  let (accSem,accSub) = acc
      -- does the literal hd subsume the input semantics?
      pRes = subsumePred tsem hd
      -- toPred reconstructs the literal hd with new parameters p.
      -- The head of the list is taken to be the handle.
      toPred p = (head p, snd3 hd, tail p)
      -- next adds a result from predication subsumption to
      -- the accumulators and goes to the next recursive step
      next (p,s) = subsumeSemHelper acc2 tsem2 tl2
         where tl2   = replace s tl
               tsem2 = replace s tsem
               acc2  = (toPred p : accSem, mergeSubst accSub s)
  in concatMap next pRes
\end{code}

\fnlabel{subsumePred}
The first Sem s1 and second Sem s2 are the same when we start we circle on s2
looking for a match for Pred, and meanwhile we apply the partical substitutions
to s1.  Note: we treat the handle as if it were a parameter.

\begin{code}
subsumePred :: Sem -> Pred -> [([GeniVal],Subst)]
subsumePred [] _ = []
subsumePred ((h1, p1, la1):l) (pred2@(h2,p2,la2)) =
    -- if we found the proper predicate
    if ((p1 == p2) && (length la1 == length la2))
    then let mrs  = unify (h1:la1) (h2:la2)
             next = subsumePred l pred2
         in maybe next (:next) mrs
    else if (p1 < p2) -- note that the semantics have to be reversed!
         then []
         else subsumePred l pred2
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
  not (null s) ==> not . null $ subsumeSem s s
 where
  s = map fromGTestPred lits

prop_subsumePred_reflexive pred =
  not . null $ subsumePred [s] s
 where
  s = fromGTestPred pred

fromGTestPred (GTestPred h r as) = (h,r,as)

data GTestPred = GTestPred GeniVal GeniVal [GeniVal]

instance Show GTestPred where
  show = showPred . fromGTestPred

instance Arbitrary GTestPred where
 arbitrary =
  do handle <- arbitrary
     rel  <- oneof [ fmap (GConst . nub . sort . map fromGTestString) arbitrary ]
     args <- arbitrary
     return $ GTestPred handle rel args
 coarbitrary =
  error "No instance of coarbitrary for GTestPred"
\end{code}
}
