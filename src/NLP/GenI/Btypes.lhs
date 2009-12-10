% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
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

\chapter{Btypes}
\label{cha:Btypes}

This module provides basic datatypes like GNode, as well as operations on trees
and nodes.

For now, it doubles as a catch-all core module by exposing functions from other
modules such as NLP.GenI.Semantics.

\ignore{
\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module NLP.GenI.Btypes(
   -- Datatypes
   GNode(..), GType(Subs, Foot, Lex, Other), NodeName,
   Ttree(..), SemPols, TestCase(..),
   Ptype(Initial,Auxiliar,Unspecified),
   Pred, Flist, AvPair(..), GeniVal(..),
   Lexicon, ILexEntry(..), Macros, Sem, LitConstr, SemInput, Subst,
   emptyLE, emptyGNode, emptyMacro,

   -- GNode stuff
   gCategory, showLexeme, lexemeAttributes, gnnameIs,

   -- Functions from Tree GNode
   plugTree, spliceTree,
   root, rootUpd, foot, setLexeme, setAnchor,

   -- Functions from Sem
   toKeys, subsumeSem, sortSem, showSem, showPred,
   emptyPred,

   -- Functions from Flist
   sortFlist, unify, unifyFeat, mergeSubst,
   showFlist, showPairs, showAv,

   -- Other functions
   replace, DescendGeniVal(..), replaceList,
   Collectable(..), Idable(..),
   alphaConvert, alphaConvertById,
   fromGConst, fromGVar,
   isConst, isVar, isAnon,

   -- Polarities

) where

-- import Debug.Trace -- for test stuff
import qualified Data.Map as Map

import NLP.GenI.FeatureStructures
import NLP.GenI.GeniVal
import NLP.GenI.Lexicon
import NLP.GenI.PolarityTypes (SemPols)
import NLP.GenI.Semantics
import NLP.GenI.Tags
import NLP.GenI.TestSuite
import NLP.GenI.TreeSchemata
