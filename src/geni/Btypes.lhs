% GeNI surface realiser
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

This module provides basic datatypes like GNode.  
Operations on these datatypes can be found in the Bfuncs module.

\begin{code}
module Btypes(
   GNode(..), GType(Subs, Foot, Lex, Other), 
   Ttree(..), MTtree,
   Ptype(Initial,Auxiliar,Unspecified), 
   Pred, Flist, AvPair, 
   Lexicon, ILexEntry(..), Macros, Sem, SemInput,
   Subst,
   emptyMacro, emptyLE, emptyGNode, emptyPred)
where
\end{code}

\ignore{
\begin{code}
import Debug.Trace -- for test stuff
import Data.Bits
import Data.Char (isUpper)
import Data.FiniteMap (FiniteMap, fmToList, 
                       emptyFM, isEmptyFM, lookupFM, 
                       addToFM, addToFM_C)
import Data.List (intersect, intersperse, sortBy, nub)
import Data.Tree
\end{code}
}

% ----------------------------------------------------------------------
\section{Grammar}
% ----------------------------------------------------------------------

A grammar is composed of some unanchored trees (macros) and individual
lexical entries. The trees are grouped into families. Every lexical
entry is associated with a single family.  See section section
\ref{sec:combine_macros} for the process that combines lexical items
and trees into a set of anchored trees.

\begin{code}
type MTtree = Ttree GNode
type Macros = FiniteMap String [MTtree]
\end{code}

\begin{code}
data Ttree a = TT{params  :: [String],
                  pidname :: String,
                  pfeat :: Flist,
                  ptype :: Ptype,
                  tree :: Tree a,
                  -- optimisation stuff
                  ptpredictors  :: [(AvPair,Int)],
                  ptpolarities  :: FiniteMap String Int
                  }
           deriving Show

data Ptype = Initial | Auxiliar | Unspecified   
             deriving (Show, Eq)

instance (Show k, Show e) => Show (FiniteMap k e) where 
  show fm = show $ fmToList fm
\end{code}

\paragraph{emptyMacro} provides a null tree which you can use for
various debugging or display purposes.

\begin{code}
emptyMacro :: MTtree
emptyMacro = TT { params  = [],
                  pidname = "", 
                  pfeat = [],
                  ptype = Unspecified,
                  tree  = Node emptyGNode [],
                  ptpredictors = [],
                  ptpolarities = emptyFM }
\end{code}

Auxiliary types used during the parsing of the Lexicon.  
A lexicon maps semantic predicates to lexical entries.

\begin{code}
type Lexicon   = FiniteMap String [ILexEntry]
data ILexEntry = ILE{iword :: String,
                     icategory :: String,
                     ifamname :: String,
                     iparams :: [String],
                     ipfeat :: Flist,
                     iptype :: Ptype,
                     isemantics :: Sem,
                     icontrol   :: String,
                     iprecedence :: Int}
               deriving (Show, Eq)

emptyLE :: ILexEntry  
emptyLE = ILE { iword = "",
                icategory = "",
                ifamname = "", 
                iparams = [],
                ipfeat  = [],
                iptype = Unspecified,
                isemantics = [],
                icontrol   = "",
                iprecedence = 0 }
\end{code}

% ----------------------------------------------------------------------
\section{GNode}
% ----------------------------------------------------------------------

A GNode is a single node of a syntactic tree. It has a name (gnname),
top and bottom feature structures (gup, gdown), a lexeme 
(ganchor, glexeme: False and empty string if n/a),  and some flags 
information (gtype, gaconstr).

\begin{code}
data GNode = GN{gnname :: String,
                gup    :: Flist,
                gdown  :: Flist,
                ganchor  :: Bool,
                glexeme  :: String,
                gtype    :: GType,
                gaconstr :: Bool}
           deriving Eq

-- Node type used during parsing of the grammar 
data GType = Subs | Foot | Lex | Other
           deriving (Show, Eq)
\end{code}

\paragraph{emptyGNode} provides a null gnode which you can use
for various debugging or display purposes.

\begin{code}
emptyGNode = GN { gnname = "",
                  gup = [], gdown = [],
                  ganchor = False,
                  glexeme = "",
                  gtype = Other,
                  gaconstr = False }
\end{code}

% ----------------------------------------------------------------------
\section{Features and variables}
% ----------------------------------------------------------------------

\begin{code}
type Flist   = [AvPair]
type AvPair  = (String,String)
\end{code}

% ----------------------------------------------------------------------
\section{Semantics}
\label{btypes_semantics}
% ----------------------------------------------------------------------

\begin{code}
-- handle, predicate, parameters
type Pred = (String, String, [String])
type Sem = [Pred]
type SemInput = (Sem,Flist)
type Subst = [(String, String)]
emptyPred = ("","",[])
\end{code}


