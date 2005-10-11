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

This module provides basic datatypes like GNode.  
Operations on these datatypes can be found in the Bfuncs module.

\begin{code}
module Btypes(
   GNode(..), GType(Subs, Foot, Lex, Other), 
   Ttree(..), MTtree, SemPols,
   Ptype(Initial,Auxiliar,Unspecified), 
   Pred, Flist, AvPair, GeniVal(..),
   Lexicon, ILexEntry(..), Macros, Sem, SemInput,
   Subst,
   emptyMacro, emptyLE, emptyGNode, emptyPred,
   readGeniVal, fromGConst, fromGVar)
where
\end{code}

\ignore{
\begin{code}
import Data.Char (isUpper)
import qualified Data.Map as Map 
import Data.List (intersect, intersperse, sortBy, nub)
import Data.Tree
import General (toUpperHead, toLowerHead)
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
type Macros = [MTtree]
\end{code}

\begin{code}
data Ttree a = TT{params  :: [GeniVal],
                  pfamily :: String,
                  pidname :: String,
                  pfeat :: Flist,
                  ptype :: Ptype,
                  tree :: Tree a,
                  -- optimisation stuff
                  ptpredictors  :: [(AvPair,Int)],
                  ptpolarities  :: Map.Map String Int
                  }
           deriving Show

data Ptype = Initial | Auxiliar | Unspecified   
             deriving (Show, Eq)
\end{code}

\paragraph{emptyMacro} provides a null tree which you can use for
various debugging or display purposes.

\begin{code}
emptyMacro :: MTtree
emptyMacro = TT { params  = [],
                  pidname = "", 
                  pfamily = "",
                  pfeat = [],
                  ptype = Unspecified,
                  tree  = Node emptyGNode [],
                  ptpredictors = [],
                  ptpolarities = Map.empty }
\end{code}

Auxiliary types used during the parsing of the Lexicon.  
A lexicon maps semantic predicates to lexical entries.

\begin{code}
type Lexicon = Map.Map String [ILexEntry]
type SemPols  = [Int]
data ILexEntry = ILE{iword       :: String,
                     ifamname    :: String,
                     iparams     :: [GeniVal],
                     ipfeat      :: Flist,
                     ifilters    :: Flist,
                     iptype      :: Ptype,
                     isemantics  :: Sem,
                     isempols    :: [SemPols] }
               deriving (Show, Eq)

emptyLE :: ILexEntry  
emptyLE = ILE { iword = "",
                ifamname = "", 
                iparams = [],
                ipfeat   = [],
                ifilters = [],
                iptype = Unspecified,
                isemantics = [],
                isempols   = [] }
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
type AvPair  = (String,GeniVal)
\end{code}

\subsection{GeniVal}

\begin{code}
data GeniVal = GConst [String]
             | GVar   String
             | GAnon
  deriving (Eq,Ord)
\end{code}

To maintain some semblance of backwards comptability, we read/show GeniVal 
in the following manner:
\begin{itemize}
\item Constants have the first letter lower cased.
\item Variables have the first letter capitalised.
\item Anonymous variables are underscores. 
\end{itemize}

\begin{code}
instance Show GeniVal where
  show (GConst x) = concat $ intersperse " ! " $ map toLowerHead x
  show (GVar x)   = toUpperHead x
  show GAnon      = "_"

-- Should figure out how to use a standard type class sometime later
readGeniVal :: String -> GeniVal
readGeniVal str = 
  let h = head str 
      r | h == '_'  = GAnon
        | isUpper h = GVar str 
        | otherwise = GConst [str]
  in r 
\end{code}

\paragraph{fromGConst and fromGVar} respectively extract the constant or  
variable string value of a GeniVal, assuming it has that kind of value.

\begin{code}
fromGConst :: GeniVal -> [String]
fromGConst (GConst x) = x
fromGConst x = error ("fromGConst on " ++ show x)

fromGVar :: GeniVal -> String
fromGVar (GVar x) = x
fromGVar x = error ("fromGVar on " ++ show x)
\end{code}

% ----------------------------------------------------------------------
\section{Semantics}
\label{btypes_semantics}
% ----------------------------------------------------------------------

\begin{code}
-- handle, predicate, parameters
type Pred = (GeniVal, String, [GeniVal])
type Sem = [Pred]
type SemInput = (Sem,Flist)
type Subst = [(String, GeniVal)]
emptyPred = (GAnon,"",[])
\end{code}


