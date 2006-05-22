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

\chapter{Tags}
\label{cha:Tags}

This module provides basic datatypes specific to Tree Adjoining Grammar
(TAG) and some low-level operations. Note that we don't handle
substitution and adjunction here; see sections \ref{sec:substitution}
and \ref{sec:adjunction} instead.  

\begin{code}
module NLP.GenI.Tags(
   -- Main Datatypes
   Tags, TagElem(..), TagItem(..), TagSite(..),
   TagDerivation, emptyTE,
   ts_synIncomplete, ts_semIncomplete, ts_tbUnificationFailure,
   ts_noRootCategory, ts_wrongRootCategory,

   -- Functions from Tags
   addToTags, tagLeaves,

   -- Functions from TagElem
   setTidnums, 

   -- General functions
   mapBySem, drawTagTrees, subsumedBy, showTagSites,
   collect, detectSites
) where
\end{code}

\ignore{
\begin{code}
import Data.Char(toUpper)
import qualified Data.Map as Map
import Data.List (intersperse)
import Data.Tree

import NLP.GenI.Btypes (Ptype(Initial, Auxiliar), SemPols,
               GNode(gup, glexeme, gnname, gaconstr, gdown, gtype),
               GType(Subs), Flist,
               Replacable(..), replace_Flist,
               Collectable(..), Idable(..),
               Sem, Pred, emptyPred, 
               emptyGNode,
               showPairs, showSem, lexemeAttributes,
               )
import NLP.GenI.General (treeLeaves, groupByFM)
\end{code}
}

% ----------------------------------------------------------------------
\section{Tags}
% ----------------------------------------------------------------------

Tags is the datatype for an anchored grammar. The grammar associates
a set of semantic predicates to a list of trees each.

\begin{code}
type Tags = Map.Map String [TagElem]                            
\end{code}

\paragraph{addToTags} Given a Tags (a FM), a key (a String) and a TagElem 
it adds the elem to the list of elements associated to the key.

\begin{code}
addToTags :: Tags -> String -> TagElem -> Tags
addToTags t k e = Map.insertWith (++) k [e] t
\end{code}

% ----------------------------------------------------------------------
\section{TagElem}
% ----------------------------------------------------------------------

Final types used for the combined macros + lexicon.  We assume that
a two trees are the same iff they have the same tidnum.  To make this
work, we assign each tree with a unique id during the process of
combining macros with lexicon (see section \ref{sec:combine_macros}).

\begin{code}
-- type TPredictors = Map.Map AvPair Int 
data TagSite = TagSite !String !Flist !Flist
  deriving (Show, Eq, Ord)

data TagElem = TE {
                   idname       :: String,
                   ttreename    :: String,
                   tidnum       :: Integer,
                   ttype        :: !Ptype,
                   ttree        :: Tree GNode,
                   tsemantics   :: Sem,
                   -- optimisation stuff
                   -- (polarity key to charge interval)
                   tpolarities  :: Map.Map String (Int,Int), 
                   tinterface   :: Flist,  -- for restrictors 
                   tsempols     :: [SemPols]
                }
             deriving (Show, Eq)
\end{code}

A tag derivation history consists of 1) a counter representing the number of 
substitution or adjunctions which have been done to the tree
(this is neccesary for distinguishing between derivation nodes later on)
(note that substitutions and adjunctions that have already been done to
subtrees are not counted),
and 2) a list of 3-tuples representing the operation (s for substitution, a
for adjunction), the name of the child tree, and the name of the parent 
tree.

\begin{code}
type TagDerivation = (Int, [ (Char, String, String) ])
\end{code}

\begin{code}
instance Ord TagElem where
  compare t1 t2 = 
    case (ttype t1, ttype t2) of
         (Initial, Initial)   -> compareId 
         (Initial, Auxiliar)  -> LT
         (Auxiliar, Initial)  -> GT
         (Auxiliar, Auxiliar) -> compareId 
         _                    -> error "TagElem compare not exhaustively defined"
    where compareId  = compare (tidnum t1) (tidnum t2)

instance Replacable TagElem where
  replace s te =
    te { tinterface = replace_Flist s (tinterface te)
       , ttree      = replace s (ttree te)
       , tsemantics = replace s (tsemantics te) }

instance Replacable TagSite where
  replace s (TagSite n fu fd) = TagSite n (replace_Flist s fu) (replace_Flist s fd)

instance Collectable TagElem where
  collect t = (collect $ tinterface t) . (collect $ ttree t) 
            . (collect $ tsemantics t)

instance Idable TagElem where
  idOf = tidnum
\end{code}

\begin{code}
emptyTE :: TagElem
emptyTE = TE { idname = "",
               ttreename = "",
               tidnum = -1,
               ttype  = Initial,
               ttree  = Node emptyGNode [],
               tsemantics = [], 
               tpolarities = Map.empty,
               tsempols    = [],
               tinterface  = []
             }

-- | Given a tree(GNode) returns a list of substitution or adjunction
--   nodes.
detectSites :: Tree GNode -> ([TagSite], [TagSite])
detectSites t = (sites isSub, sites (not.gaconstr))
 where
 sites match = [ TagSite (gnname n) (gup n) (gdown n) | n <- flatten t, match n ]
 isSub n = gtype n == Subs
\end{code}

\subsection{Unique ID}

TagElem comparison relies exclusively on \fnparam{tidnum}, so you must
ensure that every TagElem you use has a unique ID.  We provide two
helpful functions for this.  These are most likely useful \emph{between}
lexical selection and generation proper, because during generation
proper, you can simply keep a counter within a State monad to assign
unique IDs to new TagElems.

\paragraph{setTidnums} assigns a unique id to each element of this list,
that is, an integer between 1 and the size of the list.

\begin{code}
setTidnums :: [TagElem] -> [TagElem]
setTidnums xs = zipWith (\c i -> c {tidnum = i}) xs [1..]
\end{code}

% ----------------------------------------------------------------------
\section{TAG Item}
% ----------------------------------------------------------------------

TagItem is a generalisation of TagElem.  

\begin{code}
class TagItem t where 
  tgIdName    :: t -> String
  tgIdNum     :: t -> Integer
  tgSemantics :: t -> Sem
\end{code}

\begin{code}
instance TagItem TagElem where
  tgIdName = idname
  tgIdNum  = tidnum
  tgSemantics = tsemantics
\end{code}

% ----------------------------------------------------------------------
\section{Map by sem}
% ----------------------------------------------------------------------

The mapBySem function sorts trees into a Map.Map organised by the
first literal of their semantics.  This is useful in at least three
places: the polarity optimisation, the gui display code, and code for
measuring the efficiency of Geni.  Note: trees with a null semantics
are filed under an empty predicate, if any.

\begin{code}
mapBySem :: (TagItem t) => [t] -> Map.Map Pred [t]
mapBySem ts = 
  let gfn t = case tgSemantics t of
              []    -> emptyPred
              (x:_) -> x
  in groupByFM gfn ts
\end{code}

\texttt{subsumedBy} \texttt{cs} \texttt{ts} determines if the 
candidate semantics \texttt{cs} is subsumed by the proposition
semantics \texttt{ts}.  Notice how the proposition semantics
is only a single item where as the candidate semantics is a 
list.

We assume that 
\begin{itemize}
\item most importantly that cs has already its semantics
      instatiated (all variables assigned)
\item cs and ts are sorted 
\item the list in each element of cs and ts is itself sorted 
\end{itemize}

\begin{code}
subsumedBy :: Sem -> Pred -> Bool 
subsumedBy [] _ = False 
subsumedBy ((ch, cp, cla):cl) (th, tp,tla)
    | (ch == th) && (cp == tp) && (cla == tla) = True 
    -- if we haven't yet overshot, try for the next one
    | cp  < tp                   = subsumedBy cl (th, tp, tla)
    | otherwise                  = False
\end{code}

%% ----------------------------------------------------------------------
%\section{Predictors}
%% ----------------------------------------------------------------------
%
%\paragraph{sumPredictors} merges two tree predictors together.
%The idea is that if one tree has $-2np$ and the other $+np$, then
%this will merge to $-np$.  
%
%\begin{code}
%sumPredictors :: TPredictors -> TPredictors -> TPredictors
%sumPredictors tp1 tp2 = 
%  filterFM (\_ e -> e /= 0) $ plusFM_C (+) tp1 tp2
%\end{code}

% ----------------------------------------------------------------------
\section{Extracting sentences}
% ----------------------------------------------------------------------

\paragraph{tagLeaves} returns the leaves of a TAG tree as a list of
lemmas and features.  This is meant for converting TAG trees to 
sentences, the idea being that you'd pass the lemma and feature pairs 
to morphological generator and get an inflected form for each word.  
Note that because of the possibility of atomic disjunction on each
node we return a list of lemmas for each leaf.

\begin{code}
tagLeaves :: TagElem -> [([String],Flist)]
tagLeaves te = map tagLeaf $ (treeLeaves.ttree) te 

tagLeaf :: GNode -> ([String],Flist)
tagLeaf node = 
  let guppy  = gup node
      grab la = [ (map toUpper) . show $ v | (a,v) <- guppy, a == la ]
      lastResort = case concatMap grab lexemeAttributes of
                   [] -> [gnname node]
                   x  -> x
      output = case glexeme node of
               []     -> lastResort
               lexeme -> lexeme
  in (output, gup node)
\end{code}


% ----------------------------------------------------------------------
\section{Drawings TAG Tree}
% ----------------------------------------------------------------------

This provides a convenient wrapper for drawing a TAG tree or list of
trees in String form.  

\begin{code}
drawTagTrees :: [TagElem] -> String
drawTagTrees tes = concat $ intersperse "\n" $ map drawTagTree tes  

drawTagTree :: TagElem -> String 
drawTagTree te = idname te ++ ":\n"
-- FIXME : BROKEN!
-- ++ (drawTree $ ttree te)
\end{code}

\paragraph{showTagSites} is useful for debugging adjunction and
substitution nodes

\begin{code}
showTagSites :: [TagSite] -> String
showTagSites sites = concat $ intersperse "\n  " $ map fn sites
  where fn (TagSite n t b) = n ++ "/" ++ showPairs t ++ "/" ++ showPairs b
\end{code}

% ----------------------------------------------------------------------
\section{Diagnostic messages}
% ----------------------------------------------------------------------

Diagnostic messages let us know why a TAG tree is not returned as a result.
Whenever GenI decides to discard a tree, it sets the tdiagnostic field of 
the TagElem so that the person using a debugger can find out what went wrong.

\begin{code}
ts_synIncomplete, ts_tbUnificationFailure, ts_noRootCategory :: String
ts_synIncomplete = "syntactically incomplete"
ts_tbUnificationFailure = "top/bot unification failure"
ts_noRootCategory = "root category unset?!"

ts_wrongRootCategory :: [String] -> [String] -> String
ts_wrongRootCategory c cats = "wrong root category(ies): " ++ (show c) ++ " should be:" ++ (show cats)

ts_semIncomplete :: [Pred] -> String
ts_semIncomplete sem = "semantically incomplete - missing:  " ++ showSem sem
\end{code}
