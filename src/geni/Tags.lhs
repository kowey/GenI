\chapter{Tags}

This module provides basic datatypes specific to Tree Adjoining Grammar
(TAG). Note that we don't handle substitution and adjunction here; 
see sections \ref{sec:substitution} and \ref{sec:adjunction} instead.  

\begin{code}
module Tags(
   -- Main Datatypes
   Tags, TagElem(TE), TagSite, TagDerivation, 
   SemMap, emptyTE,

   -- Projection Functions
   idname, tidnum, derivation, ttype, ttree, 
   substnodes, adjnodes, 
   tsemantics, tpolarities, tpolpaths,
   showfeats,

   -- Functions from Tags
   addToTags, findInTags, 

   -- Functions from TagElem
   substTagElem, appendToVars,

   -- General functions
   mapBySem, drawTagTrees, subsumedBy, showTagSites,
) where
\end{code}

\ignore{
\begin{code}
import Data.List (intersperse)
import Data.Tree
import FiniteMap (FiniteMap, emptyFM, addToFM_C, lookupFM,
                  lookupWithDefaultFM
                  )

import Btypes (Ptype(Initial, Auxiliar), 
               Subst, GNode(gup, gdown), Flist, 
               Sem, Pred, emptyPred, BitVector,
               emptyGNode,
               substFlist, substTree, substSem, showPairs,
               isAnon, isVar, groupByFM)
\end{code}
}

% ----------------------------------------------------------------------
\section{Tags}
% ----------------------------------------------------------------------

Tags is the datatype for an anchored grammar. The grammar associates
a set of semantic predicates to a list of trees each.

\begin{code}
type Tags = FiniteMap String [TagElem]                            
\end{code}

\paragraph{addToTags} Given a Tags (a FM), a key (a String) and a TagElem 
it adds the elem to the list of elements associated to the key.

\begin{code}
addToTags :: Tags -> String -> TagElem -> Tags
addToTags t k e = addToFM_C (++) t k [e]
\end{code}

\begin{code}
findInTags :: Tags -> String -> [TagElem]
findInTags t k = lookupWithDefaultFM t [] k
\end{code}

% ----------------------------------------------------------------------
\section{TagElem}
% ----------------------------------------------------------------------

Final types used for the combined macros + lexicon.  We assume that
a two trees are the same iff they have the same tidnum.  To make this
work, we assign each tree with a unique id during the process of
combining macros with lexicon (see section \ref{sec:combine_macros}).

\begin{code}
-- type TPredictors = FiniteMap AvPair Int 
type TagSite = (String, Flist, Flist)
data TagElem = TE {
                   idname :: String,
                   tidnum :: Integer,
                   derivation :: TagDerivation,
                   ttype :: Ptype,
                   ttree :: Tree GNode,
                   substnodes :: [TagSite],
                   adjnodes :: [TagSite],
                   tsemantics :: Sem,
                   -- optimisation stuff
                   tpolarities  :: FiniteMap String Int,
                   -- tpredictors  :: TPredictors,
                   tpolpaths    :: BitVector,
                   -- display stuff
                   showfeats    :: Bool
                }
             deriving (Show, Eq)
\end{code}

A tag derivation history consists of a counter representing the number of 
substitution or adjunctions which have been done to the tree 
(this is neccesary for distinguishing between derivation nodes later on)
and a list of 3-tuples representing the operation (s for substitution, a
for adjunction), the name of the child tree, and the name of the parent 
tree.

\begin{code}
type TagDerivation = (Int, [ (Char, String, String) ])
\end{code}

\begin{code}
instance Ord TagElem where
  compare t1 t2 = 
    case (ttype t1, ttype t2) of
         (Initial, Initial)   -> compare' 
         (Initial, Auxiliar)  -> LT
         (Auxiliar, Initial)  -> GT
         (Auxiliar, Auxiliar) -> compare' 
         _                    -> error "TagElem compare not exhaustively defined"
    where compare' = compare (tidnum t1) (tidnum t2)
\end{code}

\begin{code}
emptyTE :: TagElem
emptyTE = TE { idname = "",
               tidnum = -1,
               ttype  = Initial,
               ttree  = Node emptyGNode [],
               derivation = (0,[]),
               substnodes = [], adjnodes   = [],
               tsemantics = [], 
               tpolarities = emptyFM,
               -- tpredictors = emptyFM,
               tpolpaths   = 0, 
               showfeats   = False
             }
\end{code}

% ----------------------------------------------------------------------
\section{TAG operations}
% ----------------------------------------------------------------------

\paragraph{substTag} given a TagElem and a substitution, applies the
substitution through all the TagElem

\begin{code}
substTagElem :: TagElem -> Subst -> TagElem
substTagElem te l =
    let substNodes sn = map (\ (n, fu, fd) -> (n, substFlist fu l, substFlist fd l)) sn
        in te{substnodes = substNodes (substnodes te),
              adjnodes   = substNodes (adjnodes te),
              ttree      = substTree (ttree te) l,
              tsemantics = substSem (tsemantics te) l}
\end{code}

\paragraph{appendToVars} given a TagElem and a suffix, appends the
suffix to all the variables that occur in it. See section
\ref{sec:fs_unification} to understand why this is neccesary.

\begin{code}
appendToVars :: String -> TagElem -> TagElem
appendToVars suf te = 
  let appfn (f,v) = (f, if (isVar v && (not.isAnon) v) 
                        then v ++ suf 
                        else v)
      --
      nodefn a = a { gup = map appfn (gup a),
                     gdown = map appfn (gdown a) }
      treefn (Node a l) = Node (nodefn a) (map treefn l)
      --
      sitefn (n, fu, fd) = (n, map appfn fu, map appfn fd)
      --
  in te { ttree = treefn (ttree te),
          substnodes = map sitefn (substnodes te),
          adjnodes   = map sitefn (adjnodes te)}
\end{code}

% ----------------------------------------------------------------------
\section{Map by sem}
% ----------------------------------------------------------------------

\begin{code}
type SemMap = FiniteMap Pred [TagElem]
\end{code}

The mapBySem function sorts trees into a FiniteMap organised by the
first literal of their semantics.  This is useful in at least three
places: the polarity optimisation, the gui display code, and code for
measuring the efficiency of Geni.  Note: trees with a null semantics
are filed under an empty predicate, if any.

This function is generalised for use with data types than TagElem,
so you'll have to pass the semantics function to the semfn argument
(tsemantics in the case of TagElem).

\begin{code}
mapBySem :: (a -> Sem) -> [a] -> FiniteMap Pred [a]
mapBySem semfn ts = 
  let gfn t = if (null s) then emptyPred else head s 
              where s = semfn t 
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
\section{Drawings TAG Tree}
% ----------------------------------------------------------------------

This provides a convenient wrapper for drawing a TAG tree or list of
trees in String form.  

\begin{code}
drawTagTrees :: [TagElem] -> String
drawTagTrees tes = concat $ intersperse "\n" $ map drawTagTree tes  

drawTagTree :: TagElem -> String 
drawTagTree te = idname te ++ ":\n" ++ (drawTree $ ttree te)
\end{code}

\paragraph{showTagSites} is useful for debugging adjunction and
substitution nodes

\begin{code}
showTagSites :: [TagSite] -> String
showTagSites sites = concat $ intersperse "\n  " $ map fn sites
  where fn (n,t,b) = n ++ "/" ++ showPairs t ++ "/" ++ showPairs b
\end{code}


