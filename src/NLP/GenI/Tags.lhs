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
   ts_rootFeatureMismatch,

   -- Functions from Tags
   addToTags, tagLeaves,

   -- Functions from TagElem
   setTidnums, 

   -- General functions
   mapBySem, subsumedBy, showTagSites,
   collect, detectSites
) where
\end{code}

\ignore{
\begin{code}
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.List (intersperse)
import Data.Tree

import NLP.GenI.Btypes (Ptype(Initial, Auxiliar), SemPols,
               GeniVal(GConst),
               GNode(gup, glexeme, gnname, gaconstr, gdown, gtype, gorigin),
               GType(Subs), Flist,
               Replacable(..), replaceOneAsMap,
               Collectable(..), Idable(..),
               Sem, Pred, emptyPred, 
               emptyGNode,
               showFlist, showPairs, showSem, lexemeAttributes,
               )
import NLP.GenI.General (groupByFM, preTerminals)
\end{code}
}

% ----------------------------------------------------------------------
\section{Tags}
% ----------------------------------------------------------------------

\begin{code}
-- | An anchored grammar.
--   The grammar associates a set of semantic predicates to a list of trees each.
type Tags = Map.Map String [TagElem]                            

-- | 'addTags' @tags key elem@ adds @elem@ to the the list of elements associated
--   to the key
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
data TagSite = TagSite { tsName :: !String
                       , tsUp   :: !Flist
                       , tsDown :: !Flist
                       , tsOrigin :: !String
                       }
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
                   tinterface   :: Flist,  -- for idxconstraints (pol)
                   ttrace       :: [String],
                   tsempols     :: [SemPols]
                }
             deriving (Show, Eq)
\end{code}

A TAG derivation history consists of a list of 3-tuples representing the
operation (s for substitution, a for adjunction), the name of the child tree,
the name of the parent tree and the node affected.

\begin{code}
type TagDerivation = [ (Char, String, (String, String)) ]
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
  replaceMap s te =
    te { tinterface = replaceMap s (tinterface te)
       , ttree      = replaceMap s (ttree te)
       , tsemantics = replaceMap s (tsemantics te) }
  replaceOne = replaceOneAsMap

instance Replacable TagSite where
  replaceMap s (TagSite n fu fd o) = TagSite n (replaceMap s fu) (replaceMap s fd) o
  replaceOne s (TagSite n fu fd o) = TagSite n (replaceOne s fu) (replaceOne s fd) o

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
               tinterface  = [],
               ttrace = []
             }

-- | Given a tree(GNode) returns a list of substitution or adjunction
--   nodes, as well as remaining nodes with a null adjunction constraint.
detectSites :: Tree GNode -> ([TagSite], [TagSite], [TagSite])
detectSites t =
  ( sites isSub           -- for substitution
  , sites (not.gaconstr)  -- for adjunction
  , sites constrButNotSub -- for neither
  )
 where
 ns = flatten t
 sites match = [ TagSite (gnname n) (gup n) (gdown n) (gorigin n) | n <- ns, match n ]
 isSub n = gtype n == Subs
 constrButNotSub n = gaconstr n && (not $ isSub n)
\end{code}

\subsection{Unique ID}

TagElem comparison relies exclusively on \fnparam{tidnum}, so you must
ensure that every TagElem you use has a unique ID.  We provide two
helpful functions for this.  These are most likely useful \emph{between}
lexical selection and generation proper, because during generation
proper, you can simply keep a counter within a State monad to assign
unique IDs to new TagElems.

Note that we also label each node of the tree with its elementary tree
name and with the unique ID.  This helps us to build derivation trees
correctly

\begin{code}
-- | Assigns a unique id to each element of this list, that is, an integer
--   between 1 and the size of the list.
setTidnums :: [TagElem] -> [TagElem]
setTidnums xs = zipWith (\c i -> setOrigin $ c {tidnum = i}) xs [1..]

setOrigin :: TagElem -> TagElem
setOrigin te = te { ttree = fmap setLabel . ttree $ te }
 where setLabel g = g { gorigin = idname te ++ ":" ++ (show.tidnum) te }
\end{code}

% ----------------------------------------------------------------------
\section{TAG Item}
% ----------------------------------------------------------------------

\begin{code}
-- | 'TagItem' is a generalisation of 'TagElem'.
class TagItem t where 
  tgIdName    :: t -> String
  tgIdNum     :: t -> Integer
  tgSemantics :: t -> Sem

instance TagItem TagElem where
  tgIdName = idname
  tgIdNum  = tidnum
  tgSemantics = tsemantics
\end{code}

% ----------------------------------------------------------------------
\section{Map by sem}
% ----------------------------------------------------------------------

\begin{code}
-- | Sorts trees into a Map.Map organised by the first literal of their
--   semantics.  This is useful in at least three places: the polarity
--   optimisation, the gui display code, and code for measuring the efficiency
--   of GenI.  Note: trees with a null semantics are filed under an empty
--   predicate, if any.
mapBySem :: (TagItem t) => [t] -> Map.Map Pred [t]
mapBySem ts = 
  let gfn t = case tgSemantics t of
              []    -> emptyPred
              (x:_) -> x
  in groupByFM gfn ts

-- | 'subsumedBy' @cs ts@ determines if the candidate semantics @cs@ is
--   subsumed by the proposition semantics @ts@.  Notice how the proposition
--   semantics is only a single item where as the candidate semantics is a
--   list.
--
--  We assume
--
--  * most importantly that @cs@ has already its semantics instatiated
--    (all variables assigned)
--
--  * @cs@ and @ts@ are sorted
--
--  * the list in each element of cs and ts is itself sorted 
subsumedBy :: Sem -> Pred -> Bool 
subsumedBy [] _ = False 
subsumedBy ((ch, cp, cla):cl) (th, tp,tla)
    | (ch == th) && (cp == tp) && (cla == tla) = True 
    -- if we haven't yet overshot, try for the next one
    | cp  < tp                   = subsumedBy cl (th, tp, tla)
    | otherwise                  = False
\end{code}

% ----------------------------------------------------------------------
\section{Extracting sentences}
% ----------------------------------------------------------------------

Normally, extracting the sentences from a TAG tree would just consist of
reading its leaves.  But if you want the generator to return inflected
forms instead of just lemmas, you also need to return the relevant
features for each leaf.  In TAG, or at least our use of it, the features
come from the \emph{pre-terminal} nodes, that is, not the leaves
themselves but their parents.  Another bit of trickiness: because of
atomic disjunction, leaves might have more than one value, so we can't
just return a String lemma but a list of String, one for each
possibility.

\begin{code}
type UninflectedDisjunction = ([String], Flist)

tagLeaves :: TagElem -> [ (String, UninflectedDisjunction) ]
tagLeaves te = [ (gnname pt, (getLexeme t, gup pt)) | (pt,t) <- preTerminals . ttree $ te ]

-- | Try in order: lexeme, lexeme attributes, node name
getLexeme :: GNode -> [String]
getLexeme node =
  case glexeme node of
    []   -> fromMaybe [gnname node] $ firstMaybe grab lexemeAttributes
    lexs -> lexs
  where
   grab la =
     let match (a, (GConst v)) | a == la = Just v
         match _ = Nothing
     in firstMaybe match guppy
   guppy      = gup node

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe fn = listToMaybe . mapMaybe fn

\end{code}

% ----------------------------------------------------------------------
\section{Debugging}
% ----------------------------------------------------------------------

\begin{code}
-- Useful for debugging adjunction and substitution nodes
showTagSites :: [TagSite] -> String
showTagSites sites = concat $ intersperse "\n  " $ map fn sites
  where
   fn (TagSite n t b o) =
    concat . intersperse "/" $ [ n, showPairs t, showPairs b, o ]
\end{code}

% ----------------------------------------------------------------------
\section{Diagnostic messages}
% ----------------------------------------------------------------------

Diagnostic messages let us know why a TAG tree is not returned as a result.
Whenever GenI decides to discard a tree, it sets the tdiagnostic field of 
the TagElem so that the person using a debugger can find out what went wrong.

\begin{code}
ts_synIncomplete, ts_tbUnificationFailure :: String
ts_synIncomplete = "syntactically incomplete"
ts_tbUnificationFailure = "top/bot unification failure"

ts_rootFeatureMismatch :: Flist -> String
ts_rootFeatureMismatch good = "root feature does not unify with " ++ showFlist good

ts_semIncomplete :: [Pred] -> String
ts_semIncomplete sem = "semantically incomplete - missing:  " ++ showSem sem
\end{code}
