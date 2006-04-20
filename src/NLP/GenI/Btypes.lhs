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

This module provides basic datatypes like GNode, as well as operations
on trees, nodes and semantics.  Things here are meant to be relatively
low-level and primitive (well, with the exception of feature structure
unification, that is).

\begin{code}
module NLP.GenI.Btypes(
   -- Datatypes 
   GNode(GN), GType(Subs, Foot, Lex, Other), 
   Ttree(..), MTtree, SemPols,
   Ptype(Initial,Auxiliar,Unspecified), 
   Pred, Flist, AvPair, GeniVal(..),
   Lexicon, ILexEntry(..), Macros, Sem, SemInput, Subst,
   emptyLE, emptyGNode, emptyMacro, 

   -- GNode stuff 
   gnname, gup, gdown, ganchor, glexeme, gtype, gaconstr,
   gCategory, showLexeme,

   -- Functions from Tree GNode
   repSubst, repAdj, constrainAdj, 
   renameTree, 
   root, rootUpd, foot, setLexeme,

   -- Functions from Sem
   toKeys, subsumeSem, sortSem, showSem, showPred,
   emptyPred,

   -- Functions from Flist
   sortFlist, unify, unifyFeat, unifyFeat2,
   showPairs, showAv,

   -- Other functions
   Replacable(..), Collectable(..), Idable(..),
   alphaConvert,
   fromGConst, fromGVar,
   isConst, isVar, isAnon, testBtypes,

   -- Polarities

   -- Tests
   prop_unify_anon, prop_unify_self, prop_unify_sym 
) where
\end{code}

\ignore{
\begin{code}
-- import Debug.Trace -- for test stuff
import QuickCheck hiding (collect) -- needed for testing via ghci 
import Control.Monad (liftM)
import Data.List
import Data.Maybe (maybe)
import qualified Data.Map as Map
import qualified Data.Set as Set 
import Data.Tree

import NLP.GenI.General(mapTree, filterTree, listRepNode, snd3, geniBug)
--instance Show (IO()) where
--  show _ = ""
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
data Ttree a = TT
  { params  :: [GeniVal]
  , pfamily :: String
  , pidname :: String
  , pfeat :: Flist
  , ptype :: Ptype
  , tree :: Tree a } 
  deriving Show

data Ptype = Initial | Auxiliar | Unspecified   
             deriving (Show, Eq)
\end{code}

\fnlabel{emptyMacro} provides a null tree which you can use for
various debugging or display purposes.

\begin{code}
emptyMacro :: MTtree
emptyMacro = TT { params  = [],
                  pidname = "", 
                  pfamily = "",
                  pfeat = [],
                  ptype = Unspecified,
                  tree  = Node emptyGNode []
                 }
\end{code}

Auxiliary types used during the parsing of the Lexicon.  
A lexicon maps semantic predicates to lexical entries.

\begin{code}
type Lexicon = Map.Map String [ILexEntry]
type SemPols  = [Int]
data ILexEntry = ILE
    { -- normally just a singleton, useful for merging synonyms 
      iword       :: [String]
    , ifamname    :: String
    , iparams     :: [GeniVal]
    , ipfeat      :: Flist
    , ifilters    :: Flist
    , iptype      :: Ptype
    , isemantics  :: Sem
    , isempols    :: [SemPols] }
  deriving (Show, Eq)

instance Replacable ILexEntry where
  replace s i = 
    i { ipfeat  = replace s (ipfeat i)
      , iparams = replace s (iparams i) }

emptyLE :: ILexEntry  
emptyLE = ILE { iword = [],
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
                glexeme  :: [String],
                gtype    :: GType,
                gaconstr :: Bool}
           deriving Eq

-- Node type used during parsing of the grammar 
data GType = Subs | Foot | Lex | Other
           deriving (Show, Eq)
\end{code}

\fnlabel{emptyGNode} provides a null gnode which you can use
for various debugging or display purposes.

\begin{code}
emptyGNode = GN { gnname = "",
                  gup = [], gdown = [],
                  ganchor = False,
                  glexeme = [], 
                  gtype = Other,
                  gaconstr = False }
\end{code}

A TAG node may have a category.  In the core GenI algorithm, there is nothing
which distinguishes the category from any other attributes.  But for some 
other uses, such as checking if it is a result or for display purposes, we
do treat this attribute differently.  We take here the convention that the
category of a node is associated to the attribute ``cat''.  
\begin{code}
-- | Return the value of the "cat" attribute, if available
gCategory :: GNode -> Maybe GeniVal 
gCategory gn =
  case [ v | (a,v) <- gup gn, a == "cat" ] of
  []  -> Nothing
  [c] -> Just c
  _   -> geniBug $ "Impossible case: node with more than one category"
\end{code}

\paragraph{show (GNode)} the default show for GNode tries to
be very compact; it only shows the value for cat attribute 
and any flags which are marked on that node.

\begin{code}
instance Show GNode where
  show gn =
    let cat  = case gCategory gn of
               Nothing -> []
               Just c  -> show c
        lex  = showLexeme $ glexeme gn
        --
        stub = concat $ intersperse ":" $ filter (not.null) [ cat, lex ]
        extra = case (gtype gn) of
                   Subs -> " !"
                   Foot -> " *"
                   _    -> if (gaconstr gn)  then " #"   else ""
    in stub ++ extra

-- FIXME: will have to think of nicer way - one which involves
-- unpacking the trees :-(
showLexeme :: [String] -> String
showLexeme []   = ""
showLexeme [l]  = l
showLexeme xs   = concat $ intersperse "|" xs 
\end{code}

A Replacement on a GNode consists of replacements on its top and bottom
feature structures

\begin{code}
instance Replacable GNode where
  replaceOne s gn =
    gn { gup = replaceOne s (gup gn) 
       , gdown = replaceOne s (gdown gn) }
  replace s gn =
    gn { gup = replace s (gup gn) 
       , gdown = replace s (gdown gn) }
\end{code}

% ----------------------------------------------------------------------
\section{Tree manipulation}
% ----------------------------------------------------------------------

\begin{code}
instance (Replacable a) => Replacable (Tree a) where
  replaceOne s t = mapTree (replaceOne s) t
  replace s t    = mapTree (replace s) t
\end{code}

Projector and Update function for Tree

\begin{code}
root :: Tree a -> a
root (Node a _) = a

rootUpd :: Tree a -> a -> Tree a
rootUpd (Node _ l) b = (Node b l)
\end{code}

\fnlabel{foot} extracts the foot node of a tree

\begin{code}
foot :: Tree GNode -> GNode
foot t = case filterTree (\n -> gtype n == Foot) t of
         [t] -> t
         _   -> geniBug $ "foot returned weird result"
\end{code}

\fnlabel{setLexeme} 
Given a string l and a Tree GNode t, returns the tree t'
where l has been assigned to the "lexeme" node in t'

\begin{code}
setLexeme :: [String] -> Tree GNode -> Tree GNode
setLexeme s t =
  let filt (Node a _) = (gtype a == Lex && ganchor a)
      fn (Node a l)   = Node a{glexeme = s} l
  in case listRepNode fn filt [t] of
     ([r],True) -> r
     _ -> geniBug $ "setLexeme returned weird result"
\end{code}

\fnlabel{renameTree} 
Given a Char c and a tree, renames nodes in 
the tree by prefixing c.

\begin{code}
renameTree :: Char -> Tree GNode -> Tree GNode
renameTree c = mapTree (\a -> a{gnname = c:(gnname a)}) 
\end{code}


\subsection{Substitution}

\fnlabel{repSubst} Given two trees t1 t2, and the name n of a node in t2, 
replaces t1 in t2 at the (leaf) node named n.
\begin{code}
repSubst :: String -> Tree GNode -> Tree GNode -> Tree GNode
repSubst n t1 t2 =
  let filt (Node a []) = (gnname a) == n 
      filt (Node _ _)  = False
      fn _ = t1
  in case listRepNode fn filt [t2] of
     ([r], True) -> r
     (_, False)  -> geniBug $ "substitution unexpectedly failed on node " ++ n
     _           -> geniBug $ "weird result on repSubst"
\end{code}

\subsection{Adjunction}

\fnlabel{repAdj} 
Given two trees \fnparam{t1} \fnparam{t2} (where t1 is an auxiliary tree), and
the name n of a node in t2, replaces t1 in t2 at the node named n by an
adjunction move (using newFoot to replace the foot node in t1).  

Minor ugliness: we copy any lexical information from the t2 node
to the new foot node.
\begin{code}

repAdj :: GNode -> String -> Tree GNode -> Tree GNode -> Tree GNode
repAdj newFoot n t1 t2 =
  let filt (Node a _) = (gnname a == n)
                        -- replace the footnode of t1 with nf  
      fn (Node a l)   = repFoot nf t1 l
                        where nf = newFoot { ganchor = ganchor a
                                           , glexeme = glexeme a }
  in case listRepNode fn filt [t2] of
     ([r], True) -> r
     (_, False)  -> geniBug $ "adjunction unexpectedly failed on node " ++ n
     _ -> geniBug $ "repAdj returned weird result"

-- repFoot replaces the footnode of t with newFoot
repFoot :: GNode -> Tree GNode -> [Tree GNode] -> Tree GNode
repFoot newFoot t l =
  let filt (Node a _) = (gtype a == Foot)
      fn (Node _ _) = Node newFoot l
  in case listRepNode fn filt [t] of
     ([r],True) -> r
     _ -> geniBug $ "repFoot returned weird result"
\end{code}

\fnlabel{constrainAdj} searches the tree for a node with the given name
and add an adjunction constraint on it.

\begin{code}
constrainAdj :: String -> Tree GNode -> Tree GNode
constrainAdj n t =
  let filt (Node a _) = (gnname a == n)
      fn (Node a l)   = Node a { gaconstr = True } l
  in case listRepNode fn filt [t] of
     ([r],True) -> r
     _ -> geniBug $ "constrainAdj returned weird result"
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
  show (GConst x) = concat $ intersperse "|" x
  show (GVar x)   = '?':x
  show GAnon      = "?_"
\end{code}

\fnlabel{fromGConst and fromGVar} respectively extract the constant or  
variable string value of a GeniVal, assuming it has that kind of value.

\begin{code}
fromGConst :: GeniVal -> [String]
fromGConst (GConst x) = x
fromGConst x = error ("fromGConst on " ++ show x)

fromGVar :: GeniVal -> String
fromGVar (GVar x) = x
fromGVar x = error ("fromGVar on " ++ show x)
\end{code}

\subsection{Collectable}

A Collectable is something which can return its variables as a set.
By variables, what I most had in mind was the GVar values in a 
GeniVal.  This notion is probably not very useful outside the context of
alpha-conversion task, but it seems general enough that I'll keep it
around for a good bit, until either some use for it creeps up, or I find
a more general notion that I can transform this into.  

\begin{code}
class Collectable a where
  collect :: a -> Set.Set String -> Set.Set String

instance (Collectable a => Collectable [a]) where
  collect l s = foldr collect s l

instance (Collectable a => Collectable (Tree a)) where
  collect = collect.flatten

-- Pred is what I had in mind here 
instance ((Collectable a, Collectable b, Collectable c) 
           => Collectable (a,b,c)) where
  collect (a,b,c) = collect a . collect b . collect c 

instance Collectable GeniVal where
  collect (GVar v) s = Set.insert v s
  collect _ s = s

instance Collectable (String,GeniVal) where
  collect (_,b) = collect b

instance Collectable GNode where
  collect n = (collect $ gdown n) . (collect $ gup n)
\end{code}

\subsection{Replacable}
\label{sec:replacable}
\label{sec:replacements}

The idea of replacing one variable value with another is something that
appears all over the place in GenI.  So we try to smooth out its use by
making a type class out of it.

\begin{code}
class Replacable a where
  replace :: Subst -> a -> a 

  replaceOne :: (String,GeniVal) -> a -> a
  replaceOne s = {-# SCC "replace" #-} replace [s]
\end{code}

GeniVal is probably the simplest thing you would one to apply a
substitution on

\begin{code}
instance Replacable GeniVal where
  replace sl v = {-# SCC "replace" #-} foldl' (flip replaceOne) v sl
  replaceOne (s1,s2) v = {-# SCC "replace" #-} if (v == GVar s1) then s2 else v
\end{code}

Substitution on list consists of performing substitution on 
each item.  Each item, is independent of the other, 
of course.

\begin{code}
instance (Replacable a => Replacable [a]) where
  replace s    = {-# SCC "replace" #-} map (replace s)
  replaceOne s = {-# SCC "replace" #-} map (replaceOne s)
\end{code}

Substitution on an attribute/value pairs consists of ignoring
the attribute and performing substitution on the value.

\begin{code}
instance (Replacable a => Replacable (String, a)) where
  replace s (a,v) = {-# SCC "replace" #-} (a, replace s v)
\end{code}

\subsection{Idable}

An Idable is something that can be mapped to a unique id.  
You might consider using this to implement Ord, but I won't.
Note that the only use I have for this so far (20 dec 2005)
is in alpha-conversion.

\begin{code}
class Idable a where
  idOf :: a -> Integer
\end{code}

\subsection{Other feature and variable stuff}

Our approach to $\alpha$-conversion works by appending a unique suffix
to all variables in an object.  See section \ref{sec:fs_unification} for
why we want this.

\begin{code}
alphaConvert :: (Collectable a, Replacable a, Idable a) => a -> a
alphaConvert x = {-# SCC "alphaConvert" #-}
  let vars   = Set.elems $ collect x Set.empty
      suffix = "-" ++ (show $ idOf x)
      convert v = GVar (v ++ suffix)
      --
      subst = map (\v -> (v, convert v)) vars 
  in replace subst x 
\end{code}

\fnlabel{sortFlist} sorts Flists according with its feature

\begin{code}
sortFlist :: Flist -> Flist
sortFlist fl = sortBy (\(f1,_) (f2, _) -> compare f1 f2) fl
\end{code}

\begin{code}
showPairs :: Flist -> String
showPairs l = unwords $ map showAv l

showAv (y,z) = y ++ ":" ++ show z 
\end{code}

% ----------------------------------------------------------------------
\section{Semantics}
\label{btypes_semantics}
% ----------------------------------------------------------------------

\begin{code}
-- handle, predicate, parameters
type Pred = (GeniVal, GeniVal, [GeniVal])
type Sem = [Pred]
type SemInput = (Sem,Flist)
type Subst = [(String, GeniVal)]
emptyPred = (GAnon,GAnon,[])
\end{code}

\begin{code}
showSem :: Sem -> String
showSem l =
    "[" ++ (unwords $ map showPred l) ++ "]"
\end{code}

A replacement on a predicate is just a replacement on its parameters

\begin{code}
instance Replacable Pred where 
  replace s (h, n, lp) = (replace s h, replace s n, replace s lp) 
\end{code}

\begin{code}
showPred :: Pred -> String
showPred (h, p, l) = showh ++ show p ++ "(" ++ unwords (map show l) ++ ")"
  where 
    hideh (GConst [x]) = "genihandle" `isPrefixOf` x 
    hideh _ = False
    --
    showh = if (hideh h) then "" else (show h) ++ ":"
\end{code}

\fnlabel{toKeys} 
Given a Semantics, returns the string with the proper keys
(propsymbol+arity) to access the agenda
\begin{code}
toKeys :: Sem -> [String] 
toKeys l = map (\(_,prop,par) -> show prop ++ (show $ length par)) l
\end{code}

\subsection{Semantic subsumption} 
\label{fn:subsumeSem}

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
  subsumeSemHelper ([],[]) (reverse tsem) (reverse lsem)
\end{code}

This is tricky because each substep returns multiple results.  We solicit
the help of accumulators to keep things from getting confused.

\begin{code}
subsumeSemHelper :: (Sem,Subst) -> Sem -> Sem -> [(Sem,Subst)]
subsumeSemHelper _ [] _  = 
  error "input semantics is non-empty in subsumeSemHelper"
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
               acc2  = (toPred p : accSem, accSub ++ s) 
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
    then let rs   = unify (h1:la1) (h2:la2)
             next = subsumePred l pred2
         in case rs of
              Nothing -> next
              Just rs -> rs : next
    else if (p1 < p2) -- note that the semantics have to be reversed!
         then []
         else subsumePred l pred2 
\end{code}

\subsection{Other semantic stuff}

\fnlabel{sortSem} 
Sorts semantics first according to its predicate, and then to its handles.

\begin{code}
sortSem :: Sem -> Sem
sortSem = sortBy (\(h1,p1,a1) (h2,p2,a2) -> compare (p1, h1:a1) (p2, h2:a2))  
\end{code}

\begin{code}
testBtypes = False --testSubstFlist
\end{code}

% --------------------------------------------------------------------  
\subsection{Unification}
\label{sec:fs_unification}
% --------------------------------------------------------------------  

Feature structure unification takes two feature lists as input.  If it
fails, it returns Nothing.  Otherwise, it returns a tuple with:

\begin{enumerate}
\item a unified feature structure list
\item a list of variable replacements that will need to be propagated
      across other feature structures with the same variables
\end{enumerate}

Unification fails if, at any point during the unification process, the
two lists have different constant values for the same attribute.
For example, unification fails on the following inputs because they have
different values for the \textit{number} attribute:

\begin{quotation}
\fs{\it cat:np\\ \it number:3\\}
\fs{\it cat:np\\ \it number:2\\}
\end{quotation}

Note that the following input should also fail as a result on the
coreference on \textit{?X}.

\begin{quotation}
\fs{\it cat:np\\ \it one: 1\\  \it two:2\\}
\fs{\it cat:np\\ \it one: ?X\\ \it two:?X\\}
\end{quotation}

On the other hand, any other pair of feature lists should unify
succesfully, even those that do not share the same attributes.
Below are some examples of successful unifications:

\begin{quotation}
\fs{\it cat:np\\ \it one: 1\\  \it two:2\\}
\fs{\it cat:np\\ \it one: ?X\\ \it two:?Y\\}
$\rightarrow$
\fs{\it cat:np\\ \it one: 1\\ \it two:2\\},
\end{quotation}

\begin{quotation}
\fs{\it cat:np\\ \it number:3\\}
\fs{\it cat:np\\ \it case:nom\\}
$\rightarrow$
\fs{\it cat:np\\ \it case:nom\\ \it number:3\\},
\end{quotation}

\fnlabel{unifyFeat} is an implementation of feature structure
unification. It makes the following assumptions:

\begin{itemize}
\item Features are ordered

\item The Flists do not share variables!!!
      
      More precisely, if the two Flists have the same variable, they
      will have the same value. Though this behaviour may not be
      desirable, we don't really care because we never encounter the
      situation  (see page \pageref{par:lexSelection}).
\end{itemize}

\begin{code}
unifyFeat :: Flist -> Flist -> Maybe (Flist, Subst)
unifyFeat f1 f2 = 
  {-# SCC "unification" #-}
  do let (att, val1, val2) = alignFeat f1 f2
     (res, subst) <- unify val1 val2 
     return (zip att res, subst)

-- does the same thing but returning True/False on success or failure
unifyFeat2 :: Flist -> Flist -> (Bool, Flist, Subst)
unifyFeat2 a b = maybe failure success $ unifyFeat a b
  where failure       = (False, [], [])
        success (f,s) = (True, f, s)
\end{code}

\fnlabel{alignFeat}

The less trivial case is when neither list is empty.  If we are looking
at the same attribute, then we transfer control to the helper function.
Otherwise, we remove the (alphabetically) smaller att-val pair, add it
to the results, and move on.  This only works if the lists are
alphabetically sorted beforehand!

\begin{code}
alignFeat :: Flist -> Flist -> ([String], [GeniVal], [GeniVal])
alignFeat [] [] = ([], [], [])

alignFeat [] ((f,v):x) = 
  let (att, left, right) = alignFeat [] x
  in  (f:att, GAnon:left, v:right)

alignFeat x [] = 
  let (att, left, right) = alignFeat [] x
  in  (att, right, left)

alignFeat fs1@((f1, v1):l1) fs2@((f2, v2):l2) 
   | f1 == f2  = (f1:att0, v1:left0,    v2:right0)
   | f1 <  f2  = (f1:att1, v1:left1, GAnon:right1) 
   | f1 >  f2  = (f2:att2, GAnon:left2, v2:right2)
   | otherwise = error "Feature structure unification is badly broken"
   --
  where (att0, left0, right0) = alignFeat l1 l2
        (att1, left1, right1) = alignFeat l1 fs2
        (att2, left2, right2) = alignFeat fs1 l2
\end{code}

\subsection{GeniVal}

We throw in some simple predicates for accessing the GeniVal
cases.

\begin{code}
isConst :: GeniVal -> Bool
isConst (GConst _) = True
isConst _ = False

isVar :: GeniVal -> Bool
isVar (GVar _) = True
isVar _        = False

isAnon :: GeniVal -> Bool
isAnon GAnon = True
isAnon _     = False
\end{code}

\subsection{Unification}

\fnlabel{unify} performs unification on two lists of GeniVal.  If
unification succeeds, it returns \verb!Just (r,s)! where \verb!r! is 
the result of unification and \verb!s! is a list of substitutions that this
unification results in.  

Notes: 
\begin{itemize}
\item there may be multiple results because of disjunction
\item we need to return \verb!r! because of anonymous variables
\item the lists need not be same length; we just assume you want
      the longer of the two
\end{itemize}

The core unification algorithm follows these rules in order:

\begin{enumerate}
\item if either h1 or h2 are anonymous, we add the other to the result,
      and we don't add any replacements.
\item if h1 is a variable then we replace it by h2,
      regardless of whether or not h2 is a variable
\item if h2 is a variable then we replace it by h1
\item if neither h1 and h2 are variables, but they match, we arbitarily 
      add one of them to the result, but we don't add any replacements.
\item if neither are variables and they do \emph{not} match, we fail
\end{enumerate}

\begin{code}
unify :: [GeniVal] -> [GeniVal] -> Maybe ([GeniVal], Subst)
unify [] l2 = Just (l2, [])
unify l1 [] = Just (l1, [])
unify (GAnon:t1) (h2:t2) = unifySansRep h2 t1 t2
unify (h1:t1) (GAnon:t2) = unifySansRep h1 t1 t2
unify (h1@(GVar _):t1) (h2:t2) = unifyWithRep h1 h2 t1 t2
unify (h1:t1) (h2@(GVar _):t2) = unifyWithRep h2 h1 t1 t2
unify ((GConst h1v):t1) ((GConst h2v):t2) =
  case h1v `intersect` h2v of
  []   -> Nothing
  newH -> unifySansRep (GConst newH) t1 t2

{-# INLINE unifySansRep #-}
{-# INLINE unifyWithRep #-}
unifySansRep x2 t1 t2 =
 do (res,subst) <- unify t1 t2
    return (x2:res, subst)

unifyWithRep (GVar h1) x2 t1 t2 =
 case (h1,x2) of
 s -> do (res,subst) <- unify (replaceOne s t1) (replaceOne s t2)
         return (x2:res, s:subst)
unifyWithRep _ _ _ _ = geniBug "unification error"
\end{code}

\subsubsection{Unification tests} The unification algorithm should satisfy
the following properties:

Unifying something with itself should always succeed

\begin{code}
prop_unify_self x =
  (all qc_not_empty_GConst) x ==>
    case unify x x of
    Nothing  -> False
    Just unf -> (fst unf == x)
\end{code}

Unifying something with only anonymous variables should succeed.

\begin{code}
prop_unify_anon x = 
  case (unify x y) of
    Nothing  -> False
    Just unf -> (fst unf == x)
  where -- 
    y  = take (length x) $ repeat GAnon
\end{code}

Unification should be symmetrical.  We can't guarantee these if there
are cases where there are variables in the same place on both sides, so we
normalise the sides so that this doesn't happen.
         
\begin{code}
prop_unify_sym x y = 
  let u1 = unify x y 
      u2 = unify y x
      --
      notOverlap (GVar _, GVar _) = False
      notOverlap _ = True
  in (all qc_not_empty_GConst) x &&
     (all qc_not_empty_GConst) y &&
     all (notOverlap) (zip x y) ==> u1 == u2
\end{code}

\ignore{
\begin{code}
-- Definition of Arbitrary GeniVal for QuickCheck
instance Arbitrary GeniVal where
  arbitrary = oneof [ return $ GAnon, 
                      liftM GVar arbitrary, 
                      liftM (GConst . nub) arbitrary ]
  coarbitrary = error "no implementation of coarbitrary for GeniVal"

qc_not_empty_GConst :: GeniVal -> Bool
qc_not_empty_GConst (GConst []) = False
qc_not_empty_GConst _ = True
\end{code}
}

