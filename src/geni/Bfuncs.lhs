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

\chapter{Bfuncs}

This module provides basic operations on trees, nodes and semantics.
Things here are meant to be relatively low-level and primitive 
(well, with the exception of feature structure unification, that is).

This has been seperated from the module Btypes for the basic purpose
of reducing the number of modules that get re-compiled 
(such as the yacc parsers) everytime we hack around with some not so
low-level operation.

\begin{code}
module Bfuncs(
   -- Datatypes (re-exported)
   GNode(GN), GType(Subs, Foot, Lex, Other), 
   Ttree(TT), MTtree, SemPols,
   Ptype(Initial,Auxiliar,Unspecified), 
   Pred, Flist, AvPair, 
   Lexicon, ILexEntry(..), Macros, Sem, SemInput, Subst,
   emptyGNode, emptyMacro, 

   -- Projectors from GNode (re-exported)
   gnname, gup, gdown, ganchor, glexeme, gtype, gaconstr,

   -- Projectors from Tdesc (re-exported)
   params, pidname, pfeat, ptype, tree, 
   ptpolarities, ptpredictors,

   -- Functions from Tree GNode
   repSubst, repAdj, constrainAdj, 
   renameTree, substTree, substGNode,
   root, rootUpd, foot, setLexeme,

   -- Functions from Sem
   toKeys, subsumeSem, sortSem, substSem, showSem, showPred,
   emptyPred,

   -- Functions from Flist
   substFlist, substFlist', sortFlist, unifyFeat,
   showPairs, showAv,

   -- Other functions
   isVar, isAnon, testBtypes, 
        
   -- generic functions
   BitVector, groupByFM, multiGroupByFM,
   isEmptyIntersect, trim, fst3, snd3, thd3, 
   mapTree, filterTree, treeLeaves, listRepNode
) where
\end{code}

\ignore{
\begin{code}
import Btypes
import Debug.Trace -- for test stuff
import Data.Char (isUpper, isSpace)
import Data.FiniteMap (emptyFM, FiniteMap, addToFM_C)
import Data.List (intersect, sortBy, nub, unwords)
import Data.Tree
\end{code}
}

\paragraph{show (GNode)} the default show for GNode tries to
be very compact; it only shows the value for cat attribute 
and any flags which are marked on that node.

\begin{code}
instance Show GNode where
  show gn = 
    let cat' = filter (\ (f,_) -> f == "cat") $ gup gn
        cat  = if (null cat') then "" else snd $ head cat'
        lex  = if (null $ glexeme gn) then "" else glexeme gn
        -- 
        extra = case (gtype gn) of         
                   Subs -> " (s)"
                   Foot -> " *"
                   _    -> if (gaconstr gn)  then " (na)"   else ""
    in if (not (null cat || null lex))
       then cat ++ ":" ++ lex ++ extra
       else cat ++ lex ++ extra
\end{code}

\paragraph{substGNode} 
Given a GNode and a substitution, it applies the
 substitution to GNode
\begin{code}
substGNode :: GNode -> Subst -> GNode
substGNode gn l =
    gn{gup = substFlist (gup gn) l,
       gdown = substFlist (gdown gn) l}
\end{code}

\subsection{Tree and GNode}

Projector and Update function for Tree

\begin{code}
root :: Tree a -> a
root (Node a _) = a

rootUpd :: Tree a -> a -> Tree a
rootUpd (Node _ l) b = (Node b l)
\end{code}

\paragraph{foot} extracts the foot node of a tree

\begin{code}
foot :: Tree GNode -> GNode
foot t = head $ filterTree (\n -> gtype n == Foot) t
\end{code}

\paragraph{setLexeme} 
Given a string l and a Tree GNode t, returns the tree t'
where l has been assigned to the "lexeme" node in t'

\begin{code}
setLexeme :: String -> Tree GNode -> Tree GNode
setLexeme s t =
  let filt (Node a _) = (gtype a == Lex && ganchor a)
      fn (Node a l)   = Node a{glexeme = s} l
  in (head.fst) $ listRepNode fn filt [t]
\end{code}

\paragraph{substTree} 
Given a tree GNode and a substitution, applies the 
substitution to the tree.

\begin{code}
substTree :: Tree GNode -> Subst -> Tree GNode
substTree t s = mapTree (\n -> substGNode n s) t
\end{code}

\paragraph{renameTree} 
Given a Char c and a tree, renames nodes in 
the tree by prefixing c.

\begin{code}
renameTree :: Char -> Tree GNode -> Tree GNode
renameTree c = mapTree (\a -> a{gnname = c:(gnname a)}) 
\end{code}

\subsection{Substitution}

\paragraph{repSubst} 
Given two trees t1 t2, and the name n of a node in t2, 
replaces t1 in t2 at the (leaf) node named n.
\begin{code}
repSubst :: String -> Tree GNode -> Tree GNode -> Tree GNode
repSubst n t1 t2 =
  let filt (Node a []) = (gnname a) == n 
      filt (Node _ _)  = False
      fn _ = t1
      -- 
      (lt,flag) = listRepNode fn filt [t2]
  in if flag 
     then head lt 
     else error ("substitution unexpectedly failed on node " ++ n)
\end{code}

\subsection{Adjuction}

\paragraph{repAdj} 
Given two trees t1 t2 (where t1 is an auxiliar tree), and
the name n of a node in t2, replaces t1 in t2 at the node named n by an
adjunction move (using newFoot to replace the foot node in t1).  

Minor ugliness: we copy any lexical information from the t2 node
to the new foot node.
\begin{code}

repAdj :: GNode -> String -> Tree GNode -> Tree GNode -> Tree GNode
repAdj newFoot n t1 t2 =
  let filt (Node a _) = (gnname a == n)
      fn (Node a l)   = repFoot nf t1 l
                        where nf = newFoot { ganchor = ganchor a
                                           , glexeme = glexeme a }
      (lt,flag) = listRepNode fn filt [t2] 
  in if flag 
     then head lt 
     else error ("adjunction unexpectedly failed on node " ++ n)

repFoot :: GNode -> Tree GNode -> [Tree GNode] -> Tree GNode
repFoot newFoot t l =
  let filt (Node a _) = (gtype a == Foot)
      fn (Node _ _) = Node newFoot l
  in (head.fst) $ listRepNode fn filt [t]  
\end{code}

\paragraph{constrainAdj} could be moved to Btypes if the 
ordered adjunction becomes standard.  We search the tree for a 
node with the given name and add an adjunction constraint on it.

\begin{code}
constrainAdj :: String -> Tree GNode -> Tree GNode
constrainAdj n t =
  let filt (Node a _) = (gnname a == n)
      fn (Node a l)   = Node a { gaconstr = True } l
  in (head.fst) $ listRepNode fn filt [t] 
\end{code}

% ----------------------------------------------------------------------
\section{Features and variables}
% ----------------------------------------------------------------------

\paragraph{substFlist} 
Given an Flist and a substitution, applies 
 the substitution to the Flist.
\begin{code}
substFlist :: Flist -> Subst -> Flist
substFlist fl sl = foldl substFlist' fl sl
\end{code}

\ignore{
\begin{code}
testSubstFlist =
  let input    = [ ("a","1") ]
      expected = [ ("a","3") ]
      subst    = [ ("1","2"), ("2","3")]
      output   = substFlist input subst 
      debugstr =  "input: "    ++ showPairs input
               ++ "\nsubst: "  ++ showPairs expected 
               ++ "\noutput: " ++ showPairs output
  in trace debugstr (output == expected) 
\end{code}
}

\paragraph{substFlist'} Given an Flist and a single substition, applies
that substitution to the Flist... 

\begin{code}
substFlist' :: Flist -> (String,String) -> Flist 
substFlist' fl (s1, s2) = map (\ (f, v) -> (f, if (v ==s1) then s2 else v)) fl
\end{code}

\paragraph{sortFlist} sorts Flists according with its feature

\begin{code}
sortFlist :: Flist -> Flist
sortFlist fl = sortBy (\(f1,_) (f2, _) -> compare f1 f2) fl
\end{code}

\begin{code}
showPairs :: Flist -> String
showPairs l = unwords $ map showAv l
showAv (y,z) = y ++ ":" ++ z 
\end{code}

% --------------------------------------------------------------------  
\subsection{Unification}
\label{sec:fs_unification}
% --------------------------------------------------------------------  

Feature structure unification takes two feature lists as input and
returns a tuple:

\begin{enumerate}
\item true if unification is possible
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

\paragraph{unifyFeat} is an implementation of feature structure
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
unifyFeat :: Flist -> Flist -> (Bool, Flist, [(String,String)])
\end{code}

Trivial base cases:

\begin{code}
unifyFeat [] [] = (True, [], [])

unifyFeat [] (a:x) = 
  (succ, a:res, subst)
  where (succ, res, subst) = unifyFeat [] x

unifyFeat (a:x) [] = 
  (succ, a:res, subst)
  where (succ, res, subst) = unifyFeat x []
\end{code}

The less trivial case is when neither list is empty.  If we are looking
at the same attribute, then we transfer control to the helper function.
Otherwise, we remove the (alphabetically) smaller att-val pair, add it
to the results, and move on.  This only works if the lists are
alphabetically sorted beforehand!

\begin{code}
unifyFeat fs1@((f1, v1):l1) fs2@((f2, v2):l2) =
  case () of _ | f1 == f2 -> unifyFeatI f1 v1 v2 l1 l2 
               | f1 <  f2 -> (succ1, (f1, v1):res1, subst1)
               | f1 >  f2 -> (succ2, (f2, v2):res2, subst2)
               | otherwise -> error "Feature structure unification is badly broken"
  where (succ1, res1, subst1) = unifyFeat l1 fs2
        (succ2, res2, subst2) = unifyFeat fs1 l2
\end{code}

\paragraph{unifyFeatI} is a helper function that determines what we
should do when we have two values for the same attribute.

\begin{code}
unifyFeatI :: String ->String -> String -> Flist -> Flist -> (Bool, Flist, [(String,String)])
\end{code}

\begin{enumerate}
\item if either v1 or v2 are anonymous, we add the other to the result,
      and we don't add any replacements.
\item if v1 is a variable then we replace it by v2,
      regardless of whether or not v2 is a variable
\item if v2 is a variable then we replace it by v1
\item if neither v1 and v2 are variables, but they match, we arbitarily add one
      of them to the result, but we don't add any replacements.
\item if neither are variables and they do \emph{not} match, we fail
\end{enumerate}

\begin{code}
unifyFeatI f v1 v2 l1 l2 = 
  let unifyval
        | (isAnon v1) = (succ3, (f, v2):res3, subst3)
        | (isAnon v2) = (succ3, (f, v1):res3, subst3)
        | (isVar v1)  = (succ1, (f, v2):res1, (v1, v2):subst1) 
        | (isVar v2)  = (succ2, (f, v1):res2, (v2, v1):subst2)
        | (v1 == v2)  = (succ3, (f, v1):res3, subst3)
        | otherwise   = (False, [], [])
      --
      (succ1, res1, subst1) = unifyFeat (substFlist' l1 (v1,v2)) l2 
      (succ2, res2, subst2) = unifyFeat l1 (substFlist' l2 (v2,v1)) 
      (succ3, res3, subst3) = unifyFeat l1 l2
      --
  in unifyval 
\end{code}

\subsection{Variables}

\paragraph{isVar} 
Returns true if the string starts with a capital or is an anonymous variable.  

\begin{code}
isVar :: String -> Bool
isVar s  = (isUpper . head) s || (isAnon s)
\end{code}

\paragraph{isAnon}
Returns true if the string is an underscore 
\begin{code}
isAnon :: String -> Bool
isAnon = (==) "_" 
\end{code}

% ----------------------------------------------------------------------
\section{Semantics}
\label{bfuncs_semantics}
% ----------------------------------------------------------------------

\begin{code}
showSem :: Sem -> String
showSem l =
    "[" ++ (unwords $ map showPred l) ++ "]"
\end{code}

\begin{code}
showPred (h, p, l) = showh ++ p ++ "(" ++ unwords l++ ")"
                     where hideh = null h || (take 2 h == "gh")
                           showh = if hideh then "" else h ++ ":"
\end{code}

\paragraph{substSem} 
Given a Sem and a substitution, applies the substitution
  to Sem
\begin{code}
substSem :: Sem -> Subst -> Sem
substSem s l = map (\p -> substPred p l) s
\end{code}

\paragraph{toKeys} 
Given a Semantics, returns the string with the proper keys
(propsymbol+arity) to access the agenda
\begin{code}
toKeys :: Sem -> [String] 
toKeys l = map (\(_,prop,par) -> prop++(show (length par))) l
\end{code}

\paragraph{repXbyY} 
Given two values s1 and s2 and a list, it replace the 
first by the second in the list
\begin{code}
repXbyY :: (Eq a) => a -> a -> [a] -> [a] 
repXbyY s1 s2 l = map (\x->if (x == s1) then s2 else x) l
\end{code}

%\paragraph{instantiate} 
%Given a predicate (name, listParams) p and the
%semantics s of a candidate, it instantiates s in terms of p.  
%I.e variables in s are instantiated according to p, but notice
%that variables in s are left as is and no error is reported.  
%Candidates should be checked for subsumeSem afterwards 
%
%\begin{code}
%instCandSem :: (String, [String]) -> Sem -> Sem
%instCandSem p [] =
%    []
%instCandSem p@(pn1, lp1) (h@(pn2, lp2):rl) =
%    if ((pn1 == pn2) && (length lp1 == length lp2))
%       then let sub = findSubstCand lp1 lp2
%                in (substPred p sub):(instCandSem p (substSem rl sub))
%       else h:(instCandSem p rl) -}
%\end{code}
%
%\begin{code}
%findSubstCand :: [String] -> [String] -> Subst
%findSubstCand [] [] =
%    []
%findSubstCand (w1:l1) (w2:l2) =
%    if (isVar w2) 
%       then (w2, w1):findSubstCand l1 l2
%       else findSubstCand l1 l2
%\end{code}

\begin{code}
substPred :: Pred -> Subst -> Pred
substPred p [] = p
substPred (h, n, lp) ((a,b):l) = substPred (fixHandle, n, repXbyY a b lp) l
  where fixHandle = if (h == a) then b else h 
\end{code}

\paragraph{subsumeSem} 
\label{fn:subsumeSem}

Given the target Sem ts and the Sem s of a potential lexical candidate,
returns the list of possible substitutions so that s is a subset of ts.
Note: we return more than one possible substitution because s could be
different subsets of ts.  Consider, for example, \semexpr{love(j,m),
  name(j,john), name(m,mary)} and the candidate \semexpr{name(X,Y)}.

TODO WE ASSUME BOTH SEMANTICS ARE ORDERED and non-empty.

\begin{code}
subsumeSem :: Sem -> Sem -> [Subst]
subsumeSem [] _  = error "target semantics is non-empty in subsumeSem"
subsumeSem _  [] = error "tree semantics is non-empty in subsumeSem"
subsumeSem ts [(h,p,par)] = subsumePred ts (h,p,par)
subsumeSem ts (at:l) =
    let psubst = subsumePred ts at
        res    = map (\x -> subsumeSem (substSem ts x) (substSem l x)) psubst
        pairs  = zip psubst res
        res2   = map (\ (s1,s2) -> map (\x -> s1++x) s2) pairs
        in concat res2
\end{code}

\paragraph{subsumePred}
The first Sem s1 and second Sem s2 are the same when we start we cicle on s2
looking for a match for Pred, and meanwhile we apply the partical substitutions
to s1.  Note: we treat the handle as if it were a parameter.

\begin{code}
subsumePred :: Sem -> Pred -> [Subst]
subsumePred [] _ = []
subsumePred ((h1, p1, la1):l) (h2,p2,la2) =
    -- if we found the proper predicate
    if ((p1 == p2) && (length la1 == length la2))
    then let subst = map nub (pairVar (h1:la1) (h2:la2) [])
             isNotVar = not.isVar
             -- defines the subst, taking care of clashing of var. with check
             pairVar [] [] _ = [[]]   -- [[]] means: Empty subst is a solution
             pairVar _ [] _ = error "unequal parameter lengths" 
             pairVar [] _ _ = error "unequal parameter lengths" 
             pairVar (v1:l1) (v2:l2) l  
               | v1 == v2 = pairVar l1 l2 l
               | isNotVar v1 && isNotVar v2 = [] -- no solution
               | isVar v1 && checkAss (v1,v2) l = 
                   map ((v1,v2):) (pairVar l1 l2 ((v1,v2):l))
               | isVar v2 && checkAss (v2,v1) l =
                   map ((v2,v1):) (pairVar l1 l2 ((v2,v1):l))
               | otherwise                      = []
             checkAss (_,_) [] = True
             checkAss (v1,v2) ((v3,v4):l)  
               | (v1 /= v3) = checkAss (v1,v2) l
               | (v2 == v4) = checkAss (v1,v2) l
               | otherwise  = False
         in subst++(subsumePred l (h2, p2,la2))
    else if (p1 > p2)
         then []
         else subsumePred l (h2, p2,la2)
\end{code}

\paragraph{sortSem} 
Sorts semantics according with it's predicate
\begin{code}
sortSem :: Sem -> Sem
sortSem s = sortBy (\(_, p1, _) -> \(_, p2, _) -> compare p1 p2) s
\end{code}



% ----------------------------------------------------------------------
\section{General}
% ----------------------------------------------------------------------

This section contains miscellaneous bits of generic code.

\begin{code}
trim :: String -> String
trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace) 
\end{code}

\begin{code}
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x
\end{code}

\begin{code}
type BitVector = Integer
\end{code}

\paragraph{isEmptyIntersect} is true if the intersection of two lists is
empty.

\begin{code}
isEmptyIntersect :: (Eq a) => [a] -> [a] -> Bool
isEmptyIntersect a b = null $ intersect a b
\end{code}

\paragraph{groupByFM} serves the same function as Data.List.groupBy.  It
groups together items by some property they have in common. The
difference is that the property is used as a key to a FiniteMap that you
can lookup.  \texttt{fn} extracts the property from the item.

\begin{code}
groupByFM :: (Ord b) => (a -> b) -> [a] -> (FiniteMap b [a])
groupByFM fn list = 
  let addfn  x acc key = addToFM_C (++) acc key [x]
      helper x acc     = addfn x acc (fn x)
  in foldr helper emptyFM list 
\end{code}

\paragraph{multiGroupByFM} is the same as groupByFM, except that we
assume an item can appear in multiple groups.  \texttt{fn} extracts the
property from the item, and returns multiple results in the form of a
list.

\begin{code}
multiGroupByFM :: (Ord b) => (a -> [b]) -> [a] -> (FiniteMap b [a])
multiGroupByFM fn list = 
  let addfn  x key acc = addToFM_C (++) acc key [x]
      helper x acc     = foldr (addfn x) acc (fn x)
  in foldr helper emptyFM list 
\end{code}

\paragraph{mapTree} is like map, except on Trees.  This has to be
tucked away somewhere (i.e. i must be reinventing the wheel)!

\begin{code}
mapTree :: (a->b) -> Tree a -> Tree b
mapTree fn (Node a []) = (Node (fn a) [])
mapTree fn (Node a l)  = (Node (fn a) (map (mapTree fn) l))
\end{code}

\paragraph{filterTree} is like filter, except on Trees.  Filter 
might not be a good name, though, because we return a list of 
nodes, not a tree.

\begin{code}
filterTree :: (a->Bool) -> Tree a -> [a]
filterTree fn (Node a []) = 
  if fn a then [a] else []
filterTree fn (Node a l)  = 
  if fn a then a:next else next
  where next = concatMap (filterTree fn) l
\end{code}

\paragraph{treeLeaves} returns the leaf nodes of a Tree.

\begin{code}
treeLeaves :: Tree a -> [a]
treeLeaves (Node n []) = [n]
treeLeaves (Node _ l ) = concatMap treeLeaves l
\end{code}

\paragraph{listRepNode} is a generic tree-walking/editing function.  It
takes a replacement function, a filtering function and a tree.  It
returns the tree, except that the first node for which the filtering
function returns True is transformed with the replacement function.

\begin{code}
listRepNode :: (Tree a -> Tree a) -> (Tree a -> Bool) 
              -> [Tree a] -> ([Tree a], Bool)
listRepNode _ _ [] = ([], False)
listRepNode fn filt ((n@(Node a l1)):l2) = 
  if filt n
  then ((fn n):(l2), True)
  else let (lt1, flag1) = listRepNode fn filt l1 
           (lt2, flag2) = listRepNode fn filt l2
       in if flag1
          then ((Node a lt1):l2, flag1)
          else (n:lt2, flag2)
\end{code}

\begin{code}
testBtypes = testSubstFlist
\end{code}


