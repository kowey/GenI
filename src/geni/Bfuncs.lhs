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
   Ttree(..), MTtree, SemPols,
   Ptype(Initial,Auxiliar,Unspecified), 
   Pred, Flist, AvPair, GeniVal(..),
   Lexicon, ILexEntry(..), Macros, Sem, SemInput, Subst,
   emptyGNode, emptyMacro, 

   -- Projectors from GNode (re-exported)
   gnname, gup, gdown, ganchor, glexeme, gtype, gaconstr,

   -- Functions from Tree GNode
   repSubst, repAdj, constrainAdj, 
   renameTree, substTree, substGNode,
   root, rootUpd, foot, setLexeme,

   -- Functions from Sem
   toKeys, subsumeSem, sortSem, substSem, showSem, showPred,
   emptyPred,

   -- Functions from Flist
   substFlist, sortFlist, unifyFeat, substHelper,
   showPairs, showAv,

   -- Other functions
   fromGConst, fromGVar,
   isVar, isAnon, testBtypes,

   -- Tests
   prop_unify_anon, prop_unify_self, prop_unify_sym 
) where
\end{code}

\ignore{
\begin{code}
-- import Debug.Trace -- for test stuff
import QuickCheck -- needed for testing via ghci 
import Control.Monad (liftM)
import Data.List (intersect, sortBy, foldl')
import Data.Tree

import Btypes
import General(mapTree, filterTree, listRepNode, snd3, bugInGeni)
--instance Show (IO()) where
--  show _ = ""
\end{code}
}

\paragraph{show (GNode)} the default show for GNode tries to
be very compact; it only shows the value for cat attribute 
and any flags which are marked on that node.

\begin{code}
instance Show GNode where
  show gn = 
    let cat' = [ av | av <- gup gn, fst av == "cat" ]
        cat  = if (null cat') then "" else show $ snd $ head cat'
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
      (lt,flag) = listRepNode fn filt [t2] 
  in if flag 
     then head lt 
     else error ("adjunction unexpectedly failed on node " ++ n)

-- repFoot replaces the footnode of t with newFoot
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
Given an Flist and a substitution, applies the substitution to the Flist.

\begin{code}
substFlist :: Flist -> Subst -> Flist
substFlist fl sl = foldl' helper fl sl
  where -- note: written sans map for performance reasons
        helper :: Flist -> (String,GeniVal) -> Flist
        helper [] _ = []
        helper ((f,v):xs) (s1, s2) = (f, v2) : helper xs (s1,s2) 
          where v2 = substHelper (s1,s2) v 
\end{code}

\ignore{
\begin{code}
{-
testSubstFlist =
  let input    = [ ("a","1") ]
      expected = [ ("a","3") ]
      subst    = [ ("1","2"), ("2","3")]
      output   = substFlist input subst 
      debugstr =  "input: "    ++ showPairs input
               ++ "\nsubst: "  ++ showPairs expected 
               ++ "\noutput: " ++ showPairs output
  in trace debugstr (output == expected) 
-}
\end{code}
}

\paragraph{sortFlist} sorts Flists according with its feature

\begin{code}
sortFlist :: Flist -> Flist
sortFlist fl = sortBy (\(f1,_) (f2, _) -> compare f1 f2) fl
\end{code}

\begin{code}
showPairs :: Flist -> String
showPairs l = unwords $ map showAv l
showAv (y,z) = y ++ ":" ++ show z 
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
unifyFeat :: Flist -> Flist -> (Bool, Flist, Subst)
unifyFeat f1 f2 = 
  let (att, val1, val2) = alignFeat f1 f2
  in  case unify val1 val2 of
        Nothing -> (False, [], [])
        Just (res, subst) -> (True, zip att res, subst)
\end{code}

\paragraph{alignFeat}

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
isVar :: GeniVal -> Bool
isVar (GVar _) = True
isVar _        = False

isAnon :: GeniVal -> Bool
isAnon GAnon = True
isAnon _     = False
\end{code}

\subsection{Unification}

\paragraph{unify} performs unification on two lists of GeniVal.  If
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

unify (h1:t1) (h2:t2) =
  let sect = intersect (fromGConst h1) (fromGConst h2)
      unifyval
        | (isAnon h1) = sansrep    h2
        | (isAnon h2) = sansrep    h1 
        | (isVar h1)  = withrep h1 h2
        | (isVar h2)  = withrep h2 h1
        | (not.null) sect = sansrep (GConst sect)
        | otherwise   = Nothing
      --
      withrep (GVar h1) x2 = do
        let s = (h1,x2)
            subfn l = map (substHelper s) l
        (res,subst) <- unify (subfn t1) (subfn t2)
        return (x2:res, s:subst) 
      withrep _ _ = error ("unification error\n" ++ bugInGeni)
      sansrep x2 = do
        (res,subst) <- unify t1 t2 
        return (x2:res, subst)
      --
  in unifyval 
\end{code}

\subsubsection{Unification tests} The unification algorithm should satisfy
the following properties:

Unifying something with itself should always succeed

\begin{code}
prop_unify_self x = 
  case (unify x x) of 
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
  in all (notOverlap) (zip x y) ==> u1 == u2
\end{code}

\ignore{
\begin{code}
-- Definition of Arbitrary GeniVal for QuickCheck
instance Arbitrary GeniVal where
  arbitrary = oneof [ return $ GAnon, 
                      liftM GVar arbitrary, 
                      liftM GConst arbitrary ] 
\end{code}
}

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
showPred :: Pred -> String
showPred (h, p, l) = showh ++ p ++ "(" ++ unwords (map show l) ++ ")"
                     where hideh = h == GAnon -- FIXME: when handles reinstated, we'll have to do this differently || (take 2 h == "gh")
                           showh = if hideh then "" else (show h) ++ ":"
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
substPred (h, n, lp) (s:l) = substPred (subst h, n, map subst lp) l
  where subst = substHelper s 

-- substVals :: [GeniVal] -> Subst -> [GeniVal] 
-- substVals gl sl = foldl' helper gl sl
--   where helper :: [GeniVal] -> (String,GeniVal) -> [GeniVal] 
--         helper lst s = map (substHelper s) lst
\end{code}

\paragraph{substHelper} applies a single substitution on a single value.  
Note that the value is disjunctive, which is which it is represented as
\verb![GeniVal]! instead of just GeniVal.

\begin{code}
substHelper :: (String,GeniVal) -> GeniVal -> GeniVal
substHelper (s1,s2) v = if (v == GVar s1) then s2 else v
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
         where tl2   = substSem tl s
               tsem2 = substSem tsem s
               acc2  = (toPred p : accSem, accSub ++ s) 
  in concatMap next pRes
\end{code}

\paragraph{subsumePred}
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

FIXME: move this up slightly later

\paragraph{sortSem} 
Sorts semantics first according to its predicate, and then to its handles.

\begin{code}
sortSem :: Sem -> Sem
sortSem = sortBy (\(h1,p1,a1) (h2,p2,a2) -> compare (p1, h1:a1) (p2, h2:a2))  
\end{code}


\begin{code}
testBtypes = False --testSubstFlist
\end{code}


