\chapter{Btypes.lhs}

\begin{code}
module Btypes(
   -- Datatypes
   GNode(GN), GType(Subs, Foot, Lex, Other), GTtree,
   Ttree(TT), Ptype(Initial,Auxiliar,Unspecified), 
   Pred, Flist, AvPair, 
   ILexEntry(ILE), Grammar, Sem, Subst,
   BitVector,

   -- Functions from Tree GNode
   repSubst, repAdj, constrainAdj, 
   renameTree, substTree, root, rootUpd, foot, setLexeme,
   showGNodeAll,

   -- Functions from Sem
   toKeys, subsumeSem, sortSem, substSem, showSem, showPred,

   -- Projectors from GNode
   gnname, gup, gdown, ganchor, glexeme, gtype, gaconstr,

   -- Projectors from Tdesc
   params, pfeat, ptype, tree, 
   ptpolarities, ptpredictors,

   -- Projectors from ILexEntry
   iword, itreename, iparams, ipfeat, iptype, isemantics, ipredictors, 
 
   -- Functions from Flist
   substFlist, substFlist', sortFlist,
   showPairs, showAv,

   -- Other functions
   isVar, isAnon, emptyGNode, testBtypes, appendToVars
) where
\end{code}

\begin{code}
import Debug.Trace
import Data.Bits
import Data.Char (isUpper)
import Data.Tree
import FiniteMap (FiniteMap, fmToList)
import List (intersperse, sortBy, nub)
\end{code}

\begin{code}
type BitVector = Integer
\end{code}

\subsection{Tree and GNode}

Projector and Update function for Tree

\begin{code}
root :: Tree a -> a
root (Node a l) = a
\end{code}

\begin{code}
rootUpd :: Tree a -> a -> Tree a
rootUpd (Node a l) b = (Node b l)
\end{code}

\begin{code}
foot :: Tree GNode -> GNode
foot t = let (ln, flag) = listFoot [t] in (head ln)
\end{code}

\begin{code}
listFoot :: [Tree GNode] -> ([GNode], Bool)
listFoot [] = ([], False)
listFoot ((Node a l1):l2) =
    if (gtype a == Foot)
    then ([a], True)
    else let (ln1, flag1)  = listFoot l1 
             (ln2, flag2) = listFoot l2 
             in if flag1
                then (ln1, flag1)
                else (ln2, flag2)
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
substTree (Node a l) s =
    Node (substGNode a s) (map (\t -> substTree t s) l)
\end{code}

\paragraph{renameTree} 
Given a Char c and a tree, renames nodes in 
the tree by prefixing c.

\begin{code}
renameTree :: Char -> Tree GNode -> Tree GNode
renameTree c (Node a l) =
    Node a{gnname = c:(gnname a)} (map (renameTree c) l)
\end{code}

\paragraph{appendToVars}
Appends a string to all the variables in the tree.  This can be used to
assure that every tree in the grammar has a unique set of variable names.
See section \ref{sec:fs_unification}.

\begin{code}
appendToVars :: String -> Tree GNode -> Tree GNode
appendToVars suf (Node a l) =
    Node a{ gup   = map appFn (gup a),
            gdown = map appFn (gdown a) }
          (map (appendToVars suf) l)
    where appFn (f,v) = (f, if (isVar v && (not.isAnon) v)
                            then v ++ suf 
                            else v)
\end{code}

\subsection{Substitution}

\paragraph{repSubst} 
Given two trees t1 t2, and the name n of a node in t2, 
replaces t1 in t2 at the (leaf) node named n.
\begin{code}
repSubst :: String -> Tree GNode -> Tree GNode -> Tree GNode
repSubst n t1 t2 =
  let filt (Node a []) = (gnname a) == n 
      filt (Node a _)  = False
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
adjunction move.  
\begin{code}

repAdj :: Bool -> String -> Tree GNode -> Tree GNode -> Tree GNode
repAdj fconstr n t1 t2 =
  let filt (Node a _) = (gnname a == n)
      fn (Node _ l)   = repFoot fconstr t1 l
      (lt,flag) = listRepNode fn filt [t2] 
  in if flag 
     then head lt 
     else error ("adjunction unexpectedly failed on node " ++ n)

repFoot :: Bool -> Tree GNode -> [Tree GNode] -> Tree GNode
repFoot fconstr t l =
  let filt (Node a _) = (gtype a == Foot)
      -- TODO More things might have to be changed here
      fn (Node a _) = Node a2 l
                      where a2 = a { gtype = Other, gaconstr = fconstr } 
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

\subsection{repNode} 

listRepNode is a generic tree-walking/editing function.  It takes a
replacement function, a filtering function and a tree.  It returns the
tree, except that the first node for which the filtering function
returns True is transformed with the replacement function.

\begin{code}
listRepNode :: (Tree GNode -> Tree GNode) -> (Tree GNode -> Bool) 
              -> [Tree GNode] -> ([Tree GNode], Bool)
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

% ----------------------------------------------------------------------
\section{Other types}
% ----------------------------------------------------------------------

\begin{code}
data Ptype = Initial | Auxiliar | Unspecified   
             deriving (Show, Eq)
\end{code}

\begin{code}
type Flist   = [AvPair]
type AvPair  = (String,String)
\end{code}

% ----------------------------------------------------------------------
\section{Macros}
% ----------------------------------------------------------------------

\begin{code}
instance (Show k, Show e) => Show (FiniteMap k e) where 
  show fm = show $ fmToList fm
\end{code}

\begin{code}
data Ttree a = TT{params :: [String],
                  pfeat :: Flist,
                  ptype :: Ptype,
                  tree :: Tree a,
                  -- optimisation stuff
                  ptpolarities  :: FiniteMap String Int,
                  ptpredictors  :: [(AvPair,Int)]}
           deriving Show
\end{code}

% ----------------------------------------------------------------------
\section{GNode}
% ----------------------------------------------------------------------

Node type used during parsing of the grammar 

\begin{code}
data GType = Subs | Foot | Lex | Other
           deriving (Show, Eq)
\end{code}

\begin{code}
data GNode = GN{gnname :: String,
                gup :: Flist,
                gdown :: Flist,
                ganchor :: Bool,
                glexeme :: String,
                gtype :: GType,
                gaconstr :: Bool}
           deriving Eq

instance Show GNode where
  show gn = 
    let cat' = filter (\ (f,_) -> f == "cat") $ gup gn
        cat  = if (null cat') then "" else snd $ head cat'
        lex  = if (null $ glexeme gn) then "" else glexeme gn
        -- 
        extra = case (gtype gn) of         
                   Subs -> " (s)"
                   Foot -> " *"
                   otherwise -> if (gaconstr gn)  then " (na)"   else ""
    in if (not (null cat || null lex))
       then cat ++ ":" ++ lex ++ extra
       else cat ++ lex ++ extra
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

\paragraph{showGNodeAll} shows everything you would want to know about a
gnode, probably more than you want to know

\begin{code}
showGNodeAll gn = 
        let sgup = if (null $ gup gn) 
                   then "" 
                   else "Top: [" ++ showPairs (gup gn) ++ "]\n"
            sgdown = if (null $ gdown gn)
                     then ""
                     else "Bot: [" ++ showPairs (gdown gn) ++ "]\n"
            label  = if (null $ glexeme gn) then "" else (glexeme gn) ++ "\n"
        in label ++ sgup ++ sgdown -- (show gn ++ "\n" ++)
\end{code}


\begin{code}
type GTtree = Ttree GNode

type Grammar = FiniteMap String GTtree
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

\paragraph{substFlist} 
Given an Flist and a substitution, applies 
 the substitution to the Flist.
\begin{code}
substFlist :: Flist -> Subst -> Flist
substFlist fl sl = foldl substFlist' fl sl

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

substFlist': Given an Flist and a single substition, 
applies that substitution to the Flist... 

\begin{code}
substFlist' :: Flist -> (String,String) -> Flist 
substFlist' fl (s1, s2) = map (\ (f, v) -> (f, if (v ==s1) then s2 else v)) fl
\end{code}

\paragraph{sortFlist} 
Sorts Flists according with it's feature
\begin{code}
sortFlist :: Flist -> Flist
sortFlist fl = sortBy (\(f1,v1) (f2, v2) -> compare f1 f2) fl
\end{code}

% ----------------------------------------------------------------------
\section{Lexicon}
% ----------------------------------------------------------------------

\begin{code}
showPairs :: Flist -> String
showPairs l = concat $ intersperse " " $ map showAv l
showAv (y,z) = y ++ ":" ++ z 
\end{code}

Auxiliar types used during the parsing of the Lexicon 

\begin{code}
data ILexEntry = ILE{iword :: String,
                     itreename :: String,
                     iparams :: [String],
                     ipfeat :: Flist,
                     iptype :: Ptype,
                     isemantics :: Sem,
                     ipredictors :: [(AvPair,Int)]}
               deriving Show
\end{code}

% ----------------------------------------------------------------------
\section{Semantics}
% ----------------------------------------------------------------------

\begin{code}
-- handle, id, parameters
type Pred = (String, String, [String])
type Sem = [Pred]
type Subst = [(String, String)]
\end{code}

\begin{code}
showSem :: Sem -> String
showSem l =
    "[" ++ (concat $ intersperse "," $ map showPred l) ++ "]"
\end{code}

\begin{code}
showPred (h, p, l) = showh ++ p ++ "(" ++ (showAtr l)++ ")"
                     where showh = if (null h) then "" else h ++ ":"
showAtr l = concat $ intersperse "," l
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
toKeys l = map (\(h,prop,par) -> prop++(show (length par))) l
\end{code}

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

\paragraph{repXbyY} 
Given two values s1 and s2 and a list, it replace the 
first by the second in the list
\begin{code}
repXbyY :: (Eq a) => a -> a -> [a] -> [a] 
repXbyY s1 s2 l = map (\x->if (x == s1) then s2 else x) l
\end{code}

\paragraph{instantiate} 
Given a predicate (name, listParams) p and the
semantics s of a candidate, it instantiates s in terms of p.  
I.e variables in s are instantiated according to p, but notice
that variables in s are left as is and no error is reported.  
Candidates should be checked for subsumeSem afterwards 

\begin{code}
{-
instCandSem :: (String, [String]) -> Sem -> Sem
instCandSem p [] =
    []
instCandSem p@(pn1, lp1) (h@(pn2, lp2):rl) =
    if ((pn1 == pn2) && (length lp1 == length lp2))
       then let sub = findSubstCand lp1 lp2
                in (substPred p sub):(instCandSem p (substSem rl sub))
       else h:(instCandSem p rl) -}
\end{code}

\begin{code}
{-
findSubstCand :: [String] -> [String] -> Subst
findSubstCand [] [] =
    []
findSubstCand (w1:l1) (w2:l2) =
    if (isVar w2) 
       then (w2, w1):findSubstCand l1 l2
       else findSubstCand l1 l2
-}
\end{code}

\begin{code}
substPred :: Pred -> Subst -> Pred
substPred p [] = p
substPred (h, n, lp) ((a,b):l) = substPred (fixHandle, n, repXbyY a b lp) l
  where fixHandle = if (h == a) then b else h 
\end{code}

\paragraph{subsumeSem} 
\label{fn:subsumeSem}

Given the target Sem ts and the Sem s of a TagElem,
returns the list of possible substitutions so that s is a subset of ts.

TODO WE ASSUME BOTH SEMANTICS ARE ORDERED and non-empty.

\begin{code}
subsumeSem :: Sem -> Sem -> [Subst]
subsumeSem ts [(h,p,par)] = subsumePred ts (h,p,par)
subsumeSem ts (at@(h,p,par):l) =
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
subsumePred [] (h, p, la) = []
subsumePred ((h1, p1, la1):l) (h2,p2,la2) =
    -- if we found the proper predicate
    if ((p1 == p2) && (length la1 == length la2))
    then let subst = map nub (pairVar (h1:la1) (h2:la2) [])
             isNotVar = not.isVar
             -- defines the subst, taking care of clashing of var. with check
             pairVar [] [] l = [[]]   -- [[]] means: Empty subst is a solution
             pairVar (v1:l1) (v2:l2) l  
               | v1 == v2 = pairVar l1 l2 l
               | isNotVar v1 && isNotVar v2 = [] -- no solution
               | isVar v1 && checkAss (v1,v2) l = 
                   map ((v1,v2):) (pairVar l1 l2 ((v1,v2):l))
               | isVar v2 && checkAss (v2,v1) l =
                   map ((v2,v1):) (pairVar l1 l2 ((v2,v1):l))
               | otherwise                      = []
             checkAss (v1,v2) [] = True
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
sortSem s = sortBy (\(h1, p1, par1) -> \(h2, p2, par2) -> compare p1 p2) s
\end{code}

\begin{code}
testBtypes = testSubstFlist
\end{code}


