\chapter{Mstate}

Mstate implements the GenI chart generation algorithm and related
operations like TAG substitution, adjunction and feature structure
unification are also implemented here.

TODO:
\begin{enumerate}
\item For some reason, trees with incomplete semantics (eg, mia loves )but with an 
  empty list of substitution nodes are generated
\item in the list of adjnodes, some pairs feature:Var where Var is not instantiates
  are generated
\end{enumerate}

\begin{code}
module Mstate (
   -- Types
   InitRep, AuxRep, GenRep, Mstate, MS, Gstats,

   -- From Gstats,
   szchart, numcompar, geniter, initGstats, addGstats, avgGstats,

   -- From Mstate
   initrep, auxrep, genrep,
   initMState, 
   addToInitRep, addToGenRep,
   genstats,

   -- Re-export from MonadState
   evalState, runState,

   -- Generation
   generate, generateStep,
  
   -- Make your own generator!   
   iapplySubstNode, nullInitRep, getSem, selectGiven,
   incrNumcompar, incrSzchart, incrGeniter, 
   renameTagElem, getGenRep, lookupGenRep, genRepToList,
   getInitRep,
   addListToGenRep,

   -- for debugging only!
   unifyFeat
)

where
\end{code}

\ignore{
\begin{code}
import Monad (ap, 
              when, 
              foldM)

import MonadState (State, 
                   evalState, 
                   runState,
                   get, 
                   put)

import Data.List (intersect, partition, delete, sort, nub, (\\))
import Data.Bits
import FiniteMap 

import Btypes (Ptype(Initial,Auxiliar),
               Flist, 
               Sem, sortSem,
               GType(Other), GNode(..),
               BitVector,
               rootUpd,
               repAdj,
               isVar, isAnon,
               renameTree,
               repSubst,
               constrainAdj, 
               root, foot, 
               substFlist, substFlist')

import Tags (TagElem, TagDerivation, 
             idname, tidnum,
             derivation,
             ttree, ttype, tsemantics, 
             tpolpaths,
             substTagElem,
             adjnodes,
             substnodes)
import Configuration (Params, semfiltered, orderedadj, footconstr)
\end{code}
}

% --------------------------------------------------------------------  
% Code for debugging. (should be latex-commented
% when not in use)
% --------------------------------------------------------------------  

%\begin{code}
%import Debug.Trace
%import Data.Tree 
%import Polarity 
%import Tags
%import Btypes
%\end{code}

% --------------------------------------------------------------------  
\section{Types}
% --------------------------------------------------------------------  

\begin{code}
type InitRep = [TagElem]
type AuxRep  = [TagElem]
-- bitvector of polarity automaton paths
type GenRep  = FiniteMap BitVector [TagElem] 

iaddToInitRep :: InitRep -> TagElem -> InitRep
iaddToInitRep a te = te:a

iaddToAuxRep :: AuxRep -> TagElem -> AuxRep 
iaddToAuxRep a te = te:a

iaddToGenRep :: GenRep -> TagElem -> GenRep
iaddToGenRep c te = addToFM_C (++) c (tpolpaths te) [te] 

listToGenRep :: [TagElem] -> GenRep
listToGenRep = addListToGenRep emptyFM 

addListToGenRep :: GenRep -> [TagElem] -> GenRep
addListToGenRep g tes = foldr (flip iaddToGenRep) g tes 

genRepToList :: GenRep -> [TagElem]
genRepToList gr = concat $ eltsFM gr
\end{code}

\subsection{Generator statistics}

These numbers allow us to keep track of how well the generator is doing.

\begin{code}
data Gstats = Gstats {
  szchart   :: Int,
  numcompar :: Int,
  geniter   :: Int
} deriving Show


initGstats :: Gstats 
initGstats = Gstats {
  szchart   = 0, 
  numcompar = 0,
  geniter   = 0
}

addGstats :: Gstats -> Gstats -> Gstats
addGstats a b = Gstats {
    szchart   = (szchart a) + (szchart b),
    numcompar = (numcompar a) + (numcompar b),
    geniter   = (geniter a) + (geniter b)
  }

avgGstats :: [Gstats] -> Gstats
avgGstats lst = 
 s { szchart   = (szchart s) `div` len,
     numcompar = (numcompar s) `div` len,
     geniter   = (geniter s) `div` len }
 where s = foldr addGstats initGstats lst
       len = length lst
\end{code}

\subsection{Mstate}

\begin{code}
data Mstate = S{initrep    :: InitRep, 
                auxrep     :: AuxRep,
                genrep     :: GenRep,
                tsem       :: Sem,
                step       :: Ptype,
                gencounter :: Integer,
                genconfig  :: Params,
                genstats   :: Gstats}
      deriving Show
\end{code}

\paragraph{initMState} Creates an initial Mstate.  

\begin{code}
initMState ::  [TagElem] -> [TagElem] -> Sem -> Params -> Mstate
initMState cands chart ts config = 
  let (a,i) = partition isPureAux cands 
      c = listToGenRep chart
  in S{initrep  = i, 
       auxrep   = a,
       genrep   = c,
       tsem     = ts,
       step     = Initial,
       gencounter = toInteger $ length cands,
       genconfig  = config,
       genstats   = initGstats}

type MS = State Mstate
\end{code}

\subsubsection{Mstate updaters}

\begin{code}
addToInitRep :: TagElem -> MS ()
addToInitRep te = do 
  s <- get
  put s{initrep = (iaddToInitRep (initrep s) te)}
     
updateInitRep :: InitRep -> MS ()
updateInitRep a = do 
  s <- get  
  put s{initrep = a}

addToAuxRep :: TagElem -> MS ()
addToAuxRep te = do 
  s <- get
  -- each new tree gets a unique id... this makes comparisons faster 
  let counter = (gencounter s) + 1
      te2 = te { tidnum = counter }
  put s{gencounter = counter,
        auxrep = iaddToAuxRep (auxrep s) te2}
 
addToGenRep :: TagElem -> MS ()
addToGenRep te = do 
  s <- get  
  put s { genrep = (iaddToGenRep (genrep s) te) }
  incrSzchart 1

incrGeniter :: Int -> MS ()
incrGeniter n = do
  s <- get
  let oldstats = genstats s 
      newstats = oldstats { geniter = (geniter oldstats) + n }
  put s { genstats = newstats }

incrSzchart :: Int -> MS ()
incrSzchart n = do
  s <- get
  let oldstats = genstats s 
      newstats = oldstats { szchart = (szchart oldstats) + n }
  put s { genstats = newstats }

incrNumcompar :: Int -> MS ()
incrNumcompar n = do
  s <- get
  let oldstats = genstats s 
      newstats = oldstats { numcompar = (numcompar oldstats) + n }
  put s { genstats = newstats }
\end{code}

\subsubsection{Mstate accessors}

We retrieve the Mstate from the State monad and then retrieve a field from it.

\begin{code}
getInitRep :: MS InitRep
getInitRep = do 
  s <- get
  return (initrep s)

getGenRep :: MS GenRep
getGenRep = do 
  s <- get
  return (genrep s)

-- getAuxRep :: MS AuxRep
-- getAuxRep = do 
--   s <- get
--   return (auxrep s)
 
getStep :: MS Ptype
getStep = do 
  s <- get
  return (step s)

getSem :: MS Sem
getSem = do 
  s <- get
  return (tsem s)
\end{code}

These functions let us find out if the InitRep or the AuxRep are null.

\begin{code}
nullInitRep :: MS Bool
nullInitRep = do 
  s <- get  
  return (null (initrep s))

{-
nullAuxRep :: MS Bool
nullAuxRep = do 
  s <- get
  return (null (auxrep s))
-}
\end{code}

\paragraph{lookupGenRep} retrieves a list of trees from the chart which 
could be combined with the given agenda tree.

The current implementation searches for trees which 
\begin{itemize}
\item are on the some of the same polarity automaton paths as the given.
\end{itemize}

\begin{code}
lookupGenRep :: TagElem -> MS [TagElem]
lookupGenRep given = do
  chart <- getGenRep
  -- we ought to count each key lookup
  -- incrNumcompar (length $ keysFM chart)
  -- do the lookup itself
  let gpaths = tpolpaths given
      isGood k _ = isect /= 0
                   where isect = k .&. gpaths 
      goodChart  = filterFM isGood chart 
  return (genRepToList goodChart)
\end{code}

\paragraph{intersectPolPaths} calculates the intersection of two trees'
polarity paths
\begin{code}
intersectPolPaths :: TagElem -> TagElem -> BitVector
intersectPolPaths te1 te2 = (tpolpaths te1) .&. (tpolpaths te2) 
\end{code}

% --------------------------------------------------------------------  
\section{Feature Structure Unification}
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

% --------------------------------------------------------------------  
\section{Substitution}
\label{sec:substitution}
% --------------------------------------------------------------------  

\paragraph{applySubstitution} Given a TagElem it returns the list of all
possible substitutions between it and the elements in GenRep 

\begin{code}
applySubstitution :: TagElem -> MS ([TagElem])
applySubstitution te =  
  do gr <- lookupGenRep te
     let tesem = tsemantics te
         -- we only substitute tags with no overlaping semantics
         gr' = filter (\x -> null (intersect (tsemantics x) tesem)) gr
         -- we rename tags to do a proper substitution
         rte = renameTagElem 'A' te
         rgr' = map (renameTagElem 'B') gr'
         res = ((concat (map (\x -> iapplySubst rte x (substnodes   x)) rgr')) ++
                (concat (map (\x -> iapplySubst x rte (substnodes rte)) rgr')))
     incrNumcompar (2 * (length gr))
     return res
\end{code}

\paragraph{iapplySubst} Given two TagElem t1 t2 (with no overlaping names) and the list of
substitution nodes in t2 it returns ONE possible substitution (the head node)
  of the first in the second.  As all substitutions nodes should be substituted
  we force substitution in orden. 

\begin{code}
iapplySubst :: TagElem -> TagElem -> [(String, Flist, Flist)] -> [TagElem]
iapplySubst _ _ []      = []
iapplySubst te1 te2 (sn:_) = 
  if not (null (substnodes te1))
  then []
  else iapplySubstNode te1 te2 sn

iapplySubstNode :: TagElem -> TagElem -> (String, Flist, Flist) -> [TagElem]
iapplySubstNode te1 te2 sn@(n, fu, _) =
  let isInit x = (ttype x) == Initial
      t1 = ttree te1
      t2 = ttree te2
      r = root t1
      tfup = gup r
      (success, newgup, subst) = unifyFeat tfup fu
      -- IMPORTANT: nt1 should be ready for replacement 
      -- (e.g, top features unified, type changed to Other) 
      -- when passed to repSubst
      nr  = r { gup = newgup,
                gtype = Other }
      nt1 = rootUpd t1 nr 
      ntree = repSubst n nt1 t2 
      
      --
      ncopy x = (gnname x, gup x, gdown x)
      adj1  = (ncopy nr) : (delete (ncopy r) $ adjnodes te1) 
      adj2  = adjnodes te2
      newadjnodes   = sort $ nub $ adj1 ++ adj2
      newTe = te2{derivation = addToDerivation 's' te1 te2,
                  ttree = ntree,
                  substnodes = (delete sn (substnodes te2))++ (substnodes te1),
                  adjnodes =   newadjnodes,
                  tsemantics = sort (nub ((tsemantics te1) ++ (tsemantics te2))),
                  -- tpredictors = sumPredictors (tpredictors te1) (tpredictors te2),
                  tpolpaths  = intersectPolPaths te1 te2 }
      res = substTagElem newTe subst   
      {- debugstr = ("============================================\n" 
                  ++ "substitute " ++ showLite te1 
                  ++ "\ninnertree:\n" ++ (drawTree $ ttree te1) 
                  ++ "\nbefore:\n" ++ (drawTree $ ttree te2) 
                  ++ "\nafter:\n"    ++ (if success then (drawTree ntree) else "n/a")
                  ++ "\nfs: " ++ showPairs tfup ++ " vs. " ++ showPairs fu ++ "\n"
                  ++ "\n ~~~~~~~~"
                  ++ "\nmain  adjnodes: " ++ showTagSites (adjnodes te2)
                  ++ "\ninner adjnodes: " ++ showTagSites (adjnodes te1)
                  ++ "\nnew   adjnodes: " ++ (if success then showTagSites newadjnodes else "n/a")
                 ) -}
  in if (isInit te1 && success) then [res] else []
\end{code}

% --------------------------------------------------------------------  
\section{Adjunction}
\label{sec:adjunction}
\label{sec:ordered_adjunction}
\label{sec:foot_constraint}
% ---------------------------------------------------------------  

\paragraph{applyAdjunction} Given a TagElem, it returns the list of all 
possible adjunctions between it and the elements in GenRep.  GenRep
contains Auxiliars, while TagElem is an Initial

\begin{code}
applyAdjunction :: TagElem -> MS ([TagElem])
applyAdjunction te = do
   gr <- lookupGenRep te
   st <- get
   let tesem = tsemantics te
       -- we only adjunct tags with no overlaping semantics
       gr' =  filter (\x -> null (intersect (tsemantics x) tesem)) gr
       -- we rename tags to do a proper adjunction
       rte  = renameTagElem 'A' te
       rgr' = map (renameTagElem 'B') gr'
       -- strip Nothing
       sn [] = []
       sn (Nothing:xs) = sn xs
       sn ((Just x):xs) = x:(sn xs)
       --
       anodes  = adjnodes te
       ahead   = head anodes
       atail   = tail anodes
       ranodes = adjnodes rte
       -- check if the foot constraint optimisation is enabled
       isFootC = (footconstr.genconfig) st
       -- te2 is to account for the case where we simply don't do
       -- adjunction on that particular node
       resOrdered = if (null ranodes) then [] else te2:applied
                    where gn (a,_,_) = a 
                          ntree = constrainAdj (gn ahead) (ttree te)
                          te2   = te {adjnodes = atail, ttree = ntree}
                          applied = sn $ map fn rgr'
                          fn x = iapplyAdjNode isFootC x rte (head ranodes)
       resNormal  = concatMap fn rgr'
                    where fn x = iapplyAdj isFootC x rte ranodes
       --
       isOrdered = (orderedadj.genconfig) st
       res     = if isOrdered then resOrdered else resNormal
       countTe = if isOrdered then 1 else (length $ adjnodes rte)  
       count   = (length gr) * countTe
   incrNumcompar count 
   return res
\end{code}

\paragraph{iapplyAdj} Given two TagElem t1 (auxiliar) t2 (initial) with
no overlaping names, and the list of adjunction nodes in t2 it returns
the list of possible adjunctions of the first in the second.  Note: first
argument (boolean) is a configuration setting that determines if foot nodes 
should retain an adjunction constraint even after adjunction is complete.

\begin{code}
iapplyAdj :: Bool -> TagElem -> TagElem -> [(String, Flist, Flist)] -> [TagElem]
iapplyAdj _ _ _ [] = []
iapplyAdj fconstr te1 te2 (an:l) =
  let cur  = iapplyAdjNode fconstr te1 te2 an
      next = iapplyAdj fconstr te1 te2 l
  in case cur of
       Nothing  -> next
       Just res -> res:next
\end{code}

The main work for adjunction is done in the helper function below
(see also figure \ref{fig:adjunction}).
Auxiliary tree \texttt{te1} has a root node \texttt{r} and a foot
node \texttt{f}. Main tree \texttt{te2} has an adjunction site \texttt{an}.  
The resulting tree \texttt{res} is a result of splicing \texttt{te1} into
\texttt{te2}.  We replace \texttt{s} with the nodes \texttt{anr} and 
\texttt{anf} (which are the results of unifying \texttt{an} with \texttt{r}
             and \texttt{f} respectively).

\begin{figure}
\begin{center}
\includegraphics[scale=0.5]{images/adjunction.pdf}
\label{fig:adjunction}
\caption{iapplyAdjNode}
\end{center}
\end{figure}

In addition to the trees proper, we have to consider that each tree has
a list with a copy of its adjunction sites.  The adjunction list of the
result (\texttt{adjnodes res}) should then contain \texttt{adjnodes te1}
and \texttt{adjnodes te2}, but replacing \texttt{r} and \texttt{an}
with \texttt{anr} and \texttt{anf}\footnote{\texttt{anf} is only added
if the foot node constraint is disabled}.

\begin{code}
iapplyAdjNode :: Bool -> TagElem -> TagElem -> (String, Flist, Flist) -> Maybe TagElem
iapplyAdjNode fconstr te1 te2 an@(n, an_up, an_down) =
  let t1 = ttree te1
      t2 = ttree te2
      r = root t1
      f = foot t1
      r_up   = gup r    -- top features of the root of the auxiliar tree
      f_down = gdown f  -- bottom features of the foot of the auxiliar tree
      (succ1, anr_up',  subst1)  = unifyFeat r_up an_up 
      (succ2, anf_down, subst2)  = unifyFeat (substFlist f_down subst1) (substFlist an_down subst1)
      -- don't forget to propagate the substitution set from the down stuff
      anr_up = substFlist anr_up' subst2
      -- combined substitution list and success condition
      subst   = subst1++subst2
      success = succ1 && succ2
   
      -- the adjoined tree
      -- ----------------- 
      -- the result of unifying the t1 root and the t2 an 
      anr = r { gup = anr_up,
                gtype = Other }
      -- the result of unifying the t1 foot and the t2 an
      anf = f { gdown = anf_down,
                gtype = Other,
                gaconstr = fconstr }
      -- calculation of the adjoined tree
      nt1 = rootUpd t1 anr
      ntree = repAdj anf n nt1 t2
      
      -- the new adjunction nodes
      -- ------------------------
      ncopy x = (gnname x, gup x, gdown x)
      -- 1) delete the adjunction site and the aux root node 
      auxlite = delete (ncopy r) $ adjnodes te1
      telite  = delete an $ adjnodes te2
      -- 2) union the remaining adjunction nodes 
      newadjnodes' = auxlite ++ telite 
      -- 3) apply the substitutions 
      nte2 = te2 { derivation = addToDerivation 'a' te1 te2,
                   ttree = ntree,
                   adjnodes = newadjnodes', 
                   tsemantics = sort ((tsemantics te1) ++ (tsemantics te2)),
                   tpolpaths = intersectPolPaths te1 te2
                 }
      res' = substTagElem nte2 subst 
      -- 4) add the new adjunction nodes 
      --    this has to come after 3 so that we don't repeat the subst
      addextra a = if fconstr then a2 else (ncopy anf) : a2
                   where a2 = (ncopy anr) : a

      -- the final result  
      -- ----------------
      res  = res' { adjnodes = (addextra.adjnodes) res' }
      {- debugstr = ("============================================\n" 
                  ++ "adjoin " ++ showLite te1 ++ " to node " ++ n
                  ++ "\nfs aux : " ++ showPairs r_up ++ " and " ++ showPairs f_down
                  ++ "\nfs main: " ++ showPairs an_up ++ " and " ++ showPairs an_down 
                  ++ "\nsucc: " ++ show succ1 ++ " and " ++ show succ2
                  ++ "\n------" 
                  ++ "\nmain adjnodes: " ++ showTagSites [an]
                  ++ "\naux adjnodes: " ++ showTagSites (adjnodes te1)
                  ++ "\nnew adjnodes: " ++ (if success then showTagSites (adjnodes res) else "n/a")
                  ++ "\n------" 
                  ++ "\nauxtree:\n" ++ (drawTree $ ttree te1) 
                  ++ "\n ~~~~~~~~"
                  ++ "\nbefore:\n" ++ (drawTree $ ttree te2) 
                  ++ "\n ~~~~~~~~"
                  ++ "\nafter:\n"  ++ (if success then (drawTree ntree) else "n/a")
                  ++ "\n ~~~~~~~~"
                 )  -}
  in if success then Just res else Nothing 
\end{code}


% --------------------------------------------------------------------  
\section{Generate}
% --------------------------------------------------------------------  


\begin{itemize}
\item If both InitRep and AuxRep are empty then there is nothing to do,
  otherwise, if InitRep is empty then we switch to the application of the 
  Adjunction rule. 
\item After the rule is applied we classify solutions into those that are complete 
  and cover the semantics and those that don't.  The first ones are returned 
  and added to the result, while the others are sent back to InitRep.  
\item Notice that if we are applying the Substitution rule then given is added
  to GenRep, otherwise it is deleted. 
\end{itemize}

\begin{code}
generate :: MS [TagElem]
generate = do 
  nir     <- nullInitRep 
  curStep <- getStep

  if (nir && curStep == Auxiliar)
     -- then trace "=================================== END" $ return []
     then return []
     else do res <- generateStep
             -- next!
             return (res ++) `ap` generate

generateStep :: MS [TagElem]
generateStep = do
  nir     <- nullInitRep
  curStep <- getStep
  -- this check may seem redundant with generate, but it's needed 
  -- to protect against a user who calls generateStep on a finished
  -- state
  if (nir && curStep == Auxiliar) 
    then return []
    else do incrGeniter 1
            -- this triggers exactly once in the whole process
            if nir 
               then do { switchToAux; return [] } 
               else generateStep' 

generateStep' :: MS [TagElem] 
generateStep' = do 
  -- choose an item from the agenda
  given <- selectGiven
  -- have we triggered the switch to aux yet?
  curStep <- getStep
  -- do either substitution or adjunction 
  res <- if (curStep == Initial)
         then applySubstitution given
         else applyAdjunction given
  {-
  genrep  <- getGenRep
  initrep <- getInitRep
  let debugstr =  "\ngiven: " ++ (idname given) 
               ++ " | " ++ (showLeaves given)
               ++ "\nagenda: " ++ (showLite initrep)
               ++ "\nchart: " ++ (showLite genrep)
               ++ "\nresult: " ++ (showLite res)
  -}
  -- determine which of the res should go in the agenda 
  -- (monadic state) and which should go in the result (res')
  res' <- classifyNew res
  -- put the given into the chart untouched 
  when (curStep == Initial) (addToGenRep given)
  return res'
\end{code}

\subsection{Generate helper functions}

\paragraph{selectGiven} Arbitrarily selects and removes an element from
the Initial and returns it.

\begin{code}
selectGiven :: MS TagElem
selectGiven = do 
  a <- getInitRep
  updateInitRep (tail a)
  return (head a)
\end{code}

\paragraph{classifyNew} 

Given a list of TagElem, for each tree: 
\begin{enumerate}
\item if the tree is both syntactically complete (no more subst nodes) and
      semantically complete (matches target semantics), it is a result, so
      return it
\item if it is only syntactically complete and it is an auxiliary tree, 
      then we don't need to do any more substitutions with it, so set it 
      aside on the auxiliary agenda (AuxRep)
\item otherwise, put it on the regular agenda (InitRep)
\end{enumerate}

\begin{code}
classifyNew :: [TagElem] -> MS [TagElem]
classifyNew [] =
  return []
classifyNew l = do 
  inputSem <- getSem
  let isResult x = (ttype x /= Auxiliar) && (null $ substnodes x) 
                   && (inputSem == treeSem) 
                   where treeSem = (sortSem $ tsemantics x)
      classify ls x = 
        case () of _ | isResult  x -> return (x:ls)
                     | isPureAux x -> do addToAuxRep x
                                         return ls
                     | otherwise   -> do addToInitRep x
                                         return ls
  foldM classify [] l
\end{code}

\paragraph{switchToAux} When all substitutions has been done, tags with
substitution nodes still open are deleted, then the auxiliars tags are put in
GenRep and the (initial) tags in the repository are moved into the InitRep. The
step is then changed to Auxiliary

\begin{code}
switchToAux :: MS ()
switchToAux = do
  st <- get
  let doneSub    = null.substnodes 
      isInit  x  = (ttype x) == Initial
      chart = genRepToList $ genrep st
      -- You might be wondering why we ignore the auxiliary trees in the 
      -- chart; this is because all the syntactically complete auxiliary
      -- trees have already been filtered away by calls to classifyNew
      initial' = filter (\x -> isInit x && doneSub x) chart 
      aux   = auxrep st
      --
      initialFiltered = semfilter (tsem st) aux initial'
      initial = if (semfiltered $ genconfig st) then initialFiltered else initial'
  {- let debugstr =  "\n====== switch! =====" 
                   ++ "\ninit: " ++ (showLite initial)
                   ++ "\naux: " ++ (showLite aux) -}
  put st{initrep = initial,
         auxrep = [], 
         genrep = listToGenRep aux,
         step = Auxiliar}
\end{code}

% --------------------------------------------------------------------  
\section{SemFilter Optimisation}
\label{sec:semfilter}
% --------------------------------------------------------------------  

This implements the semantic filter optimisation.  The idea is to take full
advantage of Carroll's delayed adjunction.  Consider the semantics
\semexpr{def(m), poor(m), brokenhearted(m), man(m), def(w), woman(w),
beautiful(w), heartless(w), rejects(w,m)}.  At the switchToAux step, we 
are left with the initial trees \natlang{man}, \natlang{woman}, \natlang{the
  woman rejects the man}.  

It would be nice to filter out the structures \natlang{man} and \natlang{woman}
since we know that they are not going to be semantically complete even with
adjunction.  More precisely, on the switch to adjunction, we do the following:

\begin{itemize}
\item Take the union of the semantics of all auxiliary trees; which 
      we call $\phi^*$
\item Delete any initial tree with semantics $\phi^s$ such that
      $\phi^s \cup \phi^*$ is not the target semantics
\end{itemize}

In other words, we delete all initial trees that cannot produce a semantically
complete result even with the help of auxiliary trees.  

\begin{code}
semfilter :: Sem -> [TagElem] -> [TagElem] -> [TagElem] 
semfilter inputsem aux initial = 
  let auxsem     = sortSem $ nub $ concatMap tsemantics aux
      missingsem = sortSem $ inputsem     \\ auxsem
      restsem x  = sortSem $ (tsemantics x \\ auxsem)
      goodsem x  = (restsem x == missingsem)
  in filter goodsem initial
\end{code}

% --------------------------------------------------------------------  
\section{Miscellaneous}
% --------------------------------------------------------------------  

\paragraph{renameTagElem} Given a Char c and a TagElem te, renames nodes in
substnodes, adjnodes and the tree in te by prefixing c. 

\begin{code}
renameTagElem :: Char -> TagElem -> TagElem
renameTagElem c te = 
  let sn = map (\(n, fu, fd) -> (c:n, fu, fd)) (substnodes te)
      an = map (\(n, fu, fd) -> (c:n, fu, fd)) (adjnodes te)
      t = renameTree c (ttree te)
  in te{substnodes = sn, 
        adjnodes = an,
        ttree = t}
\end{code}


\paragraph{isPureAux} returns True if a tree is an auxiliary tree with
no substitution nodes

\begin{code}
isPureAux :: TagElem -> Bool 
isPureAux x = ((ttype x) == Auxiliar && (null $ substnodes x))
\end{code}

\subsection{Derivation trees}

The basic problem of derivation trees is that you have to account for
the same tree being used in two seperate places; these two uses must
be treated as different trees, or else you'll get all sorts of 
unpredicted behaviour like your drawing software displaying multiple
edges or loops in the derivation tree.  We approach this problem by
prepending a Gorn address to each node in the derivation history.

Given a trees $t_p$ with derivation $(c_p, h_p)$ and a tree $t_c$ with
derivation $(c_c, h_c)$, the derivation history that results from performing
some operation on $t_p$ (either substituting or adjoining $t_c$ into it) is:
$(c,h)$ where $c = c' + 1$ and $h$ is $h_p$ appended to $h_c'$ where $h_c'$ is
the result of prepending $c_p$ to every item of $h_c$.

\begin{code}
addToDerivation :: Char -> TagElem -> TagElem -> TagDerivation
addToDerivation op tc tp =
  let (cp,hp) = derivation tp
      (_ ,hc) = derivation tc
      --
      newcp   = cp + 1
      addcp x = (show newcp) ++ "." ++ x
      newhc   = map (\ (o,c,p) -> (o, addcp c, addcp p)) hc
      --
      newnode = (op, addcp $ idname tc, idname tp) 
  in (newcp, newnode:(hp++newhc) )
\end{code}
