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

\chapter{Mstate}

Mstate implements the GenI chart generation algorithm and related
operations like TAG substitution, adjunction. 

TODO:
\begin{enumerate}
\item For some reason, trees with incomplete semantics (eg, mia loves )but with an 
  empty list of substitution nodes are generated
\item in the list of adjnodes, some pairs feature:Var where Var is not instantiates
  are generated
\item To eliminate redundant generations enforce that substitution
      tree should have an empty list of substitution nodes when applying 
      Substitution 
\end{enumerate}

\begin{code}
module Mstate (
   -- Types
   InitRep, AuxRep, GenRep, Mstate, MS, Gstats,

   -- From Gstats,
   szchart, numcompar, geniter, initGstats, addGstats, avgGstats,

   -- From Mstate
   initrep, auxrep, genrep, trashrep,
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
   renameTagElem, getGenRep, lookupGenRep,
   getInitRep)
where
\end{code}

\ignore{
\begin{code}
import Control.Monad (ap, 
              when, 
              foldM)

import Control.Monad.State (State, 
                   evalState, 
                   runState,
                   get, 
                   put)

import Data.List (intersect, partition, delete, sort, nub, (\\))
import Data.Maybe (catMaybes)
import Data.Tree 
import Data.Bits

import Bfuncs (Ptype(Initial,Auxiliar),
               Flist, 
               Sem, sortSem, Subst,
               GType(Other), GNode(..),
               rootUpd,
               repAdj,
               renameTree,
               repSubst,
               constrainAdj, 
               root, foot, 
               substTree, substGNode, substFlist, unifyFeat)

import Tags (TagElem, TagSite, TagDerivation,  TagStatus(..),
             idname, tidnum,
             derivation,
             ttree, ttype, tsemantics, thighlight, tdiagnostic,
             tpolpaths,
             substTagElem, 
             adjnodes,
             substnodes,
             tadjlist
            )
import Configuration (Params, semfiltered, footconstr,
                      usetrash)
import General (BitVector, fst3, mapTree)
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
%import Bfuncs
%\end{code}

% --------------------------------------------------------------------  
\section{Types}
% --------------------------------------------------------------------  

\begin{code}
type InitRep = [TagElem]
type AuxRep  = [TagElem]
type GenRep  = [TagElem] 
type TrashRep = [TagElem]

iaddToInitRep :: InitRep -> TagElem -> InitRep
iaddToInitRep a te = te:a

iaddToAuxRep :: AuxRep -> TagElem -> AuxRep 
iaddToAuxRep a te = te:a

iaddToGenRep :: GenRep -> TagElem -> GenRep
iaddToGenRep c te = te:c

iaddToTrashRep :: TrashRep -> TagElem -> TrashRep 
iaddToTrashRep t te = te:t
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

Note the trashrep is not actually essential to the operation of the
generator; it is for pratical debugging of grammars.  Instead of
trees dissapearing off the face of the debugger; they go into the
trash where the user can inspect them and try to figure out why they
went wrong.  To keep the generator from exploding we also keep an
option not to use the trash, so that it is only enabled in debugger
mode.

\begin{code}
data Mstate = S{initrep    :: InitRep, 
                auxrep     :: AuxRep,
                genrep     :: GenRep,
                trashrep   :: TrashRep,
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
      c = chart
  in S{initrep  = i, 
       auxrep   = a,
       genrep   = c,
       trashrep = [],
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

addToTrashRep :: TagElem -> TagStatus -> MS ()
addToTrashRep te err = do 
  s <- get
  let te2 = te { tdiagnostic = err }
  when ((usetrash.genconfig) s) $
    put s { trashrep = (iaddToTrashRep (trashrep s) te2) }

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
\label{fn:lookupGenRep}

The current implementation searches for trees which 
\begin{itemize}
\item do not have overlapping semantics with the given
\item are on the some of the same polarity automaton paths as the given.
\end{itemize}

\begin{code}
lookupGenRep :: TagElem -> MS [TagElem]
lookupGenRep given = do
  chart <- getGenRep
  let gpaths = tpolpaths given
      gsem   = tsemantics given
  return [ t | t <- chart
             -- should be on the same polarity path (chart sharing)
             , (tpolpaths t) .&. gpaths /= 0 
             -- semantics should not be overlapping
             && (null $ intersect (tsemantics t) gsem)
         ] 
\end{code}

\paragraph{intersectPolPaths} calculates the intersection of two trees'
polarity paths
\begin{code}
intersectPolPaths :: TagElem -> TagElem -> BitVector
intersectPolPaths te1 te2 = (tpolpaths te1) .&. (tpolpaths te2) 
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
     let -- tesem = tsemantics te
         -- we rename tags to do a proper substitution
         rte = renameTagElem 'A' te
         rgr' = map (renameTagElem 'B') gr
         res = ((concatMap (\x -> iapplySubst rte x (substnodes   x)) rgr') ++
                (concatMap (\x -> iapplySubst x rte (substnodes rte)) rgr'))
     incrNumcompar (2 * (length gr)) 
     return res
\end{code}

\paragraph{iapplySubst} Given two TagElem t1 t2 (with no overlaping names) and the list of
substitution nodes in t2 it returns ONE possible substitution (the head node)
  of the first in the second.  As all substitutions nodes should be substituted
  we force substitution in order.

\begin{code}
iapplySubst :: TagElem -> TagElem -> [(String, Flist, Flist)] -> [TagElem]
iapplySubst _ _ []      = []
iapplySubst te1 te2 (sn:_) = 
  if not (null (substnodes te1))
  then []
  else iapplySubstNode te1 te2 sn

iapplySubstNode :: TagElem -> TagElem -> (String, Flist, Flist) -> [TagElem]
iapplySubstNode te1 te2 sn@(n, fu, fd) =
  let isInit x = (ttype x) == Initial
      t1 = ttree te1
      t2 = ttree te2
      r = root t1
      tfup = gup r
      (success, newgup, subst) = unifyFeat tfup fu

      -- IMPORTANT: nt1 should be ready for replacement 
      -- (e.g, top features unified, type changed to Other) 
      -- when passed to repSubst
      nr  = r { gup   = newgup,
                -- note that the bot features come from sn, not r!
                gdown = substFlist fd subst,
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
                  tsemantics = sortSem (tsemantics te1 ++ tsemantics te2),
                  -- tpredictors = sumPredictors (tpredictors te1) (tpredictors te2),
                  tpolpaths  = intersectPolPaths te1 te2,
                  thighlight = [gnname nr]} 
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

13 april 2005 - only uses ordered adjunction as described in \cite{kow04a}
\begin{code}
applyAdjunction :: TagElem -> MS ([TagElem])
applyAdjunction te = do
   gr <- lookupGenRep te
   st <- get
   let -- we rename tags to do a proper adjunction
       rte  = renameTagElem 'A' te
       rgr' = map (renameTagElem 'B') gr
       --
       anodes  = adjnodes te
       ahead   = head anodes
       atail   = tail anodes
       ranodes = adjnodes rte
       -- check if the foot constraint optimisation is enabled
       isFootC = (footconstr.genconfig) st
       -- te2 is to account for the case where we simply don't do
       -- adjunction on that particular node
       res = if (null ranodes) then [] else te2:applied
                    where gn    = fst3 ahead
                          ntree = constrainAdj gn (ttree te)
                          te2   = te {adjnodes = atail, 
                                      ttree = ntree,
                                      thighlight = [gn]}
                          applied = catMaybes $ map fn rgr'
                          fn x = iapplyAdjNode isFootC x rte (head ranodes)
       --
       count   = (length gr) 
   incrNumcompar count 
   return res
\end{code}

\paragraph{iapplyAdj} Given two TagElem \texttt{te1} (auxiliar) \texttt{te2} (initial) with
no overlapping names, and the list of adjunction nodes in \texttt{te2} it returns
the list of possible adjunctions of the first in the second.  Note: first
argument (boolean) is a configuration setting that determines if foot nodes 
should retain an adjunction constraint even after adjunction is complete.

Note: no longer used because of mandatory ordered adjunction (change 13 april 2005)
\begin{code}
{--
iapplyAdj :: Bool -> TagElem -> TagElem -> [(String, Flist, Flist)] -> [TagElem]
iapplyAdj _ _ _ [] = []
iapplyAdj fconstr te1 te2 (an:l) =
  let cur  = iapplyAdjNode fconstr te1 te2 an
      next = iapplyAdj fconstr te1 te2 l
  in case cur of
       Nothing  -> next
       Just res -> res:next
--}
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
      -- success = succ1 && succ2

-- jackie
      success' = succ1 && succ2
      repeatadj = elem (n, (tidnum te1)) (tadjlist te2)
      success = success' && (not repeatadj)
-- jackie

      -- the adjoined tree
      -- ----------------- 
      -- the result of unifying the t1 root and the t2 an 
      anr = r { gnname = n, -- jackie
                gup = anr_up,
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
                   tsemantics = sortSem (tsemantics te1 ++ tsemantics te2),
                   tpolpaths = intersectPolPaths te1 te2,
                   thighlight = map gnname [anr, anf] 
                 }
      res' = substTagElem nte2 subst 
      -- 4) add the new adjunction nodes 
      --    this has to come after 3 so that we don't repeat the subst
      addextra a = if fconstr then a2 else (ncopy anf) : a2
                   where a2 = (ncopy anr) : a

      -- the final result  
      -- ----------------
      res  = res' { adjnodes = (addextra.adjnodes) res' 
-- jackie
, tadjlist = (n, (tidnum te1)):(tadjlist te2)
-- jackie
}
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
  if (curStep == Initial) 
     then addToGenRep   given
     else when ((null.adjnodes) given) $ addToTrashRep given TS_NotAResult
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
      unify the top and bottom feature structures of each node.  If that
      succeeds, return it, otherwise discard it completely.
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
                   && (inputSem == treeSem) && (null $ adjnodes x)
                   where treeSem = tsemantics x
      tbUnify x ls = case (tbUnifyTree x) of
                       Left n  -> do let x2  = x { thighlight = [n] }
                                     addToTrashRep x2 TS_TbUnify 
                                     return ls
                       Right x2 -> return (x2:ls)
      classify ls x 
        | isResult  x = tbUnify x ls
        | isPureAux x = do addToAuxRep x
                           return ls
        | otherwise   = do addToInitRep x
                           return ls
  -- return list of completed trees (as defined above in comments)
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
      chart = genrep st
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
         genrep = aux,
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
\section{Top and bottom unification}
% --------------------------------------------------------------------  

\paragraph{tbUnifyTree} unifies the top and bottom feature structures
of each node on each tree. If succesful we return the tree, otherwise we
return a string indicating the name of the offending node.  This is is the
final step in generation of a result.

We do unification in twe steps: the first time is to check if
unification is possible and to determine/apply variable substitutions
throughout the entire tree.  The first time we do unification, we
discard the results.  The second time we do unification is to get the
result and only that; we do not do any more success checks or
substitutions.  

Note: this does not detect if there are multiple nodes which cause top and
bottom unification to fail.

\begin{code}
type TbEither = Either String (Subst, Tree GNode)
tbUnifyTree :: TagElem -> Either String TagElem
tbUnifyTree te =
  let tryUnification :: Tree GNode -> TbEither 
      tryUnification t = foldr tbUnifyNode start flat 
        where start = Right ([], t)
              flat  = flatten t
      --
      fixNode :: GNode -> GNode
      fixNode gn = gn { gup = u, gdown = [] }
                   where (_,u,_) = unifyFeat (gup gn) (gdown gn)
      -- 
      fixSite :: Subst -> TagSite -> TagSite 
      fixSite sb (n, u, d) = (n, u3, [])
        where u2 = substFlist sb u 
              d2 = substFlist sb d
              (_,u3,_) = unifyFeat u2 d2
      --
      fixTe :: Subst -> Tree GNode -> TagElem
      fixTe sb tt2 = te { ttree      = mapTree fixNode tt2
                        , adjnodes   = map (fixSite sb) (adjnodes te)
                        , substnodes = map (fixSite sb) (substnodes te)}
  in case (tryUnification $ ttree te) of 
       Left  n        -> Left  n  
       Right (sb,tt2) -> Right (fixTe sb tt2)
\end{code}

Our helper function corresponds to the first unification step.  It is
meant to be called from a fold.  The node argument represents the
current node being explored.  The Maybe argument holds a list of 
pending substitutions and a copy of the entire tree.

There are three things going on in here:

\begin{enumerate}
\item check if unification is possible - first we apply the pending
      substitutions on the node and then we check if unification
      of the top and bottom feature structures of that node 
      succeeds
\item keep track of the substitutions that need to be performed -
      any new substitutions that result from unification are 
      added to the pending list
\item propagate new substitutions throughout the tree - this is
      why we keep a copy of the tree; note that we \emph{have}
      to keep the entire tree around because variable substitutions
      must be back-propagated to nodes we had visited in the past.
\end{enumerate}

You might also think it redundant to apply substitutions both to the
entire tree and the current node; but we have to because the node is
only a copy of the tree data and not a reference to it.  This is why 
we keep a list of pending substitutions instead of simply returning
the corrected tree.  

Note that we wrap the second argument in a Maybe; this is used to
indicate that if unification suceeds or fails.  We also use it to
prevent the function from doing any work if a unification failure
from a previous call has already occured. 

Getting this right was a big pain in the butt, so don't go trying to
simplify this over-complicated code unless you know what you're doing.

\begin{code}
tbUnifyNode :: GNode -> TbEither -> TbEither 
tbUnifyNode gnRaw st = 
  case st of 
    Right (pending, whole) ->
      let -- apply pending substitutions
          gn = substGNode gnRaw pending
          -- check top/bottom unification on this node
          (succ, _, sb) = unifyFeat (gup gn) (gdown gn)
          pending2 = pending ++ sb
          whole2   = substTree whole sb
      in if succ 
         then -- apply any new substutions to the whole tree
              Right (pending2, whole2)
         else -- stop all future iterations
              Left (gnname gn)
    -- don't bother   
    Left n -> Left n  
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
      al = map (\(n, tid) -> (c:n, tid)) (tadjlist te)
      t = renameTree c (ttree te)
  in te{substnodes = sn, 
        adjnodes = an,
        tadjlist = al,
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
