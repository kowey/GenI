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

\chapter{SimpleBuilder}
\label{cha:SimpleBuilder}

A SimpleBuilder is a Builder which constructs derived trees using a
simple agenda control mechanism and two-phase realisation (substitution
before adjunction).  There is no packing strategy whatsoever; each chart
item is a derived tree.

\begin{code}
module SimpleBuilder (
   -- Types
   Agenda, AuxAgenda, Chart, SimpleStatus, MS, Gstats,

   -- From SimpleStatus
   simpleBuilder, setup,
   theAgenda, theAuxAgenda, theChart, theTrash, theResults,
   initSimpleBuilder, 
   addToAgenda, addToChart,
   genconfig, genstats, 

   getChart, getAgenda)
where
\end{code}


\ignore{
\begin{code}
import Control.Monad (when, guard, filterM)

import Control.Monad.State (State, get, put, runState)

import Data.List (intersect, partition, delete, sort, nub, (\\))
import Data.Maybe (catMaybes)
import Data.Tree 
import Data.Bits
import qualified Data.Map as Map 
import qualified Data.Set as Set

import Automaton ( automatonPaths, NFA(..), addTrans )
import Btypes 
  ( Ptype(Initial,Auxiliar),
  , Flist 
  , Replacable(..), alphaConvert
  , Sem, sortSem, Subst
  , GType(Other), GNode(..)
  , rootUpd
  , repAdj
  , renameTree
  , repSubst
  , constrainAdj
  , root, foot
  , unifyFeat, unifyFeat2)
import Builder (Gstats, UninflectedWord, UninflectedSentence)
import qualified Builder as B

import Tags (TagElem, TagSite, TagDerivation,  
             collect,
             idname, tidnum,
             derivation,
             setTidnums,
             ttree, ttype, tsemantics, thighlight, tdiagnostic,
             tpolpaths,
             adjnodes,
             substnodes,
             tadjlist,
             tagLeaves,
             ts_synIncomplete, ts_semIncomplete, ts_tbUnificationFailure,
            )
import Configuration 
import General (BitVector, fst3, mapTree)
import Polarity
\end{code}
}

Here is our implementation of Builder.

\begin{code}
simpleBuilder = B.Builder 
  { B.init     = initSimpleBuilder 
  , B.step     = generateStep
  , B.stepAll  = B.defaultStepAll simpleBuilder
  , B.run      = run 
  , B.finished = \s -> (null.theAgenda) s && (step s == Auxiliar)
  , B.stats    = genstats 
  , B.setStats = \t s -> s { genstats = t } 
  , B.unpack   = unpackResults.theResults }
\end{code}

% --------------------------------------------------------------------  
\section{Main stuff}
% --------------------------------------------------------------------  

\fnlabel{run} performs surface realisation from an input semantics and a
lexical selection.  If the polarity automaton optimisation is enabled,
it first constructs the polarity automaton and uses that to guide the
surface realisation process.

There's an unfortunate source of complexity here : one way to do
generation is to treat polarity automaton path as a seperate 
generation task.  We do this by mapping the surface realiser 
across each path and then merging the final states back into one.

You should be careful if you want to translate this to a builder
that uses a packing strategy; you should take care to rename the
chart edges accordingly.

\begin{code}
run input config = 
  let stepAll = B.stepAll simpleBuilder
      init    = B.init simpleBuilder
      -- combos = polarity automaton paths 
      combos  = fst (setup input config)
      --
      nullSt = initSt [] 
      finalStates = map (snd.generate.initSt) combos
        where generate = runState stepAll 
      initSt c = init (input { B.inCands = c }) config
      -- WARNING: will not work with packing!
      mergeSt st1 st2 = 
        st1 { genstats   = B.addGstats (genstats st1) (genstats st2)
            , theResults = (theResults st1) ++ (theResults st2) }
  in foldr mergeSt nullSt finalStates 
\end{code}

\fnlabel{setup} is one of the substeps of run (so unless you are a
graphical debugger, you probably don't need to invoke it yourself)

\begin{code}
setup input config = 
 let cand     = B.inCands input
     seminput = B.inSemInput input 
     --
     extraPol = extrapol config
     rootCats = rootCatsParam config
     -- do any optimisations
     isPol      = polarised config
     -- polarity optimisation (if enabled)
     autstuff = buildAutomaton seminput cand rootCats extraPol
     finalaut = (snd.fst) autstuff
     paths    = map toTagElem (automatonPaths finalaut)
       where toTagElem = concatMap (lookupAndTweak $ snd autstuff)
     combosPol  = if isPol then paths else [cand]
     -- chart sharing optimisation (if enabled)
     isChartSharing = chartsharing config
     combosChart = if isChartSharing 
                    then [ detectPolPaths combosPol ] 
                    else map defaultPolPaths combosPol 
     -- 
     combos = map (map alphaConvert.setTidnums) combosChart
  in (combos, fst autstuff)
\end{code}

% --------------------------------------------------------------------  
\section{Types}
% --------------------------------------------------------------------  

\begin{code}
type Agenda = [TagElem]
type AuxAgenda  = [TagElem]
type Chart  = [TagElem] 
type Trash = [TagElem]

iaddToAgenda :: Agenda -> TagElem -> Agenda
iaddToAgenda a te = te:a

iaddToAuxAgenda :: AuxAgenda -> TagElem -> AuxAgenda 
iaddToAuxAgenda a te = te:a

iaddToChart :: Chart -> TagElem -> Chart
iaddToChart c te = te:c

iaddtoTrash :: Trash -> TagElem -> Trash 
iaddtoTrash t te = te:t
\end{code}

\subsection{SimpleStatus}

Note the theTrash is not actually essential to the operation of the
generator; it is for pratical debugging of grammars.  Instead of
trees dissapearing off the face of the debugger; they go into the
trash where the user can inspect them and try to figure out why they
went wrong.  To keep the generator from exploding we also keep an
option not to use the trash, so that it is only enabled in debugger
mode.

\begin{code}
data SimpleStatus = S
  { theAgenda    :: Agenda 
  , theAuxAgenda :: AuxAgenda
  , theChart     :: Chart
  , theTrash   :: Trash
  , theResults :: [TagElem]
  , tsem       :: Sem
  , step       :: Ptype
  , gencounter :: Integer
  , genconfig  :: Params
  , genstats   :: Gstats
  }
  deriving Show
\end{code}

\paragraph{initSimpleBuilder} Creates an initial SimpleStatus.  

\begin{code}
initSimpleBuilder ::  B.Input -> Params -> SimpleStatus
initSimpleBuilder input config = 
  let cands = B.inCands input
      ts    = fst $ B.inSemInput input
      (a,i) = partition closedAux cands 
      --
      nullSemCands   = [ idname t | t <- cands, (null.tsemantics) t ] 
      unInstSemCands = [ idname t | t <- cands, not $ Set.null $ collect (tsemantics t) Set.empty ]
      nullSemErr     = "The following trees have a null semantics: " ++ (unwords nullSemCands)
      unInstSemErr   = "The following trees have an uninstantiated semantics: " ++ (unwords unInstSemCands)
      semanticsErr   = (if null nullSemCands then "" else nullSemErr ++ "\n") ++ 
                       (if null unInstSemCands then "" else unInstSemErr) 
      --
  in if (null semanticsErr || ignoreSemantics config)
        then S{ theAgenda    = i
              , theAuxAgenda = a
              , theChart     = []
              , theTrash     = []
              , theResults   = []
              , tsem     = ts
              , step     = Initial
              , gencounter = toInteger $ length cands
              , genconfig  = config
              , genstats   = B.initGstats}
        else error semanticsErr 

type MS = State SimpleStatus
\end{code}

\subsubsection{SimpleStatus updaters}

\begin{code}
addToAgenda :: TagElem -> MS ()
addToAgenda te = do 
  s <- get
  put s{theAgenda = (iaddToAgenda (theAgenda s) te)}
     
updateAgenda :: Agenda -> MS ()
updateAgenda a = do 
  s <- get  
  put s{theAgenda = a}

addToAuxAgenda :: TagElem -> MS ()
addToAuxAgenda te = do 
  s <- get
  -- each new tree gets a unique id... this makes comparisons faster 
  let counter = (gencounter s) + 1
      te2 = te { tidnum = counter }
  put s{gencounter = counter,
        theAuxAgenda = iaddToAuxAgenda (theAuxAgenda s) te2}
 
addToChart :: TagElem -> MS ()
addToChart te = do 
  s <- get  
  put s { theChart = (iaddToChart (theChart s) te) }
  incrSzchart 1

addToTrash :: TagElem -> String -> MS ()
addToTrash te err = do 
  s <- get
  let te2 = te { tdiagnostic = err:(tdiagnostic te) }
  when ((usetrash.genconfig) s) $
    put s { theTrash = (iaddtoTrash (theTrash s) te2) }

addToResults :: TagElem -> MS ()
addToResults te = do
  s <- get
  put s { theResults = te : (theResults s) }

incrGeniter = B.incrGeniter simpleBuilder
incrSzchart = B.incrSzchart simpleBuilder
incrNumcompar = B.incrNumcompar simpleBuilder
\end{code}

\subsubsection{SimpleStatus accessors}

We retrieve the SimpleStatus from the State monad and then retrieve a field from it.

\begin{code}
getAgenda :: MS Agenda
getAgenda = do 
  s <- get
  return (theAgenda s)

getChart :: MS Chart
getChart = do 
  s <- get
  return (theChart s)

-- getAuxAgenda :: MS AuxAgenda
-- getAuxAgenda = do 
--   s <- get
--   return (theAuxAgenda s)
 
getStep :: MS Ptype
getStep = do 
  s <- get
  return (step s)
\end{code}

These functions let us find out if the Agenda or the AuxAgenda are null.

\begin{code}
nullAgenda :: MS Bool
nullAgenda = do 
  s <- get  
  return (null (theAgenda s))
\end{code}

\paragraph{lookupChart} retrieves a list of trees from the chart which 
could be combined with the given agenda tree.
\label{fn:lookupChart}

The current implementation searches for trees which 
\begin{itemize}
\item do not have overlapping semantics with the given
\item are on the some of the same polarity automaton paths as the given.
\end{itemize}

\begin{code}
lookupChart :: TagElem -> MS [TagElem]
lookupChart given = do
  chart <- getChart
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
possible substitutions between it and the elements in Chart 

\begin{code}
applySubstitution :: TagElem -> MS ([TagElem])
applySubstitution te =  
  do gr <- lookupChart te
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
      t2 = ttree te2
      -- root of t1
      t1 = ttree te1
      r = root t1
      tfup   = gup r
      tfdown = gdown r
      -- FIXME: now that this behaves the same way as adjunction, 
      -- maybe we could refactor?
      (succ1, newgup,   subst1) = unifyFeat2 tfup fu
      (succ2, newgdown, subst2) = unifyFeat2 tfdown2 fd2
        where tfdown2 = replace subst1 tfdown 
              fd2     = replace subst1 fd
      subst = subst1 ++ subst2
      -- IMPORTANT: nt1 should be ready for replacement 
      -- (e.g, top features unified, type changed to Other) 
      -- when passed to repSubst
      nr  = r { gup   = newgup,
                -- note that the bot features come from sn, not r!
                gdown = newgdown,
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
      res = replace subst newTe 
  in if (isInit te1 && succ1 && succ2) then [res] else []
\end{code}

% --------------------------------------------------------------------  
\section{Adjunction}
\label{sec:adjunction}
\label{sec:ordered_adjunction}
\label{sec:foot_constraint}
% ---------------------------------------------------------------  

\paragraph{applyAdjunction} Given a TagElem, it returns the list of all 
possible adjunctions between it and the elements in Chart.  
The Chart contains Auxiliars, while TagElem is an Initial

Note: as of 13 april 2005 - only uses ordered adjunction as described in
\cite{kow04a}
\begin{code}
applyAdjunction :: TagElem -> MS ([TagElem])
applyAdjunction te = do
   gr <- lookupChart te
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
iapplyAdjNode fconstr te1 te2 an@(n, an_up, an_down) = do
  -- block repeated adjunctions of the same TagElem (for ignore semantics mode)
  guard $ not $ elem (n, (tidnum te1)) (tadjlist te2)
  -- let's go!
  let t1 = ttree te1
      t2 = ttree te2
      r = root t1
      f = foot t1
      r_up   = gup r    -- top features of the root of the auxiliar tree
      f_down = gdown f  -- bottom features of the foot of the auxiliar tree
  (anr_up',  subst1) <- unifyFeat r_up an_up 
  (anf_down, subst2) <- unifyFeat (replace subst1 f_down) (replace subst1 an_down)
  let -- don't forget to propagate the substitution set from the down stuff
      anr_up = replace subst2 anr_up' 
      -- combined substitution list and success condition
      subst   = subst1++subst2

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
      res' = replace subst nte2 
      -- 4) add the new adjunction nodes 
      --    this has to come after 3 so that we don't repeat the subst
      addextra a = if fconstr then a2 else (ncopy anf) : a2
                   where a2 = (ncopy anr) : a

  -- the final result  
  -- ----------------
  return $ res' { adjnodes = (addextra.adjnodes) res' 
                , tadjlist = (n, (tidnum te1)):(tadjlist te2) }
\end{code}

% --------------------------------------------------------------------  
\section{Generate step}
% --------------------------------------------------------------------  

\begin{itemize}
\item If both Agenda and AuxAgenda are empty then there is nothing to do,
  otherwise, if Agenda is empty then we switch to the application of the 
  Adjunction rule. 
\item After the rule is applied we classify solutions into those that are complete 
  and cover the semantics and those that don't.  The first ones are returned 
  and added to the result, while the others are sent back to Agenda.  
\item Notice that if we are applying the Substitution rule then given is added
  to Chart, otherwise it is deleted. 
\end{itemize}

\begin{code}
generateStep :: MS () 
generateStep = do
  nir     <- nullAgenda
  curStep <- getStep
  -- this check may seem redundant with generate, but it's needed 
  -- to protect against a user who calls generateStep on a finished
  -- state
  if (nir && curStep == Auxiliar) 
    then return () 
    else do incrGeniter 1
            -- this triggers exactly once in the whole process
            if nir 
               then switchToAux
               else generateStep' 

generateStep' :: MS () 
generateStep' = 
  do -- choose an item from the agenda
     given <- selectGiven
     -- have we triggered the switch to aux yet?
     curStep <- getStep
     -- do either substitution or adjunction 
     res <- if (curStep == Initial)
            then applySubstitution given
            else applyAdjunction given
     -- determine which of the res should go in the agenda 
     -- (monadic state) and which should go in the result (res')
     dispatchNew res
     -- put the given into the chart untouched 
     if (curStep == Initial) 
        then addToChart given
        else when (null $ adjnodes given) $ trashIt given 
  where 
     trashIt t = 
       do s <- get
          let missingSem = tsem s \\ tsemantics t
          addToTrash t (ts_semIncomplete missingSem)
\end{code}

\subsection{Generate helper functions}

\paragraph{selectGiven} Arbitrarily selects and removes an element from
the Initial and returns it.

\begin{code}
selectGiven :: MS TagElem
selectGiven = do 
  a <- getAgenda
  updateAgenda (tail a)
  return (head a)
\end{code}

\fnlabel{dispatchNew} Given a list of TagElem, for each tree: 
\begin{enumerate}
\item if the tree is both syntactically complete (no more subst nodes) and
      semantically complete (matches target semantics), it is a result, so
      unify the top and bottom feature structures of each node.  If that
      succeeds, return it, otherwise discard it completely.
\item if the number of subtrees exceeds the numTreesLimit, discard
\item if it is only syntactically complete and it is an auxiliary tree, 
      then we don't need to do any more substitutions with it, so set it 
      aside on the auxiliary agenda (AuxAgenda)
\item otherwise, put it on the regular agenda (Agenda)
\end{enumerate}

\begin{code}
dispatchNew :: [TagElem] -> MS () 
dispatchNew l = 
  do -- first we seperate the results from the non results
     st <- get
     let inputSem = tsem st
         synComplete x = 
           (not (aux x)) && closed x
           -- don't forget about null adjnodes
         semComplete x = inputSem == treeSem 
           where treeSem = sortSem (tsemantics x)
         isResult x = 
           synComplete x && semComplete x 
     let (res, notRes) = partition isResult l
     -- -------------------------------------------------- 
     -- the non results
     -- -------------------------------------------------- 
     -- now... throw out any trees which are over the num trees limit 
     -- (this only applies in IgnoreSemantics mode) 
     state <- get
     let numTreesLimit = (maxTrees.genconfig) state
         numTrees  x = length $ snd $ derivation x
         overLimit x = case numTreesLimit of
                         Nothing  -> False
                         Just lim -> numTrees x > lim
     let notRes2 = filter (not.overLimit) notRes
     -- put any pure auxiliary trees on the auxiliary agenda
     let dispatchAux x = 
           if closedAux x 
              then do { addToAuxAgenda x; return False }
              else return True
     notRes3 <- filterM dispatchAux notRes2
     -- put any other trees on the agenda
     mapM addToAgenda notRes3
     -- -------------------------------------------------- 
     -- the results
     -- -------------------------------------------------- 
     -- we perform top/bottom unification on any results
     let tbUnify x =
          case (tbUnifyTree x) of
            Left n  -> do let x2 = x { thighlight = [n] }
                          addToTrash x2 ts_tbUnificationFailure 
                          return False 
            Right _ -> return True 
     res2 <- filterM tbUnify res
     mapM addToResults res2
     return ()
\end{code}


\paragraph{switchToAux} When all substitutions has been done, tags with
substitution nodes still open are deleted, then the auxiliars tags are put in
Chart and the (initial) tags in the repository are moved into the Agenda. The
step is then changed to Auxiliary

\begin{code}
switchToAux :: MS ()
switchToAux = do
  st <- get
  let chart = theChart st
      -- You might be wondering why we ignore the auxiliary trees in the 
      -- chart; this is because all the syntactically complete auxiliary
      -- trees have already been filtered away by calls to classifyNew
      initialT = filter (\x -> ttype x == Initial) chart
      (compT, incompT) = partition (null.substnodes) initialT
      aux   = theAuxAgenda st
      --
      filteredT = semfilter (tsem st) aux compT 
      initial = if (semfiltered $ genconfig st) 
                then filteredT else compT 
  -- toss the syntactically incomplete stuff in the trash
  mapM (\t -> addToTrash t ts_synIncomplete) incompT
  put st{theAgenda = initial,
         theAuxAgenda = [], 
         theChart = aux,
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
                   where (_,u,_) = unifyFeat2 (gup gn) (gdown gn)
      -- 
      fixSite :: Subst -> TagSite -> TagSite 
      fixSite sb (n, u, d) = (n, u3, [])
        where u2 = replace sb u
              d2 = replace sb d
              (_,u3,_) = unifyFeat2 u2 d2
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
tbUnifyNode gnRaw (Right (pending,whole)) = 
  let -- apply pending substitutions
      gn = replace pending gnRaw 
  -- check top/bottom unification on this node
  in case unifyFeat (gup gn) (gdown gn) of
     -- stop all future iterations
     Nothing -> Left (gnname gn)
     -- apply any new substutions to the whole tree
     Just (_,sb) -> Right (pending ++ sb, replace sb whole)

-- if earlier we had a failure, don't even bother 
tbUnifyNode _ (Left n) = Left n
\end{code}

% --------------------------------------------------------------------  
\section{Unpack results}
% --------------------------------------------------------------------  

Unpacking the results consists of converting each result into a sentence 
automaton (to take care of atomic disjunction) and reading the paths of
each automaton.

\begin{code}
unpackResults :: [TagElem] ->  [B.UninflectedSentence]
unpackResults tes =
  -- sentence automaton
  let treeLeaves   = map tagLeaves tes
      sentenceAuts = map listToSentenceAut treeLeaves 
      uninflected  = concatMap automatonPaths sentenceAuts
  in uninflected
\end{code}

\subsection{Sentence automata}

\fnlabel{listToSentenceAut} converts a list of GNodes into a sentence
automaton.  It's a actually pretty stupid conversion in fact.  We pretty
much make a straight path through the automaton, with the only
cleverness being that we provide a different transition for each 
atomic disjunction.  

\begin{code}
listToSentenceAut :: [ B.UninflectedDisjunction ] -> B.SentenceAut 
listToSentenceAut nodes = 
  let theStart  = 0
      theEnd = (length nodes) - 1
      theStates = [theStart..theEnd]
      --
      emptyAut = NFA 
        { startSt     = theStart 
        , isFinalSt   = Nothing
        , finalStList = [theEnd]
        , states      = [theStates]
        , transitions = Map.empty }
      -- create a transition for each lexeme in the node to the 
      -- next state... 
      helper :: (Int, B.UninflectedDisjunction) -> B.SentenceAut -> B.SentenceAut
      helper (current, word) aut = foldr addT aut lemmas 
        where 
          lemmas   = fst word
          features = snd word
          --
          addT t a = addTrans a current (toTrans t) next 
          next = current + 1
          toTrans :: String -> Maybe UninflectedWord
          toTrans l = Just (l, features)
      --
  in foldr helper emptyAut (zip theStates nodes)
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

\fnlabel{closed} returns true if the chart item has no open substitution
nodes
\begin{code}
closed :: TagElem -> Bool
closed = null.substnodes
\end{code}

\fnlabel{aux} returns true if the chart item is an auxiliary tree
\begin{code}
aux :: TagElem -> Bool
aux t = (ttype t == Auxiliar)
\end{code}

\fnlabel{closedAux} returns true if both \fnreflite{closed} and
\fnreflite{aux} return true
\begin{code}
closedAux :: TagElem -> Bool 
closedAux x = (aux x) && (closed x)
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
the result of prepending $c_p$ + 1 to every item of $h_c$. The counter $c$ is
used (uniquely?) in the Gorn address.

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

\section{FIXME or delete me}

%\subsection{Returning results}
%
%We provide a data structure to be used by verboseGeni for returning the results
%(grDerived) along with the intermediary steps and some useful statistics.  
%
%\begin{code}
%data GeniResults a = GR {
%  -- optimisations and extra polarities
%  grOptStr   :: (String,String),
%  -- some numbers (in string form)
%  grStats     :: B.Gstats,
%  grAutPaths  :: String,
%  grAmbiguity :: String,
%  grTimeStr   :: String,
%  -- the final results
%  grDerived   :: [a],
%  grSentences :: [String]
%} 
%\end{code}
%
%We provide a default means of displaying the results
%
%\begin{code}
%instance Show (GeniResults a) where
%  show gres = 
%    let gstats = grStats gres 
%        gopts  = grOptStr gres
%        sentences = grSentences gres
%    in    "Optimisations: " ++ fst gopts ++ snd gopts ++ "\n"
%       ++ "\nAutomaton paths explored: " ++ (grAutPaths  gres)
%       ++ "\nEst. lexical ambiguity:   " ++ (grAmbiguity gres)
%       ++ "\nTotal agenda size: " ++ (show $ B.geniter gstats) 
%       ++ "\nTotal chart size:  " ++ (show $ B.szchart gstats) 
%       ++ "\nComparisons made:  " ++ (show $ B.numcompar gstats)
%       ++ "\nGeneration time:  " ++ (grTimeStr gres) ++ " ms"
%       -- ++ "\n\nRealisations:\n" ++ (showRealisations sentences)
%\end{code}
