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
module NLP.GenI.Simple.SimpleBuilder (
   -- Types
   Agenda, AuxAgenda, Chart, SimpleStatus, SimpleState,
   SimpleItem(..),

   -- From SimpleStatus
   simpleBuilder_1p, simpleBuilder_2p, simpleBuilder,
   theAgenda, theAuxAgenda, theChart, theResults,
   initSimpleBuilder,
   addToAgenda, addToChart,
   genconfig,

#ifndef DISABLE_GUI
   SimpleGuiItem(..),
   theTrash, unpackResult,
#endif
   )
where
\end{code}


\ignore{
\begin{code}
import Control.Monad (when, liftM2)
import Control.Monad.State
  (get, put, modify, gets, runState, execStateT)

import Data.List (intersect, partition, delete, foldl')
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Bits
import qualified Data.Map as Map
import Data.Tree

import NLP.GenI.Statistics (Statistics)

import NLP.GenI.Automaton ( automatonPaths, NFA(..), addTrans )
import NLP.GenI.Btypes
  ( Ptype(Initial,Auxiliar)
  , Replacable(..), replaceOneAsMap
  , GNode(..), gCategory, NodeName
  , GeniVal(GConst)
  , root, foot
  , plugTree, spliceTree
  , unifyFeat, Flist, Subst, mergeSubst
  )
import NLP.GenI.Builder (
    incrCounter, num_iterations, num_comparisons, chart_size,
    SemBitMap, defineSemanticBits, semToBitVector, bitVectorToSem,
    DispatchFilter, (>-->), condFilter, nullFilter,
    semToIafMap, IafAble(..), IafMap, fromUniConst, getIdx,
    recalculateAccesibility, iafBadSem, ts_iafFailure,
    )
import qualified NLP.GenI.Builder as B

import NLP.GenI.Tags (TagElem, TagSite(TagSite),
             tagLeaves, tidnum,
             ttree, ttype, tsemantics,
             detectSites,
             ts_noRootCategory, ts_wrongRootCategory,
            )
import NLP.GenI.Configuration
import NLP.GenI.General
 ( BitVector, mapMaybeM, mapTree', geniBug, preTerminals, )

#ifndef DISABLE_GUI
import NLP.GenI.Btypes ( GType(Other), sortSem, Sem, gnnameIs )
import NLP.GenI.General ( repList, )
import NLP.GenI.Tags ( TagDerivation, idname,
    ts_synIncomplete, ts_semIncomplete, ts_tbUnificationFailure,
    )
#endif
\end{code}
}

% --------------------------------------------------------------------
\section{The Builder interface}
% --------------------------------------------------------------------

Here is our implementation of Builder.

\begin{code}
type SimpleBuilder = B.Builder SimpleStatus SimpleItem Params
simpleBuilder_2p, simpleBuilder_1p :: SimpleBuilder
simpleBuilder_2p = simpleBuilder True
simpleBuilder_1p = simpleBuilder False

simpleBuilder :: Bool -> SimpleBuilder
simpleBuilder twophase = B.Builder
  { B.init     = initSimpleBuilder twophase
  , B.step     = if twophase then generateStep_2p else generateStep_1p
  , B.stepAll  = B.defaultStepAll (simpleBuilder twophase)
  , B.finished = \s -> (null.theAgenda) s && (not twophase || step s == Auxiliar)
  , B.unpack   = unpackResults.theResults }
\end{code}

% --------------------------------------------------------------------
\section{Key types}
% --------------------------------------------------------------------

\begin{code}
type Agenda = [SimpleItem]
type AuxAgenda  = [SimpleItem]
type Chart  = [SimpleItem]
#ifndef DISABLE_GUI
type Trash = [SimpleItem]
#endif
\end{code}

\subsection{SimpleState and SimpleStatus}

The \fnreflite{SimpleState} is a state monad where the state being
thread through is a \fnreflite{SimpleStatus}.  The two are named
deliberately alike to indicate their close relationship.

To prevent confusion, we ought to keep a somewhat consistent naming
scheme across the builders: FooState for the state monad, FooStatus for
the state monad's ``contents'', and FooItem for the chart items
manipulated.

Note the theTrash is not actually essential to the operation of the
generator; it is for pratical debugging of grammars.  Instead of
trees dissapearing off the face of the debugger; they go into the
trash where the user can inspect them and try to figure out why they
went wrong.

\begin{code}
type SimpleState a = B.BuilderState SimpleStatus a

data SimpleStatus = S
  { theAgenda    :: Agenda
  , theAuxAgenda :: AuxAgenda
  , theChart     :: Chart
#ifndef DISABLE_GUI
  , theTrash   :: Trash
#endif
  , theResults :: [SimpleItem]
  , theIafMap  :: IafMap -- for index accessibility filtering
  , tsem       :: BitVector
  , step       :: Ptype
  , gencounter :: Integer
  , genconfig  :: Params
  -- we keep a SemBitMap strictly to help display the semantics
  , semBitMap  :: SemBitMap
  }
  deriving Show
\end{code}

\subsubsection{SimpleStatus updaters}

\begin{code}
addToAgenda :: SimpleItem -> SimpleState ()
addToAgenda te = do
  modify $ \s -> s{theAgenda = te:(theAgenda s) }

updateAgenda :: Agenda -> SimpleState ()
updateAgenda a = do
  modify $ \s -> s{theAgenda = a}

addToAuxAgenda :: SimpleItem -> SimpleState ()
addToAuxAgenda te = do
  s <- get
  -- each new tree gets a unique id... this makes comparisons faster
  let counter = (gencounter s) + 1
      te2 = te { siId = counter }
  put s{gencounter = counter,
        theAuxAgenda = te2:(theAuxAgenda s) }

addToChart :: SimpleItem -> SimpleState ()
addToChart te = do
  modify $ \s -> s { theChart = te:(theChart s) }
  incrCounter chart_size 1

#ifndef DISABLE_GUI
addToTrash :: SimpleItem -> String -> SimpleState ()
addToTrash te err = do
  let te2 = modifyGuiStuff (\g -> g { siDiagnostic = err:(siDiagnostic g) }) te
  modify $ \s -> s { theTrash = te2 : (theTrash s) }
#endif

addToResults :: SimpleItem -> SimpleState ()
addToResults te = do
  modify $ \s -> s { theResults = te : (theResults s) }
\end{code}

\subsection{SimpleItem}

\begin{code}
data SimpleItem = SimpleItem
 { siId        :: ChartId
 --
 , siSubstnodes :: ![TagSite]
 , siAdjnodes   :: ![TagSite]
 --
 , siSemantics :: !BitVector
 , siPolpaths  :: !BitVector
 -- for generation sans semantics
 -- , siAdjlist :: [(String,Integer)] -- (node name, auxiliary tree id)
 -- for index accesibility filtering (one-phase only)
 , siAccesible    :: [ String ] -- it's acc/inacc/undetermined
 , siInaccessible :: [ String ] -- that's why you want both
 --
 -- | actually: a set of pre-terminals and their leaves
 , siLeaves  :: [(String, B.UninflectedDisjunction)]
 , siDerived :: Tree String
 , siRoot    :: TagSite
 , siFoot    :: Maybe TagSite
 --
 , siPendingTb :: [ TagSite ] -- only for one-phase
 -- how was this item produced?
 , siDerivation :: TagDerivation
#ifndef DISABLE_GUI
 -- for the debugger only
 , siGuiStuff :: SimpleGuiItem
#endif
 } deriving Show

#ifndef DISABLE_GUI
-- | Things whose only use is within the graphical debugger
data SimpleGuiItem = SimpleGuiItem
 { siHighlight :: [String] -- ^ nodes to highlight
 , siNodes :: [GNode]    -- ^ actually a set
 -- if there are things wrong with this item, what?
 , siDiagnostic :: [String]
 , siFullSem :: Sem
 , siIdname  :: String
 } deriving Show

modifyGuiStuff :: (SimpleGuiItem -> SimpleGuiItem) -> SimpleItem -> SimpleItem
modifyGuiStuff fn i = i { siGuiStuff = fn . siGuiStuff $ i }
#endif

type ChartId = Integer

instance Replacable SimpleItem where
  replaceMap s i =
    i { siSubstnodes = replaceMap s (siSubstnodes i)
      , siAdjnodes   = replaceMap s (siAdjnodes i)
      , siLeaves  = replaceMap s (siLeaves i)
      , siRoot    = replaceMap s (siRoot i)
      , siFoot    = replaceMap s (siFoot i)
      , siPendingTb = replaceMap s (siPendingTb i)
#ifndef DISABLE_GUI
      , siGuiStuff = replaceMap s (siGuiStuff i)
#endif
     }
  replaceOne = replaceOneAsMap

#ifndef DISABLE_GUI
instance Replacable SimpleGuiItem where
 replaceMap s i = i { siNodes = replaceMap s (siNodes i) }
 replaceOne = replaceOneAsMap
#endif
\end{code}

\begin{code}
{-# INLINE closedAux #-}

-- | True if the chart item has no open substitution nodes
closed :: SimpleItem -> Bool
closed = null.siSubstnodes

-- | True if the chart item is an auxiliary tree
aux :: SimpleItem -> Bool
aux = isJust . siFoot

-- | True if both 'closed' and 'aux' are True
closedAux :: SimpleItem -> Bool
closedAux x = (aux x) && (closed x)

adjdone :: SimpleItem -> Bool
adjdone = null.siAdjnodes

siInitial :: SimpleItem -> Bool
siInitial =  isNothing . siFoot
\end{code}

% --------------------------------------------------------------------
\section{Initialisation}
% --------------------------------------------------------------------

\begin{code}
-- | Creates an initial SimpleStatus.
initSimpleBuilder ::  Bool -> B.Input -> Params -> (SimpleStatus, Statistics)
initSimpleBuilder twophase input config =
  let cands   = map (initSimpleItem bmap) $ B.inCands input
      (sem,_,_) = B.inSemInput input
      bmap    = defineSemanticBits sem
      -- FIXME: I don't know if this matters for one-phase
      -- because of on-the-fly tb unification (in 2p), we
      -- need an initial tb step that only addresses the
      -- nodes with null adjunction constraints
      simpleDp = if twophase then simpleDispatch_2p
                 else simpleDispatch_1p (isIaf config)
      initialDp = dpTbFailure >--> simpleDp
      --
      initS = S{ theAgenda    = []
               , theAuxAgenda = []
               , theChart     = []
#ifndef DISABLE_GUI
               , theTrash     = []
#endif
               , theResults   = []
               , semBitMap = bmap
               , tsem      = semToBitVector bmap sem
               , theIafMap = semToIafMap sem
               , step     = Initial
               , gencounter = toInteger $ length cands
               , genconfig  = config }
      --
  in B.unlessEmptySem input config $
     runState (execStateT (mapM initialDp cands) initS) (B.initStats config)


initSimpleItem :: SemBitMap -> (TagElem, BitVector) -> SimpleItem
initSimpleItem bmap (teRaw,pp) =
 let (te,tlite) = renameNodesWithTidnum teRaw in
 case detectSites (ttree te) of
 (snodes,anodes,nullAdjNodes) -> setIaf $ SimpleItem
  { siId        = tidnum te
  , siSemantics = semToBitVector bmap (tsemantics te)
  , siSubstnodes = snodes
  , siAdjnodes   = anodes
  , siPolpaths  = pp
  -- for index accesibility filtering
  , siAccesible    = [] -- see below
  , siInaccessible = []
  -- for generation sans semantics
  -- , siAdjlist = []
  , siLeaves  = tagLeaves te
  , siDerived = tlite
  , siRoot = ncopy.root $ theTree
  , siFoot = if ttype te == Initial then Nothing
             else Just . ncopy.foot $ theTree
  , siDerivation = []
  -- note: see comment in initSimpleBuilder re: tb unification
  , siPendingTb = nullAdjNodes
  --
#ifndef DISABLE_GUI
  , siGuiStuff = initSimpleGuiItem te
#endif
  }
  where setIaf i = i { siAccesible = iafNewAcc i }
        theTree = ttree te

#ifndef DISABLE_GUI
initSimpleGuiItem :: TagElem -> SimpleGuiItem
initSimpleGuiItem te = SimpleGuiItem
 { siHighlight = []
 , siNodes = flatten.ttree $ te
 , siDiagnostic = []
 , siFullSem = tsemantics te
 , siIdname = idname te }
#endif

renameNodesWithTidnum :: TagElem -> (TagElem, Tree NodeName)
renameNodesWithTidnum te =
  ( te { ttree = mapTree' renameNode theTree }
  , mapTree' newName theTree )
  where theTree = ttree te
        renameNode n = n { gnname = newName n }
        newName n = gnname n ++ "-" ++ tidstr
        tidstr = show . tidnum $ te
\end{code}

% --------------------------------------------------------------------
\section{Generate}
% --------------------------------------------------------------------

\subsection{One-phase generation}

This is a standard chart-and-agenda mechanism, where each iteration
consists of picking an item off the agenda and combining it with
elements from the chart.

\begin{code}
generateStep_1p :: SimpleState ()
generateStep_1p =
 do isDone <- gets (null.theAgenda)
    iaf <- gets (isIaf.genconfig)
    let dispatch = mapM (simpleDispatch_1p iaf)
    if isDone
       then return ()
       else do incrCounter num_iterations 1
               given <- selectGiven
               -- do both substitution and adjunction
               applySubstitution1p given >>= dispatch
               passiveAdjunction1p given >>= dispatch
               activeAdjunction1p  given >>= dispatch
               sansAdjunction1p    given >>= dispatch
               -- determine which of the res should go in the agenda
               -- (monadic state) and which should go in the result (res')
               addToChart given
\end{code}

\subsection{Two-phase generation}

Following \cite{carroll1999ecg}, we could also separate realisation into
two distinct phases.  This requires that we maintain two seperate
agendas and process them sequentially, one loop after the other.  See
\fnref{switchToAux} for details.

\begin{itemize}
\item If both Agenda and AuxAgenda are empty then there is nothing to do,
  otherwise, if Agenda is empty then we switch to the application of the
  Adjunction rule.
\item After the rule is applied we classify solutions into those that are complete
  and cover the semantics and those that don't.  The first ones are returned
  and added to the result, while the others are sent back to Agenda.
\item Notice that if we are applying the Substitution rule then the
  current agenda item is added to the chart, otherwise it is deleted.
\end{itemize}

\begin{code}
generateStep_2p :: SimpleState ()
generateStep_2p = do
  nir     <- gets (null.theAgenda)
  curStep <- gets step
  -- this check may seem redundant with generate, but it's needed
  -- to protect against a user who calls generateStep_2p on a finished
  -- state
  if (nir && curStep == Auxiliar)
    then return ()
    else do incrCounter num_iterations 1
            -- this triggers exactly once in the whole process
            if nir
               then switchToAux
               else generateStep_2p'

generateStep_2p' :: SimpleState ()
generateStep_2p' =
  do -- choose an item from the agenda
     given <- selectGiven
     -- have we triggered the switch to aux yet?
     curStep <- gets step
     -- do either substitution or adjunction
     res <- if (curStep == Initial)
            then applySubstitution given
            else liftM2 (++) (sansAdjunction2p given) (applyAdjunction2p given)

     -- determine which of the res should go in the agenda
     -- (monadic state) and which should go in the result (res')
     mapM simpleDispatch_2p res
     -- put the given into the chart untouched
     if (curStep == Initial)
        then addToChart given
        else when (adjdone given) $ trashIt given
\end{code}

\subsection{Helpers for the generateSteps}

\begin{code}
trashIt :: SimpleItem -> SimpleState ()
#ifdef DISABLE_GUI
trashIt _ = return ()
#else
trashIt item =
 do s <- get
    let bmap = semBitMap s
        itemSem = siSemantics item
        inputSem = tsem s
        reason = if inputSem == itemSem
                    then "unknown reason!"
                    else ts_semIncomplete $ bitVectorToSem bmap $ inputSem `xor` itemSem
    addToTrash item reason
#endif

-- | Arbitrarily selects and removes an element from the agenda and
--   returns it.
selectGiven :: SimpleState SimpleItem
selectGiven = do
  agenda <- gets theAgenda
  case agenda of
   []        -> geniBug "null agenda in selectGiven"
   (a:atail) -> updateAgenda atail >> return a
\end{code}

\subsection{Switching phases}

\fnlabel{switchToAux} When all substitutions has been done, tags with
substitution nodes still open are deleted, then the auxiliars tags are put in
Chart and the (initial) tags in the repository are moved into the Agenda. The
step is then changed to Auxiliary

\begin{code}
switchToAux :: SimpleState ()
switchToAux = do
  st <- get
  let chart  = theChart st
      config = genconfig st
      -- You might be wondering why we ignore the auxiliary trees in the
      -- chart; this is because all the syntactically complete auxiliary
      -- trees have already been filtered away by calls to classifyNew
      initialT  = filter siInitial chart
      res1@(compT1, incompT1) =
         partition (null.siSubstnodes) initialT
      --
      auxAgenda = theAuxAgenda st
      (compT2, incompT2) =
        if semfiltered config
        then semfilter (tsem st) auxAgenda compT1
        else res1
      --
      compT = compT2
  put st{ theAgenda = []
        , theAuxAgenda = []
        , theChart = auxAgenda
        , step = Auxiliar}
  -- the root cat filter by Claire
  let switchFilter =
        if rootcatfiltered config
        then dpRootCatFailure2 >--> dpToAgenda
        else dpToAgenda
  mapM switchFilter compT
  -- toss the syntactically incomplete stuff in the trash
#ifndef DISABLE_GUI
  mapM (\t -> addToTrash t ts_synIncomplete) incompT1
  mapM (\t -> addToTrash t "sem-filtered") incompT2
#endif
  return ()
\end{code}

\subsubsection{SemFilter Optimisation}
\label{sec:semfilter}

The purpose of the semantic filter optimisation is to take full
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

FIXME: comment 2006-04-18: sem filter each polarity path separately (this is
more aggressive; it gives us much more filtering)

\begin{code}
semfilter :: BitVector -> [SimpleItem] -> [SimpleItem] -> ([SimpleItem], [SimpleItem])
semfilter inputsem auxs initial =
  let auxsem x = foldl' (.|.) 0 [ siSemantics a | a <- auxs, siPolpaths a .&. siPolpaths x /= 0 ]
      -- lite, here, means sans auxiliary semantics
      notjunk x = (siSemantics x) .&. inputsemLite == inputsemLite
                  where inputsemLite = inputsem `xor` (auxsem x)
      -- note that we can't just compare against siSemantics because
      -- that would exclude trees that have stuff in the aux semantics
      -- which would be overzealous
  in partition notjunk initial
\end{code}

% --------------------------------------------------------------------
\section{Operations}
% --------------------------------------------------------------------

We implement the two TAG operations, substitution and adjunction, below.
These are the only two operations we have, because we're working with a
very simple builder that constructs derived trees.

% --------------------------------------------------------------------
\subsection{Substitution}
\label{sec:substitution}
% --------------------------------------------------------------------

\paragraph{applySubstitution} Given a SimpleItem it returns the list of all
possible substitutions between it and the elements in Chart

\begin{code}
applySubstitution :: SimpleItem -> SimpleState ([SimpleItem])
applySubstitution item =
 do gr <- lookupChart item
    active  <- mapM (\x -> iapplySubst True item x) gr
    passive <- mapM (\x -> iapplySubst True x item) gr
    let res = concat $ active ++ passive
    incrCounter num_comparisons (2 * (length gr))
    return res

applySubstitution1p :: SimpleItem -> SimpleState ([SimpleItem])
applySubstitution1p item =
 do gr <- lookupChart item
    active  <- if adjdone item then return []
               else mapM (\x -> iapplySubst False item x) gr
    passive <- mapM (\x -> iapplySubst False x item) $ filter adjdone gr
    let res = concat $ active ++ passive
    incrCounter num_comparisons (2 * (length gr))
    return res

-- | Note: returns ONE possible substitution (the head node)
--   of the first in the second.  As all substitutions nodes should
--   be substituted we force substitution in order.
iapplySubst :: Bool -> SimpleItem -> SimpleItem -> SimpleState [SimpleItem]
iapplySubst twophase item1 item2 | siInitial item1 && closed item1 = {-# SCC "applySubstitution" #-}
 case siSubstnodes item2 of
 [] -> return []
 ((TagSite n fu fd nOrigin) : stail) ->
  let doIt =
       do -- Maybe monad
          let r@(TagSite rn ru rd rOrigin) = siRoot item1
          (newU, subst1) <- unifyFeat ru fu
          (newD, subst2) <- unifyFeat (replace subst1 rd)
                                      (replace subst1 fd)
          let subst = mergeSubst subst1 subst2
              nr    = TagSite rn newU newD rOrigin
              adj1  = nr : (delete r $ siAdjnodes item1)
              adj2  = siAdjnodes item2
#ifdef DISABLE_GUI
              item1g = item1
#else
              item1g = item1 { siGuiStuff = g2 }
                where g2 = g { siNodes = repList (gnnameIs rn) newRoot (siNodes g) }
                      g  = siGuiStuff item1
              -- gui stuff
              newRoot g = g { gup = newU, gdown = newD, gtype = Other }
#endif
          let pending = if twophase then []
                        else nr : ((siPendingTb item1) ++ (siPendingTb item2))
          return $! replace subst $ combineSimpleItems [rn] item1g $
                     item2 { siSubstnodes = stail ++ (siSubstnodes item1)
                           , siAdjnodes   = adj2 ++ adj1
                           , siDerived    = plugTree (siDerived item1) n (siDerived item2)
                           , siDerivation = addToDerivation 's' (item1g,rOrigin) (item2,nOrigin)
                           , siLeaves     = (siLeaves item1) ++ (siLeaves item2)
                           , siPendingTb  = pending
                           }
  in case doIt of
     Nothing -> return []
     Just x  -> do incrCounter "substitutions" 1
                   return [x]
iapplySubst _ _ _ = return []
\end{code}

% --------------------------------------------------------------------
\subsection{Adjunction}
\label{sec:adjunction}
\label{sec:ordered_adjunction}
\label{sec:foot_constraint}
% ---------------------------------------------------------------

\paragraph{applyAdjunction2p} Given a SimpleItem, it returns the list of all
possible adjunctions between it and the elements in Chart.
The Chart contains Auxiliars, while SimpleItem is an Initial

Note: as of 13 april 2005 - only uses ordered adjunction as described in
\cite{kow04a}
\begin{code}
applyAdjunction2p :: SimpleItem -> SimpleState ([SimpleItem])
applyAdjunction2p item = {-# SCC "applyAdjunction2p" #-}
 do gr <-lookupChart item
    incrCounter num_comparisons (length gr)
    mapMaybeM (\a -> tryAdj True a item) gr

passiveAdjunction1p :: SimpleItem -> SimpleState [SimpleItem]
passiveAdjunction1p item | closed item && siInitial item =
  do gr <- lookupChart item
     mapMaybeM (\a -> tryAdj False a item) $ filter validAux gr
passiveAdjunction1p _ = return []

activeAdjunction1p :: SimpleItem -> SimpleState [SimpleItem]
activeAdjunction1p item | validAux item =
  do gr <- lookupChart item
     mapMaybeM (\p -> tryAdj False item p) $ filter (\x -> siInitial x && closed x) gr
activeAdjunction1p _ = return []

validAux :: SimpleItem -> Bool
validAux t = closedAux t && adjdone t

tryAdj :: Bool -> SimpleItem -> SimpleItem -> SimpleState (Maybe SimpleItem)
tryAdj twophase aItem pItem =
 do case iapplyAdjNode twophase aItem pItem of
     Just x  -> do incrCounter "adjunctions" 1
                   return $ Just x
     Nothing -> return Nothing
\end{code}

Note that in the one-phase variant of non-adjunction, we can't do top/bot
unification on the fly, because afaik we can't tell that a node will never
be revisited.  One example of this is if you try to adjoin into the root

\begin{code}
-- | Ignore the next adjunction node
sansAdjunction1p, sansAdjunction2p :: SimpleItem -> SimpleState [SimpleItem]
sansAdjunction1p item | closed item =
 case siAdjnodes item of
 [] -> return []
 (ahead : atail) ->
   return $ [item { siAdjnodes = atail
                  , siPendingTb = ahead : (siPendingTb item) } ]
sansAdjunction1p _ = return []

-- | Ignore the next adjunction node
sansAdjunction2p item | closed item =
 case siAdjnodes item of
 [] -> return []
 (TagSite gn t b o: atail) -> do
  -- do top/bottom unification on the node
  case unifyFeat t b of
   Nothing ->
#ifndef DISABLE_GUI
     do addToTrash (modifyGuiStuff (\g -> g { siHighlight = [gn] }) item)
                   ts_tbUnificationFailure
#endif
        return []
   Just (tb,s) ->
     let item1 = if isRootOf item gn
                 then item { siRoot = TagSite gn tb [] o }
                 else item
#ifdef DISABLE_GUI
         item2 = item1
#else
         item2 = modifyGuiStuff (constrainAdj gn tb) item1
#endif
     in return $! [replace s $! item2 { siAdjnodes = atail }]
sansAdjunction2p _ = return []
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
with \texttt{anr}.

\begin{code}
iapplyAdjNode :: Bool -> SimpleItem -> SimpleItem -> Maybe SimpleItem
iapplyAdjNode twophase aItem pItem = {-# SCC "iapplyAdjNode" #-}
 case siAdjnodes pItem of
 [] -> Nothing
 (TagSite an_name an_up an_down nOrigin : atail) -> do
  -- block repeated adjunctions of the same SimpleItem (for ignore semantics mode)
  -- guard $ not $ (an_name, siId aItem) `elem` (siAdjlist pItem)
  -- let's go!
  let r@(TagSite r_name r_up r_down rOrigin) = siRoot aItem -- auxiliary tree, eh?
  (TagSite f_name f_up f_down _) <- siFoot aItem -- should really be an error if fails
  (anr_up',  subst1)  <- unifyFeat r_up an_up
  (anf_down, subst2)  <- unifyFeat (replace subst1 f_down) (replace subst1 an_down)
  let -- combined substitution list and success condition
      subst12 = mergeSubst subst1 subst2
      -- the result of unifying the t1 root and the t2 an
      anr = TagSite r_name (replace subst2 anr_up') r_down rOrigin
  let anf_up = replace subst2 f_up
      -- the new adjunction nodes
      auxlite = delete r $ siAdjnodes aItem
      newadjnodes = anr : (atail ++ auxlite)
      --
      rawCombined =
        combineSimpleItems [r_name, f_name] aItem $ pItem
               { siAdjnodes = newadjnodes
               , siLeaves  = siLeaves aItem ++ siLeaves pItem
               , siDerived = spliceTree f_name (siDerived aItem) an_name (siDerived pItem)
               , siDerivation = addToDerivation 'a' (aItem,rOrigin) (pItem,nOrigin)
               -- , siAdjlist = (n, (tidnum te1)):(siAdjlist item2)
               -- if we adjoin into the root, the new root is that of the aux
               -- tree (affects 1p only)
               , siRoot = if isRootOf pItem an_name then r else siRoot pItem
               , siPendingTb =
                  if twophase then []
                  else (TagSite an_name anf_up anf_down nOrigin) : (siPendingTb pItem) ++ (siPendingTb aItem)
               }
      -- one phase = postpone tb unification
      -- two phase = do tb unification on the fly
      finalRes1p = return $ replace subst12 rawCombined
      finalRes2p =
       do -- tb on the former foot
          tbRes <- unifyFeat anf_up anf_down
#ifdef DISABLE_GUI
          let (_, subst3) = tbRes
              myRes = res'
#else
          let (anf_tb, subst3) = tbRes
              myRes = modifyGuiStuff (constrainAdj an_name anf_tb) res'
#endif
          -- apply the substitutions
              res' = replace (mergeSubst subst12 subst3) rawCombined
          return myRes
  -- ---------------
  if twophase then finalRes2p else finalRes1p
\end{code}

% --------------------------------------------------------------------
\subsection{Helper functions for operations}
% --------------------------------------------------------------------

\begin{code}
ncopy :: GNode -> TagSite
ncopy x = TagSite (gnname x) (gup x) (gdown x) (gorigin x)

isRootOf :: SimpleItem -> String -> Bool
isRootOf item n = n == rname
  where (TagSite rname _ _ _) = siRoot item

-- | Retrieves a list of trees from the chart which could be combined with the given agenda tree.
-- The current implementation searches for trees which
--  * do not have overlapping semantics with the given
--  * are on the some of the same polarity automaton paths as the
--    current agenda item
lookupChart :: SimpleItem -> SimpleState [SimpleItem]
lookupChart given = do
  chart <- gets theChart
  let gpaths = siPolpaths given
      gsem   = siSemantics given
  return [ i | i <- chart
             -- should be on the same polarity path (chart sharing)
             , (siPolpaths i) .&. gpaths /= 0
             -- semantics should not be overlapping
             && (siSemantics i .&. gsem ) == 0
         ]

-- | Helper function for when chart operations succeed.
combineSimpleItems :: [NodeName] -- ^ nodes to highlight
                   -> SimpleItem -> SimpleItem -> SimpleItem
combineSimpleItems hi item1 item2 = {-# SCC "combineSimpleItems" #-}
  item2 { siSemantics = (siSemantics item1) .|. (siSemantics item2)
        , siPolpaths  = (siPolpaths  item1) .&. (siPolpaths  item2)
#ifndef DISABLE_GUI
        , siGuiStuff  = combineSimpleGuiItems hi (siGuiStuff item1) (siGuiStuff item2)
#endif
        }

#ifndef DISABLE_GUI
combineSimpleGuiItems :: [NodeName]
                      -> SimpleGuiItem -> SimpleGuiItem -> SimpleGuiItem
combineSimpleGuiItems hi item1 item2 =
 item2 { siFullSem = sortSem $ (siFullSem item1) ++ (siFullSem item2)
       , siNodes = (siNodes item1) ++ (siNodes item2)
       , siDiagnostic = (siDiagnostic item1) ++ (siDiagnostic item2)
       , siHighlight = hi
       }

constrainAdj :: String -> Flist -> SimpleGuiItem -> SimpleGuiItem
constrainAdj gn newT g =
  g { siNodes = repList (gnnameIs gn) fixIt (siNodes g) }
  where fixIt n = n { gup = newT, gdown = [], gaconstr = True }
#endif
\end{code}

\subsubsection{Derivation trees}

We make the simplifying assumption that each chart item is only used once.
This is clearly wrong if we allow for items with an empty semantics, but
since we do not actually allow such a thing, we're ok.

\begin{code}
addToDerivation :: Char
                -> (SimpleItem,String)
                -> (SimpleItem,String)
                -> TagDerivation
addToDerivation op (tc,tcOrigin) (tp,tpOrigin) =
  let hp = siDerivation tp
      hc = siDerivation tc
      newnode = (op, tcOrigin, tpOrigin)
  in newnode:hp++hc
\end{code}



% --------------------------------------------------------------------
\section{Dispatching new results}
% --------------------------------------------------------------------

Dispatching is the process where new chart items are assigned to one of
the trash, agenda, auxiliary agenda or chart.  The item could be
modified during dispatch-time; for example, we might do top/bottom
unification on it.  See \ref{sec:dispatching} for more details.

\begin{code}
type SimpleDispatchFilter = DispatchFilter SimpleState SimpleItem

simpleDispatch_2p :: SimpleDispatchFilter
simpleDispatch_2p =
 simpleDispatch (dpRootCatFailure >--> dpToResults)
                (dpAux >--> dpToAgenda)

simpleDispatch_1p :: Bool -> SimpleDispatchFilter
simpleDispatch_1p iaf =
 simpleDispatch (dpRootCatFailure >--> dpTbFailure >--> dpToResults)
                (maybeDpIaf >--> dpToAgenda)
 where maybeDpIaf = if iaf then dpIafFailure else nullFilter

simpleDispatch :: SimpleDispatchFilter -> SimpleDispatchFilter -> SimpleDispatchFilter
simpleDispatch resFilter nonResFilter item =
 do inputsem <- gets tsem
    let synComplete x = siInitial x && closed x && adjdone x
        semComplete x = inputsem == siSemantics x
        isResult x = synComplete x && semComplete x
    condFilter isResult resFilter nonResFilter item

dpAux, dpToAgenda :: SimpleDispatchFilter
dpTbFailure, dpRootCatFailure, dpRootCatFailure2, dpToResults :: SimpleDispatchFilter
dpToTrash :: String -> SimpleDispatchFilter

dpToAgenda x  = addToAgenda x  >> return Nothing
dpToResults x = addToResults x >> return Nothing
#ifdef DISABLE_GUI
dpToTrash _ _ = return Nothing
#else
dpToTrash m x = addToTrash x m >> return Nothing
#endif

dpAux item =
  if closedAux item
  then addToAuxAgenda item >> return Nothing
  else return $ Just item

{-
-- | Dispatches to the trash and returns Nothing if there is a tree
--   size limit in effect and the item is over that limit.  The
--   tree size limit is used in 'IgnoreSemantics' mode.
dpTreeLimit item =
 do config <- gets genconfig
    case maxTrees config of
     Nothing  -> return $ Just item
     Just lim -> if (length.snd.siDerivation) item > lim
                 then do addToTrash item (ts_overnumTrees lim)
                         return Nothing
                 else return $ Just item
   where ts_overnumTrees l = "Over derivation size of " ++ (show l)
-}

-- | This is only used for the one-phase algorithm
dpTbFailure item =
 return $ if tbUnifyTree item then Just item else Nothing

-- | If the item (ostensibly a result) does not have the correct root
--   category, return Nothing; otherwise return Just item
dpRootCatFailure  = dpRootCatFailure_ False
dpRootCatFailure2 = dpRootCatFailure_ True

dpRootCatFailure_ :: Bool -> SimpleDispatchFilter
dpRootCatFailure_ count item =
 do config <- gets genconfig
    let rootCats = fromMaybe [] $ getFlagP RootCategoriesFlg config
        (TagSite _ top _ _) = siRoot item
    case gCategory top of
     Just (GConst c) ->
      if null $! intersect c rootCats
         then do when count $ incrCounter "root_cat_discards" 1
                 dpToTrash (ts_wrongRootCategory c rootCats) item
         else return $ Just item
     _ -> do dpToTrash ts_noRootCategory item
\end{code}
% --------------------------------------------------------------------
\subsection{Top and bottom unification}
% --------------------------------------------------------------------

\paragraph{tbUnifyTree} unifies the top and bottom feature structures
of each node on each tree.  Note: this only determines if it is
possible to do so.  Actually returning the results is possible
and even easy
(you'll have to look back into the darcs repository and unpull the
 patch from 2006-05-21T15:40:51 ``Remove top/bot unification standalone
 code.'')
but since it is only used in the one-phase algorithm and for the
graphical interface, I decided not to bother.

\begin{code}
type TbEither = Either String Subst
tbUnifyTree :: SimpleItem -> Bool
tbUnifyTree item = {-# SCC "tbUnifyTree" #-}
  case foldl tbUnifyNode (Right Map.empty) (siPendingTb item) of
    Left  _ -> False
    Right _ -> True
\end{code}

Our helper function corresponds to the first unification step.  It is
meant to be called from a fold.  The node argument represents the
current node being explored.  The Either argument holds a list of
pending substitutions and a copy of the entire tree.

There are two things going on in here:

\begin{enumerate}
\item check if unification is possible - first we apply the pending
      substitutions on the node and then we check if unification
      of the top and bottom feature structures of that node
      succeeds
\item keep track of the substitutions that need to be performed -
      any new substitutions that result from unification are
      added to the pending list
\end{enumerate}

Note that we wrap the second argument in a Maybe; this is used to
indicate that if unification suceeds or fails.  We also use it to
prevent the function from doing any work if a unification failure
from a previous call has already occured.

Getting this right was a big pain in the butt, so don't go trying to
simplify this over-complicated code unless you know what you're doing.

\begin{code}
tbUnifyNode :: TbEither -> TagSite -> TbEither
tbUnifyNode (Right pending) rawSite =
  -- apply pending substitutions
  case replace pending rawSite of
  (TagSite name up down _) ->
    -- check top/bottom unification on this node
    case unifyFeat up down of
    -- stop all future iterations
    Nothing -> Left name
    -- apply any new substutions to the whole tree
    Just (_,sb) -> Right (mergeSubst pending sb)

-- if earlier we had a failure, don't even bother
tbUnifyNode (Left n) _ = Left n
\end{code}

% --------------------------------------------------------------------
\subsection{Index accesibility filtering}
\label{sec:simple:iaf}
% --------------------------------------------------------------------

Note that index accesibility filtering only makes sense for the one-phase
algorithm.  See also \ref{sec:iaf} for more details about what this is.

\begin{code}
instance IafAble SimpleItem where
  iafAcc   = siAccesible
  iafInacc = siInaccessible
  iafSetAcc   a i = i { siAccesible = a }
  iafSetInacc a i = i { siInaccessible = a }
  iafNewAcc i =
    concatMap fromUniConst $
    concat [ getIdx up | (TagSite _ up _ _) <- siSubstnodes i ]

dpIafFailure :: SimpleDispatchFilter
dpIafFailure item | aux item = return $ Just item
dpIafFailure itemRaw =
 do s <- get
    let bmap = semBitMap s
        item = recalculateAccesibility itemRaw
        badSem = iafBadSem (theIafMap s) bmap (tsem s) siSemantics item
        inAcc = iafInacc item
    if badSem == 0
      then -- can't dispatch, but that's good!
           -- (note that we return the item with its iaf criteria updated)
           return $ Just item
      else dpToTrash (ts_iafFailure inAcc $ bitVectorToSem bmap badSem) item
\end{code}


% --------------------------------------------------------------------
\section{Unpacking the results}
% --------------------------------------------------------------------

Unpacking the results consists of converting each result into a sentence
automaton (to take care of atomic disjunction) and reading the paths of
each automaton.

\begin{code}
unpackResults :: [SimpleItem] ->  [B.Output]
unpackResults = concatMap unpackResult

unpackResult :: SimpleItem -> [B.Output]
unpackResult item =
  let leafMap :: Map.Map String B.UninflectedDisjunction
      leafMap = Map.fromList . siLeaves $ item
      lookupOrBug :: NodeName -> B.UninflectedDisjunction
      lookupOrBug k = case Map.lookup k leafMap of
                      Nothing -> geniBug $ "unpackResult : could not find node " ++ k
                      Just w  -> w
      derivation = siDerivation item
      paths = automatonPaths . listToSentenceAut $
              [ lookupOrBug k | (k,_) <- (preTerminals . siDerived) item ]
 in zip paths (repeat derivation)
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
      helper (current, (lemmas, features)) aut =
        foldl' addT aut lemmas
        where
          addT a t = addTrans a current (Just (t, features)) next
          next = current + 1
      --
  in foldr helper emptyAut (zip theStates nodes)
\end{code}
