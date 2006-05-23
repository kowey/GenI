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
import Control.Monad (when, liftM, liftM2)

import Control.Monad.State
  (get, put, modify, gets)

import Data.List (intersect, partition, delete, foldl')
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Bits
import qualified Data.Map as Map
import Data.Tree

import Statistics (Statistics)

import NLP.GenI.Automaton ( automatonPaths, NFA(..), addTrans )
import NLP.GenI.Btypes
  ( Ptype(Initial,Auxiliar),
  , Replacable(..), replace_Flist,
  , Sem,
  , GType(Other), GNode(..), gCategory, NodeName, gnnameIs,
  , GeniVal(GConst)
  , root, foot
  , plugTree, spliceTree
  , unifyFeat, Flist,
  )
import NLP.GenI.Builder (UninflectedSentence,
    incrCounter, num_iterations, num_comparisons, chart_size,
    SemBitMap, defineSemanticBits, semToBitVector, bitVectorToSem,
    DispatchFilter, (>-->), condFilter, nullFilter,
    semToIafMap, IafAble(..), IafMap, fromUniConst, getIdx,
    recalculateAccesibility, iafBadSem, ts_iafFailure,
    )
import qualified NLP.GenI.Builder as B

import NLP.GenI.Tags (TagElem, TagSite(TagSite), TagDerivation,
             idname, tidnum,
             ttree, ttype, tsemantics,
             detectSites, tagLeaf,
             ts_synIncomplete, ts_semIncomplete, ts_tbUnificationFailure,
             ts_noRootCategory, ts_wrongRootCategory,
            )
import NLP.GenI.Configuration
import NLP.GenI.General
 ( BitVector, mapMaybeM, mapTree', geniBug, treeLeaves, repList)

#ifndef DISABLE_GUI
import NLP.GenI.Btypes (sortSem)
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
type Trash = [SimpleItem]
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
 , siLeaves  :: [(String, ([String], Flist))] -- ^ actually a set
 , siDerived :: Tree String
 , siRoot    :: TagSite
 , siFoot    :: Maybe TagSite
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
 -- how was this item produced?
 , siDerivation :: TagDerivation
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
  replace s i = i { siSubstnodes = replace s (siSubstnodes i)
                  , siAdjnodes   = replace s (siAdjnodes i)
                  , siLeaves  = replace s (siLeaves i)
                  , siRoot    = replace s (siRoot i)
                  , siFoot    = replace s (siFoot i)
#ifndef DISABLE_GUI
                  , siGuiStuff = replace s (siGuiStuff i)
#endif
  }

#ifndef DISABLE_GUI
instance Replacable SimpleGuiItem where
 replace s i = i { siNodes = replace s (siNodes i) }
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
      (sem,_) = B.inSemInput input
      bmap    = defineSemanticBits sem
      --
      (a,i) = if twophase then partition closedAux cands else ([],cands)
      initS = S{ theAgenda    = i
               , theAuxAgenda = a
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
     (initS, B.initStats config)


initSimpleItem :: SemBitMap -> (TagElem, BitVector) -> SimpleItem
initSimpleItem bmap (teRaw,pp) =
 let (te,tlite) = renameNodesWithTidnum teRaw in
 case (detectSites.ttree) te of
 (snodes,anodes) -> setIaf $ SimpleItem
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
  , siLeaves  = (map getLeaf).treeLeaves $ theTree
  , siDerived = tlite
  , siRoot = ncopy.root $ theTree
  , siFoot = if ttype te == Initial then Nothing
             else Just . ncopy.foot $ theTree
  --
#ifndef DISABLE_GUI
  , siGuiStuff = initSimpleGuiItem te
#endif
  }
  where setIaf i = i { siAccesible = iafNewAcc i }
        getLeaf n = (gnname n, tagLeaf n)
        theTree = ttree te

#ifndef DISABLE_GUI
initSimpleGuiItem :: TagElem -> SimpleGuiItem
initSimpleGuiItem te = SimpleGuiItem
 { siHighlight = []
 , siNodes = flatten.ttree $ te
 , siDerivation = (0, [])
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
               sansAdjunction      given >>= dispatch
               -- determine which of the res should go in the agenda
               -- (monadic state) and which should go in the result (res')
               addToChart given
\end{code}

\subsection{Two-phase generation}

Following \cite{carroll99}, we could also separate realisation into
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
            else liftM2 (++) (sansAdjunction given) (applyAdjunction given)

     -- determine which of the res should go in the agenda
     -- (monadic state) and which should go in the result (res')
     mapM simpleDispatch res
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
        missingSem = bitVectorToSem bmap $ tsem s `xor` siSemantics item
    addToTrash item (ts_semIncomplete missingSem)
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
  let chart = theChart st
      -- You might be wondering why we ignore the auxiliary trees in the
      -- chart; this is because all the syntactically complete auxiliary
      -- trees have already been filtered away by calls to classifyNew
      initialT  = filter siInitial chart
      (compT, incompT) = partition (null.siSubstnodes) initialT
      auxAgenda = theAuxAgenda st
      --
      filteredT = semfilter (tsem st) auxAgenda compT
      initial = if (semfiltered $ genconfig st)
                then filteredT else compT
  -- toss the syntactically incomplete stuff in the trash
#ifndef DISABLE_GUI
  mapM (\t -> addToTrash t ts_synIncomplete) incompT
#endif
  put st{theAgenda = initial,
         theAuxAgenda = [],
         theChart = auxAgenda,
         step = Auxiliar}
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
semfilter :: BitVector -> [SimpleItem] -> [SimpleItem] -> [SimpleItem]
semfilter inputsem auxs initial =
  let auxsem x = foldl' (.|.) 0 [ siSemantics a | a <- auxs, siPolpaths a .&. siPolpaths x /= 0 ]
      -- lite, here, means sans auxiliary semantics
      notjunk x = (siSemantics x) .&. inputsemLite == inputsemLite
                  where inputsemLite = inputsem `xor` (auxsem x)
      -- note that we can't just compare against siSemantics because
      -- that would exclude trees that have stuff in the aux semantics
      -- which would be overzealous
  in filter notjunk initial
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
    active  <- mapM (\x -> iapplySubst item x) gr
    passive <- mapM (\x -> iapplySubst x item) gr
    let res = concat $ active ++ passive
    incrCounter num_comparisons (2 * (length gr))
    return res

applySubstitution1p :: SimpleItem -> SimpleState ([SimpleItem])
applySubstitution1p item | (not.adjdone) item = return []
applySubstitution1p item =
 do gr <- lookupChart item
    active  <- mapM (\x -> iapplySubst item x) gr
    passive <- mapM (\x -> iapplySubst x item) $ filter adjdone gr
    let res = concat $ active ++ passive
    incrCounter num_comparisons (2 * (length gr))
    return res

-- | Note: returns ONE possible substitution (the head node)
--   of the first in the second.  As all substitutions nodes should
--   be substituted we force substitution in order.
iapplySubst :: SimpleItem -> SimpleItem -> SimpleState [SimpleItem]
iapplySubst item1 item2 | siInitial item1 && closed item1 = {-# SCC "applySubstitution" #-}
 case siSubstnodes item2 of
 [] -> return []
 ((TagSite n fu fd) : stail) ->
  let doIt =
       do -- Maybe monad
          let r@(TagSite rn ru rd) = siRoot item1
          (newgup,   subst1) <- unifyFeat ru fu
          (newgdown, subst2) <- unifyFeat (replace_Flist subst1 rd)
                                          (replace_Flist subst1 fd)
          let subst = subst1 ++ subst2
              nr  = TagSite rn newgup newgdown
              adj1  = nr : (delete r $ siAdjnodes item1)
              adj2  = siAdjnodes item2
              -- gui stuff
              newRoot g = g { gup = newgup, gdown = newgdown
                            , gtype = Other }
#ifdef DISABLE_GUI
              item1g = item1
#else
              item1g = item1 { siGuiStuff = g2 }
                where g2 = g { siNodes = repList (gnnameIs rn) newRoot (siNodes g) }
                      g  = siGuiStuff item1
#endif
          return $! replace subst $ combineSimpleItems 's' [rn] item1g $
                     item2 { siSubstnodes = stail ++ (siSubstnodes item1)
                           , siAdjnodes   = adj2 ++ adj1
                           , siDerived    = plugTree (siDerived item1) n (siDerived item2)
                           , siLeaves     = (siLeaves item1) ++ (siLeaves item2)
                           }
  in case doIt of
     Nothing -> return []
     Just x  -> do incrCounter "substitutions" 1
                   return [x]
iapplySubst _ _ = return []
\end{code}

% --------------------------------------------------------------------
\subsection{Adjunction}
\label{sec:adjunction}
\label{sec:ordered_adjunction}
\label{sec:foot_constraint}
% ---------------------------------------------------------------

\paragraph{applyAdjunction} Given a SimpleItem, it returns the list of all
possible adjunctions between it and the elements in Chart.
The Chart contains Auxiliars, while SimpleItem is an Initial

Note: as of 13 april 2005 - only uses ordered adjunction as described in
\cite{kow04a}
\begin{code}
applyAdjunction :: SimpleItem -> SimpleState ([SimpleItem])
applyAdjunction item = {-# SCC "applyAdjunction" #-}
 do gr <-lookupChart item
    incrCounter num_comparisons (length gr)
    mapMaybeM (\a -> tryAdj a item) gr

passiveAdjunction1p :: SimpleItem -> SimpleState [SimpleItem]
passiveAdjunction1p item | closed item && siInitial item =
  do gr <- lookupChart item
     catMaybes `liftM` (mapM (\a -> tryAdj a item) $ filter validAux gr)
passiveAdjunction1p _ = return []

activeAdjunction1p :: SimpleItem -> SimpleState [SimpleItem]
activeAdjunction1p item | validAux item =
  do gr <- lookupChart item
     catMaybes `liftM` (mapM (\p -> tryAdj item p) $ filter (\x -> siInitial x && closed x) gr)
activeAdjunction1p _ = return []

validAux :: SimpleItem -> Bool
validAux t = closedAux t && adjdone t

tryAdj :: SimpleItem -> SimpleItem -> SimpleState (Maybe SimpleItem)
tryAdj aItem pItem =
  case iapplyAdjNode aItem pItem of
  Just x  -> do incrCounter "adjunctions" 1
                return $ Just x
  Nothing -> return Nothing

-- | Ignore the next adjunction node
sansAdjunction :: SimpleItem -> SimpleState [SimpleItem]
sansAdjunction item | closed item =
 case siAdjnodes item of
 [] -> return []
 (TagSite gn t b : atail) -> do
  -- do top/bottom unification on the node
  case unifyFeat t b of
   Nothing ->
#ifndef DISABLE_GUI
     do addToTrash (modifyGuiStuff (\g -> g { siHighlight = [gn] }) item)
                   ts_tbUnificationFailure
#endif
        return []
#ifdef DISABLE_GUI
   Just (_,s) ->
     let item_ = item
#else
   Just (tb,s) ->
     let newGuiItem g =
           g { siHighlight = [gn]
             , siNodes = repList (gnnameIs gn) fixIt (siNodes g) }
           where fixIt n = n { gup = tb, gdown = [], gaconstr = True }
         item_ = modifyGuiStuff newGuiItem item
#endif
     in return $! [replace s $! item_ { siAdjnodes = atail }]
sansAdjunction _ = return []
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
iapplyAdjNode :: SimpleItem -> SimpleItem -> Maybe SimpleItem
iapplyAdjNode item1 item2 = {-# SCC "iapplyAdjNode" #-}
 case siAdjnodes item2 of
 [] -> Nothing
 (TagSite n an_up an_down : atail) -> do
  -- block repeated adjunctions of the same SimpleItem (for ignore semantics mode)
  -- guard $ not $ (n, siId item1) `elem` (siAdjlist item2)
  -- let's go!
  let r@(TagSite r_name r_up r_down) = siRoot item1 -- auxiliary tree, eh?
  (TagSite f_name f_up f_down) <- siFoot item1 -- should really be an error if fails
  (anr_up',  subst1)  <- unifyFeat r_up an_up
  (anf_down, subst2)  <- unifyFeat (replace_Flist subst1 f_down) (replace_Flist subst1 an_down)
  let -- combined substitution list and success condition
      subst12 = subst1++subst2
      -- the result of unifying the t1 root and the t2 an
      anr = TagSite r_name (replace_Flist subst2 anr_up') r_down
  -- top and bottom unification on the former foot
  (_, subst3) <- unifyFeat (replace_Flist subst12 f_up)
                                    (replace_Flist subst2 anf_down)
  let -- the new adjunction nodes
      auxlite = delete r $ siAdjnodes item1
      newadjnodes = anr : (atail ++ auxlite)
      -- apply the substitutions
      subst = subst12 ++ subst3
      res' = replace subst $ combineSimpleItems 'a' [r_name, f_name] item1 $ item2
               { siAdjnodes = newadjnodes
               , siLeaves  = (siLeaves item1) ++ (siLeaves item2)
               , siDerived = spliceTree f_name (siDerived item1) n (siDerived item2)
               -- , siAdjlist = (n, (tidnum te1)):(siAdjlist item2)
               }
  return $! res'
\end{code}

% --------------------------------------------------------------------
\subsection{Helper functions for operations}
% --------------------------------------------------------------------

\begin{code}
ncopy :: GNode -> TagSite
ncopy x = TagSite (gnname x) (gup x) (gdown x)

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
combineSimpleItems :: Char -- ^ operation
                   -> [NodeName] -- ^ nodes to highlight
                   -> SimpleItem -> SimpleItem -> SimpleItem
combineSimpleItems d hi item1 item2 = {-# SCC "combineSimpleItems" #-}
  item2 { siSemantics = (siSemantics item1) .|. (siSemantics item2)
        , siPolpaths  = (siPolpaths  item1) .&. (siPolpaths  item2)
#ifndef DISABLE_GUI
        , siGuiStuff  = combineSimpleGuiItems d hi (siGuiStuff item1) (siGuiStuff item2)
#endif
        }

#ifndef DISABLE_GUI
combineSimpleGuiItems :: Char -> [NodeName]
                      -> SimpleGuiItem -> SimpleGuiItem -> SimpleGuiItem
combineSimpleGuiItems d hi item1 item2 =
 item2 { siFullSem = sortSem $ (siFullSem item1) ++ (siFullSem item2)
       , siDerivation = addToDerivation d item1 item2
       , siNodes = (siNodes item1) ++ (siNodes item2)
       , siDiagnostic = (siDiagnostic item1) ++ (siDiagnostic item2)
       , siHighlight = hi
       }
#endif
\end{code}

\subsubsection{Derivation trees}

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
#ifndef DISABLE_GUI
addToDerivation :: Char -> SimpleGuiItem -> SimpleGuiItem -> TagDerivation
addToDerivation op tc tp = {-# SCC "addToDerivation" #-}
  let (cp,hp)  = siDerivation tp
      (_ ,hc)  = siDerivation tc
      --
      newcp   = cp + 1
      addcp x = (show newcp) ++ "." ++ x
      newhc   = map (\ (o,c,p) -> (o, addcp c, addcp p)) hc
      --
      newnode = (op, (addcp.siIdname) tc, siIdname tp)
  in (newcp, newnode:(hp++newhc) )
#endif DISABLE_GUI
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

simpleDispatch :: SimpleDispatchFilter
simpleDispatch item =
 do inputsem <- gets tsem
    let synComplete x = siInitial x && closed x && adjdone x
        semComplete x = inputsem == siSemantics x
        isResult x = synComplete x && semComplete x
    let theFilter = condFilter isResult
                      (dpRootCatFailure >--> dpToResults)
                      (dpAux >--> dpToAgenda)
    theFilter item

-- FIXME: refactor me later!
simpleDispatch_1p :: Bool -> SimpleDispatchFilter
simpleDispatch_1p iaf item =
 do inputsem <- gets tsem
    let synComplete x = siInitial x && closed x && adjdone x
        semComplete x = inputsem == siSemantics x
        isResult x = synComplete x && semComplete x
    let maybeDpIaf = if iaf then dpIafFailure else nullFilter
        theFilter = condFilter isResult
                      (dpRootCatFailure >--> dpToResults)
                      (maybeDpIaf >--> dpToAgenda)
    theFilter item

dpAux, dpToAgenda :: SimpleDispatchFilter
dpRootCatFailure, dpToResults :: SimpleDispatchFilter
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

-- | If the item (ostensibly a result) does not have the correct root
--   category, return Nothing; otherwise return Just item
dpRootCatFailure item =
 do config <- gets genconfig
    let rootCats = rootCatsParam config
        (TagSite _ top _) = siRoot item
    case gCategory top of
     Just (GConst c) ->
      if null $! intersect c rootCats
         then dpToTrash (ts_wrongRootCategory c rootCats) item
         else return $ Just item
     _ -> dpToTrash ts_noRootCategory item
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
    concat [ getIdx up | (TagSite _ up _) <- siSubstnodes i ]

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
unpackResults :: [SimpleItem] ->  [B.UninflectedSentence]
unpackResults = concatMap unpackResult

unpackResult :: SimpleItem -> [B.UninflectedSentence]
unpackResult item =
  let leafMap :: Map.Map String B.UninflectedDisjunction
      leafMap = Map.fromList . siLeaves $ item
      lookupOrBug :: NodeName -> B.UninflectedDisjunction
      lookupOrBug k = case Map.lookup k leafMap of
                      Nothing -> geniBug $ "unpackResult : could not find node " ++ k
                      Just w  -> w
  in automatonPaths . listToSentenceAut $
     [ lookupOrBug k | k <- (treeLeaves . siDerived) item ]
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
