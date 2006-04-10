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
   Agenda, AuxAgenda, Chart, SimpleStatus, SimpleState, SimpleItem(..),

   -- From SimpleStatus
   simpleBuilder,
   theAgenda, theAuxAgenda, theChart, theTrash, theResults,
   initSimpleBuilder,
   addToAgenda, addToChart,
   genconfig)
where
\end{code}


\ignore{
\begin{code}
import Control.Exception (assert)
import Control.Monad (when, guard, filterM, liftM)

import Control.Monad.State
  (get, put, modify, gets)

import Data.List (intersect, partition, delete, sort, nub)
import Data.Maybe (catMaybes)
import Data.Tree
import Data.Bits
import qualified Data.Map as Map

import Automaton ( automatonPaths, NFA(..), addTrans )
import Btypes
  ( Ptype(Initial,Auxiliar),
  , Flist
  , Replacable(..), Subst
  , sortSem
  , GType(Other), GNode(..), gCategory
  , GeniVal(GConst)
  , rootUpd, root, foot
  , repAdj, repSubst
  , renameTree
  , constrainAdj
  , unifyFeat, unifyFeat2
  )
import Builder (UninflectedWord, UninflectedSentence,
    incrCounter, num_iterations, num_comparisons, chart_size,
    SemBitMap, defineSemanticBits, semToBitVector, bitVectorToSem,
    )
import qualified Builder as B

import Tags (TagElem, TagSite, TagDerivation,
             idname, tidnum,
             ttree, ttype, tsemantics,
             adjnodes,
             substnodes,
             tagLeaves,
             ts_synIncomplete, ts_semIncomplete, ts_tbUnificationFailure,
             ts_noRootCategory, ts_wrongRootCategory,
            )
import Configuration
import General (BitVector, fst3, mapTree)
import Statistics (Statistics)
\end{code}
}

% --------------------------------------------------------------------
\section{The Builder interface}
% --------------------------------------------------------------------

Here is our implementation of Builder.

\begin{code}
simpleBuilder = B.Builder
  { B.init     = initSimpleBuilder
  , B.step     = generateStep
  , B.stepAll  = B.defaultStepAll simpleBuilder
  , B.finished = \s -> (null.theAgenda) s && (step s == Auxiliar)
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
  , theTrash   :: Trash
  , theResults :: [SimpleItem]
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

addToTrash :: SimpleItem -> String -> SimpleState ()
addToTrash te err = do
  let te2 = te { siDiagnostic = err:(siDiagnostic te) }
  modify $ \s -> s { theTrash = te2 : (theTrash s) }

addToResults :: SimpleItem -> SimpleState ()
addToResults te = do
  modify $ \s -> s { theResults = te : (theResults s) }
\end{code}

\subsection{SimpleItem}

\begin{code}
data SimpleItem = SimpleItem
 { siTagElem   :: TagElem
 , siId        :: ChartId
 --
 , siSemantics :: BitVector
 , siPolpaths  :: BitVector
 -- if there are things wrong with this item, what?
 , siDiagnostic :: [String]
 -- how was this item produced?
 , siDerivation :: TagDerivation
 -- nodes to highlight
 , siHighlight  :: [String]
 -- for generation sans semantics
 , siAdjlist :: [(String,Integer)] -- (node name, auxiliary tree id)
 } deriving Show

type ChartId = Integer

instance Replacable SimpleItem where
  replace s i = i { siTagElem = replace s (siTagElem i) }
\end{code}

\begin{code}
-- | True if the chart item has no open substitution nodes
closed :: SimpleItem -> Bool
closed = null.substnodes.siTagElem

-- | True if the chart item is an auxiliary tree
aux :: SimpleItem -> Bool
aux t = ((ttype.siTagElem) t == Auxiliar)

-- | True if both 'closed' and 'aux' are True
closedAux :: SimpleItem -> Bool
closedAux x = (aux x) && (closed x)

siSubstnodes = substnodes.siTagElem
siInitial = not.aux
\end{code}

% --------------------------------------------------------------------
\section{Initialisation}
% --------------------------------------------------------------------

\begin{code}
-- | Creates an initial SimpleStatus.
initSimpleBuilder ::  B.Input -> Params -> (SimpleStatus, Statistics)
initSimpleBuilder input config =
  let cands   = map (initSimpleItem bmap) $ B.inCands input
      (sem,_) = B.inSemInput input
      bmap    = defineSemanticBits sem
      --
      (a,i) = partition closedAux cands
      initS = S{ theAgenda    = i
               , theAuxAgenda = a
               , theChart     = []
               , theTrash     = []
               , theResults   = []
               , semBitMap = bmap
               , tsem      = semToBitVector bmap sem
               , step     = Initial
               , gencounter = toInteger $ length cands
               , genconfig  = config }
      --
  in B.unlessEmptySem input config $
     (initS, B.initStats config)


initSimpleItem :: SemBitMap -> (TagElem, BitVector) -> SimpleItem
initSimpleItem bmap (te,pp) = SimpleItem
 { siId        = tidnum te
 , siTagElem   = te
 , siSemantics = semToBitVector bmap (tsemantics te)
 , siPolpaths  = pp
 , siDiagnostic = []
 -- how was this item produced?
 , siDerivation = (0, [])
 -- nodes to highlight
 , siHighlight  = []
 -- for generation sans semantics
 , siAdjlist = []
 }
\end{code}

% --------------------------------------------------------------------
\section{Generate}
% --------------------------------------------------------------------

\paragraph{GenerateStep} is the main loop of the surface realiser.  This
is a standard chart-and-agenda mechanism, where each iteration consists
of picking an item off the agenda and combining it with elements from
the chart.  One slightly complicated detail: we maintain two seperate
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
generateStep :: SimpleState ()
generateStep = do
  nir     <- gets (null.theAgenda)
  curStep <- gets step
  -- this check may seem redundant with generate, but it's needed
  -- to protect against a user who calls generateStep on a finished
  -- state
  if (nir && curStep == Auxiliar)
    then return ()
    else do incrCounter num_iterations 1
            -- this triggers exactly once in the whole process
            if nir
               then switchToAux
               else generateStep'

generateStep' :: SimpleState ()
generateStep' =
  do -- choose an item from the agenda
     given <- selectGiven
     -- have we triggered the switch to aux yet?
     curStep <- gets step
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
        else when (null.adjnodes.siTagElem $ given) $ trashIt given
  where
     trashIt item =
       do s <- get
          let bmap = semBitMap s
              missingSem = bitVectorToSem bmap $ tsem s `xor` siSemantics item
          addToTrash item (ts_semIncomplete missingSem)

-- | Arbitrarily selects and removes an element from the agenda and
--   returns it.
selectGiven :: SimpleState SimpleItem
selectGiven = do
  a <- gets theAgenda
  updateAgenda (tail a)
  return (head a)
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
      initialT  = filter (not.aux) chart
      (compT, incompT) = partition (null.siSubstnodes) initialT
      auxAgenda = theAuxAgenda st
      --
      filteredT = semfilter (tsem st) auxAgenda compT
      initial = if (semfiltered $ genconfig st)
                then filteredT else compT
  -- toss the syntactically incomplete stuff in the trash
  mapM (\t -> addToTrash t ts_synIncomplete) incompT
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

\begin{code}
semfilter :: BitVector -> [SimpleItem] -> [SimpleItem] -> [SimpleItem]
semfilter inputsem aux initial =
  let auxsem = foldr (.|.) 0 $ map siSemantics aux
      -- lite, here, means sans auxiliary semantics
      inputsemLite      = inputsem `xor` auxsem
      siSemanticsLite x = siSemantics x .&. inputsemLite
      -- note that we can't just compare against siSemantics because
      -- that would exclude trees that have stuff in the aux semantics
      -- which would be overzealous
  in filter (\x -> siSemanticsLite x == inputsemLite) initial
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
applySubstitution item =  {-# SCC "applySubstitution" #-}
  do gr <- lookupChart item
     let -- tesem = tsemantics te
         -- we rename tags to do a proper substitution
         rItem = renameSimpleItem 'A' item
         rgr'  = map (renameSimpleItem 'B') gr
     active  <- mapM (\x -> iapplySubst rItem x (siSubstnodes     x)) rgr'
     passive <- mapM (\x -> iapplySubst x rItem (siSubstnodes rItem)) rgr'
     let res = concat $ active ++ passive
     incrCounter num_comparisons (2 * (length gr))
     return res
\end{code}

\paragraph{iapplySubst} Given two SimpleItem t1 t2 (with no overlaping names) and the list of
substitution nodes in t2 it returns ONE possible substitution (the head node)
  of the first in the second.  As all substitutions nodes should be substituted
  we force substitution in order.

\begin{code}
iapplySubst :: SimpleItem -> SimpleItem -> [(String, Flist, Flist)] -> SimpleState [SimpleItem]
iapplySubst _ _ []      = return []
iapplySubst item1 item2 (sn:_) = {-# SCC "applySubstitution" #-}
  if not (null (siSubstnodes item1))
  then return []
  else iapplySubstNode item1 item2 sn

iapplySubstNode :: SimpleItem -> SimpleItem -> (String, Flist, Flist) -> SimpleState [SimpleItem]
iapplySubstNode item1 item2 sn@(n, fu, fd) = {-# SCC "applySubstitution" #-}
  let te1 = siTagElem item1
      te2 = siTagElem item2
      --
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
      newTe = te2{ttree = ntree,
                  substnodes = (delete sn (substnodes te2))++ (substnodes te1),
                  adjnodes =   newadjnodes}
      res = replace subst $ combineSimpleItems 's' item1 $
            item2 { siTagElem   = newTe
                  , siHighlight = [gnname nr] }
  in if (siInitial item1 && succ1 && succ2)
     then do incrCounter "substitutions" 1
             return [res]
     else return []
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
applyAdjunction item = {-# SCC "applyAdjunction" #-} do
   gr <- lookupChart item
   let -- we rename tags to do a proper adjunction
       rItem = renameSimpleItem 'A' item
       rgr'  = map (renameSimpleItem 'B') gr
       --
       rte     = siTagElem rItem
       ranodes = adjnodes rte
       -- te2 is to account for the case where we simply don't do
       -- adjunction on that particular node
   res <- if null ranodes then return []
             else let tryAdj x = case iapplyAdjNode x rItem (head ranodes) of
                                  Nothing -> return Nothing
                                  Just x  -> do incrCounter "adjunctions" 1
                                                return $ Just x
                  in do attempts <- catMaybes `liftM` mapM tryAdj rgr'
                        return (sansAdjunction item : attempts)
   --
   let count   = (length gr)
   incrCounter num_comparisons count
   return res

-- | Ignore the next adjunction node
sansAdjunction :: SimpleItem -> SimpleItem
sansAdjunction item =
 let te = siTagElem item
     (ahead:atail) = assert ((not.null.adjnodes) te) $ adjnodes te
     gn    = fst3 ahead
     ntree = constrainAdj gn (ttree te)
     te2   = te { adjnodes = atail, ttree = ntree }
 in item { siTagElem = te2
         , siHighlight = [gn] }
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
iapplyAdjNode :: SimpleItem -> SimpleItem -> (String, Flist, Flist) -> Maybe SimpleItem
iapplyAdjNode item1 item2 an@(n, an_up, an_down) = do
  -- block repeated adjunctions of the same SimpleItem (for ignore semantics mode)
  guard $ not $ (n, siId item1) `elem` (siAdjlist item2)
  -- let's go!
  let te1 = siTagElem item1
      te2 = siTagElem item2
      t1 = ttree te1
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
                gaconstr = True }
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
      nte2 = te2 { ttree = ntree, adjnodes = newadjnodes' }
      res' = replace subst $ combineSimpleItems 'a' item1 $ item2
               { siTagElem = nte2
               , siHighlight = map gnname [anr, anf]
               , siAdjlist = (n, (tidnum te1)):(siAdjlist item2)
               }
      -- 4) add the new adjunction nodes
      --    this has to come after 3 so that we don't repeat the subst
      addextra a = (ncopy anr) : a
  -- the final result
  -- ----------------
  let res = res' { siTagElem = t2 }
            where t2 = t { adjnodes = (addextra.adjnodes) t }
                  t  = siTagElem res'
  return res
\end{code}

% --------------------------------------------------------------------
\subsection{Helper functions for operations}
% --------------------------------------------------------------------

\begin{code}
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
combineSimpleItems :: Char -> SimpleItem -> SimpleItem -> SimpleItem
combineSimpleItems d item1 item2 =
  item2 { siSemantics = (siSemantics item1) .|. (siSemantics item2)
        , siPolpaths  = (siPolpaths  item1) .&. (siPolpaths  item2)
        , siDerivation = addToDerivation d item1 item2
        -- only used for graphical debugging!
        , siTagElem = te2 { tsemantics = combineSem (tsemantics te1) (tsemantics te2) }
  }
  where te1 = siTagElem item1
        te2 = siTagElem item2
        combineSem s1 s2 = sortSem (s1 ++ s2)

-- | Just a wrapper to 'renameTagElem'
renameSimpleItem :: Char -> SimpleItem -> SimpleItem
renameSimpleItem c item =
 let al = map (\(n, tid) -> (c:n, tid)) (siAdjlist item)
 in item { siTagElem = renameTagElem c $ siTagElem item
         , siAdjlist = al }

-- | Given a 'Char' c and a 'TagElem' te, renames nodes in
-- substnodes, adjnodes and the tree in te by prefixing c.
renameTagElem :: Char -> TagElem -> TagElem
renameTagElem c te =
  let sn = map (\(n, fu, fd) -> (c:n, fu, fd)) (substnodes te)
      an = map (\(n, fu, fd) -> (c:n, fu, fd)) (adjnodes te)
      t = renameTree c (ttree te)
  in te{substnodes = sn,
        adjnodes = an,
        ttree = t}
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
addToDerivation :: Char -> SimpleItem -> SimpleItem -> TagDerivation
addToDerivation op tc tp =
  let (cp,hp)  = siDerivation tp
      (_ ,hc)  = siDerivation tc
      siIdname = idname.siTagElem
      --
      newcp   = cp + 1
      addcp x = (show newcp) ++ "." ++ x
      newhc   = map (\ (o,c,p) -> (o, addcp c, addcp p)) hc
      --
      newnode = (op, (addcp.siIdname) tc, siIdname tp)
  in (newcp, newnode:(hp++newhc) )
\end{code}

% --------------------------------------------------------------------
\section{Dispatching new results}
% ---------------------------------------------------------------

\fnlabel{dispatchNew} Given a list of SimpleItem, for each tree:
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
dispatchNew :: [SimpleItem] -> SimpleState ()
dispatchNew l = {-# SCC "dispatchNew" #-}
  do -- first we seperate the results from the non results
     st <- get
     let synComplete x =
           (not (aux x)) && closed x
           -- don't forget about null adjnodes
         semComplete x = (tsem st) == (siSemantics x)
         isResult x =
           synComplete x && semComplete x
     let (res, notRes) = partition isResult l
     -- --------------------------------------------------
     -- the non results
     -- --------------------------------------------------
     -- now... throw out any trees which are over the num trees limit
     -- (this only applies in IgnoreSemantics mode)
     state <- get
     let config = genconfig state
     let numTreesLimit = maxTrees config
         numTrees  x = length $ snd $ siDerivation x
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
            Left n  -> do let x2 = x { siHighlight = [n] }
                          addToTrash x2 ts_tbUnificationFailure
                          return False
            Right _ -> return True
     res2 <- filterM tbUnify res
     let rootCats = rootCatsParam config
         rootCatCheck x =
           case (gCategory.root.ttree.siTagElem) x of
           Just (GConst c) ->
             if null $ intersect c rootCats
             then do addToTrash x (ts_wrongRootCategory c rootCats)
                     return False
             else return True
           _ -> do addToTrash x ts_noRootCategory
                   return False
     res3 <- filterM rootCatCheck res2
     mapM addToResults res3
     return ()
\end{code}

% --------------------------------------------------------------------
\subsection{Top and bottom unification}
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
tbUnifyTree :: SimpleItem -> Either String SimpleItem
tbUnifyTree item = {-# SCC "tbUnifyTree" #-}
  let te = siTagElem item
      --
      tryUnification :: Tree GNode -> TbEither
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
      fixItem :: Subst -> Tree GNode -> SimpleItem
      fixItem sb tt2 = item { siTagElem =
        te { ttree      = mapTree fixNode tt2
           , adjnodes   = map (fixSite sb) (adjnodes te)
           , substnodes = map (fixSite sb) (substnodes te)}
      }
  in case (tryUnification $ (ttree.siTagElem) item) of
       Left  n        -> Left  n
       Right (sb,tt2) -> Right (fixItem sb tt2)
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
\section{Unpacking the results}
% --------------------------------------------------------------------

Unpacking the results consists of converting each result into a sentence
automaton (to take care of atomic disjunction) and reading the paths of
each automaton.

\begin{code}
unpackResults :: [SimpleItem] ->  [B.UninflectedSentence]
unpackResults tes =
  {- #SCC "unpackResults" -}
  -- sentence automaton
  let treeLeaves   = map (tagLeaves.siTagElem) tes
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
