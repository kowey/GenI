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

\chapter{Derivations builder}
\label{cha:DerivationsBuilder}

GenI current has two backends, Mstate (chapter \ref{cha:Mstate}) and 
this module.  This backend does not attempt to build derived trees
until the very end.  Instead, we build a derivation tree which can
then be read to extract the sentences.  We do, however, use an agenda
and chart control mechanism like Mstate, just with different items.

\begin{code}
module DerivationsBuilder
where
\end{code}

\ignore{
\begin{code}
import Control.Monad 
  (guard, filterM)

import Control.Monad.State 
  (State, get, put)
import Data.List (intersect, partition, (\\))
import Data.Maybe (isJust)
import Data.Bits

import Btypes 
  (Ptype(Initial,Auxiliar),
   Flist, 
   Replacable(..),
   Sem, sortSem, Subst,
   GType(Other), GNode(..),
   rootUpd,
   repAdj,
   renameTree,
   repSubst,
   constrainAdj, 
   root, foot, 
   unifyFeat)

import qualified Builder as B
import Tags 
  (TagElem, TagSite, TagDerivation,  TagStatus(..),
   idname, tidnum,
   derivation,
   ttree, ttype, tsemantics, thighlight, tdiagnostic,
   tpolpaths,
   adjnodes, substnodes, tadjlist)
import Configuration (Params, semfiltered, footconstr,
                      usetrash, maxTrees)
import General (BitVector, fst3, snd3, thd3, mapTree)
\end{code}
}

% --------------------------------------------------------------------  
\section{Key types}
% --------------------------------------------------------------------  

\subsection{BuilderState}

Note the theTrash is not actually essential to the operation of the
generator; it is for pratical debugging of grammars.  Instead of
trees dissapearing off the face of the debugger; they go into the
trash where the user can inspect them and try to figure out why they
went wrong.  To keep the generator from exploding we also keep an
option not to use the trash, so that it is only enabled in debugger
mode.

\begin{code}
data BuilderStatus = S
    { theAgenda    :: Agenda
    , theAuxAgenda :: AuxAgenda
    , theChart     :: Chart
    , theTrash   :: Trash
    , tsem       :: Sem
    , theStep    :: Ptype
    , gencounter :: Integer
    , genconfig  :: Params
    , genstats   :: B.Gstats } 
  deriving Show

type BState = State BuilderStatus 

derivationsBuilder = B.Builder 
  { B.init = initBuilder
  , B.step = generateStep
  , B.stepAll = B.run
  , B.finished = finished -- should add check that step is aux
  , B.stats    = genstats 
  , B.setStats = \t s -> s { genstats = t }
  }

type Agenda = [ChartItem]
type AuxAgenda  = [ChartItem]
type Chart  = [ChartItem] 
type Trash = [ChartItem]
\end{code}

\end{code}

\paragraph{initBuilderState} Creates an initial Builder.  

\begin{code}
initBuilder :: SemInput -> [TagElem] -> Params -> BuilderStatus 
initBuilder (ts,_) cands config = 
  let items = zipWith toChartItem [0..] cands
      (a,i) = partition closedAux items
  in S { theAgenda  = i
       , theAuxAgenda = a
       , theChart = []
       , theTrash = []
       , tsem     = ts
       , theStep  = Initial
       , gencounter = toInteger (length cands)
       , genconfig  = config
       , genstats   = B.initGstats}
\end{code}

\subsubsection{BuilderState updaters}

\begin{code}
addToAgenda :: ChartItem -> BState ()
addToAgenda te = do 
  s <- get
  put s{ theAgenda = te : (theAgenda s) }
     
updateAgenda :: Agenda -> BState ()
updateAgenda a = do 
  s <- get  
  put s{ theAgenda = a }

addToAuxAgenda :: ChartItem -> BState ()
addToAuxAgenda te = do 
  s <- get
  put s{ theAuxAgenda = te : (theAuxAgenda s) }
 
addToChart :: ChartItem -> BState ()
addToChart te = do 
  s <- get  
  put s { theChart = te : (theChart s) }
  incrSzchart 1

addToTrash :: ChartItem -> TagStatus -> BState ()
addToTrash te err = do 
  return ()
{-
  s <- get
  let te2 = te { tdiagnostic = err }
  when ((usetrash.genconfig) s) $
    put s { theTrash = te2 : (theTrash s) }
  -}

incrSzchart = B.incrSzchart derivationsBuilder
incrGeniter = B.incrGeniter derivationsBuilder
incrNumcompar = B.incrNumcompar derivationsBuilder
\end{code}

\subsection{Chart items}

A chart item has two parts.

The things which define an equivalence class are a tuple 
$\tuple{root, foot, snodes, paths, semantics}$ where
\begin{enumerate}
\item $root$ - is the root node associated with the chart 
\item $foot$ - is the foot node of the item - if this is not being used
               during the adjunction phase, it will be ignored
\item $snodes$ - open substitution nodes
\item $paths$  - the polarity automaton paths that the item
                 belongs to
\item $semantics$ - the semantics associated with this item
\end{enumerate}

Additionally, a chart item has an

\begin{enumerate}
\item $id$   - a unique identifier for this item
\item $operations$ - a list of operations that may be used to
      to construct this item.  Note that this list is a disjunction,
      not a sequence.  It is meant to be interpreted as ``either do Foo or
      Bar'', not ``do Foo, then Bar''.
\end{enumerate}

\begin{code}
data ChartItem = ChartItem 
  { ciRootNode   :: TagSite 
  , ciFootNode   :: Maybe TagSite 
  , ciSubstnodes :: [TagSite]
  --
  , ciPolpaths   :: BitVector
  , ciSemantics  :: Sem
  --
  , ciId         :: ChartId 
  , ciOperations :: [ChartOperation] 
  } deriving Show

type ChartId = Integer
\end{code}

A chart item may come from  
\begin{enumerate}
\item some elementary tree (these items are "atomic") 
\item a substitution between two items: the first item is the one
      being substituted; the second one is the one receiving 
      substitution
\item an adjunction between two items: the first one is an auxiliary
      tree; the second is some node in an aux tree
\end{enumerate}

\begin{code}
data ChartOperation = 
    ElemTree String
  | Subst ChartId (ChartId, String)
  | Adj   ChartId (ChartId, String)
  deriving (Show)
\end{code}

% --------------------------------------------------------------------  
\section{Generate}
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
generateStep :: BState [ChartItem]
generateStep = 
 do st  <- get
    nir <- nullAgenda
    -- this check may seem redundant with generate, but it's needed 
    -- to protect against a user who calls generateStep on a finished
    -- state
    if finished st then return [] else generateStep2 

generateStep2 :: BState [ChartItem] 
generateStep2 = 
  do incrGeniter 1
     -- choose an item from the agenda
     given <- selectGiven
     -- have we triggered the switch to aux yet?
     -- curStep <- getStep
     -- make sure the item is not equivalent to something in chart 
     unique <- checkIfUnique given 
     -- if the given already exists, do nothing
     -- otherwise, perform either substitution or adjunction
     let performOperation 
           | not unique         = return []
           | otherwise          = applySubstitution given
{-
           | curStep == Initial = applySubstitution given
           | otherwise          = return []
-}
     -- put any new results where they belong
     -- (agenda, aux agenda, results list, etc) 
     res <- performOperation >>= dispatchNew 
     -- put the given into the chart untouched 
     addToChart given
{-
     if (curStep == Initial) 
        then addToChart given
        else when (null $ adjnodes given) $ trashIt given 
          -}
     return res
  where 
     trashIt t = 
       return ()
{-
       do s <- get
          let missingSem = tsem s \\ ciSemantics t
          addToTrash t (TS_SemIncomplete missingSem)
-}
\end{code}

\subsection{Generate helper functions}

\fnlabel{finished} tells us if it is time to stop generation

\begin{code}
finished :: BuilderStatus -> Bool
finished = null.theAgenda -- should add check that step is aux

nullAgenda :: BState Bool
nullAgenda = do 
  s <- get  
  return (null $ theAgenda s)
\end{code}

\paragraph{selectGiven} Arbitrarily selects and removes an element from
the agenda and returns it.

\begin{code}
selectGiven :: BState ChartItem
selectGiven = 
 do s <- get
    let a = theAgenda s
    put ( s { theAgenda = tail a } )
    return (head a)
\end{code}

\paragraph{dispatchNew} assigns new items to the right data structure
following these critieria:
\begin{enumerate}
\item if the item is both syntactically complete (no more subst nodes) and
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
dispatchNew :: [ChartItem] -> BState [ChartItem]
dispatchNew l = 
  do -- first we seperate the results from the non results
     st <- get
     let inputSem = tsem st
         synComplete x = 
           (not (aux x)) && closed x
           -- don't forget about null adjnodes
         semComplete x = inputSem == treeSem 
           where treeSem = sortSem (ciSemantics x)
         isResult x = 
           synComplete x && semComplete x 
     let (res, notRes) = partition isResult l
     -- -------------------------------------------------- 
     -- the non results
     -- -------------------------------------------------- 
     -- now... throw out any trees which are over the num trees limit 
     -- (this only applies in IgnoreSemantics mode) 
     state <- get
{-
     let numTreesLimit = (maxTrees.genconfig) state
         numTrees  x = length $ snd $ derivation x
         overLimit x = case numTreesLimit of
                         Nothing  -> False
                         Just lim -> numTrees x > lim
-}
     let notRes2 = notRes
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
     return res
{-
     let tbUnify x =
          case (tbUnifyTree x) of
            Left n  -> do let x2 = x { thighlight = [n] }
                          addToTrash x2 TS_TbUnify 
                          return False 
            Right _ -> return True 
     filterM tbUnify res
-}
\end{code}

% --------------------------------------------------------------------  
\section{Substitution}
\label{sec:substitution}
% --------------------------------------------------------------------  

\paragraph{applySubstitution} returns the list of possible substitutions
between an agenda $aItem$ and the chart.

\begin{code}
applySubstitution :: ChartItem -> BState [ChartItem]
applySubstitution agendaItem =  
  do chartItems <- lookupChart agendaItem 
     let trySubst = tryOperation iapplySubst
     -- active completion
     resA <- mapM (\c -> trySubst agendaItem c) chartItems 
     -- passive completion
     resP <- mapM (\c -> trySubst c agendaItem) chartItems
     incrNumcompar (length chartItems) 
     return (concat $ resA ++ resP)
\end{code}

\paragraph{iapplySubst} determines if adjunction is possible between a
chart item and an agenda item.  If so, it returns a new agenda item.
If not, it returns Nothing.  

\begin{code}
iapplySubst :: ChartId -> ChartItem -> ChartItem -> Maybe ChartItem 
iapplySubst newId activeItem passiveItem = 
  do -- reject immediately any passive items that are still open
     guard (closed passiveItem)
     -- get the first open substitution node in the active item
     -- (if available)
     let split []     = Nothing 
         split (x:xs) = Just (x, xs)
     (aNode, aRest) <- split (ciSubstnodes activeItem)
     -- nodes to compare
     let pRoot     = ciRootNode passiveItem     
         aLocation = fst3 aNode
         --
     unfResults <- unifyTagSites aNode pRoot 
     let subst = thd3 unfResults
         oldOps = ciOperations activeItem
         newOp  = Subst (ciId passiveItem) (ciId activeItem, aLocation) 
         newItem = activeItem
           { ciId         = newId 
           , ciSubstnodes = replace subst aRest 
           , ciOperations = newOp : oldOps } 
     return newItem 
\end{code}

% --------------------------------------------------------------------  
\section{Adjunction}
\label{sec:adjunction}
\label{sec:ordered_adjunction}
\label{sec:foot_constraint}
% ---------------------------------------------------------------  

% --------------------------------------------------------------------  
\section{Chart manipulation}
% --------------------------------------------------------------------  

\subsection{Equivalence}

\paragraph{checkIfUnique} checks to see a candidate item is 
unique with respect to the chart.  If it is unique, we simply
return false.  If it is true, we merge it with any items it
is equivalent to.

\begin{code}
checkIfUnique :: ChartItem -> BState Bool
checkIfUnique cand = 
  do s <- get
     let chart = theChart s
         helper o =  
           let isEq = o `equivalent` cand
           in  (isEq, if isEq then mergeItems o cand else o)
         (isEq, newChart) = unzip $ map helper chart
     --
     if (or isEq)
        then do put ( s {theChart = newChart} )
                return False 
        else return True 
\end{code}

\fnlabel{equivalent} returns true if two chart items are equivalent.
Note that this is not the same thing as equality!  

\begin{code}
equivalent :: ChartItem -> ChartItem -> Bool
equivalent c1 c2 =
  stuff c1 == stuff c2
  where 
    stuff x = 
     ( ciRootNode x, ciFootNode x, ciSubstnodes x
     , ciSemantics x, ciPolpaths x )
\end{code}

\fnlabel{mergeItems} combines two chart items into one, with the
assumption being that you have already determined that they are
equivalent.  Information from the second ``slave'' item is merged
into information from the first ``master'' item.

\begin{code}
mergeItems :: ChartItem -> ChartItem -> ChartItem
mergeItems master slave =
  master { ciOperations = (ciOperations master) ++ (ciOperations slave) }
\end{code}

\subsection{Lookup}

\fnlabel{lookupChart} retrieves a list of chart items from the chart which
could be combined with the current item from the agenda.

The current implementation searches for items which 
\begin{itemize}
\item do not have overlapping semantics with the given
\item are on the some of the same polarity automaton paths as the given.
\end{itemize}

\begin{code}
lookupChart :: ChartItem -> BState [ChartItem]
lookupChart given = do
  s <- get
  let chart  = theChart s
      -- gpaths = tpolpaths given
      gsem   = ciSemantics given
  return [ t | t <- chart
             -- should be on the same polarity path (chart sharing)
             -- , (tpolpaths t) .&. gpaths /= 0 
             -- semantics should not be overlapping
             , (null $ intersect (ciSemantics t) gsem)
         ] 
\end{code}

% --------------------------------------------------------------------  
\section{Helper functions}
% --------------------------------------------------------------------  

\paragraph{unifyTagNodes} performs feature structure unification 
on TAG nodes.  First we try unification on the top node.  We
propagate any results from that unification and proceed to trying
unification on the bottom nodes.  If succesful, we return the 
results of both unifications and a list of substitutions to
propagate.  Otherwise we return Nothing.

\begin{code}
unifyTagSites :: TagSite -> TagSite -> Maybe (Flist, Flist, Subst)
unifyTagSites (_, t1, b1) (_, t2, b2) =
  let (succ1, newTop, subst1)  = unifyFeat t1 t2
      (succ2, newBot, subst2)  = unifyFeat (replace subst1 b1) (replace subst1 b2)
  in if succ1 && succ2  
     then Just (newTop, newBot, subst1 ++ subst2)
     else Nothing
\end{code}


\paragraph{tryOperation} tries some operation between two chart items,
for example, TAG substitution.  If the operation returns a new item,
we increment the chart items counter.

\begin{code}
type OperationFn = (ChartId -> ChartItem -> ChartItem -> Maybe ChartItem) 
tryOperation :: OperationFn -> ChartItem -> ChartItem -> BState [ChartItem]
tryOperation fn a p =
  do s <- get
     let counter = gencounter s
         res  = fn counter a p
         newS = s { gencounter = counter + 1 }
     case res of 
       Just r  -> do put newS
                     return [r]
       Nothing -> return []
\end{code}

\subsection{Manipulating chart items}

\fnlabel{toChartItem} returns a chart item that refers to a TAG elementary
tree.  You'll need to supply an id for the item, though

\begin{code}
toChartItem :: Integer -> TagElem -> ChartItem
toChartItem id te = ChartItem 
  { ciRootNode   = (toSite.root.ttree) te
  , ciSubstnodes = substnodes te
  , ciFootNode   = Nothing 
  , ciSemantics  = tsemantics te
  , ciPolpaths   = tpolpaths te
  -- if (ttype te == Auxiliar) 
  -- then Just $ foot $ ttree te else Nothing
  , ciId = id
  , ciOperations = [ ElemTree (idname te) ] 
  } 
  where 
    toSite :: GNode -> TagSite
    toSite x = (gnname x, gup x, gdown x)
\end{code}

\fnlabel{closed} returns true if the chart item has no open substitution
nodes
\begin{code}
closed :: ChartItem -> Bool
closed = null.ciSubstnodes
\end{code}

\fnlabel{aux} returns true if the chart item is an auxiliary tree
\begin{code}
aux :: ChartItem -> Bool
aux = isJust.ciFootNode
\end{code}

\fnlabel{closedAux} returns true if both \fnreflite{closed} and
\fnreflite{aux} return true
\begin{code}
closedAux :: ChartItem -> Bool 
closedAux x = (aux x) && (closed x)
\end{code}


