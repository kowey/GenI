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
import Control.Monad (ap, when, foldM)

import Control.Monad.State 
  (State, evalState, runState, get, put)
import Data.List (intersect, partition, delete, sort, nub, (\\))
import Data.Maybe (catMaybes)
import Data.Tree 
import Data.Bits

import Btypes 
  (Ptype(Initial,Auxiliar),
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
                      usetrash, maxTrees)
import General (BitVector, fst3, mapTree)
\end{code}
}

% --------------------------------------------------------------------  
\section{Key types}
% --------------------------------------------------------------------  

A chart item has two parts.

The things which define an equivalence class are a tuple 
$\tuple{root, foot, snodes}$ where
\begin{enumerate}
\item $root$ - is the root node associated with the chart 
\item $foot$ - is the foot node of the item - if this is not being used
               during the adjunction phase, it will be ignored
\item $snodes$ - open substitution nodes
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
  , ciSubstnodes :: [ TagSites ]
  --
  , ciId         :: ChartId 
  , ciOperations :: [ ChartOperation ] 
  }
\end{code}

A chart item is associated with some equivalence class equivalence class

A node in both chart and agenda items is defined as a pair of feature
structures corresponding to the top and bottom features of a TAG
node.

\begin{code}
type TAGNode = (Flist, Flist)
\end{code}

\begin{code}
data ChartOperation = 
    Subst ChartId (ChartId, String)
  | Adj   ChartId (ChartId, String)
  | ElemTree String
\end{code}

\begin{code}
type ChartId = Int

type Agenda = [ChartItem]
type AuxAgenda  = [ChartItem]
type Chart  = [ChartItem] 
type Trash = [ChartItem]
\end{code}

\subsection{BuilderState}

Note the theTrash is not actually essential to the operation of the
generator; it is for pratical debugging of grammars.  Instead of
trees dissapearing off the face of the debugger; they go into the
trash where the user can inspect them and try to figure out why they
went wrong.  To keep the generator from exploding we also keep an
option not to use the trash, so that it is only enabled in debugger
mode.

\begin{code}
data Builder = S
    { theAgenda    :: Agenda
    , theAuxAgenda :: AuxAgenda
    , theChart     :: Chart
    , theTrash   :: Trash
    , tsem       :: Sem
    , step       :: Ptype
    , chartIdCounter :: Integer
    , gencounter :: Integer
    , genconfig  :: Params
    , genstats   :: Gstats } 
  deriving Show

type BState = State BuilderStatus 
\end{code}

\paragraph{initBuilderState} Creates an initial Builder.  

\begin{code}
initBuilder :: [TagElem] -> Sem -> Params -> Builder
initBuilder cands ts config = 
  let (a,i) = partition isPureAux (map toChartItem cands)
  in S { theAgenda  = i
       , theAuxAgenda = a
       , theChart = []
       , theTrash = []
       , tsem     = ts
       , step     = Initial
       , chartIdCounter = 0  
       , gencounter = toInteger (length cands)
       , genconfig  = config
       , genstats   = initGstats}

\end{code}

\subsubsection{BuilderState updaters}

\begin{code}
addToAgenda :: ChartItem -> BState ()
addToAgenda te = do 
  s <- get
  put s{theAgenda = te : (theAgenda s)}
     
updateAgenda :: Agenda -> BState ()
updateAgenda a = do 
  s <- get  
  put s{theAgenda = a}

addToAuxAgenda :: ChartItem -> BState ()
addToAuxAgenda te = do 
  s <- get
  -- each new tree gets a unique id... this makes comparisons faster 
  let counter = (gencounter s) + 1
      te2 = te { tidnum = counter }
  put s{ gencounter = counter
       , theAuxAgenda = te2 : (theAuxAgenda s)}
 
addToChart :: ChartItem -> BState ()
addToChart te = do 
  s <- get  
  put s { theChart = (te : (theChart s) }
  incrSzchart 1

addToTrash :: ChartItem -> TagStatus -> BState ()
addToTrash te err = do 
  s <- get
  let te2 = te { tdiagnostic = err }
  when ((usetrash.genconfig) s) $
    put s { theTrash = te2 : (iaddtoTrash (theTrash s) }

incrGeniter :: Int -> BState ()
incrGeniter n = do
  s <- get
  let oldstats = genstats s 
      newstats = oldstats { geniter = (geniter oldstats) + n }
  put s { genstats = newstats }

incrSzchart :: Int -> BState ()
incrSzchart n = do
  s <- get
  let oldstats = genstats s 
      newstats = oldstats { szchart = (szchart oldstats) + n }
  put s { genstats = newstats }

incrNumcompar :: Int -> BState ()
incrNumcompar n = do
  s <- get
  let oldstats = genstats s 
      newstats = oldstats { numcompar = (numcompar oldstats) + n }
  put s { genstats = newstats }
\end{code}

\subsubsection{BuilderState accessors}

We retrieve the BuilderState from the State monad and then retrieve a field from it.

\begin{code}
getAgenda :: BState Agenda
getAgenda = do 
  s <- get
  return (theAgenda s)

getChart :: BState Chart
getChart = do 
  s <- get
  return (theChart s)

getStep :: BState Ptype
getStep = do 
  s <- get
  return (step s)

getSem :: BState Sem
getSem = do 
  s <- get
  return (tsem s)
\end{code}

These functions let us find out if the Agenda or the AuxAgenda are null.

\begin{code}
nullAgenda :: BState Bool
nullAgenda = do 
  s <- get  
  return (null $ theAgenda s)
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
generate :: BState [ChartItem]
generate = do 
  nir <- nullAgenda 
  if nir
     then return []
     else do res  <- generateStep
             next <- generate
             return (res ++ next)

generateStep :: BState [ChartItem]
generateStep = do
  nir     <- nullAgenda
  -- this check may seem redundant with generate, but it's needed 
  -- to protect against a user who calls generateStep on a finished
  -- state
  if (nir && curStep == Auxiliar) 
    then return []
    else do incrGeniter 1
            generateStep' 

generateStep' :: BState [ChartItem] 
generateStep' = 
  do -- choose an item from the agenda
     given <- selectGiven
     -- have we triggered the switch to aux yet?
     curStep <- getStep
     -- do either substitution or adjunction 
     res <- applySubstitution given
     -- determine which of the res should go in the agenda 
     -- (monadic state) and which should go in the result (res')
     res' <- dispatchNew res
     -- put the given into the chart untouched 
     if (curStep == Initial) 
        then addToChart   given
        else when (null $ adjnodes given) $ trashIt given 
     return res'
  where 
     trashIt t = 
       do s <- get
          let missingSem = tsem s \\ tsemantics t
          addToTrash t (TS_SemIncomplete missingSem)
\end{code}

\subsection{Generate helper functions}

\paragraph{selectGiven} Arbitrarily selects and removes an element from
the agenda and returns it.

\begin{code}
selectGiven :: BState ChartItem
selectGiven = do 
  s <- get
  a <- theAgenda s
  put ( s { theAgenda = tail a } )
  return (head a)
\end{code}


% --------------------------------------------------------------------  
\section{Substitution}
\label{sec:substitution}
% --------------------------------------------------------------------  

\paragraph{applySubstitution} returns the list of possible substitutions
between an agenda $aItem$ and the chart.

\begin{code}
applySubstitution :: ChartItem -> BState [AgendaItem]
applySubstitution agendaItem =  
  do chartItems <- lookupChart agendaItem 
     let trySubst = tryOperation iapplySubst
         -- active completion
         resA = mapMaybe (\c -> trySubst agendaItem c) chartItems 
         -- passive completion
         resP = mapMaybe (\c -> trySubst c agendaItem) chartItems
     incrNumcompar (length chartItems) 
     return (resA ++ resP)
\end{code}

\paragraph{iapplySubst} determines if adjunction is possible between a
chart item and an agenda item.  If so, it returns a new agenda item.
If not, it returns Nothing.  

\begin{code}
iapplySubst :: ChartId -> ChartItem -> TagNode -> ChartItem -> Maybe ChartItem 
iapplySubst newId activeItem passiveItem = 
  do -- get the first open substitution node in the active item
     -- (if available)
     let split []     = Nothing 
         split (x:xs) = Just (x, xs)
     (aNode, aRest) <- split (ciSubstnodes activeItem)
     -- nodes to compare
     let pTopBot = (snd3 pRoot, thd3 pRoot)
           where pRoot = ciRootNode passiveItem     
         aTopBot = (snd3 aNode, thd3 aNode) 
         aLocation = fst3 aNode
         --
     unfResults <- unifyTagNodes aTopBot pTopBot 
     let subst = thd3 unfResults
         newItem = activeItem
           { ciId         = newId 
           , ciSubstnodes = substFlist aRest subst 
           , ciOperation  = Subst passiveItem (activeItem, aLocation) }
     return newItem 
\end{code}

% --------------------------------------------------------------------  
\section{Adjunction}
\label{sec:adjunction}
\label{sec:ordered_adjunction}
\label{sec:foot_constraint}
% ---------------------------------------------------------------  

\paragraph{dispatchNew} 

Given a list of ChartItem, for each tree: 
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
dispatchNew :: [ChartItem] -> BState [ChartItem]
dispatchNew l = 
  do -- merge any items which are already equivalent to something to the chart
     l2 <- filterM checkIfUnique l
     -- first we seperate the results from the non results
     inputSem <- getSem
     let isResult  x = 
           (ttype x /= Auxiliar) && (null $ substnodes x) 
           && (inputSem == treeSem) && (null $ adjnodes x)
             where treeSem = (sortSem $ tsemantics x)
         partitionRes = liftM (partition isResult)
     (res, notRes) <- partitionRes l2
     -- -------------------------------------------------- 
     -- the non results
     -- -------------------------------------------------- 
     -- now... throw out any trees which are over the num trees limit 
     -- (this only applies in IgnoreSemantics mode) 
     state <- get
     let numTreesLimit = (maxTrees.genconfig) state
         numTrees  x = length (snd $ derivation x)
         overLimit x = case numTreesLimit of
                         Nothing  -> False
                         Just lim -> numTrees x > lim
     notRes2 <- filterM (liftM overLimit) notRes
     -- put any pure auxiliary trees on the auxiliary agenda
     let dispatchAux x = 
           when (isPureAux x) $ do { addToAuxAgenda x; return False }
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
                          addToTrash x2 TS_TbUnify 
                          return False 
            Right _ -> return True 
     filterM tbUnify res
\end{code}

\paragraph{switchToAux} When all substitutions has been done, tags with
substitution nodes still open are deleted, then the auxiliars tags are put in
Chart and the (initial) tags in the repository are moved into the Agenda. The
step is then changed to Auxiliary

\begin{code}
switchToAux :: BState ()
switchToAux = do
  st <- get
  let chart = theChart st
      -- You might be wondering why we ignore the auxiliary trees in the 
      -- chart; this is because all the syntactically complete auxiliary
      -- trees have already been filtered away by calls to dispatchNew
      initialT = filter (\x -> ttype x == Initial) chart
      (compT, incompT) = partition (null.substnodes) initialT
      aux   = theAuxAgenda st
      --
      filteredT = semfilter (tsem st) aux compT 
      initial = if (semfiltered $ genconfig st) 
                then filteredT else compT 
  -- toss the syntactically incomplete stuff in the trash
  mapM (\t -> addToTrash t TS_SynIncomplete) incompT
  put st{theAgenda = initial,
         theAuxAgenda = [], 
         theChart = aux,
         step = Auxiliar}
\end{code}

% --------------------------------------------------------------------  
\section{Chart manipulation}
% --------------------------------------------------------------------  

\subsection{Equivalence}

\paragraph{checkIfUnique} checks to see a candidate item is 
unique with respect to the chart.  If it is unique, we simply
return false.  If it is true, we merge it with any items it
is equivalent to.

\begin{code}
checkIfUnique ChartItem -> Bstate Bool
checkIfUnique cand = 
  do s <- get
     let chart = theChart s
         helper o =  
           let isEq = o `equivalent` cand)
           in  (isEq, if isEq then mergeItems o cand else o)
         (isEq, newChart) = unzip $ map helper chart
     --
     if any isEq
        then do put ( s {chart = newChart} )
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
    stuff x = (ciRootNode x, ciFootNode x, ciSubstnodes x)
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
lookupChart :: AgendaItem -> BState [ChartItem]
lookupChart given = do
  chart <- getChart
  let gpaths = tpolpaths given
      gsem   = tsemantics given
  return [ t | t <- chart
             -- should be on the same polarity path (chart sharing)
             -- , (tpolpaths t) .&. gpaths /= 0 
             -- semantics should not be overlapping
             (null $ intersect (tsemantics t) gsem)
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
unifyTagNodes :: TagNode -> TagNode -> Maybe (TagNode, TagNode, Subst)
unifyTagNodes (t1, b1) (t2, b2) =
  let (succ1, newTop, subst1)  = unifyFeat t1 t2
      (succ2, newBot, subst2)  = unifyFeat (substFlist b1 subst1) (substFlist b1 subst1)
  in if succ1 && succ2  
     then Just (newTop, newBot, subst1 ++ subst2)
     else Nothing
\end{code}


\paragraph{tryOperation} tries some operation between two chart items,
for example, TAG substitution.  If the operation returns a new item,
we increment the chart items counter.

\begin{code}
type OperationFn = (ChartId -> ChartItem -> ChartItem -> Maybe ChartItem) 
tryOperation :: OperationFn -> ChartItem -> ChartItem -> BState (Maybe ChartItem)
tryOperation fn a p =
  do s <- get
     let counter = chartIdCounter s
         res  = fn counter a p
         newS = s { chartIdCounter = counter + 1 }
     when (isJust res) $
       do put newS 
 return res
\end{code}

\paragraph{isPureAux} returns True if a tree is an auxiliary tree with
no substitution nodes

\begin{code}
isPureAux :: ChartItem -> Bool 
isPureAux x = ((ttype x) == Auxiliar && (null $ substnodes x))
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
