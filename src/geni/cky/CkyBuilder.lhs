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

\chapter{Cky builder}
\label{cha:CkyBuilder}

GenI current has two backends, SimpleBuilder (chapter \ref{cha:SimpleBuilder})
and this module.  This backend does not attempt to build derived trees until
the very end.  Instead, we build a derivation tree using the CKY algorithm for
TAGs.  

\begin{code}
module CkyBuilder
where
\end{code}

\ignore{
\begin{code}
import Control.Monad 
  (unless, when, foldM)

import Control.Monad.State 
  (State, get, put, liftM, runState )
import Data.List (intersect) 
import Data.Maybe (isJust, catMaybes)
import Data.Tree (Tree(Node))

import Btypes 
  ( alphaConvert
  , Ptype(Initial), Flist
  , Replacable(..), Sem, Subst
  , GNode(..)
  , root, foot
  , unifyFeat )

import qualified Builder as B

import Configuration 
  ( extrapol, rootCatsParam, polarised, chartsharing )
import General ( treeLeaves )
import Polarity 
  ( automatonPaths, buildAutomaton, detectPolPaths, lookupAndTweak  )

import Tags 
  (TagElem, TagSite, TagStatus(..),
   idname, tidnum,
   derivation,
   ttree, ttype, tsemantics, thighlight, tdiagnostic,
   tpolpaths,
   adjnodes, substnodes, setTidnums )
import Configuration (Params, footconstr, usetrash)
import General (BitVector, thd3, mapTree)

import Debug.Trace
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
    , theChart     :: Chart
    , theTrash   :: Trash
    , tsem       :: Sem
    , theStep    :: Ptype
    , gencounter :: Integer
    , genconfig  :: Params
    , genstats   :: B.Gstats 
    , theRules   :: [CKY_InferenceRule] 
    , theResults :: [ChartItem] } 
  deriving Show

type BState = State BuilderStatus 

ckyBuilder = B.Builder 
  { B.init = initBuilder
  , B.step = generateStep
  , B.stepAll  = B.defaultStepAll ckyBuilder 
  , B.finished = finished -- should add check that step is aux
  , B.stats    = genstats 
  , B.setStats = \t s -> s { genstats = t }
  , B.unpack   = \s -> [[ (show $ length $ theResults s, []) ]] 
  , B.run = run
  }

type Agenda = [ChartItem]
type Chart  = [ChartItem] 
type Trash = [ChartItem]
\end{code}

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
  let stepAll = B.stepAll ckyBuilder 
      init    = B.init ckyBuilder 
      -- combos = polarity automaton paths 
      combos  = fst (setup input config)
      --
      generate = runState stepAll 
  in snd $ runState stepAll $ init input config
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
     candsWithPaths = detectPolPaths combosPol 
     -- 
     cands = map alphaConvert $ setTidnums $ candsWithPaths 
  in (cands, fst autstuff)
\end{code}


\paragraph{initBuilder} Creates an initial Builder.  

\begin{code}
initBuilder :: B.Input -> Params -> BuilderStatus 
initBuilder input config = 
  let seminput = B.inSemInput input
      cands = B.inCands input
  in trace (show cands) $ S 
       { theAgenda  = [] 
       , theChart = []
       , theTrash = []
       , theResults = []
       , theRules = concatMap treeToRules cands
       , tsem     = fst seminput
       , theStep  = Initial
       , gencounter = toInteger (length cands)
       , genconfig  = config
       , genstats   = B.initGstats}

treeToRules :: TagElem -> [CKY_InferenceRule]
treeToRules te = 
  let iRules = map (initRule te) $ treeLeaves $ ttree te
      kRules = decompose te
  in iRules ++ kRules

-- | decompose a TagElem into the inference rules used for bottom-up derivation on it
decompose :: TagElem -> [CKY_InferenceRule]
decompose te =  helper (ttree te) where
   helper (Node n [])   = []
   helper (Node n kidNodes) = 
     let kids = map (\ (Node x _) -> x) kidNodes
     in  (kidsToParentRule te n kids) : concatMap helper kidNodes
\end{code}

\subsubsection{BuilderState updaters}

\begin{code}
addToAgenda :: ChartItem -> BState ()
addToAgenda te = do 
  s <- get
  put s{ theAgenda = te : (theAgenda s) }

addToResults :: ChartItem -> BState ()
addToResults te = do 
  s <- get
  put s{ theResults = te : (theResults s) }
     
updateAgenda :: Agenda -> BState ()
updateAgenda a = do 
  s <- get  
  put s{ theAgenda = a }

addToChart :: ChartItem -> BState ()
addToChart te = do 
  s <- get  
  put s { theChart = te : (theChart s) }

addToTrash :: ChartItem -> TagStatus -> BState ()
addToTrash te err = do 
  return ()
\end{code}

\subsection{Chart items}

\begin{enumerate}
\item $id$   - a unique identifier for this item
\item $operations$ - a list of operations that may be used to
      to construct this item.  Note that this list is a disjunction,
      not a sequence.  It is meant to be interpreted as ``either do Foo or
      Bar'', not ``do Foo, then Bar''.
\end{enumerate}

\begin{code}
data ChartItem = ChartItem 
  { ciNode       :: GNode
  , ciSourceTree :: String
  --
  , ciPolpaths   :: BitVector
  , ciSemantics  :: Sem
  --
  , ciId         :: ChartId 
  } deriving Show

type ChartId = Integer
\end{code}

\begin{code}
\end{code}

% FIXME: diagram and comment

\begin{code}
ckyShow name source = name ++ " (" ++ source ++ ")"

nodeToItem :: TagElem -> GNode -> ChartItem 
nodeToItem te node = ChartItem 
  { ciNode       = node 
  , ciSourceTree = idname te
  , ciPolpaths   = tpolpaths te
  , ciSemantics  = tsemantics te 
  , ciId         = -1 } -- id unset

-- | initialisation rule for subst node
initRule :: TagElem -> GNode -> CKY_InferenceRule
initRule te node = Rule 
  { showRule = ckyShow "init" (idname te) ++ " " ++ show node
  , retrieve = \_ -> Just [] -- always suceeds
  , infer    = \_ -> [nodeToItem te node] }

-- | CKY inference rule 
kidsToParentRule :: TagElem -> GNode -> [GNode] -> CKY_InferenceRule
kidsToParentRule te parent kids = Rule 
  { showRule = showFn
  , retrieve = retrieveFn
  , infer    = \_ -> [nodeToItem te parent] } 
  where 
    source = idname te
    showFn = ckyShow "kidsToParents" source ++ " p: " ++ show parent ++ " k: " ++ show kids 
    retrieveFn chart = 
      let isMatch n c =  
            (gaconstr.ciNode) c && ciSourceTree c == source &&
            (gnname.ciNode) c == gnname n
          hasMatch n = any (isMatch n) chart
      in  if all hasMatch kids then Just [] else Nothing
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
generateStep :: BState () 
generateStep = 
 do -- this check may seem redundant with generate, but it's needed 
    -- to protect against a user who calls generateStep on a finished
    -- state
    isFinished <- query finished
    unless (isFinished) generateStep2 

generateStep2 :: BState () 
generateStep2 = 
  do st <- get
     -- incrGeniter 1
     -- try the permanent inference rules
     
     -- try the disposable inference rules
     let chart = theChart st
         results = map (\r -> applyInferenceRule r chart) (theRules st) 
     -- remove all the rules that suceeded
     put $ st { theRules = [ i | (i,r) <- zip inferenceRules results, isJust r ] }
     -- put all newly generated items into the right pigeon-holes
     mapM dispatchNew (concat $ catMaybes results)
     return ()
\end{code}

\begin{code}
data InferenceRule a = Rule 
  { showRule :: String
  , retrieve :: [a] -> Maybe [a] -- chart to list of candidates
  , infer    :: [a] -> [a] }

instance Show (InferenceRule a) where 
  show r = "inf rule " ++ showRule r

inferenceRules = [] -- kidsToParentRule, substitutionRule ]

applyInferenceRule :: InferenceRule a -> [a] -> Maybe [a]
applyInferenceRule r chart = 
  trace ("applying " ++ show r) $ (infer r) `liftM` retrieve r chart 


type CKY_InferenceRule = InferenceRule ChartItem
\end{code}

\subsection{Generate helper functions}

\fnlabel{finished} tells us if it is time to stop generation

\begin{code}
finished :: BuilderStatus -> Bool
finished = (null.theRules) -- should add check that step is aux

query :: (BuilderStatus -> a) -> BState a
query fn = fn `liftM` get 
\end{code}

\paragraph{dispatchNew} assigns a new item to the right data structure
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
dispatchNew :: ChartItem -> BState () 
dispatchNew item = 
  do let filts = [ removeRedundant, removeResults ]
     -- keep trying dispatch filters until one of them suceeds
     foldM (\prev f -> if prev then return True else f item) True filts
     return ()
  -- >>= addToAgenda 

-- merges non-new items with the chart; assigns a unique id to new items
removeRedundant :: ChartItem -> BState Bool 
removeRedundant item = 
  do st <- get
     let chart = theChart st
         mergeEquivItems o =  
           let isEq = o `equivalent` item 
           in  (isEq, if isEq then mergeItems o item else o)
         (isEq, newChart) = unzip $ map mergeEquivItems chart
     --
     if or isEq
        then do put ( st {theChart = newChart} )
                return True 
        else do setId item
                return False 

-- puts result items into the results list
removeResults :: ChartItem -> BState Bool 
removeResults item = return True --FIXME: to implement
{-
  do st <- get
     let inputSem = tsem st
         synComplete x = (not (aux x)) && closed x
         -- FIXME don't forget about null adjnodes
         semComplete x = inputSem == treeSem 
           where treeSem = sortSem (ciSemantics x)
     if synComplete item && semComplete item  
        then removeTbFailures >>= do addToResults item
                                     mzero
        else item 
-}

-- FIXME: to implement
removeTbFailures item = return True 
\end{code}

%% --------------------------------------------------------------------  
%\section{Substitution}
%\label{sec:substitution}
%% --------------------------------------------------------------------  
%
%\paragraph{applySubstitution} returns the list of possible substitutions
%between an agenda $aItem$ and the chart.
%
%\begin{code}
%applySubstitution :: ChartItem -> [ChartItem]
%applySubstitution agendaItem =  
%  do chartItems <- lookupChart agendaItem 
%     let activeCompletion  c = iapplySubst agendaItem c 
%         passiveCompletion c = iapplySubst c agendaItem
%     return (concatMap activeCompletion chartItems ++ 
%             concatMap passiveCompletion chartItems)
%-- incrNumcompar (length chartItems) 
%\end{code}
%
%\paragraph{iapplySubst} determines if adjunction is possible between a
%chart item and an agenda item.  If so, it returns a new agenda item.
%If not, it returns Nothing.  
%
%\begin{code}
%iapplySubst :: ChartId -> ChartItem -> ChartItem -> Maybe ChartItem 
%iapplySubst newId activeItem passiveItem = 
%  do -- reject immediately any passive items that are still open
%     guard (closed passiveItem)
%     -- get the first open substitution node in the active item
%     -- (if available)
%     let split []     = Nothing 
%         split (x:xs) = Just (x, xs)
%     (aNode, aRest) <- split (ciSubstnodes activeItem)
%     -- nodes to compare
%     let pRoot     = ciRootNode passiveItem     
%         aLocation = fst3 aNode
%         --
%     unfResults <- unifyTagSites aNode pRoot 
%     let subst = thd3 unfResults
%         oldOps = ciOperations activeItem
%         newOp  = Subst (ciId passiveItem) (ciId activeItem, aLocation) 
%         newItem = activeItem
%           { ciId         = newId 
%           , ciSubstnodes = replace subst aRest 
%           , ciOperations = newOp : oldOps } 
%     return newItem 
%\end{code}
%
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

\fnlabel{equivalent} returns true if two chart items are equivalent.
Note that this is not the same thing as equality!  

\begin{code}
equivalent :: ChartItem -> ChartItem -> Bool
equivalent c1 c2 =
  stuff c1 == stuff c2
  where 
    stuff x = 
     ( ciNode x, ciSemantics x, ciPolpaths x )
\end{code}

\fnlabel{mergeItems} combines two chart items into one, with the
assumption being that you have already determined that they are
equivalent.  Information from the second ``slave'' item is merged
into information from the first ``master'' item.

\begin{code}
mergeItems :: ChartItem -> ChartItem -> ChartItem
mergeItems master slave = master  --FIXME: to implement
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

\begin{code}
setId :: ChartItem -> BState ChartItem 
setId item =
  do s <- get
     let counter = gencounter s 
     put $ s { gencounter = counter + 1 }
     return $ item { ciId = counter }
\end{code}

\subsection{Manipulating chart items}

\begin{code}
\end{code}

%\fnlabel{closed} returns true if the chart item has no open substitution
%nodes
%\begin{code}
%closed :: ChartItem -> Bool
%closed = null.ciSubstnodes
%\end{code}
%
%\fnlabel{aux} returns true if the chart item is an auxiliary tree
%\begin{code}
%aux :: ChartItem -> Bool
%aux = isJust.ciFootNode
%\end{code}
%
%\fnlabel{closedAux} returns true if both \fnreflite{closed} and
%\fnreflite{aux} return true
%\begin{code}
%closedAux :: ChartItem -> Bool 
%closedAux x = (aux x) && (closed x)
%\end{code}


