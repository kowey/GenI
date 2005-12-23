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
  (unless, foldM)

import Control.Monad.State 
  (State, get, put, liftM, runState, execState )
import Data.List ( delete, intersect, intersperse )
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Tree (Tree(Node))

import Btypes 
  ( alphaConvert
  , Flist
  , Replacable(..), Sem, Subst
  , GNode(..), GType(Subs, Other)
  , root, foot
  , unifyFeat )

import qualified Builder as B

import Configuration 
  ( extrapol, rootCatsParam, polarised)
import General ( choices, treeLeaves )
import Polarity 
  ( automatonPaths, buildAutomaton, detectPolPaths, lookupAndTweak  )

import Tags 
  (TagElem, TagSite, TagStatus(..),
   idname, 
   ttree, tsemantics, 
   tpolpaths,
   setTidnums )
import Configuration (Params)
import General (BitVector)

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
      cands   = fst (setup input config)
      input2  = input { B.inCands = cands }
      --
  in snd $ runState stepAll $ init input2 config
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
      cands = concatMap initTree $ B.inCands input
      initS = S 
       { theAgenda  = [] 
       , theChart = []
       , theTrash = []
       , theResults = []
       , theRules = map fst ckyRules 
       , tsem     = fst seminput
       , gencounter = toInteger (length cands)
       , genconfig  = config
       , genstats   = B.initGstats}
  in execState (mapM dispatchNew cands) initS
  
initTree :: TagElem -> [ChartItem]
initTree te = 
  let createItem n = (nodeToItem te n) { ciRouting = decompose te }
  in  map createItem $ treeLeaves $ ttree te

-- | explode a TagElem tree into a bottom-up routing map 
decompose :: TagElem -> RoutingMap 
decompose te =  helper True (ttree te) Map.empty where
  tname = idname te
  --
  helper :: Bool -> Tree GNode -> RoutingMap -> RoutingMap
  helper isRoot (Node _ []) smap = smap 
  helper isRoot (Node p kidNodes) smap = 
    let kids     = [ gnname x | (Node x _) <- kidNodes ] 
        addKid k = Map.insert (tname, k) (delete k kids, p, isRoot)
        smap2    = foldr addKid smap kids
    in -- recurse to add routing info for child nodes 
       foldr (helper False) smap2 kidNodes

-- from (tree name, node name) to a list of its sisters, its parent, and
-- whether or not the parent is a root
type RoutingMap = Map.Map (String, String) ([String], GNode, Bool)
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
addToTrash _ _ = do 
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
  , ciRoot       :: Bool 
  -- names of the sisters of this node in its tree
  , ciRouting    :: RoutingMap 
  -- variable replacements to accumulate
  , ciSubsts     :: Subst 
  } deriving Show

type ChartId = Integer
\end{code}

\begin{code}
\end{code}

% FIXME: diagram and comment

\begin{code}
ckyRules = 
 [ (kidsToParentRule, "kidsToParents")
 , (substRule       , "subst") 
 , (nonAdjunctionRule, "nonAdj") ]

ckyShow name item chart = 
  let showChart = show $ length chart
      pad s n = s ++ (take (n - length s) $ repeat ' ')
  in concat $ intersperse "\t" [pad name 15, showChart, ciSourceTree item, show $ ciNode item ]

showItems items = show $ map ciNode items

nodeToItem :: TagElem -> GNode -> ChartItem 
nodeToItem te node = ChartItem 
  { ciNode       = node
  , ciRoot       = gnname node == (gnname.root.ttree) te
  , ciSourceTree = idname te
  , ciPolpaths   = tpolpaths te
  , ciSemantics  = tsemantics te 
  , ciId         = -1 -- id unset
  , ciRouting    = Map.empty 
  , ciSubsts     = [] }

-- | CKY non adjunction rule - creates items in which
-- we do not apply any adjunction
nonAdjunctionRule :: CKY_InferenceRule
nonAdjunctionRule item _ =
  let node  = ciNode item
      node2 = node { gaconstr = True }
  in if gtype node /= Other || gaconstr node then Nothing
     else Just [ item { ciNode = node2 } ]

-- | CKY parent rule
-- FIXME: need to think about unification and multiple matches
-- something about most general unifier - do we really need a 
-- mgu mechanism in compare?
kidsToParentRule :: CKY_InferenceRule
kidsToParentRule item chart = 
 do (s,p,r)  <- Map.lookup (source, gnname node) (ciRouting item) 
    sMatches <- trace (" relevant chart: " ++ showItems relChart) $ 
                trace (" routing info: " ++ show (s,p,r)) $ 
                choices $ map matches s
    -- FIXME: need to do unification
    let pItem = item { ciNode = p, ciRoot = r } 
    trace (" matches: " ++ (show $ map showItems sMatches)) Just $ map (const pItem) sMatches
 where
   node    = ciNode item
   source  = ciSourceTree item  
   --
   relevant c = 
     let cNode = ciNode c
     in  ciSourceTree c == source && 
         gtype cNode /= Subs && gaconstr cNode 
   relChart = filter relevant chart
   --
   matches sis = [ c | c <- relChart, (gnname.ciNode) c == sis ]

-- | CKY subst rules
substRule :: CKY_InferenceRule
substRule item chart = 
 if null res then Nothing else Just res
 where
  res  = catMaybes resVariants
  --
  resVariants
   | (gtype node == Subs) = trace " subst variant" $ map (\r -> unifyWith item r) roots
   | ciRoot item          = trace " root variant"  $ map (\s -> unifyWith s item) subs 
   | otherwise            = [] 
  node = ciNode item
  --
  roots = [ r | r <- chart, ciRoot r && (gaconstr.ciNode) r ]
  subs  = [ s | s <- chart, (gtype.ciNode) s == Subs ]
  --
  unifyWith sItem rItem = 
    let rNode = ciNode rItem
        newNode u d = rNode { gnname = gnname node, gup = u, gdown = d }
    in case unifyGNodes node (ciNode rItem) of
       Nothing                -> Nothing
       Just (up, down, subst) -> Just $ sItem { ciNode = newNode up down }
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
     agendaItem <- selectAgendaItem
     -- try the inference rules
     let chart = theChart st
         apply (rule, name) = 
           trace (ckyShow name agendaItem chart) $ 
           rule agendaItem chart
         results = map apply ckyRules 
         showRes (_,name) res = 
           case res of 
           Nothing -> ""
           Just _  -> "\n" ++ (ckyShow ("<- " ++ name) agendaItem chart)
     -- put all newly generated items into the right pigeon-holes
     trace (concat $ zipWith showRes ckyRules results) $ mapM dispatchNew (concat $ catMaybes results)
     -- 
     addToChart agendaItem
     return ()

selectAgendaItem :: BState ChartItem 
selectAgendaItem = do 
  a <- query theAgenda 
  updateAgenda (tail a)
  return (head a)
\end{code}

\begin{code}
type InferenceRule a = a -> [a] -> Maybe [a] 
type CKY_InferenceRule = InferenceRule ChartItem

instance Show CKY_InferenceRule where
  show _ = "cky inference rule"
\end{code}

\subsection{Generate helper functions}

\fnlabel{finished} tells us if it is time to stop generation

\begin{code}
finished :: BuilderStatus -> Bool
finished = (null.theAgenda) -- should add check that step is aux

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
  do let filts = [ removeRedundant, removeResults, dispatchToAgenda ]
     -- keep trying dispatch filters until one of them suceeds
     foldM (\prev f -> if prev then return True else f item) False filts
     return ()

dispatchToAgenda :: ChartItem -> BState Bool
dispatchToAgenda item =
   trace (ckyShow "-> agenda" item []) $
   do addToAgenda item
      return True

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
        then trace (ckyShow "-> merge" item []) $
             do put ( st {theChart = newChart} )
                return True 
        else do setId item
                return False 

-- puts result items into the results list
removeResults :: ChartItem -> BState Bool 
removeResults _ = return False --FIXME: to implement
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
removeTbFailures _ = return False
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
equivalent _ _ = False {-
  stuff c1 == stuff c2
  where 
    stuff x = 
     ( ciNode x, ciSemantics x, ciPolpaths x ) -}
\end{code}

\fnlabel{mergeItems} combines two chart items into one, with the
assumption being that you have already determined that they are
equivalent.  Information from the second ``slave'' item is merged
into information from the first ``master'' item.

\begin{code}
mergeItems :: ChartItem -> ChartItem -> ChartItem
mergeItems master _ = master  --FIXME: to implement
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
unifyTagSites (_, t1, b1) (_, t2, b2) = unifyPair (t1,b1) (t2,b2)

unifyGNodes :: GNode -> GNode -> Maybe (Flist, Flist, Subst)
unifyGNodes g1 g2 =
  unifyPair (gupdown g1) (gupdown g2) 
  where gupdown n = (gup n, gdown n)

unifyPair :: (Flist, Flist) -> (Flist, Flist) -> Maybe (Flist, Flist, Subst)
unifyPair (t1, b1) (t2, b2) =
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


