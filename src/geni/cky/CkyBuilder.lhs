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
import Data.Bits ( (.&.), (.|.), bit )
import Data.List ( delete, intersperse )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tree 

import Btypes 
  ( alphaConvert, unify, collect
  , Flist
  , Replacable(..), Pred, Sem, Subst
  , GNode(..), GType(Subs, Foot, Other)
  , GeniVal(GVar),
  , Ptype(Auxiliar)
  , root, foot
  , showPred
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
   tpolpaths, ttype,
   setTidnums )
import Configuration (Params)
import General (BitVector, showBitVector, geniBug)

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
  let (sem, _) = B.inSemInput input
      bmap  = defineSemanticBits sem
      cands = concatMap (initTree bmap) $ B.inCands input
      initS = S 
       { theAgenda  = [] 
       , theChart = []
       , theTrash = []
       , theResults = []
       , theRules = map fst ckyRules 
       , tsem     = sem 
       , gencounter = toInteger (length cands)
       , genconfig  = config
       , genstats   = B.initGstats}
  in execState (mapM dispatchNew cands) initS
  
initTree :: SemBitMap -> TagElem -> [ChartItem]
initTree bmap te = 
  let semVector    = semToBitVector bmap (tsemantics te)
      createItem n = (nodeToItem te n) 
       { ciSemantics = semVector
       , ciSemBitMap = bmap
       , ciRouting   = decompose te 
       , ciVariables = (map GVar) $ Set.toList $ collect te Set.empty }
  in  map createItem $ treeLeaves $ ttree te

type SemBitMap = Map.Map Pred BitVector

-- | assign a bit vector value to each literal in the semantics
-- the resulting map can then be used to construct a bit vector
-- representation of the semantics
defineSemanticBits :: Sem -> SemBitMap
defineSemanticBits sem = Map.fromList $ zip sem bits
  where 
   bits = map bit [0..] -- 0001, 0010, 0100...

semToBitVector :: SemBitMap -> Sem -> BitVector
semToBitVector bmap sem = foldr (.|.) 0 $ map lookup sem 
  where lookup p = 
         case Map.lookup p bmap of 
         Nothing -> geniBug $ "predicate " ++ showPred p ++ " not found in semanticBit map"
         Just b  -> b

bitVectorToSem :: SemBitMap -> BitVector -> Sem
bitVectorToSem bmap vector = 
  mapMaybe tryKey $ Map.toList bmap
  where tryKey (p,k) = if (k .&. vector == k) then Just p else Nothing

-- | explode a TagElem tree into a bottom-up routing map 
decompose :: TagElem -> RoutingMap 
decompose te =  helper True (ttree te) Map.empty 
  where
  helper :: Bool -> Tree GNode -> RoutingMap -> RoutingMap
  helper _ (Node _ []) smap = smap 
  helper isRoot (Node p kidNodes) smap = 
    let kids     = [ gnname x | (Node x _) <- kidNodes ] 
        addKid k = Map.insert k (delete k kids, p, isRoot)
        smap2    = foldr addKid smap kids
    in -- recurse to add routing info for child nodes 
       foldr (helper False) smap2 kidNodes

-- basically, an inverted tree
-- from node name to a list of its sisters, its parent, and
-- whether or not the parent is a root
type RoutingMap = Map.Map String ([String], GNode, Bool)
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

-- TODO: decide if we want this to be an instant of Replacable 
\begin{code}
data ChartItem = ChartItem 
  { ciNode       :: GNode
  , ciSourceTree :: TagElem 
  --
  , ciPolpaths   :: BitVector
  , ciSemantics  :: BitVector
  , ciAdjPoint   :: Maybe ChartId
  --
  , ciId         :: ChartId 
  -- names of the sisters of this node in its tree
  , ciRouting    :: RoutingMap 
  -- variable replacements to accumulate
  , ciSubsts     :: Subst 
  -- a list of genivals which were variables when the node was
  -- first initialised
  , ciVariables  :: [GeniVal]
  -- we keep a SemBitMap strictly to help display the semantics
  , ciSemBitMap  :: SemBitMap
  } deriving Show

type ChartId = Integer

ciRoot, ciFoot, ciSubs, ciAdjDone, ciAux :: ChartItem -> Bool
ciRoot  i = (gnname.ciNode) i == (gnname.root.ttree.ciSourceTree) i
ciFoot  i = (gtype.ciNode) i == Foot
ciSubs  i = (gtype.ciNode) i == Subs
ciAdjDone   = gaconstr.ciNode
ciAux   i = (ttype.ciSourceTree) i == Auxiliar

combineVectors :: ChartItem -> ChartItem -> ChartItem 
combineVectors a b = 
  b { ciSemantics = (ciSemantics a) .|. (ciSemantics b)
    , ciPolpaths  = (ciPolpaths  a) .&. (ciPolpaths  b) 
    , ciSemBitMap =  ciSemBitMap a }

combineWith :: GNode -> Subst -> ChartItem -> ChartItem -> ChartItem
combineWith node subst other x =
  combineVectors other $ 
  x { ciNode      = node 
    , ciSubsts    = (ciSubsts x) ++ subst
    , ciVariables = replace subst (ciVariables x) }
\end{code}

\begin{code}
\end{code}

% FIXME: diagram and comment

\begin{code}
ckyRules :: [ (CKY_InferenceRule, String) ]
ckyRules =
 [ (kidsToParentRule, "kidsToP")
 , (substRule       , "subst") 
 , (nonAdjunctionRule, "nonAdj") 
 , (activeAdjunctionRule, "activeAdjRule") 
 , (passiveAdjunctionRule, "passiveAdjRule") ] 

ckyShow name item chart = 
  let showChart = show $ length chart
      pad s n = s ++ (take (n - length s) $ repeat ' ')
  in concat $ intersperse "\t" $
       [ pad name 10, showChart
       , pad (idname $ ciSourceTree item) 10
       , pad (showItemSem item) 5 
       , show $ ciNode item ]

showItems = unlines . (map showItem)
showItem i = (idname.ciSourceTree) i ++ " " ++ show (ciNode i) ++ " " ++  (showItemSem i)

showItemSem = (showBitVector 3) . ciSemantics

nodeToItem :: TagElem -> GNode -> ChartItem 
nodeToItem te node = ChartItem 
  { ciNode       = node
  , ciSourceTree = te
  , ciPolpaths   = tpolpaths te
  , ciSemantics  = 0  -- to be set 
  , ciId         = -1 -- to be set 
  , ciRouting    = Map.empty -- to be set
  , ciVariables  = [] -- to be set
  , ciAdjPoint   = Nothing
  , ciSubsts     = [] 
  , ciSemBitMap  = Map.empty }

-- | CKY non adjunction rule - creates items in which
-- we do not apply any adjunction
-- this rule also doubles as top
nonAdjunctionRule item _ =
  let node  = ciNode item
      node2 = node { gaconstr = True }
  in if gtype node /= Other || ciAdjDone item then Nothing
     else Just [ item { ciNode = node2 } ]

-- | CKY parent rule
-- FIXME: something about most general unifier - do we really need a 
-- mgu mechanism in compare?
kidsToParentRule item chart = 
 do (s,p,r)  <- Map.lookup (gnname node) (ciRouting item) 
    sMatches <- -- trace (" relevant chart: " ++ showItems relChart) $ 
                -- trace (" routing info: " ++ show (s,p,r)) $ 
                choices $ map matches s
    --
    let mergePoints kids = 
          case mapMaybe ciAdjPoint kids of
          []  -> Nothing
          [x] -> Just x
          _   -> error "multiple adjunction points in kidsToParentRule?!"
    let combine kids = do
          let unifyOnly (x, _) y = unify x y
          newVars <- foldM unifyOnly (ciVariables item,[]) $ 
                     map ciVariables kids  
          let newSubsts = concatMap ciSubsts (item:kids)
              newItem   = item 
               { ciNode      = replace newSubsts p 
               , ciSubsts    = newSubsts
               , ciAdjPoint  = mergePoints kids 
               , ciVariables = fst newVars }
          return $ foldr combineVectors newItem kids
              --trace (" matches: " ++ (show $ map showItems sMatches)) $ 
    --
    listAsMaybe $ mapMaybe combine sMatches
 where
   node    = ciNode item
   source  = (idname.ciSourceTree) item  
   --
   relevant c = 
     (idname.ciSourceTree) c == source && (not $ ciSubs c) && ciAdjDone c
   relChart = filter relevant chart
   --
   matches sis = [ c | c <- relChart, (gnname.ciNode) c == sis ]

-- note: not the same as Data.Maybe.listToMaybe !
listAsMaybe :: [a] -> Maybe [a]
listAsMaybe [] = Nothing
listAsMaybe l  = Just l

push = (:)
pop []    = Nothing
pop (x:_) = Just x

-- | CKY subst rules
substRule item chart = listAsMaybe resVariants
 where
  resVariants
   | ciSubs item = -- trace " subst variant" $ 
                   mapMaybe (\r -> attemptSubst item r) roots
   | ciRoot item = -- trace " root variant"  $ 
                   mapMaybe (\s -> attemptSubst s item) subs 
   | otherwise   = [] 
  --
  roots = [ r | r <- chart, compatible item r && ciRoot r && (gaconstr.ciNode) r ]
  subs  = [ s | s <- chart, ciSubs s ]

-- | unification for substitution
attemptSubst :: ChartItem -> ChartItem -> Maybe ChartItem
attemptSubst sItem rItem | ciSubs sItem = 
 do let rNode = ciNode rItem
        sNode = ciNode sItem
    (up, down, subst) <- unifyGNodes sNode (ciNode rItem) 
    let newNode = rNode { gnname = gnname sNode
                        , gup = up, gdown = down }  
    return $ combineWith newNode subst rItem sItem
attemptSubst _ _ = error "attemptSubst called on non-subst node"
 
-- | CKY adjunction rule: note - we need this split into two rules because
-- both variants could fire at the same time, for example, the passive variant
-- to adjoin into the root of an auxiliary tree, and the active variant because
-- it is an aux tree itself and it wants to adjoin somewhere
activeAdjunctionRule item chart | ciRoot item && ciAux item =
 listAsMaybe $ mapMaybe (\p -> attemptAdjunction p item) sites 
 where
  sites = [ p | p <- chart, compatible item p && 
                (gtype.ciNode) p == Other && (not.gaconstr.ciNode) p ]

activeAdjunctionRule _ _ = Nothing -- if not applicable

-- | CKY adjunction rule: we're just a regular node, minding our own business
-- looking for an auxiliary tree to adjoin into us
passiveAdjunctionRule item chart =
 listAsMaybe $ mapMaybe (attemptAdjunction item) auxItems
 where
  auxItems = [ a | a <- chart, compatible item a && ciAux a && ciRoot a ]

attemptAdjunction :: ChartItem -> ChartItem -> Maybe ChartItem
attemptAdjunction pItem aItem | ciRoot aItem && ciAux aItem =
 -- trace ("try adjoining " ++ (showItem aItem) ++ " into " ++ (showItem pItem)) $
 do let aRoot = ciNode aItem
        aFoot = (foot.ttree.ciSourceTree) aItem-- could be pre-computed?
        pNode = ciNode pItem
    (newTop, newBot, subst) <- unifyPair (gup pNode, gdown pNode) 
                                         (gup aRoot, gdown aFoot)
    let newNode = pNode { gaconstr = False, gup = newTop, gdown = newBot }
    return $ combineWith newNode subst aItem pItem
attemptAdjunction _ _ = error "attemptAdjunction called on non-aux or non-root node"

-- return True if the chart items may be combined with each other; for now, this
-- consists of a semantic check
compatible :: ChartItem -> ChartItem -> Bool
compatible a b =    ( (ciSemantics a) .&. (ciSemantics b) ) == 0
                 && ( (ciPolpaths  a) .|. (ciPolpaths  b) ) /= 0
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
           -- trace (ckyShow name agendaItem chart) $ 
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
 do (newTop, subst1) <- unifyFeat t1 t2
    (newBot, subst2) <- unifyFeat (replace subst1 b1) (replace subst1 b2)
    return (newTop, newBot, subst1 ++ subst2)
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


