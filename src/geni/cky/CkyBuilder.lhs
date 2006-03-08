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

GenI currently has two backends, SimpleBuilder (chapter
\ref{cha:SimpleBuilder}) and this module.  This backend does not attempt
to build derived trees at all.  We construct packed derivation trees
using the CKY algorithm for TAGs, and at the very end, we unpack the
results directly into an automaton.  No derived trees here!

\begin{code}
module CkyBuilder
 ( -- builder
   ckyBuilder,
   CkyStatus(..),
   -- chart item
   CkyItem(..), ChartId,
   ciAdjDone, ciRoot,
   extractDerivations,
   -- automaton stuff (for the graphical debugger)
   mJoinAutomata, mAutomatonPaths, emptySentenceAut, unpackItemToAuts,
   --
   bitVectorToSem, findId,
 )
where
\end{code}

\ignore{
\begin{code}

import Control.Exception ( assert )
import Control.Monad
  (unless, foldM)

import Control.Monad.State
  (State, gets, get, put, modify, runState, execStateT )
import Data.Bits ( (.&.), (.|.) )
import Data.List ( delete, find, span, (\\) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Tree

import Btypes
  ( unify, collect
  , Flist
  , Replacable(..), Subst
  , GNode(..), GType(Subs, Foot, Other)
  , GeniVal(GVar),
  , Ptype(Auxiliar)
  , root, foot
  , unifyFeat )

import Automaton
  ( NFA(NFA, transitions, states), isFinalSt, finalSt, finalStList, startSt, addTrans )
import qualified Builder as B
import Builder
  ( SentenceAut, incrCounter, num_iterations, chart_size,
    SemBitMap, semToBitVector, bitVectorToSem, defineSemanticBits,
  )
import Configuration ( Params, setChartsharing, orderedsubs )
import General
  ( combinations, treeLeaves, BitVector, geniBug )
import Polarity
  ( automatonPaths )
import Statistics ( Statistics )
import Tags
  ( TagElem, tidnum, ttree, tsemantics, ttype,
    ts_tbUnificationFailure
  )

-- -- Debugging stuff
-- import Data.List ( intersperse )
-- import Debug.Trace
-- import General ( showBitVector )
-- import Tags ( idname )
--
-- ckyShow name item chart =
--   let showChart = show $ length chart
--       pad s n = s ++ (take (n - length s) $ repeat ' ')
--   in concat $ intersperse "\t" $
--        [ pad name 10, showChart
--        , pad (idname $ ciSourceTree item) 10
--        , pad (showItemSem item) 5
--        , show $ ciNode item ]
--
-- showItems = unlines . (map showItem)
-- showItem i = (idname.ciSourceTree) i ++ " " ++ show (ciNode i) ++ " " ++  (showItemSem i)
-- showItemSem = (showBitVector 5) . ciSemantics
\end{code}
}

\section{Implementing the Builder interface}

\begin{code}
ckyBuilder = B.Builder
  { B.init = initBuilder
  , B.step = generateStep
  , B.stepAll  = B.defaultStepAll ckyBuilder
  , B.finished = null.theAgenda
  , B.unpack   = \s -> concatMap (unpackItem s) $ theResults s
  , B.run = run
  }

-- | Performs surface realisation from an input semantics and a lexical selection.
run input config =
  let stepAll = B.stepAll ckyBuilder
      init    = B.init ckyBuilder
      -- 1 run the setup stuff
      -- (force chart sharing!) FIXME: this needs to be looked into
      (combos, _, input2) = B.preInit input $
                            setChartsharing True config
      -- 2 call the init stuff
      cands = assert (length combos == 1) $ head combos
      (iSt, iStats) = init (input2 { B.inCands = cands }) config
      -- 3 step thorugh the whole thing
  in runState (execStateT stepAll iSt) iStats
\end{code}

The rest of the builder interface is implemented below.  I just
wanted to put the front-end functions up on top.

% --------------------------------------------------------------------
\section{Key types}
% --------------------------------------------------------------------

\subsection{CkyState and CkyStatus}

This terminology might be a bit confusing: \verb!CkyState! is just a
\verb!BuilderState! monad parameterised over \verb!CkyStatus!.  So,
status contains the actual data and state handles all the monadic stuff.

\begin{code}
type CkyState a = B.BuilderState CkyStatus a

data CkyStatus = S
    { theAgenda    :: Agenda
    , theChart     :: Chart
    , theTrash   :: Trash
    , tsemVector :: BitVector -- the semantics in bit vector form
    , gencounter :: Integer
    , genconfig  :: Params
    , theRules   :: [CKY_InferenceRule]
    , theResults :: [CkyItem]
    , genAutCounter :: Integer -- allocation of node numbers
    }

type Agenda = [CkyItem]
type Chart  = [CkyItem]
type Trash = [CkyItem]
\end{code}

Note the theTrash is not actually essential to the operation of the
generator; it is for pratical debugging of grammars.  Instead of
trees dissapearing off the face of the debugger; they go into the
trash where the user can inspect them and try to figure out why they
went wrong.

\subsubsection{CkyState getters and setters}

\begin{code}
addToAgenda :: CkyItem -> CkyState ()
addToAgenda te = do
  modify $ \s -> s{ theAgenda = te : (theAgenda s) }

addToResults :: CkyItem -> CkyState ()
addToResults te = do
  modify $ \s -> s{ theResults = te : (theResults s) }

updateAgenda :: Agenda -> CkyState ()
updateAgenda a = do
  modify $ \s -> s{ theAgenda = a }

addToChart :: CkyItem -> CkyState ()
addToChart te = do
  modify $ \s -> s { theChart = te : (theChart s) }
  incrCounter chart_size 1

addToTrash :: CkyItem -> String -> CkyState ()
addToTrash item err = do
  let item2 = item { ciDiagnostic = err:(ciDiagnostic item) }
  modify $ \s -> s { theTrash = item2 : (theTrash s) }
\end{code}

\subsection{Chart items}

-- TODO: decide if we want this to be an instant of Replacable
\begin{code}
data CkyItem = CkyItem
  { ciNode       :: GNode
  -- things which should never change
  , ciSourceTree    :: TagElem
  , ciOrigVariables :: [GeniVal]
  --
  , ciPolpaths   :: BitVector
  , ciSemantics  :: BitVector
  , ciAdjPoint   :: Maybe ChartId
  -- | the semantics of the item when it was first initialised
  , ciInitialSem :: BitVector
  -- | unique identifier for this item
  , ciId         :: ChartId
  -- names of the sisters of this node in its tree
  , ciRouting    :: RoutingMap
  -- used by the next leaf rule (if active)
  , ciPayload    :: [CkyItem]
  -- variable replacements to accumulate
  , ciSubsts     :: Subst -- do we actually need this if we have ciVariables?
  -- a list of genivals which were variables when the node was
  -- first initialised
  , ciVariables  :: [GeniVal]
  -- we keep a SemBitMap strictly to help display the semantics
  , ciSemBitMap  :: SemBitMap
  -- what side of the spine are we on? (left if initial tree: no spine)
  , ciTreeSide       :: TreeSide
  -- if there are things wrong with this item, what?
  , ciDiagnostic :: [String]
  -- what is the set of the ways you can produce this item?
  , ciDerivation :: [ ChartOperation ]
  }

type ChartId = Integer

-- | note that the order is always active item, followed by passive item
data ChartOperation = SubstOp    ChartId  ChartId
                    | AdjOp      ChartId  ChartId
                    | NullAdjOp  ChartId
                    | KidsToParentOp [ChartId]
                    | InitOp
 deriving Show -- for debugging

type ChartOperationConstructor = ChartId -> ChartId -> ChartOperation

ciRoot, ciFoot, ciSubs, ciAdjDone, ciAux, ciInit :: CkyItem -> Bool
ciRoot  i = (gnname.ciNode) i == (gnname.root.ttree.ciSourceTree) i
ciFoot  i = (gtype.ciNode) i == Foot
ciSubs  i = (gtype.ciNode) i == Subs
ciAdjDone   = gaconstr.ciNode
ciComplete i = (not.ciSubs $ i) && ciAdjDone i
ciAux   i = (ttype.ciSourceTree) i == Auxiliar
ciInit = not.ciAux

data TreeSide = LeftSide | RightSide | OnTheSpine
 deriving (Eq)

ciLeftSide, ciRightSide, ciOnTheSpine :: CkyItem -> Bool
ciLeftSide   i = ciTreeSide i == LeftSide
ciRightSide  i = ciTreeSide i == RightSide
ciOnTheSpine i = ciTreeSide i == OnTheSpine


-- basically, an inverted tree
-- from node name to a list of its sisters on the left,
-- a list of its sisters on the right, its parent
type RoutingMap = Map.Map String ([String], [String], GNode)
\end{code}

% --------------------------------------------------------------------
\section{Initialisation}
% --------------------------------------------------------------------

\begin{code}
initBuilder :: B.Input -> Params -> (CkyStatus, Statistics)
initBuilder input config =
  let (sem, _) = B.inSemInput input
      bmap  = defineSemanticBits sem
      hasOs = orderedsubs config
      cands = concatMap (initTree hasOs bmap) $ B.inCands input
      initS = S
       { theAgenda  = []
       , theChart = []
       , theTrash = []
       , theResults = []
       , theRules = map fst ckyRules
       , tsemVector    = semToBitVector bmap sem
       , gencounter    = 0
       , genAutCounter = 0
       , genconfig  = config }
  in B.unlessEmptySem input config $
     runState (execStateT (mapM dispatchNew cands) initS) (B.initStats config)
\end{code}

\subsection{Initialising a chart item}
\label{fn:cky:initTree}

\begin{code}
initTree :: Bool -> SemBitMap -> (TagElem,BitVector) -> [CkyItem]
initTree ordered bmap tepp@(te,_) =
  let semVector    = semToBitVector bmap (tsemantics te)
      createItem l n = (leafToItem l tepp n)
       { ciSemantics  = semVector
       , ciInitialSem = semVector
       , ciSemBitMap = bmap
       , ciRouting   = decompose te
       , ciVariables = map GVar $ Set.toList $ collect te Set.empty }
      --
      (left,right) = span (\n -> gtype n /= Foot) $ treeLeaves $ ttree te
      items = map (createItem True) left  ++ map (createItem False) right
  in if ordered
     then foldr (\i p -> [i { ciPayload = p }]) [] items
     else items

leafToItem :: Bool
           -- ^ is it on the left of the foot node? (yes if there is none)
           -> (TagElem, BitVector)
           -- ^ what tree does it belong to
           -> GNode
           -- ^ the leaf to convert
           -> CkyItem
leafToItem left (te,pp) node = CkyItem
  { ciNode       = node
  , ciSourceTree = te
  , ciPolpaths   = pp
  , ciSemantics  = 0  -- to be set
  , ciInitialSem = 0  -- to be set
  , ciId         = -1 -- to be set
  , ciRouting    = Map.empty -- to be set
  , ciOrigVariables = [] -- to be set
  , ciVariables     = [] -- to be set
  , ciPayload       = [] -- to be set
  , ciAdjPoint   = Nothing
  , ciSubsts     = []
  , ciSemBitMap  = Map.empty
  , ciTreeSide   = spineSide
  , ciDiagnostic   = []
  , ciDerivation   = [ InitOp ] }
  where
   spineSide | left                = LeftSide
             | gtype node == Foot  = OnTheSpine
             | otherwise           = RightSide

-- | explode a TagElem tree into a bottom-up routing map
decompose :: TagElem -> RoutingMap
decompose te = helper (ttree te) Map.empty
  where
  helper :: Tree GNode -> RoutingMap -> RoutingMap
  helper (Node _ []) smap = smap
  helper (Node p kidNodes) smap =
    let kids     = [ gnname x | (Node x _) <- kidNodes ]
        addKid k = Map.insert k (left, right, p)
          where (left, right') = span (/= k) kids
                right = if null right' then [] else tail right'
        smap2    = foldr addKid smap kids
    in -- recurse to add routing info for child nodes
       foldr helper smap2 kidNodes
\end{code}

% --------------------------------------------------------------------
\section{Generate}
% --------------------------------------------------------------------

Each iteration of the surface realisation step involves picking an item
off the agenda, applying all the relevant inference rules to it, and
dispatching the results.  Lather, rinse, repeat.  At some point we just
run out of things on the agenda and stop.

Well, ok, there are ways that this thing could loop infinitely: for
example, having null semantic lexical items would be a very bad thing.

\begin{code}
generateStep :: CkyState ()
generateStep =
 do -- this check may seem redundant with generate, but it's needed
    -- to protect against a user who calls generateStep on a finished
    -- state
    isFinished <- gets finished
    unless (isFinished) generateStep2

generateStep2 :: CkyState ()
generateStep2 =
  do st <- get
     -- incrGeniter 1
     agendaItem <- selectAgendaItem
     -- try the inference rules
     let chart = theChart st
         apply rule = rule agendaItem chart
         results = map apply (theRules st)
         -- see comments below about ordered substitution
         releasePayload = not (null results) || ciComplete agendaItem
         payload = if releasePayload && (orderedsubs $ genconfig st)
                   then ciPayload agendaItem else []
     -- put all newly generated items into the right pigeon-holes
     -- trace (concat $ zipWith showRes ckyRules results) $
     mapM dispatchNew $ payload ++ (concat results)
     addToChart agendaItem
     incrCounter num_iterations 1
     return ()

selectAgendaItem :: CkyState CkyItem
selectAgendaItem = do
  a <- gets theAgenda
  updateAgenda (tail a)
  return (head a)

finished :: CkyStatus -> Bool
finished = null.theAgenda
\end{code}

% --------------------------------------------------------------------
\section{CKY Rules}
% --------------------------------------------------------------------

Our surface realiser is defined by a set of inference rules.  Since we are
using an agenda-based algorithm, we define our inference rules to take two
arguments: the agenda item and the entire chart.  It is up to the inference
rule to filter the chart for the items which can combine with the agenda item.
If a rule is not applicable, it should simply return the empty list.

\begin{code}
type InferenceRule a = a -> [a] -> [a]
type CKY_InferenceRule = InferenceRule CkyItem

instance Show CKY_InferenceRule where
  show _ = "cky inference rule"
\end{code}

% FIXME: diagram and comment

\begin{code}
ckyRules :: [ (CKY_InferenceRule, String) ]
ckyRules =
 [ (kidsToParentRule, "kidsToP")
 , (substRule       , "subst")
 , (nonAdjunctionRule, "nonAdj")
 , (activeAdjunctionRule, "actAdjRule")
 , (passiveAdjunctionRule, "psvAdjRule") ]

-- | CKY non adjunction rule - creates items in which
-- we do not apply any adjunction
-- this rule also doubles as top
nonAdjunctionRule item _ =
  let node  = ciNode item
      node2 = node { gaconstr = True }
  in if gtype node /= Other || ciAdjDone item then []
     else [ item { ciNode = node2
                 , ciPayload = []
                 , ciDerivation = [ NullAdjOp $ ciId item ] } ]

-- | CKY parent rule
kidsToParentRule item chart | ciComplete item =
 do (leftS,rightS,p)  <- Map.lookup (gnname node) (ciRouting item)
    let mergePoints kids =
         case mapMaybe ciAdjPoint (item:kids) of
          []  -> Nothing
          [x] -> Just x
          _   -> error "multiple adjunction points in kidsToParentRule?!"
        combine p kids = do
          let unifyOnly (x, _) y = maybeToList $ unify x y
          newVars <- foldM unifyOnly (ciVariables item,[]) $
                     map ciVariables kids
          let newSubsts = concatMap ciSubsts (item:kids)
              newSide | all ciLeftSide   kids = LeftSide
                      | all ciRightSide  kids = RightSide
                      | any ciOnTheSpine kids = OnTheSpine
                      | otherwise = geniBug $ "kidsToParentRule: Weird situtation involving tree sides"
              newItem = item
               { ciNode      = replace newSubsts p
               , ciSubsts    = newSubsts
               , ciAdjPoint  = mergePoints kids
               , ciVariables = fst newVars
               , ciTreeSide     = newSide
               , ciDerivation   = [ KidsToParentOp $ map ciId kids ]
               , ciPayload      = []
               }
          return $ foldr combineVectors newItem kids
    let leftMatches  = map matches leftS
        rightMatches = map matches rightS
        allMatches = leftMatches ++ ([item] : rightMatches)
    -- trace (" relevant chart: (" ++ (show $ length relChart) ++ ") " ++ showItems relChart) $
    -- trace (" routing info: " ++ show (s,p,r)) $
    -- trace (" matches: (" ++ (show $ length allMatches) ++ ") " ++ (concat $ intersperse "-\n" $ map showItems allMatches)) $
    combinations allMatches >>= combine p
 where
   node     = ciNode item
   sourceOf = tidnum.ciSourceTree
   --
   relevant c = (sourceOf c == sourceOf item) && ciComplete c
                -- make sure the semantics only overlap in the initial parts
                && (ciSemantics c) .&. (ciSemantics item) == (ciInitialSem item)
   relChart = filter relevant chart
   --
   matches :: String -> [CkyItem]
   matches sis = [ c | c <- relChart, (gnname.ciNode) c == sis ]
kidsToParentRule _ _ = [] -- if this rule is not applicable to the item at hand
\end{code}

\subsection{Substitution}

The substitution rule has two variants: either the agenda item is active,
meaning it is a root node and is trying to subsitute into something; or it
is passive, meaning that is a substitution node waiting to receive
substitution on something.

\begin{code}
-- | CKY subst rules
substRule item chart = catMaybes $
  if ciSubs item
  then [ attemptSubst item r | r <- chart, compatibleForSubstitution r item ]
  else [ attemptSubst s item | s <- chart, compatibleForSubstitution item s ]

-- | unification for substitution
attemptSubst :: CkyItem -> CkyItem -> Maybe CkyItem
attemptSubst sItem rItem | ciSubs sItem =
 do let rNode = ciNode rItem
        sNode = ciNode sItem
    (up, down, subst) <- unifyGNodes sNode (ciNode rItem)
    let newNode = rNode { gnname = gnname sNode
                        , gup = up, gdown = down }
        newItem  = combineWith SubstOp newNode subst rItem sItem
    return newItem
attemptSubst _ _ = error "attemptSubst called on non-subst node"

-- | return True if the first item may be substituted into the second
--   as long as unification and all the nasty details work out
compatibleForSubstitution :: CkyItem -- ^ active item
                          -> CkyItem -- ^ passive item
                          -> Bool
compatibleForSubstitution a p =
  ciRoot a && ciComplete a && ciInit a
  && ciSubs p
  && compatible a p
\end{code}

\subsection{Adjunction}

As with substitution, the adjunction rule has two variants: either the agenda
item is active, meaning it is the root node of an auxliary tree is trying
to adjoin into something; or it is passive, meaning it is a node which is
waiting to receive adjunction.

Note that unlike the substitution rule, we have to split these two variants
into two actual rules.  This is because we also want auxiliary tree nodes
to be able to receive adjunction and not just perform it!

\begin{code}
-- | CKY adjunction rule: note - we need this split into two rules because
-- both variants could fire at the same time, for example, the passive variant
-- to adjoin into the root of an auxiliary tree, and the active variant because
-- it is an aux tree itself and it wants to adjoin somewhere
activeAdjunctionRule item chart | ciRoot item && ciAux item =
 mapMaybe (\p -> attemptAdjunction p item)
   [ p | p <- chart, compatibleForAdjunction item p ]
activeAdjunctionRule _ _ = [] -- if not applicable

-- | CKY adjunction rule: we're just a regular node, minding our own business
-- looking for an auxiliary tree to adjoin into us
passiveAdjunctionRule item chart =
 mapMaybe (attemptAdjunction item)
   [ a | a <- chart, compatibleForAdjunction a item ]

attemptAdjunction :: CkyItem -> CkyItem -> Maybe CkyItem
attemptAdjunction pItem aItem | ciRoot aItem && ciAux aItem =
 -- trace ("try adjoining " ++ (showItem aItem) ++ " into " ++ (showItem pItem)) $
 do let aRoot = ciNode aItem
        aFoot = (foot.ttree.ciSourceTree) aItem-- could be pre-computed?
        pNode = ciNode pItem
    (newTop, _ , subst) <- unifyPair (gup pNode, gdown pNode)
                                     (gup aRoot, gdown aFoot)
    let newNode = pNode { gaconstr = False, gup = newTop, gdown = [] }
        newItem = combineWith AdjOp newNode subst aItem pItem
    return newItem
attemptAdjunction _ _ = error "attemptAdjunction called on non-aux or non-root node"

-- | return True if the first item may be adjoined into the second
--   as long as unification and all the nasty details work out
compatibleForAdjunction :: CkyItem -- ^ active item
                        -> CkyItem -- ^ passive item
                        -> Bool
compatibleForAdjunction a p =
  ciAux a && ciRoot a && ciAdjDone a
  && (gtype.ciNode) p == Other && (not.ciAdjDone) p
  && compatible a p
\end{code}

\subsection{Helpers for inference rules}

\begin{code}
isLexeme :: GNode -> Bool
isLexeme = not.null.glexeme

-- | return True if the chart items may be combined with each other; for now, this
-- consists of a semantic check
compatible :: CkyItem -> CkyItem -> Bool
compatible a b =    ( (ciSemantics a) .&. (ciSemantics b) ) == 0
                 && ( (ciPolpaths  a) .|. (ciPolpaths  b) ) /= 0
\end{code}

To factorise the construction of new items, we provide two functions for combining
two chart items.  \fnreflite{combineVectors} merely combines the easy stuff (the
semantic bit maps and the polarity paths).  \fnreflite{combineWith} does the
heavier stuff like the list of open variables and the derivation for the new item.
The reason we expose \fnreflite{combineVectors} as a separate function is because
the \fnreflite{kidsToParentsRule} needs it.

\begin{code}
combineVectors :: CkyItem -> CkyItem -> CkyItem
combineVectors a b =
  b { ciSemantics = (ciSemantics a) .|. (ciSemantics b)
    , ciPolpaths  = (ciPolpaths  a) .&. (ciPolpaths  b)
    , ciSemBitMap =  ciSemBitMap a }

combineWith :: ChartOperationConstructor -- ^ how did we get the new item?
            -> GNode -> Subst -> CkyItem -> CkyItem -> CkyItem
combineWith operation node subst active passive =
  combineVectors active $
  passive { ciNode      = node
          , ciSubsts    = (ciSubsts passive) ++ subst
          , ciPayload      = []
          , ciVariables = replace subst (ciVariables passive)
          , ciDerivation   = [ operation (ciId active) (ciId passive) ] }
\end{code}

\paragraph{unifyTagNodes} performs feature structure unification
on TAG nodes.  First we try unification on the top node.  We
propagate any results from that unification and proceed to trying
unification on the bottom nodes.  If succesful, we return the
results of both unifications and a list of substitutions to
propagate.  Otherwise we return Nothing.

\begin{code}
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

% --------------------------------------------------------------------
\section{Dispatching new chart items}
% --------------------------------------------------------------------

Dispatching consists of assigning a chart item to the right part of the
chart (agenda, trash, results list, etc).  This is implemented as a
series of filters which can either fail or succeed.

Counter-intuitively, success is defined as returning \verb!Nothing!.
Failure is defined as return \verb!Just!, because if a filter fails, it
has the right to modify the item for the next filter.  For example, the
top and bottom unification filter succeeds if it \emph{cannot} unify
the top and bottom features of a node.  It suceeds by putting the item
into the trash and returning Nothing.  If it \emph{can} perform top and
bottom unification, we want to return the item where the top and bottom
nodes are unified.  Failure is success, war is peace, freedom is
slavery, erase is backspace.

\begin{code}
dispatchNew :: CkyItem -> CkyState ()
dispatchNew item =
 do let tryFilter Nothing _        = return Nothing
        tryFilter (Just newItem) f = f newItem
    -- keep trying dispatch filters until one of them suceeds
    foldM tryFilter (Just item) [ dispatchTbFailure
                                , dispatchRedundant
                                , dispatchResults
                                , dispatchToAgenda ]
    return ()

dispatchToAgenda, dispatchRedundant, dispatchResults, dispatchTbFailure :: CkyItem -> CkyState (Maybe CkyItem)

-- | Trivial dispatch filter: always put the item on the agenda and return
--   Nothing
dispatchToAgenda item =
   do addToAgenda item
      return Nothing

-- | If the item can merge with another, merge it with the equivalent
--   item and return Nothing.
--   If the item is indeed unique, return (Just $ setId item)
dispatchRedundant item =
  do st <- get
     let chart = theChart st
         mergeEquivItems o =
           let isEq = canMerge o item
           in  (isEq, if isEq then mergeItems o item else o)
         (isEq, newChart) = unzip $ map mergeEquivItems chart
     --
     if or isEq
        then -- trace (ckyShow "-> merge" item []) $
             do put ( st {theChart = newChart} )
                return Nothing
        else do s <- get
                let counter = gencounter s
                put $ s { gencounter = counter + 1 }
                return $ Just $ item { ciId = counter }

-- | If it is a result, put the item in the results list.
--   Otherwise, return (Just unmodified)
dispatchResults item =
 do st <- get
    let synComplete = ciInit item && ciRoot item && ciAdjDone item
        semComplete = tsemVector st == ciSemantics item
        --
    if (synComplete && semComplete )
       then -- trace ("isResult " ++ showItem item) $
            addToResults item >> return Nothing
       else return $ Just item

-- | This filter requires another inversion in thinking.  It suceeds
--   if tb unification fails by dispatching to the trash and returning
--   Nothing.  If tb unification suceeds, it returns (Just newItem),
--   where newItem has its top and bottom nodes unified.
dispatchTbFailure itemRaw =
 case tbUnify itemRaw of
  Nothing ->
    do addToTrash itemRaw ts_tbUnificationFailure
       return Nothing
  Just item -> return $ Just item

tbUnify :: CkyItem -> Maybe CkyItem
-- things for which tb unification is not relevant
tbUnify item | ciFoot item = return item
tbUnify item | (not.ciAdjDone) item = return item
-- ok, here, we should do tb unification
tbUnify item =
 do let node = ciNode item
    (newTop, sub1) <- unifyFeat (gup node) (gdown node)
    -- it's not enough if t/b unification succeeds by itself
    -- we also have to check that these unifications propagate alright
    let origVars = ciOrigVariables item
        treeVars = ciVariables item
        nodeVars = replace sub1 origVars
    (newVars, sub2) <- unify treeVars nodeVars
    return $ item
      { ciNode = node { gup = newTop, gdown = [] }
      , ciSubsts = ciSubsts item ++ sub2
      , ciVariables = newVars }
\end{code}

% --------------------------------------------------------------------
\subsection{Equivalence classes}
\label{sec:cky:merging}
% --------------------------------------------------------------------

\fnlabel{canMerge} returns true if two chart items are allowed to merge.
We do not allow items to merge when they are not "complete", because that
would complicate things like the right sister rule.

\begin{code}
canMerge :: CkyItem -> CkyItem -> Bool
canMerge c1 c2 = ciComplete c1 && ciComplete c2 && stuff c1 == stuff c2
  where stuff x = ( ciNode x, ciSourceTree x, ciSemantics x, ciPolpaths x )
\end{code}

\fnlabel{mergeItems} combines two chart items into one, with the
assumption being that you have already determined that they can be
merged.  Information from the second ``slave'' item is merged
into information from the first ``master'' item.

\begin{code}
mergeItems :: CkyItem -> CkyItem -> CkyItem
mergeItems master slave =
 master { ciDerivation = ciDerivation master ++ (ciDerivation slave) }
\end{code}

% --------------------------------------------------------------------
\section{Unpacking the chart}
% --------------------------------------------------------------------

\begin{code}
unpackItem :: CkyStatus -> CkyItem -> [B.UninflectedSentence]
unpackItem st it =
  mAutomatonPaths $ uncurry mJoinAutomata $ unpackItemToAuts st it

type SentenceAutPairMaybe = (Maybe SentenceAut, Maybe SentenceAut)

unpackItemToAuts :: CkyStatus -> CkyItem
                 -- left and right automata
                 -> SentenceAutPairMaybe
unpackItemToAuts st item =
 case map aut derivations of
      []     -> (Nothing, Nothing)
      (a:as) -> foldr pairUnion a as
 where
  pairUnion (l1,r1) (l2,r2) = (mUnionAutomata l1 l2, mUnionAutomata r1 r2)
  derivations = ciDerivation item
  retrieve = findIdOrBug st
  -- these are fleshed out in the paragraphs below
  aut (KidsToParentOp k) = unpackKidsToParentOp st $ map retrieve k
  aut (NullAdjOp p)      = unpackNullAdjOp      st $ retrieve p
  aut (SubstOp a p)      = unpackSubstOp st (retrieve a) (retrieve p)
  aut (AdjOp a p)        = unpackAdjOp   st (retrieve a) (retrieve p)
  aut InitOp             = unpackInitOp  st item
\end{code}

\paragraph{Leaf nodes}

\begin{code}
unpackInitOp :: CkyStatus -> CkyItem -> SentenceAutPairMaybe
unpackInitOp _ item =
  let node = ciNode item
      -- we have to add a transition for each choice in the lexical
      -- atomic disjunction
      lexAut = foldr (\l a -> addTrans a 0 (via l) 1) iAut (glexeme node)
      via l = Just (l, gup node)
      iAut = emptySentenceAut { startSt = 0
                              , finalStList = [1]
                              , states = [[0,1]]}
  in if isLexeme node
     then case ciTreeSide item of
          LeftSide   -> (Just lexAut, Nothing)
          RightSide  -> (Nothing, Just lexAut)
          OnTheSpine -> (Nothing, Nothing)
     else (Nothing, Nothing)

emptySentenceAut :: SentenceAut
emptySentenceAut =
  NFA { startSt     = (-1)
      , isFinalSt   = Nothing
      , finalStList = []
      , transitions = Map.empty
      , states      = [[]] }
\end{code}

\paragraph{Null adjunction} is a trivial case; we just propagate the automaton upwards.

\begin{code}
unpackNullAdjOp :: CkyStatus -> CkyItem -> SentenceAutPairMaybe
unpackNullAdjOp st psv = unpackItemToAuts st psv
\end{code}

\paragraph{Substitution} would be as simple as null adjunction, were it
not for auxiliary trees.  When dealing with an auxiliary tree, we need
to be careful which side of the spine we substitute into.  For those of
you not so familiar with TAG, the spine is the path from root node to
the foot node of an auxiliary tree.

If we're on the left side of the spine, we propagate into the left
automaton.  Likewise, we propagate into the right autamaton if we're on
the right side of the spine.  If we're trying to substitute \emph{into}
the spine, we're in trouble.

\begin{code}
unpackSubstOp :: CkyStatus -> CkyItem -> CkyItem -> SentenceAutPairMaybe
unpackSubstOp st act psv =
  case ciTreeSide psv of
    LeftSide   -> (actAut, Nothing)
    RightSide  -> (Nothing, actAut)
    OnTheSpine -> geniBug $ "Tried to substitute on the spine!"
  where actAut = fst $ unpackItemToAuts st act
\end{code}

\paragraph{Adjunction} involves joining the left sides of both items
together as well as the right side.  This is probably best explained
with a picture:

FIXME: insert figure

\begin{code}
unpackAdjOp :: CkyStatus -> CkyItem -> CkyItem -> SentenceAutPairMaybe
unpackAdjOp st act psv =
  let (actL, actR) = unpackItemToAuts st act
      (psvL, psvR) = unpackItemToAuts st psv
      newAutL = mJoinAutomata actL psvL
      newAutR = mJoinAutomata psvR actR
      newAut  = mJoinAutomata newAutL newAutR
 in case ciTreeSide psv of
      LeftSide   -> (newAut,  Nothing)
      RightSide  -> (Nothing, newAut)
      OnTheSpine -> (newAutL, newAutR)
\end{code}

\paragraph{The kids to parents rule} is complicated because of auxiliary
trees.  As usual, there are three cases:

\begin{itemize}
\item On the left of the spine: we concatenate all the left
      automata of the kids
\item On the right of the spine: we concatenate all the right
      automata of the kids
\item On the spine itself: we concatenate all the left automata
      of the stuff on the left of the spine and propagate that
      as our left side.  Similarly, we concatenate all the right
      automata of the stuff on the right of the spine and send
      that up the right side.
\end{itemize}

\begin{code}
unpackKidsToParentOp :: CkyStatus -> [CkyItem] -> SentenceAutPairMaybe
unpackKidsToParentOp st kids =
  let (bef, aft) = span (not.ciOnTheSpine) kids
      (befL, befR) = unzip $ map (unpackItemToAuts st) bef
      (_   , aftR) = unzip $ map (unpackItemToAuts st) aft
      concatAut auts = foldr mJoinAutomata Nothing auts
  in if null aft
     -- two cases in one! (we expect one of these to be Nothing)
     -- we're either on the left or the right of the spine
     then ( concatAut befL, concatAut befR )
     -- we are on the spine
     else ( concatAut befL, concatAut aftR )
\end{code}

\subsection{Core automaton stuff}

Note: you might be tempted to move this code to the generic Automaton library.
In order to do this, you will have to introduce a geniric notion of
state-renaming to the library.  I didn't want to bother with any of that.

\begin{code}
mUnionAutomata :: Maybe SentenceAut -> Maybe SentenceAut -> Maybe SentenceAut
mUnionAutomata Nothing mAut2 = mAut2
mUnionAutomata mAut1 Nothing = mAut1
mUnionAutomata (Just aut1) (Just aut2) = Just $ unionAutomata aut1 aut2

-- | Merge two sentence automata together.  This essentially calculates the
-- union of the two automata and "pinches" their final states together.
-- FIXME: could be much more sophisticated and produce smaller automata!
unionAutomata :: SentenceAut -> SentenceAut -> SentenceAut
unionAutomata aut1 rawAut2 =
 let -- rename all the states in aut2 so that they don't overlap
     aut1Max = foldr max (-1) $ concat $ states aut1
     aut2 = incrStates (1 + aut1Max) rawAut2
     -- make the start state of the new automaton also transition
     -- everything that the from the start state of aut2 transitions to
     t1 = transitions aut1
     t2 = transitions aut2
     aut2Start = startSt aut2
     addAut2Trans = Map.unionWith (++) $ Map.findWithDefault Map.empty aut2Start t2
     newT1 = Map.adjust addAut2Trans (startSt aut1) t1
     newT2 = Map.delete aut2Start t2
 in  aut1 { states      = [ delete aut2Start $ concat $ states aut1 ++ states aut2 ]
          , transitions = Map.union newT1 newT2
          , isFinalSt   = do -- in the Maybe Monad
                             f1 <- isFinalSt aut1
                             f2 <- isFinalSt aut2
                             return $ \s -> f1 s || f2 s
          , finalStList = finalStList aut1 ++ finalStList aut2 }
\end{code}

It's important not to confuse \fnreflite{joinAutomata} with
\fnreflite{unionAutomata}.  Joining automata is basically concatenation,
putting the second automaton after the first one.
Interestingly, their implementations have a lot in common.
FIXME: it might be worth refactoring the two.

\begin{code}
mJoinAutomata :: Maybe SentenceAut -> Maybe SentenceAut -> Maybe SentenceAut
mJoinAutomata Nothing mAut2 = mAut2
mJoinAutomata mAut1 Nothing = mAut1
mJoinAutomata (Just aut1) (Just aut2) = Just $ joinAutomata aut1 aut2

-- | Concatenate two sentence automata.  This merges the final state of the
-- first automaton into the initial state of the second automaton.
joinAutomata aut1 rawAut2 =
 let -- rename all the states in aut2 so that they don't overlap
     aut1Max = foldr max (-1) $ concat $ states aut1
     aut2 = incrStates (1 + aut1Max) rawAut2
     -- replace all transitions to aut1's final st by
     -- transitions to aut2's start state
     aut1Final = finalSt aut1
     t1 = transitions aut1
     t2 = transitions aut2
     maybeBridge s = if s `elem` aut1Final then startSt aut2 else s
     newT1 = Map.map (Map.map $ map maybeBridge) t1
     newStates1 = map (\\ aut1Final) $ states aut1
     --
 in  aut1 { states      = [ concat $ newStates1 ++ states aut2 ]
          , transitions = Map.union newT1 t2
          , isFinalSt   = isFinalSt aut2
          , finalStList = finalStList aut2 }

incrStates :: Int -> SentenceAut -> SentenceAut
incrStates prefix aut =
 let -- increment a state
     addP_s = (prefix +)
     -- increment all the states involved in a transition
     addP_t (k,e) = (addP_s k, Map.map (map addP_s) e)
 in aut { startSt     = addP_s (startSt aut)
        , states      = map (map addP_s) $ states aut
        , transitions = Map.fromList $ map addP_t $
                        Map.toList   $ transitions aut
        , finalStList = map addP_s $ finalStList aut }

mAutomatonPaths :: (Ord st, Ord ab) => Maybe (NFA st ab) -> [[ab]]
mAutomatonPaths Nothing  = []
mAutomatonPaths (Just x) = automatonPaths x
\end{code}

\subsection{Item history}

We don't ever really need to calculate the derivation tree for an item.
Don't get me wrong, we certainly calculate something which looks a lot
like a derivation tree and contains more or less the same stuff, but not
a derivation tree per se.

On the otherhand, debugging the generator is \emph{much} easier if you
can get a graphical representation for an item.  This is like a
derivation tree with way too much detail.  We calculate a tree-like
representation of the history of inference rule applications for this
item.

Note that because of equivalence classes, an item can be seen as having
more than one derivation.  We abstract around this fact simply by
implementing the function with a \verb!List! monad.

\begin{code}
-- | Returns all the derivations trees for this item: note that this is
-- not a TAG derivation tree but a history of inference rule applications
-- in tree form
extractDerivations :: CkyStatus -> CkyItem -> [ Tree (ChartId, String) ]
extractDerivations st item =
 do chartOp <- ciDerivation item
    case chartOp of
     KidsToParentOp kids ->
       do kidTrees <- mapM treeFor kids
          createNode "kids" kidTrees
     SubstOp act psv ->
       do actTree <- treeFor act
          let psvTree = Node (psv, "subst") [ actTree ]
          createNode "subst-finish" [psvTree]
     AdjOp act psv ->
       do actTree <- treeFor act
          let psvTree = Node (psv, "adj") [ actTree ]
          createNode "adj-finish"  [psvTree]
     NullAdjOp psv ->
       do psvTree <- treeFor psv
          createNode "no-adj" [psvTree]
     InitOp -> createNode "init" []
 where
   createNode op kids =
     return $ Node (ciId item, op) kids
   treeFor id =
     case findId st id of
       Nothing -> geniBug $ "derivation for item " ++ (show $ ciId item)
                         ++ "points to non-existent item " ++ (show id)
       Just x  -> extractDerivations st x
\end{code}

\subsection{Helpers for unpacking}

\begin{code}
findId :: CkyStatus -> ChartId -> Maybe CkyItem
findId st i = find (\x -> ciId x == i) $ theChart st ++ (theAgenda st) ++ (theResults st) ++ (theTrash st)

-- | The same as 'findId' but calls 'geniBug' if not found
findIdOrBug :: CkyStatus -> ChartId -> CkyItem
findIdOrBug st id =
 case findId st id of
   Nothing -> geniBug $ "Cannot find item in chart with id " ++ (show id)
   Just x  -> x
\end{code}

\section{Optimisations}

\paragraph{Ordered substitution}

The idea is that we to perform substitutions in a fixed order so that we avoid
generating a lot of useless chart items that aren't going to be used in a final
result anyway.

We implement this in two places.  In the initialisation phase, (page
\pageref{fn:cky:initTree}), we avoid placing all the leaf items onto the
agenda.  Instead, we make each leaf node point to the next leaf, as
with a singly linked list, and put the head of that list on the agenda.
The second part of this is implemented below as an inference rule which
takes only complete items (i.e. items for which there is no need to
perform substitution) and releases their payload.

Note that in order for this to work, we also had to introduce a
restriction into chart item merging (page \pageref{sec:cky:merging})
that no two items may merge if they are not complete in the same sense
as this inference rule.  Otherwise, we'd have to think find a way to
make sure that payloads get released correctly (which might not be as
hard as I first thought).


