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
import Data.List ( find, intersperse, span, (\\) )
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

import Automaton
  ( NFA(NFA, transitions, states), isFinalSt, finalSt, finalStList, startSt, addTrans )
import qualified Builder as B
import Builder ( SentenceAut )
import Configuration 
  ( extrapol, rootCatsParam, polarised, Params )
import General 
  ( combinations, treeLeaves, BitVector, showBitVector, geniBug )
import Polarity 
  ( automatonPaths, buildAutomaton, detectPolPaths, lookupAndTweak  )
import Tags 
  (TagElem, TagSite, 
   idname, 
   ttree, tsemantics, 
   tpolpaths, ttype,
   setTidnums,
   ts_tbUnificationFailure)

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
    , tsemVector :: BitVector -- the semantics in bit vector form
    , gencounter :: Integer
    , genconfig  :: Params
    , genstats   :: B.Gstats 
    , theRules   :: [CKY_InferenceRule] 
    , theResults :: [ChartItem] 
    , genAutCounter :: Integer -- allocation of node numbers
    } 

type BState = State BuilderStatus 

ckyBuilder = B.Builder 
  { B.init = initBuilder
  , B.step = generateStep
  , B.stepAll  = B.defaultStepAll ckyBuilder 
  , B.finished = finished -- should add check that step is aux
  , B.stats    = genstats 
  , B.setStats = \t s -> s { genstats = t }
  , B.unpack   = \s -> concatMap (automatonPaths.ciAut_befHole) $ theResults s
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
       , tsemVector    = semToBitVector bmap sem
       , gencounter    = 0 
       , genAutCounter = 0
       , genconfig  = config
       , genstats   = B.initGstats}
  in execState (mapM dispatchNew cands) initS
  
initTree :: SemBitMap -> TagElem -> [ChartItem]
initTree bmap te = 
  let semVector    = semToBitVector bmap (tsemantics te)
      createItem l n = (leafToItem l te n)
       { ciSemantics = semVector
       , ciSemBitMap = bmap
       , ciRouting   = decompose te 
       , ciVariables = map GVar $ Set.toList $ collect te Set.empty }
      --
      (left,right) = span (\n -> gtype n /= Foot) $ treeLeaves $ ttree te
  in map (createItem True) left  ++ map (createItem False) right

leafToItem :: Bool     
           -- ^ is it on the left of the foot node? (yes if there is none)
           -> TagElem  
           -- ^ what tree does it belong to
           -> GNode 
           -- ^ the leaf to convert
           -> ChartItem 
leafToItem left te node = ChartItem 
  { ciNode       = node
  , ciComplete   = False -- (not $ gtype ==  
  , ciSourceTree = te
  , ciPolpaths   = tpolpaths te
  , ciSemantics  = 0  -- to be set 
  , ciId         = -1 -- to be set 
  , ciRouting    = Map.empty -- to be set
  , ciOrigVariables = [] -- to be set
  , ciVariables     = [] -- to be set
  , ciAdjPoint   = Nothing
  , ciSubsts     = [] 
  , ciSemBitMap  = Map.empty 
  , ciAut_befHole = if left then theAutomaton else emptySentenceAut
  , ciAut_aftHole  = if left then emptySentenceAut else theAutomaton
  , ciDiagnostic   = [] 
  , ciDerivation   = [ InitOp ] }
  where 
   theAutomaton = if isLexeme node then lexAut else emptySentenceAut 
   -- add a transition from f -> t via label l
   addTransFor l a = addTrans a 0 (Just (l,[])) 1 
   -- FIXME: note that you'll have to add features later! and make
   -- sure that they are correctly propagated
   -- add a transition for each word in the disjunction
   lexAut  = foldr addTransFor lexAut' (glexeme node)
   lexAut' = emptySentenceAut { startSt = 0, finalStList = [1], states = [[0,1]] }

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

-- basically, an inverted tree
-- from node name to a list of its sisters on the left, 
-- a list of its sisters on the right, its parent
type RoutingMap = Map.Map String ([String], [String], GNode)
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

addToTrash :: ChartItem -> String -> BState ()
addToTrash item err = do 
  s <- get
  let item2 = item { ciDiagnostic = err:(ciDiagnostic item) }
  put s { theTrash = item2 : (theTrash s) }
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
  -- if there is really nothing more than needs to be done to this node
  , ciComplete   :: Bool
  -- things which should never change
  , ciSourceTree    :: TagElem 
  , ciOrigVariables :: [GeniVal]
  --
  , ciPolpaths   :: BitVector
  , ciSemantics  :: BitVector
  , ciAdjPoint   :: Maybe ChartId
  --
  , ciId         :: ChartId 
  -- names of the sisters of this node in its tree
  , ciRouting    :: RoutingMap 
  -- variable replacements to accumulate
  , ciSubsts     :: Subst -- do we actually need this if we have ciVariables? 
  -- a list of genivals which were variables when the node was
  -- first initialised
  , ciVariables  :: [GeniVal]
  -- we keep a SemBitMap strictly to help display the semantics
  , ciSemBitMap  :: SemBitMap
  -- the sentence automaton which corresponds to this item
  , ciAut_befHole :: SentenceAut
  , ciAut_aftHole  :: SentenceAut
  -- if there are things wrong with this item, what?
  , ciDiagnostic :: [String]
  -- what is the set of the ways you can produce this item?
  , ciDerivation :: [ ChartOperation ]
  } 

-- | note that the order is always active item, followed by passive item
data ChartOperation = SubstOp    ChartId  ChartId 
                    | AdjOp      ChartId  ChartId
                    | NullAdjOp  ChartId
                    | KidsToParentOp [ChartId]
                    | InitOp
                    
type ChartOperationConstructor = ChartId -> ChartId -> ChartOperation 

type ChartId = Integer

ciRoot, ciFoot, ciSubs, ciAdjDone, ciAux, ciInit :: ChartItem -> Bool
ciRoot  i = (gnname.ciNode) i == (gnname.root.ttree.ciSourceTree) i
ciFoot  i = (gtype.ciNode) i == Foot
ciSubs  i = (gtype.ciNode) i == Subs
ciAdjDone   = gaconstr.ciNode
ciAux   i = (ttype.ciSourceTree) i == Auxiliar
ciInit = not.ciAux


combineVectors :: ChartItem -> ChartItem -> ChartItem 
combineVectors a b = 
  b { ciSemantics = (ciSemantics a) .|. (ciSemantics b)
    , ciPolpaths  = (ciPolpaths  a) .&. (ciPolpaths  b) 
    , ciSemBitMap =  ciSemBitMap a }

combineWith :: ChartOperationConstructor -- ^ how did we get the new item?
            -> GNode -> Subst -> ChartItem -> ChartItem -> ChartItem
combineWith operation node subst active passive =
  combineVectors active $
  passive { ciNode      = node
          , ciSubsts    = (ciSubsts passive) ++ subst
          , ciVariables = replace subst (ciVariables passive)
          , ciAut_befHole  = ciAut_befHole active
          , ciAut_aftHole  = ciAut_aftHole active 
          , ciDerivation   = [ operation (ciId active) (ciId passive) ] }
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
 , (activeAdjunctionRule, "actAdjRule") 
 , (passiveAdjunctionRule, "psvAdjRule") ] 

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
             -- FIXME: yeck! automata stuff
             ++ " " ++ (show $ states $ ciAut_befHole i)

showItemSem = (showBitVector 3) . ciSemantics

isLexeme :: GNode -> Bool
isLexeme = not.null.glexeme

-- | CKY non adjunction rule - creates items in which
-- we do not apply any adjunction
-- this rule also doubles as top
nonAdjunctionRule item _ =
  let node  = ciNode item
      node2 = node { gaconstr = True }
  in if gtype node /= Other || ciAdjDone item then Nothing
     else Just [ item { ciNode = node2
                      , ciDerivation = [ NullAdjOp $ ciId item ] } ]

-- | CKY parent rule
kidsToParentRule item chart = 
 do (leftS,rightS,p)  <- Map.lookup (gnname node) (ciRouting item) 
    let mergePoints kids = 
         case mapMaybe ciAdjPoint (item:kids) of
          []  -> Nothing
          [x] -> Just x
          _   -> error "multiple adjunction points in kidsToParentRule?!"
    let combineAuts kids =
          if null aft 
          then ( concatAut $ map ciAut_befHole bef 
               , emptySentenceAut)
          else ( concatAut $ map ciAut_befHole $ bef ++ [theFoot]
               , concatAut $ map ciAut_aftHole  $ theFoot : aft )
          where theFoot = head aft
                concatAut auts = foldr joinAutomata emptySentenceAut auts
                (bef, aft) = span (not.ciFoot) kids
    let combine kids = do
          let unifyOnly (x, _) y = unify x y
          newVars <- foldM unifyOnly (ciVariables item,[]) $ 
                     map ciVariables kids  
          let newSubsts = concatMap ciSubsts (item:kids)
              newItem = item 
               { ciNode      = replace newSubsts p 
               , ciSubsts    = newSubsts 
               , ciAdjPoint  = mergePoints kids 
               , ciVariables = fst newVars 
               , ciAut_befHole = (fst.combineAuts) kids
               , ciAut_aftHole  = (snd.combineAuts) kids
               , ciDerivation   = [ KidsToParentOp $ map ciId kids ]
               }
          return $ foldr combineVectors newItem kids
    --
    let leftMatches  = map matches leftS
        rightMatches = map matches rightS
        allMatches = leftMatches ++ ([item] : rightMatches)
    -- trace (" relevant chart: (" ++ (show $ length relChart) ++ ") " ++ showItems relChart) $ 
    -- trace (" routing info: " ++ show (s,p,r)) $ 
    -- trace (" matches: (" ++ (show $ length allMatches) ++ ") " ++ (concat $ intersperse "-\n" $ map showItems allMatches)) $
    combinations allMatches >>= listAsMaybe . mapMaybe combine 
 where
   node    = ciNode item
   source  = (idname.ciSourceTree) item  
   --
   relevant c = 
     (idname.ciSourceTree) c == source && (not $ ciSubs c) && ciAdjDone c
   relChart = filter relevant chart
   --
   matches :: String -> [ChartItem]
   matches sis = [ c | c <- relChart, (gnname.ciNode) c == sis ]

-- note: not the same as Data.Maybe.listToMaybe !
listAsMaybe :: [a] -> Maybe [a]
listAsMaybe [] = Nothing
listAsMaybe l  = Just l

-- | CKY subst rules
substRule item chart = listAsMaybe resVariants
 where
  resVariants
   | ciSubs item = -- trace " subst variant" $ 
                   mapMaybe (\r -> attemptSubst item r) roots
   | ciRoot item && ciInit item = -- trace " root variant"  $ 
                   mapMaybe (\s -> attemptSubst s item) subs 
   | otherwise   = [] 
  --
  roots = [ r | r <- chart, compatible item r && ciRoot r && (gaconstr.ciNode) r && ciInit r ]
  subs  = [ s | s <- chart, ciSubs s ]

-- | unification for substitution
attemptSubst :: ChartItem -> ChartItem -> Maybe ChartItem
attemptSubst sItem rItem | ciSubs sItem = 
 do let rNode = ciNode rItem
        sNode = ciNode sItem
    (up, down, subst) <- unifyGNodes sNode (ciNode rItem) 
    let newNode = rNode { gnname = gnname sNode
                        , gup = up, gdown = down }
    return $ combineWith SubstOp newNode subst rItem sItem
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
        newItem = combineWith AdjOp newNode subst aItem pItem
        --
        newAut_beforeHole = joinAutomata (ciAut_befHole aItem) (ciAut_befHole pItem)
        newAut_afterHole  = joinAutomata (ciAut_aftHole pItem)  (ciAut_aftHole  aItem)
    return $ newItem { ciAut_befHole = newAut_beforeHole
                     , ciAut_aftHole  = newAut_afterHole }
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
         apply (rule, _) = 
           rule agendaItem chart
         results = map apply ckyRules 
     -- put all newly generated items into the right pigeon-holes
     -- trace (concat $ zipWith showRes ckyRules results) $ 
     mapM dispatchNew (concat $ catMaybes results)
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
 do let tryFilter Nothing _        = return Nothing
        tryFilter (Just newItem) f = f newItem
    -- keep trying dispatch filters until one of them suceeds
    foldM tryFilter (Just item) [ dispatchTbFailure
                                , dispatchRedundant
                                , dispatchResults
                                , dispatchToAgenda ]
    return ()

-- A dispatch filter makes a somewhat counter-intuitive use of
-- Maybe: 
-- 
--  If the filter succesfully dispatches the object; we're no
--  longer interested in its result, so we happily return 
--  Nothing.
-- 
--  If the filter does not dispatch the item, then maybe the
--  item needs to be modified, so it returns Just newItem
--  Of course, if no modifications are neccesary, then it
--  just returns the same item
dispatchToAgenda, dispatchRedundant, dispatchResults, dispatchTbFailure :: ChartItem -> BState (Maybe ChartItem)

-- | Trivial dispatch filter: always put the item on the agenda and return 
--   Nothing
dispatchToAgenda item =
   do addToAgenda item
      return Nothing

-- | If the item is equivalent to another, merge it with the equivalent
--   item and return Nothing.
--   If the item is indeed unique, return (Just $ setId item)
dispatchRedundant item = 
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
                return Nothing 
        else do Just `liftM` setId item

-- | If it is a result, put the item in the results list.
--   Otherwise, return (Just unmodified) 
dispatchResults item = 
 do st <- get
    let synComplete = ciInit item && ciRoot item 
        semComplete = tsemVector st == ciSemantics item 
        -- join the two automata together - no more holes!
        itemJoined  = item { ciAut_befHole = joinAutomata bef aft
                           , ciAut_aftHole = emptySentenceAut }
         where bef = ciAut_befHole item
               aft = ciAut_aftHole item
        --
    if (synComplete && semComplete ) 
       then trace ("isResult" ++ showItem item) $ 
            addToResults itemJoined >> return Nothing 
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

tbUnify :: ChartItem -> Maybe ChartItem
-- things for which tb unification is not relevant
tbUnify item | ciFoot item = return item
tbUnify item | (not.gaconstr.ciNode) item = return item
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
\section{Equivalence classes}
% --------------------------------------------------------------------  

\fnlabel{equivalent} returns true if two chart items are equivalent.
Note that this is not the same thing as equality!  

\begin{code}
equivalent :: ChartItem -> ChartItem -> Bool
equivalent c1 c2 = stuff c1 == stuff c2
  where stuff x = ( ciNode x, ciSemantics x, ciPolpaths x ) 
\end{code}

\fnlabel{mergeItems} combines two chart items into one, with the
assumption being that you have already determined that they are
equivalent.  Information from the second ``slave'' item is merged
into information from the first ``master'' item.

\begin{code}
mergeItems :: ChartItem -> ChartItem -> ChartItem
--FIXME: to implement
mergeItems master slave = 
 master { ciDerivation = ciDerivation master ++ (ciDerivation slave) }
\end{code}

% --------------------------------------------------------------------  
\section{Automaton stuff}
% --------------------------------------------------------------------  

\begin{code}
emptySentenceAut :: SentenceAut
emptySentenceAut = 
  NFA { startSt     = (-1) 
      , isFinalSt   = Nothing
      , finalStList = []
      , transitions = Map.empty
      , states      = [[]] }

joinAutomata :: SentenceAut -> SentenceAut -> SentenceAut
joinAutomata rawAut1 rawAut2 | states rawAut1 == states emptySentenceAut = rawAut2
joinAutomata rawAut1 rawAut2 | states rawAut2 == states emptySentenceAut = rawAut1
joinAutomata rawAut1 rawAut2 | states rawAut2 == states emptySentenceAut = rawAut1
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

\subsection{ChartItem}

\begin{code}
setId :: ChartItem -> BState ChartItem 
setId item =
  do s <- get
     let counter = gencounter s 
     put $ s { gencounter = counter + 1 }
     return $ item { ciId = counter }

-- | Returns all the derivation trees for this item: note that this is
-- not a TAG derivation tree but a history of inference rule applications
-- in tree form
-- This is written using a list monad to capture the fact that a node 
-- can have more than one derivation (because of merging)
extractDerivations :: BuilderStatus -> ChartItem -> [ Tree (ChartId, String) ]
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

findId :: BuilderStatus -> ChartId -> Maybe ChartItem
findId st i = find (\x -> ciId x == i) $ theChart st ++ (theAgenda st) ++ (theResults st) ++ (theTrash st)
\end{code}
