-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LiberalTypeSynonyms, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.Simple.SimpleBuilder (
   -- Types
   Agenda, AuxAgenda, Chart, SimpleStatus, SimpleState,
   SimpleItem(..),

   -- From SimpleStatus
   simpleBuilder_1p, simpleBuilder_2p, simpleBuilder,
   theAgenda, theHoldingPen, theChart, theResults,
   initSimpleBuilder,
   addToAgenda, addToChart,
   genconfig,
   SimpleGuiItem(..),
   theTrash, step,

   unpackResult,

   -- * Aliases to non-exported functions
   testCanAdjoin, testIapplyAdjNode, testEmptySimpleGuiItem
   )
where

import Control.Arrow (first)
import Control.Applicative ( (<$>) )
import Control.Monad (when, unless, liftM2)
import Control.Monad.State.Strict (get, put, modify, gets, runState, execStateT)
import Data.Bits
import Data.Generics ( Data )
import Data.List (partition, foldl', sortBy, unfoldr )
import Data.Ord (comparing)
import Data.Text ( Text )
import Data.Tree
import Data.Typeable ( Typeable )
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Error

import NLP.GenI.Automaton ( automatonPaths, NFA(..), addTrans )
import NLP.GenI.Builder ( incrCounter, num_iterations, num_comparisons
                        , chart_size, SemBitMap, defineSemanticBits, semToBitVector, bitVectorToSem
                        , DispatchFilter, (>-->), condFilter, FilterStatus(Filtered, NotFiltered)
                        , GenStatus(..),
                        )
import NLP.GenI.FeatureStructure ( unifyFeat, Flist )
import NLP.GenI.Flag
import NLP.GenI.General ( BitVector, mapMaybeM, mapTree', geniBug, preTerminals, repList )
import NLP.GenI.GeniVal
import NLP.GenI.Morphology.Types ( LemmaPlus(..) )
import NLP.GenI.Pretty
import NLP.GenI.Semantics ( sortSem, Sem )
import NLP.GenI.Statistics (Statistics)
import NLP.GenI.Tag
    ( TagElem, TagSite(..), getLexeme, toTagSite
    , tidnum, idname, ttree, ttype, tsemantics
    , detectSites, TagDerivation, DerivationStep(..)
    , plugTree, spliceTree
    , ts_rootFeatureMismatch, ts_synIncomplete, ts_semIncomplete
    , ts_tbUnificationFailure
    )
import NLP.GenI.TreeSchema
    ( Ptype(Initial), GNode(..), NodeName, gnnameIs
    , GType(Other), root, foot )
import qualified NLP.GenI.Builder as B

-- --------------------------------------------------------------------
-- The Builder interface
-- --------------------------------------------------------------------

type SimpleBuilder = B.Builder SimpleStatus SimpleItem
simpleBuilder_2p, simpleBuilder_1p :: SimpleBuilder
simpleBuilder_2p = simpleBuilder True
simpleBuilder_1p = simpleBuilder False

simpleBuilder :: Bool -> SimpleBuilder
simpleBuilder twophase = me
 where
  me = B.Builder
   { B.init     = initSimpleBuilder twophase
   , B.step     = if twophase then generateStep_2p else generateStep_1p
   , B.stepAll  = B.defaultStepAll me
   , B.finished = finished twophase
   , B.unpack   = unpackResults.theResults
   , B.partial  = unpackResults.partialResults
   }

-- --------------------------------------------------------------------
-- Key types
-- --------------------------------------------------------------------

type Agenda = [SimpleItem]
type AuxAgenda  = [SimpleItem]
type Chart  = [SimpleItem]
type Trash = [SimpleItem]

data GenerationPhase = SubstitutionPhase
                     | AdjunctionPhase
 deriving (Show)

isAdjunctionPhase :: GenerationPhase -> Bool
isAdjunctionPhase AdjunctionPhase = True
isAdjunctionPhase _ = False

type SimpleState a = B.BuilderState SimpleStatus a

data SimpleStatus = S
  { theAgenda    :: Agenda
  , theHoldingPen :: AuxAgenda
  , theChart     :: Chart
  , theTrash   :: Trash
  , theResults :: [SimpleItem]
  , tsem       :: BitVector
  , step       :: GenerationPhase
  , gencounter :: Integer
  , genconfig  :: [Flag]
  -- we keep a SemBitMap strictly to help display the semantics
  , semBitMap  :: SemBitMap
  }
  -- deriving Show

-- SimpleStatus updaters

assignNewId :: SimpleItem -> SimpleState SimpleItem
assignNewId item = do
  modify $ \s -> s{ gencounter = gencounter s + 1 }
  counter <- gets gencounter
  return $ item { siId = counter }

addToAgenda :: SimpleItem -> SimpleState ()
addToAgenda te = do
  te2 <- assignNewId te
  modify $ \s -> s{theAgenda = te2 : theAgenda s }

updateAgenda :: Agenda -> SimpleState ()
updateAgenda a =
  modify $ \s -> s{theAgenda = a}

addToAuxAgenda :: SimpleItem -> SimpleState ()
addToAuxAgenda te = do
  te2 <- assignNewId te
  modify $ \s -> s { theHoldingPen = te2 : theHoldingPen s }

addToChart :: SimpleItem -> SimpleState ()
addToChart te = do
  modify $ \s -> s { theChart = te:theChart s
                   }
  incrCounter chart_size 1

addToTrash :: SimpleItem -> String -> SimpleState ()
addToTrash te msg = do
  disableGui <- gets (hasFlag DisableGuiFlg . genconfig)
  unless disableGui $
    modify $ \s -> s { theTrash = te2 : theTrash s }
  where
    te2 = modifyGuiStuff (\g -> g { siDiagnostic = msg : siDiagnostic g }) te

addToResults :: SimpleItem -> SimpleState ()
addToResults te =
  modify $ \s -> s { theResults = te : theResults s }

-- ----------------------------------------------------------------------
-- SimpleItem
-- ----------------------------------------------------------------------

data SimpleItem = SimpleItem
 { siId        :: ChartId
 --
 , siSubstnodes :: [NodeName]
 , siAdjnodes   :: [NodeName]
 --
 , siSemantics :: BitVector
 , siPolpaths  :: BitVector
 -- for generation sans semantics
 -- , siAdjlist :: [(String,Integer)] -- (node name, auxiliary tree id)
 , siNodes   :: [GNode GeniVal]    -- ^ actually a set
 , siDerived :: Tree Text
 , siRoot_    :: NodeName
 , siFoot_    :: Maybe NodeName
 --
 , siPendingTb :: [NodeName] -- only for one-phase
 -- how was this item produced?
 , siDerivation :: TagDerivation
 -- for the debugger only
 , siGuiStuff :: SimpleGuiItem
 } -- deriving (Show)


lookupOrBug :: Text -> SimpleItem -> NodeName -> GNode GeniVal
lookupOrBug fnname item k =
        case filter (gnnameIs k) (siNodes item) of
          []   -> geniBug $ T.unpack fnname ++ ": could not find node " ++ T.unpack k
          [gn] -> gn
          _    -> geniBug $ T.unpack fnname ++ ": more than one node named " ++ T.unpack k

siRoot :: SimpleItem -> TagSite
siRoot x = toTagSite . lookupOrBug "siRoot" x $ siRoot_ x

siFoot :: SimpleItem -> Maybe TagSite
siFoot x = (toTagSite . lookupOrBug "siFoot" x) `fmap` siFoot_ x

instance DescendGeniVal (Text, B.UninflectedDisjunction) where
  descendGeniVal m (s,d) = (s, descendGeniVal m d)

-- | Things whose only use is within the graphical debugger
data SimpleGuiItem = SimpleGuiItem
 { siHighlight :: [Text] -- ^ nodes to highlight
 -- if there are things wrong with this item, what?
 , siDiagnostic :: [String]
 , siFullSem :: Sem
 , siIdname  :: Text
 } deriving (Data, Typeable)

emptySimpleGuiItem :: SimpleGuiItem
emptySimpleGuiItem = SimpleGuiItem [] [] [] ""

testEmptySimpleGuiItem :: SimpleGuiItem
testEmptySimpleGuiItem = emptySimpleGuiItem

modifyGuiStuff :: (SimpleGuiItem -> SimpleGuiItem) -> SimpleItem -> SimpleItem
modifyGuiStuff fn i = i { siGuiStuff = fn . siGuiStuff $ i }

type ChartId = Integer

instance DescendGeniVal SimpleItem where
  descendGeniVal s i = s `seq` i `seq`
    i { siNodes   = descendGeniVal s (siNodes i) }

{-# INLINE closedAux #-}

-- | True if the chart item has no open substitution nodes
closed :: SimpleItem -> Bool
closed = null.siSubstnodes

-- | True if the chart item is an auxiliary tree
aux :: SimpleItem -> Bool
aux = isJust . siFoot

-- | True if both 'closed' and 'aux' are True
closedAux :: SimpleItem -> Bool
closedAux x = aux x && closed x

adjdone :: SimpleItem -> Bool
adjdone = null.siAdjnodes

siInitial :: SimpleItem -> Bool
siInitial =  isNothing . siFoot

-- --------------------------------------------------------------------
-- Initialisation
-- --------------------------------------------------------------------

-- | Creates an initial SimpleStatus.
initSimpleBuilder ::  Bool -> B.Input -> [Flag] -> (SimpleStatus, Statistics)
initSimpleBuilder twophase input flags =
  let disableGui = hasFlag DisableGuiFlg flags
      cands   = map (initSimpleItem disableGui bmap) $ B.inCands input
      (sem,_,_) = B.inSemInput input
      bmap    = defineSemanticBits sem
      -- FIXME: I don't know if this matters for one-phase
      -- because of on-the-fly tb unification (in 2p), we
      -- need an initial tb step that only addresses the
      -- nodes with null adjunction constraints
      simpleDp = if twophase then simpleDispatch_2p else simpleDispatch_1p
      initialDp = dpTbNaFailure >--> dpTbFailure >--> simpleDp
      --
      initS = S{ theAgenda    = []
               , theHoldingPen = []
               , theChart     = []
               , theTrash     = []
               , theResults   = []
               , semBitMap = bmap
               , tsem      = semToBitVector bmap sem
               , step     =  SubstitutionPhase
               , gencounter = 0
               , genconfig  = flags }
      --
  in B.unlessEmptySem input flags $
     runState (execStateT (mapM initialDp cands) initS) (B.initStats flags)


initSimpleItem :: Bool -- ^ disable gui
               -> SemBitMap -> (TagElem, BitVector) -> SimpleItem
initSimpleItem disableGui bmap (teRaw,pp) =
 let (te,tlite) = renameNodesWithTidnum teRaw in
 case detectSites (ttree te) of
 (snodes,anodes,nullAdjNodes) -> SimpleItem
  { siId        = tidnum te
  , siSemantics = semToBitVector bmap (tsemantics te)
  , siSubstnodes = snodes
  , siAdjnodes   = anodes
  , siPolpaths  = pp
  -- for generation sans semantics
  -- , siAdjlist = []
  , siNodes = flatten.ttree $ te
  , siDerived = tlite
  , siRoot_ = gnname . root $ theTree
  , siFoot_ = if ttype te == Initial then Nothing else Just . gnname . foot $ theTree
  , siDerivation = [ InitStep (gorigin . root $ theTree) ]
  -- note: see comment in initSimpleBuilder re: tb unification
  , siPendingTb = nullAdjNodes
  --
  , siGuiStuff = if disableGui then emptySimpleGuiItem else initSimpleGuiItem te
  }
  where theTree = ttree te

initSimpleGuiItem :: TagElem -> SimpleGuiItem
initSimpleGuiItem te = SimpleGuiItem
 { siHighlight = []
 , siDiagnostic = []
 , siFullSem = tsemantics te
 , siIdname = idname te }

renameNodesWithTidnum :: TagElem -> (TagElem, Tree NodeName)
renameNodesWithTidnum te =
    ( te { ttree = mapTree' renameNode theTree }
    , mapTree' newName theTree
    )
  where
    theTree = ttree te
    renameNode n = n { gnname = newName n }
    newName n = gnname n `T.append` "-" `T.append` tidstr te
    tidstr    = T.pack . show . tidnum

-- --------------------------------------------------------------------
-- Generate
-- --------------------------------------------------------------------

-- One-phase generation

generateStep_1p :: SimpleState ()
generateStep_1p =
 do isDone <- gets (null.theAgenda)
    let dispatch = mapM simpleDispatch_1p
    if isDone
       then return ()
       else do incrCounter num_iterations 1
               given <- selectGiven
               -- do both substitution and adjunction
               _ <- applySubstitution1p given >>= dispatch
               _ <- passiveAdjunction1p given >>= dispatch
               _ <- activeAdjunction1p  given >>= dispatch
               _ <- sansAdjunction1p    given >>= dispatch
               -- determine which of the res should go in the agenda
               -- (monadic state) and which should go in the result (res')
               addToChart given

-- Two-phase generation

generateStep_2p :: SimpleState ()
generateStep_2p = do
  nir     <- gets (null.theAgenda)
  curStep <- gets step
  case curStep of
   SubstitutionPhase -> if nir then switchToAux else generateStep_2p_sub
   AdjunctionPhase   -> if nir then return ()   else generateStep_2p_adj

generateStep_2p_sub :: SimpleState ()
generateStep_2p_sub =
  do incrCounter num_iterations 1
     -- choose an item from the agenda
     given <- selectGiven
     res <- applySubstitution given
     mapM_ simpleDispatch_2p res
     -- put the given into the chart untouched
     addToChart given

generateStep_2p_adj :: SimpleState ()
generateStep_2p_adj =
  do incrCounter num_iterations 1
     -- choose an item from the agenda
     given <- selectGiven
     res <- liftM2 (++) (applyAdjunction2p given) (sansAdjunction2p given)
     mapM_ simpleDispatch_2p_adjphase res
     when (adjdone given) $ trashIt given

trashIt :: SimpleItem -> SimpleState ()
trashIt item = do
    disableGui <- gets (hasFlag DisableGuiFlg . genconfig)
    unless disableGui $ do
        { missing <- missingSem item
        ; let reason = if null missing
                          then "unknown reason!"
                          else ts_semIncomplete missing
        ; addToTrash item reason
        }

missingSem :: SimpleItem -> SimpleState Sem
missingSem item = do
    s <- get
    let bmap     = semBitMap s
        inputSem = tsem s
        itemSem  = siSemantics item
    return $ bitVectorToSem bmap $ inputSem `xor` itemSem

-- | Arbitrarily selects and removes an element from the agenda and
--   returns it.
selectGiven :: SimpleState SimpleItem
selectGiven = do
  agenda <- gets theAgenda
  case agenda of
   [] -> geniBug "null agenda in selectGiven"
   (a:atail) -> updateAgenda atail >> return a

-- Switching phases

switchToAux :: SimpleState ()
switchToAux = do
  st <- get
  let oldAuxTrees = theHoldingPen st
      -- You might be wondering why we ignore the auxiliary trees in the
      -- chart; this is because all the syntactically complete auxiliary
      -- trees have already been filtered away by calls to classifyNew
      initialT  = filter siInitial (theChart st)
      (compT1, incompT1) = partition (null.siSubstnodes) initialT
      (auxTrees, compT2) =
        ( mapMaybe (detectNa oldAuxTrees) oldAuxTrees
        , mapMaybe (detectNa auxTrees) compT1 )
      (compT3, incompT3) = semfilter (tsem st) auxTrees compT2
      --
      compT = compT3
  put st{ theAgenda = []
        , theHoldingPen = []
        , theChart = auxTrees
        , step = AdjunctionPhase }
  mapM_ simpleDispatch_2p_adjphase compT
  -- toss the syntactically incomplete stuff in the trash
  mapM_ (\t -> addToTrash t ts_synIncomplete) incompT1
  mapM_ (\t -> addToTrash t =<< ts_semFiltered <$> missingSem t) incompT3
  where
    ts_semFiltered sem = T.unpack $
        "Sem-filtered, MISSING:" <+> squeezed 72 (map pretty sem)
-- Completion

finished :: Bool -> SimpleStatus -> GenStatus
finished twophase st
  | reallyDone   = B.Finished
  | atMaxResults = B.Finished
  | atMaxSteps   = B.Error $ "Max steps exceeded" <+> parens (pretty maxSteps)
  | otherwise    = B.Active
 where
  reallyDone   = null (theAgenda st) && (not twophase || isAdjunctionPhase (step st)) 
  atMaxResults = maybeIf (<= fromIntegral (length (theResults st))) $ getFlag MaxResultsFlg (genconfig st)
  atMaxSteps   = maybeIf (<  gencounter st) mMaxSteps
  mMaxSteps    = getFlag MaxStepsFlg (genconfig st)
  maxSteps     = fromMaybe (error "get maxsteps") mMaxSteps
  maybeIf bf = maybe False bf

-- SemFilter Optimisation

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

-- --------------------------------------------------------------------
-- Substitution
-- --------------------------------------------------------------------

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
 (shead : stail) ->
  let doIt =
       do -- Maybe monad
          let (TagSite n  fu fd nOrigin) = toTagSite (lookupOrBug "iapplySubst" item2 shead)
              (TagSite rn ru rd rOrigin) = siRoot item1
          (newU, subst1) <- hush $ unifyFeat ru fu
          (newD, subst2) <- hush $ unifyFeat (replace subst1 rd)
                                             (replace subst1 fd)
          let subst = appendSubst subst1 subst2
              -- gui stuff
              newRoot g = g { gup = newU, gdown = newD, gtype = Other }
          let pending = if twophase then []
                        else rn : (siPendingTb item1 ++ siPendingTb item2)
          let item1g = item1 { siNodes = repList (gnnameIs rn) newRoot (siNodes item1) }
          return $! replace subst $ combineSimpleItems [rn] item1g $
                     item2 { siSubstnodes = stail ++ (siSubstnodes item1)
                           , siAdjnodes   = siAdjnodes item1 ++ siAdjnodes item2
                           , siDerived    = plugTree (siDerived item1) n (siDerived item2)
                           , siDerivation = addToDerivation SubstitutionStep (item1, rOrigin) (item2,nOrigin,n)
                           , siPendingTb  = pending
                           }
  in case doIt of
     Nothing -> return []
     Just x  -> do incrCounter "substitutions" 1
                   return [x]
iapplySubst _ _ _ = return []

-- --------------------------------------------------------------------
-- Adjunction
-- ---------------------------------------------------------------

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
 (ahead : atail) -> do
  let (TagSite gn t b _) = toTagSite (lookupOrBug "sansAdjunction2p" item ahead)
  -- do top/bottom unification on the node
  case hush (unifyFeat t b) of
   Nothing ->
     do addToTrash (modifyGuiStuff (\g -> g { siHighlight = [gn] }) item)
                   ts_tbUnificationFailure
        return []
   Just (tb,s) ->
     let item1 = constrainAdj gn tb item
     in return $! [replace s $! item1 { siAdjnodes = atail }]
sansAdjunction2p _ = return []

iapplyAdjNode :: Bool -> SimpleItem -> SimpleItem -> Maybe SimpleItem
iapplyAdjNode twophase aItem pItem = {-# SCC "iapplyAdjNode" #-}
 case siAdjnodes pItem of
 [] -> Nothing
 (pHead : pTail) -> do
  -- let's go!
  let pSite = toTagSite (lookupOrBug "iapplyAdjNode" pItem pHead)
  (anr, anf, subst12) <- canAdjoin aItem pSite
  let r_name = siRoot_ aItem
      r = siRoot aItem
  f <- siFoot aItem
  let an_name = tsName pSite
      -- the new adjunction nodes
      aItem2 = aItem { siNodes = map (setSites anr) (siNodes aItem)  }
        where
          setSites (TagSite n u d _) gn =
            if gnname gn == n then gn { gup = u, gdown = d } else gn
      rawCombined =
        combineSimpleItems [tsName r, an_name] aItem2 $ pItem
               { siAdjnodes = pTail ++ siAdjnodes aItem
               , siDerived = spliceTree (tsName f) (siDerived aItem) an_name (siDerived pItem)
               , siDerivation = addToDerivation AdjunctionStep (aItem,tsOrigin r) (pItem,tsOrigin pSite,an_name)
               -- , siAdjlist = (n, (tidnum te1)):(siAdjlist item2)
               -- if we adjoin into the root, the new root is that of the aux
               -- tree (affects 1p only)
               , siRoot_ = if isRootOf pItem an_name then r_name else siRoot_ pItem
               , siPendingTb =
                  if twophase then []
                  else tsName f : siPendingTb pItem ++ siPendingTb aItem
               }
      -- one phase = postpone tb unification
      -- two phase = do tb unification on the fly
      finalRes1p = return $ replace subst12 rawCombined
      finalRes2p =
       do -- tb on the former foot
          tbRes <- hush $ unifyFeat (tsUp anf) (tsDown anf)
          let (anf_tb, subst3) = tbRes
              myRes = constrainAdj an_name anf_tb res'
          -- apply the substitutions
              res' = replace (appendSubst subst12 subst3) rawCombined
          return myRes
  -- ---------------
  if twophase then finalRes2p else finalRes1p

testIapplyAdjNode :: Bool -> SimpleItem -> SimpleItem -> Maybe SimpleItem
testIapplyAdjNode = iapplyAdjNode

canAdjoin :: SimpleItem -> TagSite -> Maybe (TagSite, TagSite, Subst)
canAdjoin aItem pSite = do
  -- let's go!
  let r = siRoot aItem -- auxiliary tree, eh?
  f <- siFoot aItem -- should really be an error if fails
  (anr_up',  subst1)  <- hush $ unifyFeat (tsUp r) (tsUp pSite)
  (anf_down, subst2)  <- hush $ unifyFeat (replace subst1 $ tsDown f) (replace subst1 $ tsDown pSite)
  let -- combined substitution list and success condition
      subst12 = appendSubst subst1 subst2
      anr = replace subst12 $ r { tsUp = anr_up' } --  resulting node based on the root node of the aux tree
      anf = replace subst12 $ f { tsDown = anf_down } --  resulting node based on the foot node of the aux tree
  return (anr, anf, subst12)

testCanAdjoin :: SimpleItem -> TagSite -> Maybe (TagSite, TagSite, Subst)
testCanAdjoin = canAdjoin

detectNa :: [SimpleItem] -- ^ aux trees
         -> SimpleItem   -- ^ me
         -> Maybe SimpleItem
detectNa rawAux i = helper (map look (siAdjnodes i)) Map.empty []
 where
  look = toTagSite . lookupOrBug "detectNa" i
  compatAux = filterCompatible i rawAux
  helper []     s acc = Just $ replace s $ i { siAdjnodes = acc }
  helper (t:ts) s acc =
    let hasAdj = any isJust $ map (\a -> canAdjoin a t) compatAux
    in case snd <$> (hush $ unifyFeat (tsUp t) (tsDown t)) of
        Just s2 -> if hasAdj
                   then helper ts s (tsName t : acc)
                   else helper (replace s2 ts) (appendSubst s s2) acc
        Nothing -> if hasAdj
                   then helper ts s (tsName t : acc)
                   else Nothing

-- --------------------------------------------------------------------
-- Helper functions for operations
-- --------------------------------------------------------------------

isRootOf :: SimpleItem -> Text -> Bool
isRootOf item n = n == siRoot_ item

-- | Retrieves a list of trees from the chart which could be combined with the given agenda tree.
-- The current implementation searches for trees which
--  * do not have overlapping semantics with the given
--  * are on the some of the same polarity automaton paths as the
--    current agenda item
lookupChart :: SimpleItem -> SimpleState [SimpleItem]
lookupChart given = gets (filterCompatible given . theChart)

filterCompatible :: SimpleItem -> [SimpleItem] -> [SimpleItem]
filterCompatible given chart =
  [ i | i <- chart
      -- should be on the same polarity path (chart sharing)
      , (siPolpaths i) .&. gpaths /= 0
      -- semantics should not be overlapping
      && (siSemantics i .&. gsem ) == 0
  ]
 where
  gpaths = siPolpaths given
  gsem   = siSemantics given

-- | Helper function for when chart operations succeed.
combineSimpleItems :: [NodeName] -- ^ nodes to highlight
                   -> SimpleItem -> SimpleItem -> SimpleItem
combineSimpleItems hi item1 item2 = {-# SCC "combineSimpleItems" #-}
  item2 { siSemantics = siSemantics item1 .|. siSemantics item2
        , siPolpaths  = siPolpaths  item1 .&. siPolpaths  item2
        , siGuiStuff  = combineSimpleGuiItems hi (siGuiStuff item1) (siGuiStuff item2)
        , siNodes     = siNodes item1 ++ siNodes item2
        }

combineSimpleGuiItems :: [NodeName]
                      -> SimpleGuiItem -> SimpleGuiItem -> SimpleGuiItem
combineSimpleGuiItems hi item1 item2 =
 item2 { siFullSem = sortSem $ siFullSem item1 ++ siFullSem item2
       , siDiagnostic = siDiagnostic item1 ++ siDiagnostic item2
       , siHighlight = hi
       }

constrainAdj :: Text -> Flist GeniVal -> SimpleItem -> SimpleItem
constrainAdj gn newT g =
  g { siNodes = repList (gnnameIs gn) fixIt (siNodes g) }
  where fixIt n = n { gup = newT, gdown = [], gaconstr = True }

-- Derivation trees

addToDerivation :: (Text -> Text -> Text -> DerivationStep)
                -> (SimpleItem, Text)
                -> (SimpleItem, Text, Text)
                -> TagDerivation
addToDerivation op (tc,tcOrigin) (tp,tpOrigin,tpSite) =
  let hp = siDerivation tp
      hc = filter (not . isInit) (siDerivation tc)
      newnode = op tcOrigin tpOrigin tpSite
  in newnode:hp++hc
 where
  isInit :: DerivationStep -> Bool
  isInit (InitStep _) = True
  isInit _ = False

-- --------------------------------------------------------------------
-- Dispatching new results
-- --------------------------------------------------------------------

type SimpleDispatchFilter = DispatchFilter SimpleState SimpleItem

simpleDispatch_2p :: SimpleDispatchFilter
simpleDispatch_2p =
 simpleDispatch (dpRootFeatFailure >--> dpToResults)
                (dpAux >--> dpToAgenda)

simpleDispatch_2p_adjphase :: SimpleDispatchFilter
simpleDispatch_2p_adjphase =
 simpleDispatch (dpRootFeatFailure >--> dpToResults)
                dpToAgenda

simpleDispatch_1p :: SimpleDispatchFilter
simpleDispatch_1p =
 simpleDispatch (dpRootFeatFailure >--> dpTbFailure >--> dpToResults)
                dpToAgenda

simpleDispatch :: SimpleDispatchFilter -> SimpleDispatchFilter -> SimpleDispatchFilter
simpleDispatch resFilter nonResFilter item =
 do inputsem <- gets tsem
    let synComplete x = siInitial x && closed x && adjdone x
        semComplete x = inputsem == siSemantics x
        isResult x = synComplete x && semComplete x
    condFilter isResult resFilter nonResFilter item

dpAux, dpToAgenda :: SimpleDispatchFilter
dpTbFailure, dpToResults :: SimpleDispatchFilter
dpToTrash :: String -> SimpleDispatchFilter

dpToAgenda x  = addToAgenda x  >> return Filtered
dpToResults x = addToResults x >> return Filtered
dpToTrash m x = addToTrash x m >> return Filtered

dpAux item =
  if closedAux item
  then addToAuxAgenda item >> return Filtered
  else return (NotFiltered item)

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

dpTbNaFailure :: SimpleDispatchFilter
dpTbNaFailure item =
 case tbUnifyNaNodes (siNodes item) of
   Left msg      -> dpToTrash (T.unpack $ "top-bottom unification failure in NA nodes: " <> msg) item
   Right (ns2,s) -> return . NotFiltered . replace s $ item { siNodes = ns2 }

-- | This is only used for the one-phase algorithm
dpTbFailure item =
 return (if tbUnifyTree item then NotFiltered item else Filtered)

-- | If the item (ostensibly a result) does not have the correct root
--   category, return Nothing; otherwise return Just item
dpRootFeatFailure :: SimpleDispatchFilter
dpRootFeatFailure item =
 do config <- gets genconfig
    let rootFeat = getListFlag RootFeatureFlg config
        (TagSite _ top _ _) = siRoot item
    case hush $ unifyFeat rootFeat top of
      Nothing ->
        dpToTrash (ts_rootFeatureMismatch rootFeat) item
      Just (_, s) ->
        return . NotFiltered $ replace s item

-- --------------------------------------------------------------------
-- Top and bottom unification
-- --------------------------------------------------------------------

tbUnifyNaNodes :: MonadUnify m
               => [GNode GeniVal] -> m ([GNode GeniVal], Subst)
tbUnifyNaNodes [] = return ([], Map.empty)
tbUnifyNaNodes (n:ns) =
 if gaconstr n
    then do (ud, sub) <- unifyFeat (gup n) (gdown n)
            let n2 = n { gup = ud, gdown = [] }
            (ns2, sub2) <- tbUnifyNaNodes (replace sub ns)
            return (n2:ns2, sub `appendSubst` sub2)
    else first (n:) `fmap` tbUnifyNaNodes ns

type TbEither = Either Text Subst
tbUnifyTree :: SimpleItem -> Bool
tbUnifyTree item = {-# SCC "tbUnifyTree" #-}
  case foldl' tbUnifyNode (Right Map.empty) pending of
    Left  _ -> False
    Right _ -> True
  where
   pending = map (toTagSite . lookupOrBug "tbUnifyTree" item) (siPendingTb item)

tbUnifyNode :: TbEither -> TagSite -> TbEither
tbUnifyNode (Right pending) rawSite =
  -- apply pending substitutions
  case replace pending rawSite of
  (TagSite name up down _) ->
    -- check top/bottom unification on this node
    case hush (unifyFeat up down) of
    -- stop all future iterations
    Nothing -> Left name
    -- apply any new substutions to the whole tree
    Just (_,sb) -> Right (appendSubst pending sb)

-- if earlier we had a failure, don't even bother
tbUnifyNode (Left n) _ = Left n

-- --------------------------------------------------------------------
-- Unpacking the results
-- --------------------------------------------------------------------

unpackResults :: [SimpleItem] ->  [B.Output]
unpackResults = concatMap unpackResult

--Change, instead of returning the features of the parent node for every leaf, return:
--      -the features of the parent node when the leaf doesn't have features (top and bottom feature structure empty)
--      -the features of the node in case it has (in this case return the unification of top and bottom features).
unpackResult :: SimpleItem -> [B.Output]
unpackResult item =
  let look = lookupOrBug "unpackResult" item
      toUninflectedDisjunction (pt,t) =
        --B.UninflectedDisjunction (getLexeme (look t)) (gup (look pt)) 
        B.UninflectedDisjunction (getLexeme (look t))
                                 (gup . look $ if emptyFeatureStr (look t) then pt else t)

      derivation = siDerivation item
      paths = automatonPaths . listToSentenceAut .  map toUninflectedDisjunction . preTerminals . siDerived $ item
 in map (\p -> (siId item, p, derivation)) paths

emptyFeatureStr :: GNode GeniVal -> Bool
emptyFeatureStr n= null (gdown n) && null (gup n)

-- Sentence automata

listToSentenceAut :: [ B.UninflectedDisjunction ] -> B.SentenceAut
listToSentenceAut nodes =
  let theStart  = 0
      theEnd = length nodes - 1
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
      helper (current, B.UninflectedDisjunction lemmas features) aut =
        foldl' addT aut lemmas
        where
          addT a t = addTrans a current (Just (LemmaPlus t features)) next
          next = current + 1
      --
  in foldr helper emptyAut (zip theStates nodes)

-- --------------------------------------------------------------------
-- Partial results
-- --------------------------------------------------------------------

partialResults :: SimpleStatus -> [SimpleItem]
partialResults st = unfoldr getNext 0
 where
  inputsem = tsem st
  trash  = theTrash st
  trashC = sortBy (comparing $ negate . fst) $
           map (\t -> (coverage inputsem t, t)) trash
  getNext sem = case getItems sem of
                     []     -> Nothing
                     (it:_) -> Just (it, siSemantics it .|. sem)
  getItems sem = [ i | (_,i) <- trashC, siSemantics i .&. sem == 0 ]

coverage :: BitVector -> SimpleItem -> Int
coverage sem it = countBits (sem .&. siSemantics it)

countBits :: Bits a => a -> Int
countBits 0  = 0
countBits bs = if testBit bs 0 then 1 + next else next
  where next = countBits (shiftR bs 1)

-- --------------------------------------------------------------------
-- Performance
-- --------------------------------------------------------------------

{-
instance NFData SimpleItem where
  rnf (SimpleItem x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
      ) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6
                 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` rnf x10 `seq` rnf x11
                 `seq` rnf x11 `seq` rnf x12 `seq` rnf x13
                 `seq` rnf x14
-}
