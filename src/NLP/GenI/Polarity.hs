-- GenI surface realiser
-- Copyright (C) 2009 Eric Kow
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

{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.Polarity(
                module NLP.GenI.Polarity.Types,

                -- * Entry point
                PolAut, PolState(PolSt), AutDebug, PolResult(..),
                buildAutomaton,

                -- * Inner stuff (exported for debugging?)
                makePolAut,
                fixPronouns,
                detectSansIdx, suggestPolFeatures, detectPols, detectPolPaths,
                declareIdxConstraints, detectIdxConstraints,
                prettyPolPaths, prettyPolPaths',

                -- re-exported from Automaton
                automatonPaths, finalSt,
                NFA(states, transitions),
                )
where

import Data.Bits
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.GenI.Automaton
import NLP.GenI.FeatureStructure ( Flist, AvPair(..), FeatStruct, unifyFeat )
import NLP.GenI.General
    ( BitVector, isEmptyIntersect, thd3, Interval, ival, (!+!)
    )
import NLP.GenI.GeniVal ( GeniVal(gConstraints), mkGAnon, isAnon, replace )
import NLP.GenI.Polarity.Internal
import NLP.GenI.Polarity.Types
import NLP.GenI.Pretty
import NLP.GenI.Semantics ( Literal(..), SemInput, Sem, emptyLiteral
                          , sortSem
                          )
import NLP.GenI.Tag ( TagElem(..), TagItem(..), setTidnums )
import NLP.GenI.TreeSchema ( Ptype(Initial), GNode, root, gup, gdown, rootUpd)

-- ----------------------------------------------------------------------
-- Interface
-- ----------------------------------------------------------------------

-- | intermediate auts, seed aut, final aut, potentially modified sem
data PolResult = PolResult { prIntermediate :: [AutDebug]
                           , prInitial      :: PolAut
                           , prFinal        :: PolAut
                           , prSem          :: Sem }
type AutDebug  = (PolarityKey, PolAut, PolAut)

-- | Constructs a polarity automaton.  For debugging purposes, it returns
--   all the intermediate automata produced by the construction algorithm.
buildAutomaton :: Set.Set PolarityAttr -- ^ polarities to detect (eg. "cat")
               -> FeatStruct GeniVal   -- ^ root features to compensate for
               -> PolMap               -- ^ explicit extra polarities
               -> SemInput             -- ^ input semantics
               -> [TagElem]            -- ^ lexical selection
               -> PolResult
buildAutomaton polarityAttrs rootFeat extrapol (tsem,tres,_) candRaw =
  let -- root categories, index constraints, and external polarities
      rcatPol :: PolMap
      rcatPol = detectRootCompensation polarityAttrs rootFeat
      -- index constraints on candidate trees
      detect      = detectIdxConstraints tres
      constrain t = t { tpolarities = Map.unionWith (!+!) p r
                      } --, tinterface  = [] }
                   where p  = tpolarities t
                         r  = detect . tinterface $ t
      candRest  = map constrain candRaw
      inputRest = declareIdxConstraints tres
      -- polarity detection 
      cand1   = map (detectPols polarityAttrs) candRest
      extras1 = Map.unionsWith (!+!) [ extrapol, inputRest, rcatPol ]
      ks1     = polarityKeys cand1 Map.empty
      -- expanding unconstrained polarities
      tconvert t = t { tpolarities = convertUnconstrainedPolarities ks1 (tpolarities t) }
      cand    = map tconvert cand1
      extras  = convertUnconstrainedPolarities ks1 extras1
      ks      = polarityKeys cand extras
      -- building the automaton
  in makePolAut cand tsem extras ks

makePolAut :: [TagElem] -> Sem -> PolMap -> [PolarityKey] -> PolResult
makePolAut candsRaw tsemRaw extraPol ks =
 let -- perform index counting
     (tsem, cands') = fixPronouns (tsemRaw,candsRaw)
     cands = setTidnums cands'
     -- sorted semantics (for more efficient construction)
     sortedsem = sortSemByFreq tsem cands 
     -- the seed automaton
     smap = buildColumns cands sortedsem 
     seed = buildSeedAut smap  sortedsem
     -- building and remembering the automata 
     build k xs = (k,aut,prune aut):xs
       where aut   = buildPolAut k initK (thd3 $ head xs)
             initK = Map.findWithDefault (ival 0) k extraPol
     res = foldr build [(PolarityKeyStr "(seed)",seed,prune seed)] ks
 in PolResult { prIntermediate = reverse res
              , prInitial      = seed
              , prFinal        = thd3 $ head res
              , prSem          = tsem }

-- ====================================================================
-- Polarity automaton
-- ====================================================================

-- | Note: this is not the same function as 'NLP.GenI.Tags.mapBySem'!
-- The fact that we
-- preserve the order of the input semantics is important for our handling
-- of multi-literal semantics and for semantic frequency sorting.
buildColumns :: (TagItem t) => [t] -> Sem -> Map.Map Literal [t] 
buildColumns cands [] = 
  Map.singleton emptyLiteral e
  where e = filter (null.tgSemantics) cands

buildColumns cands (l:ls) = 
  let matchfn t = l `elem` tgSemantics t
      (match, cands2) = partition matchfn cands
      next = buildColumns cands2 ls
  in Map.insert l match next

-- ----------------------------------------------------------------------
-- Initial Automaton
-- ----------------------------------------------------------------------

buildSeedAut :: SemMap -> Sem -> PolAut
buildSeedAut cands tsem = 
  let start = polstart []
      hasZero (x,y) = x <= 0 && y >= 0
      isFinal (PolSt c _ pols) = 
        c == length tsem && all hasZero pols
      initAut = NFA 
        { startSt = start
        , isFinalSt = Just isFinal
        , finalStList = []
        , states  = [[start]]
        , transitions = Map.empty }
  in nubAut $ buildSeedAut' cands tsem 1 initAut

-- for each literal...
buildSeedAut' :: SemMap -> Sem -> Int -> PolAut -> PolAut 
buildSeedAut' _ [] _ aut = aut 
buildSeedAut' cands (l:ls) i aut = 
  let -- previously created candidates 
      prev   = head $ states aut
      -- candidates that match the target semantics
      tcands = Map.findWithDefault [] l cands
      -- create the next batch of states
      fn st ap             = buildSeedAutHelper tcands l i st ap
      (newAut,newStates)   = foldr fn (aut,[]) prev
      next                 = nub newStates : states aut
      -- recursive step to the next literal
  in buildSeedAut' cands ls (i+1) (newAut { states = next })

-- for each candidate corresponding to literal l...
buildSeedAutHelper :: [TagElem] -> Literal -> Int -> PolState -> (PolAut,[PolState]) -> (PolAut,[PolState])
buildSeedAutHelper cs l i st (aut,prev) =
  let -- get the extra semantics from the last state
      (PolSt _ ex1 _) = st
      -- candidates that match the target semantics and which
      -- do not overlap the extra baggage semantics
      tcand = [ Just t | t <- cs
              , isEmptyIntersect ex1 (tsemantics t) ]
      -- add the transitions out of the current state 
      addT tr (a,n) = (addTrans a st tr st2, st2:n)
        where 
         st2 = PolSt i (delete l $ ex1 ++ ex2) []
         ex2 = case tr of 
               Nothing  -> [] 
               Just tr_ -> tsemantics tr_
  in if (l `elem` ex1) 
     then addT Nothing (aut,prev)
     else foldr addT   (aut,prev) tcand 

-- ----------------------------------------------------------------------
-- Construction
-- ----------------------------------------------------------------------

buildPolAut :: PolarityKey -> Interval -> PolAut -> PolAut
buildPolAut k initK skelAut =
  let concatPol p (PolSt pr b pol) = PolSt pr b (p:pol)
      newStart = concatPol initK $ startSt skelAut
      --
      initAut  = skelAut 
        { startSt = newStart
        , states  = [[newStart]]
        , transitions = Map.empty }
      -- cand' = observe "candidate map" cand 
  in nubAut $ buildPolAut' k (transitions skelAut) initAut 

{-
Our helper function looks at a single state in the skeleton automaton
and at one of the states in the new automaton which correspond to it.
We use the transitions from the old automaton to determine which states
to construct.  Note: there can be more than one state in the automaton
which corresponds to a state in the old automaton.  This is because we
are looking at a different polarity key, so that whereas two candidates
automaton may transition to the same state in the old automaton, their
polarity effects for the new key will make them diverge in the new
automaton.  
-}

buildPolAut' :: PolarityKey -> PolTransFn -> PolAut -> PolAut
-- for each literal... (this is implicit in the automaton state grouping)
buildPolAut' fk skeleton aut = 
  let -- previously created candidates 
      prev = head $ states aut 
      -- create the next batch of states
      fn st ap            = buildPolAutHelper fk skeleton st ap
      (newAut,newStates)  = foldr fn (aut,Set.empty) prev
      next                = Set.toList newStates : states aut
      -- recursive step to the next literal
  in if Set.null newStates
     then aut
     else buildPolAut' fk skeleton (newAut { states = next })

-- given a previously created state...
buildPolAutHelper :: PolarityKey -> PolTransFn -> PolState -> (PolAut,Set.Set PolState) -> (PolAut,Set.Set PolState)
buildPolAutHelper fk skeleton st (aut,prev) =
  let -- reconstruct the skeleton state used to build st 
      PolSt pr ex (po1:skelpo1) = st
      skelSt = PolSt pr ex skelpo1
      -- for each transition out of the current state
      -- nb: a transition is (next state, [labels to that state])
      trans = Map.toList $ Map.findWithDefault Map.empty skelSt skeleton
      result = foldr addT (aut,prev) trans
      -- . for each label to the next state st2
      addT (oldSt2,trs) (a,n) = foldr (addTS oldSt2) (a,n) trs
      -- .. calculate a new state and add a transition to it
      addTS skel2 tr (a,n) = (addTrans a st tr st2, Set.insert st2 n)
        where st2 = newSt tr skel2
      --
      newSt :: Maybe TagElem -> PolState -> PolState
      newSt t skel2 = PolSt pr2 ex2 (po2:skelPo2)
        where 
         PolSt pr2 ex2 skelPo2 = skel2 
         po2 = po1 !+! Map.findWithDefault (ival 0) fk pol
         pol = case t of Nothing -> Map.empty 
                         Just t2 -> tpolarities t2
  in result 

-- ----------------------------------------------------------------------
-- Pruning
-- ----------------------------------------------------------------------

{-|
The pruning algorithm takes as arguments a list of states to process.
Among these, any state which does not have outgoing transitions is
placed on the blacklist.  We remove all transitions to the blacklist and
all states that only transition to the blacklist, and then we repeat
pruning, with a next batch of states.

Finally, we return the pruned automaton.  Note: in order for this to
work, it is essential that the final states are *not* included in the
list of states to process.
-}
prune :: PolAut -> PolAut
prune aut = 
  let theStates   = states aut
      final       = finalSt aut
      -- (remember that states is a list of lists) 
      lastStates  = head theStates 
      nextStates  = tail theStates 
      nonFinal    = (lastStates \\ final)
      -- the helper function will rebuild the state list
      firstAut    = aut { states = [] }
      pruned      = prune' (nonFinal:nextStates) firstAut 
      -- re-add the final state!
      statesPruned = states pruned
      headPruned   = head statesPruned
      tailPruned   = tail statesPruned
  in if (null theStates) 
     then aut
     else pruned { states = (headPruned ++ final) : tailPruned } 

prune' :: [[PolState]] -> PolAut -> PolAut
prune' [] oldAut = oldAut { states = reverse $ states oldAut }
prune' (sts:next) oldAut = 
  let -- calculate the blacklist
      oldT  = transitions oldAut
      oldSt = states oldAut
      transFrom st = Map.lookup st oldT
      blacklist    = filter (isNothing.transFrom) sts
      -- given a st: filter out all transitions to the blacklist
      allTrans  = Map.toList $ transitions oldAut
      -- delete all transitions to the blacklist
      miniTrim = Map.filterWithKey (\k _ -> not (k `elem` blacklist))
      -- extra cleanup: delete from map states that only transition to the blacklist
      trim = Map.filterWithKey (\k m -> not (k `elem` blacklist || Map.null m))
      -- execute the kill and miniKill filters
      newT = trim $ Map.fromList [ (st2, miniTrim m) | (st2,m) <- allTrans ]
      -- new list of states and new automaton
      newSts = sts \\ blacklist
      newAut = oldAut { transitions = newT,
                        states = newSts : oldSt }
      {- 
      -- debugging code
      debugstr  = "blacklist: [\n" ++ debugstr' ++ "]"
      debugstr' = concat $ intersperse "\n" $ map showSt blacklist
      showSt (PolSt pr ex po) = showPr pr ++ showEx ex ++ showPo po
      showPr (_,pr,_) = pr ++ " " 
      showPo po = concat $ intersperse "," $ map show po
      showEx ex = if (null ex) then "" else (showSem ex)
      -}
      -- recursive step
  in if null blacklist
     then oldAut { states = (reverse oldSt) ++ (sts:next) }
     else prune' next newAut 

-- ====================================================================
-- Zero-literal semantics
-- ====================================================================

type PredLite = (String,[GeniVal]) -- handle is head of arg list 
type SemWeightMap = Map.Map PredLite SemPols

-- | Returns a modified input semantics and lexical selection in which pronouns
--   are properly accounted for.
fixPronouns :: (Sem,[TagElem]) -> (Sem,[TagElem])
fixPronouns (tsem,cands) = 
  let -- part 1 (for each literal get smallest charge for each idx)
      getpols :: TagElem -> [ (PredLite,SemPols) ]
      getpols x = zip [ (prettyStr p, h:as) | Literal h p as <- tsemantics x ] (tsempols x)
      sempols :: [ (PredLite,SemPols) ]
      sempols = concatMap getpols cands
      usagemap :: SemWeightMap 
      usagemap = Map.fromListWith (zipWith min) sempols
      -- part 2 (cancel sem polarities)
      chargemap :: Map.Map GeniVal Int -- index to charge 
      chargemap =  Map.fromListWith (+) $ concatMap clump $ Map.toList usagemap
        where clump ((_,is),ps) = zip is ps
      -- part 3 (adding extra semantics)
      indices = concatMap fn (Map.toList chargemap) 
        where fn (i,c) = replicate (negate c) i
      -- the extra columns 
      extraSem = map indexLiteral indices
      tsem2    = sortSem (tsem ++ extraSem)
      -- zero-literal semantic items to realise the extra columns 
      zlit = filter (null.tsemantics) cands
      cands2 = (cands \\ zlit) ++ concatMap fn indices
        where fn i = map (tweak i) zlit
              tweak i x = assignIndex i $ x { tsemantics = [indexLiteral i] }
      -- part 4 (insert excess pronouns in tree sem)
      comparefn :: GeniVal -> Int -> Int -> [GeniVal]
      comparefn i ct cm = if cm < ct then extra else []
        where maxNeeded = Map.findWithDefault 0 i chargemap -- cap the number added
              extra = replicate (min (negate maxNeeded) (ct - cm)) i
      comparePron :: (PredLite,SemPols) -> [GeniVal]
      comparePron (lit,c1) = concat $ zipWith3 comparefn idxs c1 c2
        where idxs = snd lit
              c2   = Map.findWithDefault [] lit usagemap
      addextra :: TagElem -> TagElem
      addextra c = c { tsemantics = sortSem (sem ++ extra) }
        where sem   = tsemantics c
              extra = map indexLiteral $ concatMap comparePron (getpols c)
      cands3 = map addextra cands2
  in (tsem2, cands3)

-- | Builds a fake semantic predicate that the index counting mechanism uses to
--   represent extra columns.
indexLiteral :: GeniVal -> Literal
indexLiteral x = Literal x mkGAnon []

-- Returns True if the given literal was introduced by the index counting mechanism
isExtraCol :: Literal -> Bool
isExtraCol (Literal _ p []) = isAnon p
isExtraCol _                = False

-- | 'assignIndex' is a useful way to restrict the behaviour of
-- null semantic items like pronouns using the information generated by
-- the index counting mechanism.  The problem with null semantic items 
-- is that their indices are not set, which means that they could
-- potentially combine with any other tree.  To make things more 
-- efficient, we can set the index of these items and thus reduce the
-- number of spurious combinations.  
-- 
-- Notes
--
-- * These combinations could produce false results if the
--   input has to use multiple pronouns.  For example, if you wanted to say
--   something like “John promises Mary to convince Paul to give her
--   his book”, these combinations could instead produce “give him
--   *her* book“.
--
-- * This function works by FS unification on the root node of the
--   tree with the *[idx:i]*.  If unification is not possible, 
--   we simply return the tree as is.
--
-- * This function renames the tree by appending the index to its name
assignIndex :: GeniVal -> TagElem -> TagElem 
assignIndex i te =
  let idxfs = [ AvPair __idx__ i ]
      oldt  = ttree te
      oldr  = root oldt
      tfup  = gup oldr
      --
  in case unifyFeat tfup idxfs of
     Nothing          -> te
     Just (gup2, sub) -> replace sub $ te { ttree = newt }
       where newt = rootUpd oldt $ oldr { gup = gup2 }

-- ====================================================================
-- Further optimisations
-- ====================================================================

-- Index constraints 
-- -----------------
detectIdxConstraints :: Flist GeniVal -> Flist GeniVal -> PolMap
detectIdxConstraints cs interface =
  let matches  = intersect cs interface
      matchStr = map idxConstraintKey matches
  in Map.fromList $ zip matchStr ((repeat.ival) 1)

declareIdxConstraints :: Flist GeniVal -> PolMap
declareIdxConstraints = Map.fromList . (map declare) where
   declare c = (idxConstraintKey c, minusone)
   minusone = ival (-1)

-- TODO: test that index constraints come first
idxConstraintKey :: AvPair GeniVal -> PolarityKey
idxConstraintKey = PolarityKeyStr . ("." <>) . pretty

-- Automatic polarity detection
-- ----------------------------
suggestPolFeatures :: [TagElem] -> [Text]
suggestPolFeatures tes =
  let -- only initial trees need be counted; in aux trees, the
      -- root node is implicitly canceled by the foot node
      rfeats, sfeats :: [Flist GeniVal]
      rfeats = map (gdown.root.ttree) $ filter (\t -> ttype t == Initial) tes
      sfeats = [ concat s | s <- map substTops tes, (not.null) s ]
      --
      attrs :: Flist GeniVal -> [Text]
      attrs avs = [ a | AvPair a v <- avs, isJust (gConstraints v) ]
      theAttributes = map attrs $ rfeats ++ sfeats
  in if null theAttributes then [] else foldr1 intersect theAttributes

-- FIXME: temporary HACKY code - delete me as soon as possible (written
-- 2006-03-30
--
-- only initial trees need be counted; in aux trees, the
-- root node is implicitly canceled by the foot node
detectSansIdx :: [TagElem] -> [TagElem]
detectSansIdx =
  let rfeats t = (gdown.root.ttree) t
      feats  t | ttype t == Initial = concat $ rfeats t : substTops t
      feats  t = concat $ substTops t
      attrs avs = [ a | AvPair a v <- avs, isJust (gConstraints v) ]
      hasIdx t = __idx__ `elem` (attrs.feats $ t) || (ttype t /= Initial && (null $ substTops t))
  in filter (not.hasIdx)

detectPols :: Set.Set PolarityAttr -> TagElem -> TagElem
detectPols attrs t =
  t { tpolarities = addPols (detectPolsH attrs t) (tpolarities t) }

-- Chart sharing
-- -------------

-- | Given a list of paths (i.e. a list of list of trees)
--   return a list of trees such that each tree is annotated with the paths it
--   belongs to.
detectPolPaths :: [[TagElem]] -> [(TagElem,BitVector)]
detectPolPaths paths = 
  let pathFM     = detectPolPaths' Map.empty 0 paths
      lookupTr k = Map.findWithDefault 0 k pathFM
  in map (\k -> (k, lookupTr k)) $ Map.keys pathFM

type PolPathMap = Map.Map TagElem BitVector
detectPolPaths' :: PolPathMap -> Int -> [[TagElem]] -> PolPathMap  

detectPolPaths' accFM _ [] = accFM
detectPolPaths' accFM counter (path:ps) = 
  let currentBits = shiftL 1 counter -- shift counter times the 1 bit
      fn f []     = f
      fn f (t:ts) = fn (Map.insertWith (.|.) t currentBits f) ts 
      newFM       = fn accFM path
  in detectPolPaths' newFM (counter+1) ps

-- | Render the list of polarity automaton paths as a string
prettyPolPaths :: BitVector -> Text
prettyPolPaths paths =
    T.intercalate ", " $ map pretty pathlist
  where
    pathlist = prettyPolPaths' paths 1

prettyPolPaths' :: BitVector -> Int -> [Int] 
prettyPolPaths' 0 _ = []
prettyPolPaths' bv counter = 
  if b then (counter:next) else next
  where b = testBit bv 0
        next = prettyPolPaths' (shiftR bv 1) (counter + 1)

-- Semantic sorting
-- ----------------

sortSemByFreq :: Sem -> [TagElem] -> Sem
sortSemByFreq tsem cands = 
  let counts = map lenfn tsem 
      lenfn l = length $ filter fn cands 
                where fn x = l `elem` (tsemantics x)
      -- note: we introduce an extra hack to push
      -- index-counted extra columns to the end; just for UI reasons
      sortfn a b 
        | isX a && isX b = compare (snd a) (snd b)
        | isX a          = GT
        | isX b          = LT
        | otherwise      = compare (snd a) (snd b)
        where isX = isExtraCol.fst 
      sorted = sortBy sortfn $ zip tsem counts 
  in (fst.unzip) sorted 

-- ----------------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------------

-- Polarity NFA

data PolState = PolSt Int [Literal] [(Int,Int)]     
                -- ^ position in the input semantics, extra semantics, 
                --   polarity interval
     deriving (Eq)
type PolTrans = TagElem
type PolAut   = NFA PolState PolTrans
type PolTransFn = Map.Map PolState (Map.Map PolState [Maybe PolTrans])

instance Show PolState
  where show (PolSt pr ex po) = show pr ++ " " ++ prettyStr ex ++ show po
-- showPred pr ++ " " ++ showSem ex ++ show po

instance Ord PolState where
  compare (PolSt pr1 ex1 po1) (PolSt pr2 ex2 po2) = 
    let prC   = compare pr1 pr2
        expoC = compare (ex1,po1) (ex2,po2)
    in if (prC == EQ) then expoC else prC

-- We include also some fake states which are useful for general
-- housekeeping during the main algortihms.
fakestate :: Int -> [Interval] -> PolState
fakestate s pol = PolSt s [] pol --PolSt (0, s, [""]) [] pol

-- | an initial state for polarity automata
polstart :: [Interval] -> PolState
polstart pol = fakestate 0 pol -- fakestate "START" pol
