\chapter{Polarity Optimisation}

We introduce a notion of feature structure polarities as a means of
pre-detecting incompatibilities between candidate trees for different
propositions.  This optimisation is inserted between candidate selection
(section \ref{sec:candidate_selection} and chart generation and requires
a minor modification to the generation process.  The input to this
optimisation is the \jargon{target semantics} and the corresponding
\jargon{candidate trees}.

\begin{code}
module Polarity(PolAut,makePolAut,debugMakePolAut,
                TagLite,toTagLite,tlIdnum,
                walkAutomaton, detectPolPaths, defaultPolPaths,
                showLite, showLitePm, showPolPaths, showPolPaths',
                toGvPolAut, calculateTreeCombos 
                )
where
\end{code}

\begin{code}
import FiniteMap
import Data.Bits
import Data.List
--import Data.Set

import Graphviz(GraphvizShow(..))
import Tags(TagElem(..), subsumedBy)
import Btypes(Pred, Sem, showPred, showSem,
              BitVector
              )
\end{code}

%\begin{code}
%import Debug.Trace
%--import Tags
%import Btypes
%
%emptyaut = NFA { transitions = emptyFM,
%                 startSt = polstart [],
%                 finalSt = [] }
%
%emptyaut2 = NFA { transitions = emptyFM,
%                  startSt = "start",
%                  finalSt = [] }
%
%testaut2 = let aut1 = emptyaut2
%               aut2 = addTrans aut1 "start" "t2" "st 2"
%               aut3 = addTrans aut2 "start" "t3" "st 3"
%               in aut3
%
%\end{code}

\section{Overview}

We start with the controller function (the general architecture) and
detail the individual steps in the following sections.  The basic
architecture is as follows:

\begin{enumerate}
\item Do some preprocessing and construct a list of polarity
      automata.
\item Minimise each of the automata (this serves mainly to
      optimise the next step).  See section \ref{sec:automata_pruning}).
\item Take the intersection of all the automata.
\end{enumerate}

We return everything: the initial automata, the pruned automata and the
final intersected automaton.  Only the final one is important; the other
results are useful for debugging later on.

Each candidate is pre-annotated (in the grammar) with a list of polarity 
keys.  We pack the candidates into a map so that they can be looked up
according to what parts of the target semantics they subsume.  Finally, for
each distinct $k$ in this list, we construct a polarity automaton.  

Note: the extraPol argument is a finite map containing any initial
values for polarity keys.  This is useful to impose external filters
like ``I only want sentences'' or ``I only want expressions where 
the object is topicalised''

\begin{code}
makePolAut :: [TagLite] -> Sem -> PolMap -> PolAut
makePolAut cands tsem extraPol = let
    (ks,seed) = makePolAutHelper cands tsem extraPol
    -- building the automata 
    build k oldAut = prune $ buildPolAut k initK oldAut
                     where initK = lookupWithDefaultFM extraPol 0 k
    in foldr build (prune seed) ks

makePolAutHelper :: [TagLite] -> Sem -> PolMap -> ([String],PolAut)
makePolAutHelper cands tsem extraPol =
  let -- polarity items 
      ksCands = concatMap (keysFM.tlPolarities) cands 
      ksExtra = keysFM extraPol
      ks      = nub $ ksCands ++ ksExtra
      -- candidates by predicate
      smap   = mapBySem cands 
      -- the seed automaton
      seed   = buildSeedAut smap tsem
  in (ks, seed)
\end{code}


\paragraph{debugMakePolAut} does the same thing as makePolAut but it returns
all the intermediary results, which is useful for the GUI.

\begin{code}
debugMakePolAut :: [TagLite] -> Sem -> PolMap 
           -> ([(String, PolAut, PolAut)], PolAut)
debugMakePolAut cands tsem extraPol = let
    (ks,seed) = makePolAutHelper cands tsem extraPol
    -- building and remembering the automata 
    build k xs = (k,aut,prune aut):xs
                 where aut   = buildPolAut k initK (third $ head xs)
                       initK = lookupWithDefaultFM extraPol 0 k
    res = foldr build [("(seed)",seed,prune seed)] ks
    in (reverse res, third $ head res)
\end{code}

% ----------------------------------------------------------------------
\section{Types}
\label{}
% ----------------------------------------------------------------------

A reduced version of TagElem for constructing the polarity automaton
without passing massive chunks of tree around.  Note: we assume that
a distinct tlIdnum is sufficient to tell two trees apart.

\begin{code}
data TagLite = TELite {
                   tlIdname      :: String,
                   tlIdnum       :: Integer,
                   tlSemantics   :: Sem,
                   tlPolarities  :: FiniteMap String Int
                }
        deriving (Show, Eq)

instance Ord TagLite where
  compare t1 t2 = 
    compare (tlIdname t1,tlIdnum t1) (tlIdname t2,tlIdnum t2)

emptyTL :: TagLite
emptyTL = TELite { tlIdname = "",
                   tlIdnum  = -1,
                   tlSemantics = [],
                   tlPolarities = emptyFM }
\end{code}

\paragraph{toTagLite} converts a TagElem to a TagLite

\begin{code}
toTagLite :: TagElem -> TagLite
toTagLite te = TELite { tlIdname     = idname te,
                        tlIdnum      = tidnum te,   
                        tlSemantics  = tsemantics te,
                        tlPolarities = tpolarities te
                      }
\end{code}

\begin{code}
type SemMap = FiniteMap Pred [TagLite]
type PolMap = FiniteMap String Int
\end{code}

\subsection{NFA}
Having not found a suitable automaton library for Haskell, we define our
own version of NFA using FiniteMap for the transition function.  Leon P.
Smith's Automata library requires us to know before-hand the size of our
alphabet, which is highly unacceptable for this task.  

Note: these are NFAs without the empty-transition.

\begin{code}
data NFA st ab = NFA { startSt :: st,
                       finalSt :: [st],
                       transitions :: FiniteMap st (FiniteMap ab [st]),
                       -- set of states 
                       -- note: we use a list of list to group the states,
                       -- for the polarity automaton we group them by literal
                       states    :: [[st]]
                     }
\end{code}

\paragraph{lookupTrans} takes an automaton, a state $st1$ and an element
$ab$ of the alphabet; and returns the state that $st1$ transitions to
via $a$, if possible. 

\begin{code}
lookupTrans :: (Ord ab, Ord st) => NFA st ab -> st -> ab -> [st]
lookupTrans aut st ab = lookupWithDefaultFM subT [] ab
  where subT = lookupWithDefaultFM (transitions aut) emptyFM st
\end{code}

\begin{code}
addTrans :: (Ord ab, Ord st) => NFA st ab -> st -> ab -> st -> NFA st ab 
addTrans aut st1 ab st2 = 
  aut { transitions = addToFM oldT st1 newSubT }
  where oldSt2   = lookupWithDefaultFM oldSubT [] ab  
        oldT     = transitions aut
        oldSubT  = lookupWithDefaultFM oldT emptyFM st1
        newSubT  = addToFM oldSubT ab (st2:oldSt2)
\end{code}

\paragraph{nubAut} ensures that all states and transitions in the polarity automaton 
are unique.  This is a slight optimisation so that we don't have to repeatedly check
the automaton for state uniqueness during its construction, but it is essential that
this check be done after construction

\begin{code}
nubAut :: (Ord ab, Ord st) => NFA st ab -> NFA st ab 
nubAut aut = 
  let subnub _ e = nub e
  in aut {
      transitions = mapFM (\_ e -> mapFM subnub e) (transitions aut)
     }
\end{code}

\subsection{Polarity NFA}

We can define the polarity automaton as a NFA, or a five-tuple 
$(Q, \Sigma, \delta, q_0, q_n)$ such that 

\begin{enumerate}
\item $Q$ is a set of states, each state being a tuple $(i,e,p)$ where $i$
is a single literal in the target semantics, $e$ is a list of extra literals
which are known by the state, and $p$ is a polarity.
\item $\Sigma$ is the union of the sets of candidate trees for all
propositions
\item $q_0$ is the start state $(0,0)$ which does not correspond to any
propositions and is used strictly as a starting point.
\item $q_n$ is the final state $(n,0)$ which corresponds to the last
proposition, with polarity $0$.
\item $\delta$ is the transition function between states, which we
define below.
\end{enumerate}

Note: for convenience during automaton intersection, we actually define the
states as being $(i, [p])$ where $[p]$ is a list of states.  

\begin{code}
data PolState = PolSt Pred [Pred] [Int] deriving (Eq)
type PolTrans = TagLite
type PolAut   = NFA PolState PolTrans
type PolTransFn = FiniteMap PolState (FiniteMap PolTrans [PolState])
\end{code}

\begin{code}
instance Show PolState
  where show (PolSt pr ex po) = showPred pr ++ " " ++ showSem ex ++ show po
\end{code}

\begin{code}
instance Ord PolState where
  compare (PolSt pr1 ex1 po1) (PolSt pr2 ex2 po2) = 
    let prC   = compare pr1 pr2
        expoC = compare (ex1,po1) (ex2,po2)
    in if (prC == EQ) then expoC else prC
\end{code}

We include also some fake states which are useful for general
housekeeping during the main algortihms.

\begin{code}
fakestate :: String -> [Int] -> PolState
fakestate s pol = PolSt ("h0", s, [""]) [] pol
-- an initial state for polarity automata
polstart :: [Int] -> PolState
polstart pol = fakestate "START" pol
-- a fake state is the final state of an empty automaton 
polfake :: PolState
polfake = fakestate "FAKE-FINAL" []
\end{code}

% ----------------------------------------------------------------------
\section{Map by sem}
% ----------------------------------------------------------------------

FIXME: figure out how to refactor this and Tag.mapBySem. Seems like
the problem is that this operates on TagLite and Tag operates on Tag

The mapBySem function organises trees such that each literal of the target
semantics is associated with a list of trees whose semantics are
subsumed by that literal.  This is useful in at least three places: the
polarity optimisation, the gui display code, and code for measuring 
the efficiency of Geni.

Notes: \begin{itemize}
\item A tree will only appear for the first literal that subsumes its
semantics.  During the construction process, we will have a mechanism for
dealing with these trees
\end{itemize}

\begin{code}
mapBySem :: [TagLite] -> SemMap 
mapBySem ts = 
  foldr (mapBySem' lits) emptyFM nonNullT 
  where lits = nub $ concatMap tlSemantics ts 
        nonNullT = filter (not.null.tlSemantics) ts 
\end{code}

Our helper function focuses on a single tree.  It checks the literals one by
one.  The first literal that subsumes the trees' semantics is the one that 
the tree is mapped to.

\begin{code}
mapBySem' :: Sem -> TagLite -> SemMap -> SemMap
mapBySem' [] _ fm = fm
mapBySem' (lit:lits) tr fm = 
   if subsumedBy (tlSemantics tr) lit 
   then addToFM_C (++) fm lit [tr]
   else mapBySem' lits tr fm 
\end{code}

% ====================================================================
\section{Polarity automaton}
\label{sec:automaton_construction}
% ====================================================================

We now a have a set of trees such that each tree is associated with a
list of feature-value polarity sums.  The next step is to use the
polarities to rule out definitely unusable combinations of candidates,
those whose combination results in non-zero polarities.

We construct a finite state automaton for each distinct feature-value
pair that in the set of trees.  It helps to imagine a table where each
column corresponds to a single proposition.  Each column (proposition)
has a different number of cells which corresponds to the lexical
ambiguity for that proposition, more concretely, the number of
candidate trees for that proposition.  The \jargon{polarity automaton}
describes the different ways we can traverse the table from column to
column, choosing a cell to pass through at each step and accumulating
polarity along the way.  Each state represents the polarity at a column
and each transition represents the tree we chose to get there.  All
transitions from one columns $i$ to the next $i+1$ that lead to the same
accumulated polarity lead to the same state.  

% ----------------------------------------------------------------------
\subsection{Initial Automaton}
\label{sec:seed_automaton}
% ----------------------------------------------------------------------

We first construct a relatively trivial polarity automaton without any
polarity effects.  Each state except the start state corresponds
to a literal in the target semantics, and the transitions to a state 
consist of the trees whose semantics is subsumed by that literal.  

\begin{code}
buildSeedAut :: SemMap -> Sem -> PolAut
buildSeedAut cands tsem = 
  let start = polstart []
      endst = PolSt (last tsem) [] []
      end   = if (null tsem) then [polfake] else [endst]
      initAut = NFA { startSt = start,
                      finalSt = end,
                      states  = [[start]],
                      transitions = emptyFM }
  in nubAut $ buildSeedAut' cands tsem initAut

-- for each literal...
buildSeedAut' :: SemMap -> Sem -> PolAut -> PolAut 
buildSeedAut' _ [] aut = aut 
buildSeedAut' cands (l:ls) aut = 
  let -- previously created candidates 
      prev   = head $ states aut
      -- candidates that match the target semantics
      tcands = lookupWithDefaultFM cands [] l 
      -- create the next batch of states
      fn st ap             = buildSeedAutHelper tcands l st ap
      (newAut,newStates)   = foldr fn (aut,[]) prev
      next                 = (nub newStates):(states aut)
      -- recursive step to the next literal
  in buildSeedAut' cands ls (newAut { states = next })

-- for each candidate corresponding to literal l...
buildSeedAutHelper :: [TagLite] -> Pred -> PolState -> (PolAut,[PolState]) -> (PolAut,[PolState]) 
buildSeedAutHelper cs l st (aut,prev) =
  let -- get the extra semantics from the last state
      (PolSt _ ex1 _) = st
      -- candidates that match the target semantics and which
      -- do not overlap the extra baggage semantics
      tcand     = filter notInEx cs 
      notInEx t = isEmptyIntersect ex1 (tlSemantics t)
      -- add the transitions out of the current state 
      addT tr (a,n) = (addTrans a st tr st2, st2:n)
                      where ex2  = delete l (ex1 ++ tlSemantics tr)
                            st2  = PolSt l ex2 []
  in if (l `elem` ex1) 
     then addT emptyTL (aut,prev)
     else foldr addT   (aut,prev) tcand 
\end{code}

% ----------------------------------------------------------------------
\subsection{Construction}
\label{sec:automaton_intersection}
% ----------------------------------------------------------------------

The goal is to construct a polarity automaton which accounts for a
given \jargon{polarity key} $k$.  The basic idea is that given 
literals $p_1..p_n$ in the target semantics, we create a start state,
calculate the states/transitions to $p_1$ and succesively calculate
the states/transitions from proposition $p_x$ to $p_{x+1}$ for all
$1 < x < n$. 

The ultimate goal is to construct an automaton that accounts for 
multiple polarity keys.  The simplest approach would be to 
calculate a seperate automaton for each key, prune them all and 
then intersect the pruned automaton together, but we can do much 
better than that.  Since the pruned automata are generally much
smaller in size, we perform an iterative intersection by using 
a previously pruned automaton as the skeleton for the current 
automaton.  This is why we don't pass any literals or candidates
to the construction step; it takes them directly from the previous
automaton.  See also section \ref{sec:seed_automaton} for the seed 
automaton that you can use when there is no ``previous automaton''.

\begin{code}
buildPolAut :: String -> Int -> PolAut -> PolAut 
buildPolAut k initK skelAut =
  let concatPol p (PolSt pr b pol) = PolSt pr b (p:pol)
      newStart = concatPol initK $ startSt skelAut
      initAut  = NFA { startSt = newStart,
                    finalSt = map (concatPol 0) $ finalSt skelAut,
                    states  = [[newStart]],
                    transitions = emptyFM }
      -- cand' = observe "candidate map" cand 
  in nubAut $ buildPolAut' k (transitions skelAut) initAut 
\end{code}

Our helper function looks at a single state in the skeleton automaton
and at one of the states in the new automaton which correspond to it.
We use the transitions from the old automaton to determine which states
to construct.  Note: there can be more than one state in the automaton
which corresponds to a state in the old automaton.  This is because we
are looking at a different polarity key, so that whereas two candidates
automaton may transition to the same state in the old automaton, their
polarity effects for the new key will make them diverge in the new
automaton.  

\begin{code}
buildPolAut' :: String -> PolTransFn -> PolAut -> PolAut
-- for each literal... (this is implicit in the automaton state grouping)
buildPolAut' fk skeleton aut = 
  let -- previously created candidates 
      prev = head $ states aut 
      -- create the next batch of states
      fn st ap            = buildPolAutHelper fk skeleton st ap
      (newAut,newStates)  = foldr fn (aut,[]) prev
      next                = (nub newStates):(states aut)
      -- recursive step to the next literal
  in if (null newStates)
     then aut
     else buildPolAut' fk skeleton (newAut { states = next })

-- given a previously created state...
buildPolAutHelper :: String -> PolTransFn -> PolState -> (PolAut,[PolState]) -> (PolAut,[PolState])  
buildPolAutHelper fk skeleton st (aut,prev) =
  let -- reconstruct the skeleton state used to build st 
      PolSt pr ex (po1:skelpo1) = st
      skelSt = PolSt pr ex skelpo1
      -- for each transition out of the current state...
      result      = foldr addT (aut,prev) cands 
      skelStTrans = lookupWithDefaultFM skeleton emptyFM skelSt
      cands       = keysFM skelStTrans 
      -- . for each state that a label transitions to...
      addT tr (a,n) = foldr (addTS tr) (a,n) (lookupTr tr)
      lookupTr tr   = lookupWithDefaultFM skelStTrans [] tr
      -- .. calculate a new state and add a transition to it
      addTS tr skel2 (a,n) = (addTrans a st tr st2, st2:n)
                             where st2 = newSt tr skel2
      newSt t skel2 = PolSt pr2 ex2 (po2:skelPo2)
                      where PolSt pr2 ex2 skelPo2 = skel2 
                            po2 = po1 + (lookupWithDefaultFM pol 0 fk)
                            pol = tlPolarities t   
  in result 
\end{code}

% ----------------------------------------------------------------------
\subsection{Pruning}
\label{sec:automaton_pruning}
% ----------------------------------------------------------------------

Any path through the automaton which does not lead to final
polarity of zero sum can now be eliminated.  We do this by stepping
recursively backwards from the final states: 

\begin{code}
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
\end{code}

The pruning algorithm takes as arguments a list of states to process.
Among these, any state which does not have outgoing transitions is
placed on the blacklist.  We remove all transitions to the blacklist and
all states that only transition to the blacklist, and then we repeat
pruning, with a next batch of states.  

Finally, we return the pruned automaton.  Note: in order for this to
work, it is essential that the final states are *not* included in the
list of states to process.

\begin{code}
prune' :: [[PolState]] -> PolAut -> PolAut
prune' [] oldAut = oldAut { states = reverse $ states oldAut }
prune' (sts:next) oldAut = 
  let -- calculate the blacklist
      oldT  = transitions oldAut
      oldSt = states oldAut
      sansTrans st = case (lookupFM oldT st) of 
                       Nothing -> True 
                       Just _  -> False 
      blacklist    = filter sansTrans sts
      -- removing all transitions to the blacklist
      subDelete  = mapFM (\_ e -> e \\ blacklist)
      -- removing all transitions labels that only go to the blacklist
      subCleanup = filterFM (\_ e -> not $ null e)
      -- removing all states that only transition to the blacklist
      newT    = cleanup $ mapFM (\_ e -> subCleanup $ subDelete e) oldT
      cleanup = filterFM (\_ e -> not $ isEmptyFM e)
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
  in if (null blacklist) 
     then oldAut { states = (reverse oldSt) ++ (sts:next) }
     else prune' next newAut 
\end{code}

% ----------------------------------------------------------------------
\subsection{Walking the automaton}
% ----------------------------------------------------------------------

In order to perform generation, we must recuperate all the possible
paths from the automaton.  We do this by applying a simple algorithm for
walking the automaton.  

\emph{WARNING!} This function assumes the automaton does not have any
loops (which is the case for polarity automata).  Actually if there are
any loops, it will simply ignore them.

We assume also that the automaton has already been pruned.

\begin{code}
walkAutomaton :: PolAut -> [[TagLite]]
walkAutomaton aut = walkAutomaton' (transitions aut) (startSt aut) 

walkAutomaton' :: PolTransFn -> PolState -> [[TagLite]]
walkAutomaton' transFm st = 
  let trans        = lookupWithDefaultFM transFm emptyFM st
      lookupTr ab  = lookupWithDefaultFM trans [] ab
      newTransFm   = delFromFM transFm st
      --   
      cands      = keysFM trans
      -- recursive steps: we call walkAutomaton' for each state we 
      -- transition to and then add the label for the current 
      -- transition to the results
      next  ab     = concatMap (next' ab) (lookupTr ab)
      next' ab st2 = if (ab == emptyTL) 
                     then n 
                     else if (null n) then [[ab]] else map (ab :) n
                     where n = walkAutomaton' newTransFm st2 
  in concatMap next cands 
\end{code}

% ----------------------------------------------------------------------
\subsection{Detecting automaton paths}
% ----------------------------------------------------------------------

This is for the chart-sharing optimisation.  Given a list of paths 
(i.e. a list of list of trees), we return a list of trees such that
each tree is annotated with the paths it belongs to.

\begin{code}
detectPolPaths :: [[TagElem]] -> [TagElem] 
detectPolPaths paths =
  let pathFM     = detectPolPaths' emptyFM 0 paths
      lookupTr k = lookupWithDefaultFM pathFM 0 k
      setPath k  = k {tpolpaths = lookupTr k}
  in map setPath $ keysFM pathFM

type PolPathMap = FiniteMap TagElem BitVector
detectPolPaths' :: PolPathMap -> Int -> [[TagElem]] -> PolPathMap  

detectPolPaths' accFM _ [] = accFM
detectPolPaths' accFM counter (path:ps) = 
  let currentBits = shiftL 1 counter -- shift counter times the 1 bit
      fn f []     = f
      fn f (t:ts) = fn (addToFM_C (.|.) f t currentBits) ts 
      newFM       = fn accFM path
  in detectPolPaths' newFM (counter+1) ps
\end{code}

\paragraph{defaultPolPaths} sets the polarity paths for every tree in
the list to be 1.  This is neccesary if you want to use the generator
with the chart sharing optimisation

\begin{code}
defaultPolPaths :: [TagElem] -> [TagElem]
defaultPolPaths = map fn 
                  where fn t = t { tpolpaths = 1 } 
\end{code}

\paragraph{showPolPaths} displays the list of polarity automaton paths
that the tree is on

\begin{code}
showPolPaths :: TagElem -> String  
showPolPaths te = 
  let pathlist = showPolPaths' (tpolpaths te) 1 
  in concat $ intersperse ", " $ map show pathlist

showPolPaths' :: BitVector -> Int -> [Int] 
showPolPaths' 0 _ = []
showPolPaths' bv counter = 
  if b then (counter:next) else next
  where b = testBit bv 0
        next = showPolPaths' (shiftR bv 1) (counter + 1)
\end{code}

% ----------------------------------------------------------------------
\section{Display code}
\label{sec:display_pol}
% ----------------------------------------------------------------------

\subsection{showLite}

The showLite functions could be used to replace the default instances of
show, but with some nervousness on my part because we deliberatly cut out
information that could useful.  Mostly, whenever we have trees, we try
to print the treenames instead of the entire tree.

\paragraph{showLite} can be used to display a Polarity automaton as
human readable text.

\begin{code}
class ShowLite a where
  showLite :: a -> String

instance (ShowLite a) => ShowLite [a] where
  showLite x = "[" ++ (concat $ intersperse ", " $ map showLite x) ++ "]"
instance (ShowLite a, ShowLite b) => ShowLite (a,b) where
  showLite (x,y) = "(" ++ (showLite x) ++ "," ++ (showLite y) ++ ")"

instance ShowLite Int where showLite = show 
instance ShowLite Char where showLite = show 
\end{code}

%\begin{code}
%instance (Show st, ShowLite ab) => ShowLite (NFA st ab) where
%  showLite aut = 
%    concatMap showTrans $ fmToList (transitions aut)
%    where showTrans ((st1, x), st2) = show st1 ++ showArrow x 
%                                 ++ show st2 ++ "\n"
%          showArrow x = " --" ++ showLite x ++ "--> " 
%        -- showSt (PolSt pr po) = show po
%\end{code}

\begin{code}
instance ShowLite TagElem where
  showLite = idname 
instance ShowLite TagLite where
  showLite = tlIdname
\end{code}

\paragraph{showLiteSm} can be used to display a SemMap in human readable text.

\begin{code}
{-
showLiteSm :: SemMap -> String
showLiteSm sm = 
  concatMap showPair $ fmToList sm 
  where showPair  (pr, cs) = showPred pr ++ "\t: " ++ showPair' cs ++ "\n"
        showPair' [] = ""
        showPair' (te:cs) = tlIdname te ++ "[" ++ showLitePm (tlPolarities te) ++ "]"
                                        ++ " " ++ showPair' cs 
-}
\end{code}

\paragraph{showLitePm} can be used to display a PolMap in human-friendly text.
The advantage is that it displays fewer quotation marks.
\begin{code}
showLitePm :: PolMap -> String
showLitePm pm = 
  let showPair (f, pol) = charge pol ++ f 
      charge c = case () of _ | c == -1   -> "-"
                              | c ==  1   -> "+"
                              | c  >  0   -> "+" ++ (show c)
                              | otherwise -> (show c) 
  in concat $ intersperse " " $ map showPair $ fmToList pm
\end{code}

\subsection{Drawing automata}

It would  be nice eventually to say something like
instance GraphvizShow PolAut directly.

\begin{code}
newtype GvPolAut = GvPolAut PolAut 

toGvPolAut :: PolAut -> GvPolAut
toGvPolAut aut   = GvPolAut aut

instance GraphvizShow GvPolAut where
  -- we want a directed graph (arrows)
  graphvizShow (GvPolAut aut) = 
     "digraph aut {\n" 
     ++ "rankdir=LR\n" 
     ++ "ranksep = 0.02\n"
     ++ "pack=1\n"
     ++ "edge [ fontsize=10 ]\n"
     ++ "node [ fontsize=10 ]\n"
     -- ++ "rotate=90\n" -- *sniff* graphviz can rotate, but it's unreadable
     ++ gvShowFinal aut stmap 
     -- any other state should be an ellipse
     ++ "node [ shape = ellipse, peripheries = 1 ]\n" 
     -- draw the states and transitions 
     ++ concat [ concat $ zipWith gvShowState ids st 
               , concat $ zipWith (gvShowTrans aut stmap) ids st ]
     ++ "}" 
     where st    = (concat.states) aut
           ids   = map (\x -> "n" ++ show x) [0..]
           -- map which permits us to assign an id to a state
           stmap = listToFM $ zip st ids
\end{code}

\begin{code}
gvShowState :: String -> PolState -> String
gvShowState stId st = " " ++ stId ++ " [ label=\"" ++ showSt st ++ "\" ];\n"
  where showSt (PolSt pr ex po) = showPr pr ++ showEx ex ++ showPo po
        showPr _ = "" -- (_,pr,_) = pr ++ "\\n"
        showPo po = concat $ intersperse "," $ map show po
        showEx ex = if (null ex) then "" else (showSem ex) ++ "\\n"
\end{code}

Specify that the final states are drawn with a double circle

\begin{code}
gvShowFinal :: PolAut -> FiniteMap PolState String -> String
gvShowFinal aut stmap = 
  if isEmptyIntersect (concat $ states aut) fin 
  then ""
  else "node [ peripheries = 2 ]; " 
  ++ concatMap (\x -> " " ++ lookupId x) fin
  ++ "\n"
  where fin = finalSt aut
        lookupId x = lookupWithDefaultFM stmap "error_final" x
\end{code}

Each transition is displayed with the name of the tree.  If there is more
than one transition to the same state, they are displayed on a single
label.

\begin{code}
gvShowTrans :: PolAut -> FiniteMap PolState String
               -> String -> PolState -> String 
gvShowTrans aut stmap idFrom st = 
  let -- outgoing transition labels from st
      alpha = keysFM $ lookupWithDefaultFM (transitions aut) emptyFM st
      -- associate each st2 with a list of labels that transition to it
      inverter x fm = foldr fn fm (lookupTrans aut st x)
                      where fn    s f   = addToFM f s (xlist s f x)
                            xlist s f x = x:(lookupWithDefaultFM f [] s)
      invFM = foldr inverter emptyFM alpha
      -- returns the graphviz dot command to draw a labeled transition
      drawTrans (stTo,x) = case lookupFM stmap stTo of
                             Nothing   -> drawTrans' ("id_error_" ++ (showSem stTo)) x 
                             Just idTo -> drawTrans' idTo x
                           where showSem (PolSt (_,pred,_) _ _) = pred 
      drawTrans' idTo x = " " ++ idFrom ++ " -> " ++ idTo ++ 
                          " [ label=\"" ++ drawLabel x ++ "\"];\n"
      drawLabel labels = concat $ intersperse "\\n" $ map fn labels
                         where fn x = if (x == emptyTL) then "EMPTY" else tlIdname x
  in concatMap drawTrans $ fmToList invFM
\end{code}

%gvShowTransPred te = 
%  let p = tpredictors te
%      charge fv = case () of _ | c == -1   -> "-"
%                               | c ==  1   -> "+"
%                               | c  >  0   -> "+" ++ (show c)
%                               | otherwise -> (show c) 
%                  where c = lookupWithDefaultFM p 0 fv
%      showfv (f,v) = charge (f,v) ++ f 
%                   ++ (if (null v) then "" else ":" ++ v)
%  in map showfv $ keysFM p 

% ----------------------------------------------------------------------
\section{Miscellaneous}
% ----------------------------------------------------------------------

\begin{code}
third :: (a,b,c) -> c
third (_,_,x) = x
\end{code}

\paragraph{hasNoIntersect} is true if the intersection of two lists is empty.

\begin{code}
isEmptyIntersect :: (Eq a) => [a] -> [a] -> Bool
isEmptyIntersect a b = null $ intersect a b
\end{code}

\paragraph{calculateTreeCombos} is a naive method for calculating the
combinatorics of the generation problem.  Lexical ambiguity causes the
generator to consider an exponential number of tree combinations.  If each
literal $i$ has some degree of ambiguity $a_i$ (the number of ways to realise
$i$), then the possible ways to choose realisations for the target semantics is
product of these ambiguities: $\prod_{1 \leq i \leq n} a_i$.  
(Note: if we simplify this by assuming a maximum $a$ for all literals, then the
 number of tree combinations is $a^n$).  

\begin{code}
calculateTreeCombos :: [TagLite] -> Int
calculateTreeCombos cands = 
  let semmap    = mapBySem cands
      ambiguity = map length $ eltsFM semmap
  in foldr (*) 1 ambiguity     
\end{code}
