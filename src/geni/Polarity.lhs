\chapter{Polarity Optimisation}

We introduce a notion of polarities as a means of pre-detecting
incompatibilities between candidate trees for different propositions.
This optimisation is inserted between candidate selection 
(section \ref{sec:candidate_selection})
and chart generation.  The input to this optimisation is the
\jargon{target semantics} and the corresponding \jargon{candidate
  trees}.

This whole optimisation is based on adding polarities to the grammar.
We have a set of strings which we call \jargon{polarity keys}, and some
positive or negative integers which we call \jargon{charges}.  Each tree
in the grammar may assign a charge to some number of polarity keys.  For
example, here is a simple grammar that uses the polarity keys n and v.

\begin{tabular}{|l|l|}
\hline
tree & polarity effects \\
\hline
s(n$\downarrow$, v$\downarrow$, n$\downarrow$) & -2n -v\\
v(hates) & +v\\
n(mary) & +n\\
n(john) & +n\\
\hline
\end{tabular}

For now, these annotations are done by hand, and are based on syntactic
criteria (substitution and root node categories) but one could envisage
alternate criteria or an eventual means of automating the process.  

~\\
The basic idea is to use the polarity keys to determine which subsets of
candidate trees are incompatible with each other and rule them out.  We
construct a finite state automaton which uses polarity keys to
pre-calculate the compatibility of sets of trees.  At the end of the
optimisation, we are left with an automaton, each path of which is a
potentially compatible set of trees.  We then preform surface
realisation seperately, treating each path as a set of candidate trees.

\begin{code}
module Polarity(PolAut,makePolAut,
                TagLite, reduceTags, buildSemWeights,
                walkAutomaton, detectPols, detectPolPaths, 
                defaultPolPaths,
                showLite, showLitePm, showPolPaths, showPolPaths',
                toGvPolAut, calculateTreeCombos 
                )
where
\end{code}

\begin{code}
import FiniteMap
import Data.Array (listArray, (!)) 
import Data.Bits
import Data.List
--import Data.Set

import Graphviz(GraphvizShow(..))
import Tags(TagElem(..), mapBySem, substnodes)
import Bfuncs(Pred, Sem, emptyPred, Ptype(Initial),
              showPred, showSem, root, gup,
              BitVector, toKeys,
              groupByFM, isEmptyIntersect, third)
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
%
\section{Overview}

We start with the controller function (the general architecture) and
detail the individual steps in the following sections.  The basic
architecture is as follows:

\begin{enumerate}
\item Build a seed automaton (section \ref{sec:seed_automaton}).
\item For each polarity key, elaborate the 
      automaton with the polarity information for that key
      (section \ref{sec:automaton_construction}) and minimise
      the automaton (section \ref{sec:automaton_pruning}).
\end{enumerate}

The above process can be thought of as a more efficient way of
constructing an automaton for each polarity key, minimising said
automaton, and then taking their intersection.  In any case, 
we return everything a tuple with (1) a list of the automota that
were created and (2) the final automaton.  The first item is only
neccesary for debugging; only the second is important.  

Note: the extraPol argument is a finite map containing any initial
values for polarity keys.  This is useful to impose external filters
like ``I only want sentences'' or ``I only want expressions where 
the object is topicalised''.

\begin{code}
makePolAut :: [TagLite] -> Sem -> PolMap -> SemWeightMap 
              -> ([(String, PolAut, PolAut)], PolAut)
makePolAut cands tsem extraPol swmap = let
    (ks,seed) = makePolAutHelper cands tsem extraPol swmap
    -- building and remembering the automata 
    build k xs = (k,aut,prune aut):xs
                 where aut   = buildPolAut k initK (third $ head xs)
                       initK = lookupWithDefaultFM extraPol 0 k
    res = foldr build [("(seed)",seed,prune seed)] ks
    in (reverse res, third $ head res)

makePolAutHelper :: [TagLite] -> Sem -> PolMap -> SemWeightMap ->
                    ([String],PolAut)
makePolAutHelper cands tsemRaw extraPol swmap =
  let -- polarity items 
      ksCands = concatMap (keysFM.tlPolarities) cands 
      ksExtra = keysFM extraPol
      ks      = nub $ ksCands ++ ksExtra
      -- candidates by predicate
      smapRaw  = mapBySem tlSemantics cands 
      -- accounting for multi-use items
      (tsem, smap) = addExtraIndices swmap (tsemRaw,smapRaw)
      -- sorted semantics (for more efficient construction)
      sortedsem = sortSemByFreq tsem smap
      -- the seed automaton
      seed   = buildSeedAut smap sortedsem
  in (ks, seed)
\end{code}

% ====================================================================
\section{Polarity automaton}
\label{sec:polarity_automaton}
% ====================================================================

We construct a finite state automaton for each polarity key that is in
the set of trees. It helps to imagine a table where each column
corresponds to a single proposition.  

\begin{center}
\begin{tabular}{|c|c|c|}
\hline
\semexpr{gift(g)} & \semexpr{cost(g,x)}    & \semexpr{high(x)} \\
\hline
\natlang{the gift} \color{blue}{+1np} & 
\natlang{the cost of}  & 
\natlang{is high} \color{red}{-1np} \\
%
\natlang{the present}    \color{blue}{+1np} & 
\natlang{costs} \color{red}{-1np}    & 
\natlang{a lot}    \\
%
&& \natlang{much} \\
\hline
\end{tabular}
\end{center}

Each column (proposition) has a different number of cells which
corresponds to the lexical ambiguity for that proposition, more
concretely, the number of candidate trees for that proposition.  The
\jargon{polarity automaton} describes the different ways we can traverse
the table from column to column, choosing a cell to pass through at each
step and accumulating polarity along the way.  Each state represents the
polarity at a column and each transition represents the tree we chose to
get there.  All transitions from one columns $i$ to the next $i+1$ that
lead to the same accumulated polarity lead to the same state.  

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
\label{sec:automaton_construction}
\label{sec:automaton_intersection}
% ----------------------------------------------------------------------

The goal is to construct a polarity automaton which accounts for a
given polarity key $k$.  The basic idea is that given 
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

% ====================================================================
\section{Multi-use items}
\label{sec:multiuse}
\label{semantic_weights}
% ====================================================================

There is a complication when generating items that refer to the same
entity more than once. Say for instance we have the input
\semexpr{e1:love(j,j), j:name(John)}.  To simplify matters, we will
ignore pronouns and clitics, and also forget that the generator actually
prevents items with overlapping semantics to combine.  If we take the
assumptions above, the generator would be capaable of producing the
realisation \natlang{John loves himself}.  However, once we introduce
polarity filtering, it and many other valid results get ruled out.  Why?
Because polarity counting assumes that all items are used exactly once. 

\begin{center}
\begin{tabular}{|c|c|}
\hline
\semexpr{h1:john(j)}  & \semexpr{h2:like(j,j)} \\
\hline
\tautree{John} \color{blue}{+1np}& 
\tautree{likes} \color{red}{-2np}\\
\tautree{himself} \color{blue}{+1np} & 
\\
\hline
\end{tabular}\\
\end{center}

If you look at the table above, you can see that what needs to happen is
that we count two {\color{blue}{+1np}}s for \semexpr{j:name(John)}, but
since we only have one column for \semexpr{h1:john(j)}, it only gets
counted once.

%One
%possible solution is to repeat the literal as many times as we need it.
%For example, in the the table below, \tautree{John} gets counted twice
%as is the expected/desired behaviour:
%
%\begin{center}
%\begin{tabular}{|c|c|c|}
%\hline
%\semexpr{j:john(j)}  &\semexpr{j:name(John)}  & \semexpr{e1:likes(j,j)} \\
%\hline
%\tautree{John} \color{blue}{+1np} & 
%\tautree{John} \color{blue}{+1np} & 
%\tautree{loves} \color{red}{-2np}\\
%\hline
%\end{tabular}\\
%\end{center}

\subsection{Multi-counting}

We propose a solution based on counting the indices in our target
semantics. We augment the semantic lexicon with a map of
\jargon{semantic weights}.  Semantic weights are positive or negative
integers attached to the parameters of each semantic predicate, for
example:

\begin{center}
\begin{tabular}{|c|c|c|c|}
\hline
\textbf{semantics} & \textbf{params} & \textbf{weights}
\\
\hline
\semexpr{H:john(X)}      & H X    &  -1 -1  \\
\hline
\semexpr{H:mary(X)}      & H X    &  -1 -1  \\
\hline
\semexpr{H:dog(X)}       & H X    &  -1 -1  \\
\hline
\semexpr{H:black(X)}     & H X    &  -1 0  \\
\hline
\semexpr{H:def(X)}       & H X    &  -1 0  \\
\hline
\semexpr{E:like(X Y)}    & E X Y  &  -1 1 1 \\
\hline
\semexpr{E:hope(X Y)}    & E X Y  &  -1 0 1 \\
\hline
\semexpr{E:say(X Y)}     & E X Y  &  -1 1 1 \\
\hline
\end{tabular}\\
\end{center}

The idea is that the number of (extra) times that each index must be
accounted for is equal to its total weight in the target semantics:

\begin{center}
\begin{tabular}{|c|c|}
\hline
\textbf{sentence} & \textbf{semantics and weights} \\
\hline
\multirow{3}*{\natlang{John likes Mary}} 
  & \verb$h1:like(j m) h2:john( j) h3:mary( m)$ \\
  & \verb$-1:like(1 1) -1:john(-1) -1:mary(-1)$ \\
  & j:0, m:0\\
\hline
\multirow{3}*{\natlang{John likes himself}}
  & \verb$h1:like(j j) h2:john(j)$ \\
  & \verb$-1:like(1 1) -1:john(-1)$ \\
  & j:1 \\
\hline
\multirow{3}*{\natlang{John says he likes himself}}
  & \verb$h1:say(j h3) h2:john( j) h3:like(j j)$ \\
  & \verb$-1:say(1 0 ) -1:john(-1) -1:like(1 1)$ \\
  & j:2 \\
\hline
\multirow{3}*{\natlang{John hopes to like himself}}
  & \verb$h1:hope(j h3) h2:john( j) h3:like(j j)$ \\
  & \verb$-1:hope(0 0 ) -1:john(-1) -1:like(1 1)$ \\
  & j:1 \\
\hline
\multirow{3}*{\natlang{John hopes to like Mary}}
  & \verb$h1:hope(j h3) h2:john( j) h3:like(j m) h4:mary( m)$ \\
  & \verb$-1:hope(0 0 ) -1:john(-1) -1:like(1 1) -1:mary(-1)$ \\
  & j:0, m:0 \\
\hline
\multirow{3}*{\natlang{The black dog likes itself}}
  & \verb$h1:like(d d) h2:dog( d) h3:def(d) h4:black(d)$\\
  & \verb$-1:like(1 1) -1:dog(-1) -1:def(0) -1:black(0)$ \\
  & d:1 \\
\hline
\end{tabular}\\
\end{center}

So what do we do with this information?  We add extra columns for the
indices: the number of columns that we add is equal to the weight of
each index.  As for the contents of these columns, we'll just dump in 
null semantic items like pronouns 
(assuming of course that pronouns, clitics, and so forth are treated
 as null semantic items):

\begin{center}
\natlang{John says he likes himself}
\begin{tabular}{|c|c|c|c|c|}
\hline
\semexpr{h1:say(j h3)}  & \semexpr{h2:john( j)} & 
\semexpr{h3:like(j j)}  & j & j \\
\hline
\tautree{says} \color{red}{-1np}  &
\tautree{John} \color{blue}{+1np} & 
\tautree{likes} \color{red}{-2np} &
\tautree{he}    \color{blue}{+1np} &
\tautree{he}    \color{blue}{+1np} 
\\
&
&
&
\tautree{himself} \color{blue}{+1np} & 
\tautree{himself} \color{blue}{+1np}  
\\
\hline
\end{tabular}\\
\end{center}

This model is implemented in the functions below:

\paragraph{buildSemWeights} compiles information from the semantic lexicon
regarding semantic weights (see section \ref{semantic_weights}).  We
take a list of inputs that maps a semantic weight profile to the list of
predicates that have that profile.  We output a map from a semantic keys
(predicate + arity) to its semantic weight profile, following the 
assumption that each semantic key only has a single profile.

\begin{code}
buildSemWeights :: [ ([Int], [String]) ] -> SemWeightMap 
buildSemWeights wps = 
  let helper :: ([Int], [String]) -> SemWeightMap -> SemWeightMap
      helper (ws,prs) fm = foldr (addfn ws arity) fm prs
                           where arity = show $ length ws - 1
      --
      addfn :: [Int] -> String -> String -> SemWeightMap -> SemWeightMap 
      addfn ws arity pr fm = addToFM fm (pr ++ arity) ws 
      --
  in foldr helper emptyFM wps
\end{code}

\paragraph{addExtraIndices} modifies an input semantics and a semantic
map so that 1) extra indices (extra columns) are added to the input
semantics to account for repeated mentions to an index 2) null semantic
items (stuff to fill the extra columns) are added to the semantic map.  

Note that in addition to the null semantic items, we also add the
possibility for an empty transition. Why?  Because the case for control
verbs might be more complicated than we mentioned above.  For example, 
the input semantics \semexpr{h1:hope(j h3) h2:john(j) h3:like(j j)}
could be realised not only as \natlang{John hopes to like himself} but
also as \natlang{John hopes that he likes himself}; note how one uses
more pronouns than the other.  To account for this, we actually give
control verbs the same semantic weight as regular verbs, 0:(1 1) in the
case of hope, but make use of empty transitions to allow for a
realisation with one pronoun less.

\begin{code}
addExtraIndices :: SemWeightMap -> (Sem, SemMap) -> (Sem, SemMap)
addExtraIndices swmap (tsem,smap) = 
  let emptysem' = lookupWithDefaultFM smap [] emptyPred
      emptysem  = emptyTL : emptysem'
      --
      smapDel  = delFromFM smap emptyPred
      extra  = map (\x -> (x,"",[])) i
               where i = countExtraIndices swmap tsem
      --
      tsem2  = tsem ++ extra
      smap2  = foldr addfn smapDel extra
               where addfn x f = addToFM f x emptysem 
      --
  in (tsem2, smap2)
\end{code}

\paragraph{countExtraIndices} is a helper function to addExtraIndices.
It takes a map of semantic weights 
(defined above) and an input semantics.  It compares the items in the
input semantics against the weight map, and determines what extra
columns need to be added.  These extra columns are returned as a list,
with columns appearing as many times as they need to be added.

\begin{code}
countExtraIndices :: SemWeightMap -> Sem -> [String]
countExtraIndices swmap sem = 
  let semkeys  = zip sem (toKeys sem)
      -- convert from (literal, key) to [(index,count)]
      extra :: (Pred, String) -> [(String,Int)]
      extra (s,k) = zip is ws
        where is = idxfn s 
              ws = lookupWithDefaultFM swmap [] k     
      idxfn :: Pred -> [String]
      idxfn (h,_,i) = h:i
      -- the total number of times each index is used
      counts :: FiniteMap String Int
      counts = foldr helper emptyFM semkeys 
      helper :: (Pred, String) -> FiniteMap String Int 
                               -> FiniteMap String Int
      helper sk fm = addListToFM_C (+) fm (extra sk)
      -- 
      indices (i,c) = take c (repeat i)
  in concatMap indices (fmToList counts)
\end{code}

%\subsection{Null semantic items}
%
%Now let's say that we are no longer satisfied with \natlang{John loves
%John} and that we want to generate \natlang{John loves himself}.  This
%part is simple; now that we have a mechanism for tracking the number of
%times an item is used, all we have to do is to add null semantic items
%like pronouns and clitics to every column of the table:
%
%\begin{center}
%\begin{tabular}{|c|c|c|}
%\hline
%\semexpr{j:name(John)}  &\semexpr{j:name(John)}  & \semexpr{e1:love(j,j)} \\
%\hline
%\tautree{John} \color{blue}{+1np} & 
%\tautree{John} \color{blue}{+1np} & 
%\tautree{loves} \color{red}{-1np}\\
%
%\tautree{himself} \color{blue}{+1np} & 
%\tautree{himself} \color{blue}{+1np} & 
%\tautree{himself} \color{blue}{+1np} \\ 
%\hline
%\end{tabular}\\
%\end{center}
%
%That's it! We just construct the polarity automaton as before.  The
%resulting automaton allows us to construct \natlang{John loves John},
%\natlang{John loves himself}, \natlang{himself loves John} and
%\natlang{himself loves himself}.  And once we pass these to the 
%generator, the incorrect results among these are easily avoided by
%the generator proper.  Here is why:
%
%\begin{center}
%\begin{tabular}{|l|l|}
%\hline
%\textbf{bad result} & \textbf{ruled out by...} \\  
%\hline
%\natlang{John loves John} &
%repeated use of \semexpr{j:name(John)} \\ 
%\natlang{himself loves John} &
%\natlang{himself} in subj position (fs conflict)\\
%\natlang{himself loves himself} &
%\semexpr{j:name(John)} is not covered \\
%\hline
%\end{tabular}\\
%\end{center}

\subsection{Pitfalls}

I do not know what the behaviour of this code would be if there are
items that are multi-use and which have multi-literal semantics! For
now, we assume that this never happens.

% ====================================================================
\section{Further optimisations}
% ====================================================================

\subsection{Automatic detection}

Automatic detection is not an optimisation in itself, but a means to
make grammar development with polarities more convenient.  We assign
every initial tree with a +1 charge for the category of its root node
and a -1 charge for every category for every substitution node it has.
So for example, the tree 
s(n$\downarrow$, cl$\downarrow$, v(aime), n$\downarrow$) should have
the following polarities: s +1, cl -1, n -2 
These charges are added to any that previously been defined in the
grammar.

\begin{code}
detectPols :: [TagElem] -> [TagElem]
detectPols = map detectPols'

detectPols' :: TagElem -> TagElem
detectPols' te =
  let rcat  = (getcat.gup.root.ttree) te
      scats = map (getcat.upfn) (substnodes te)
              where upfn (_,u,_) = u
      getcat fl = if null f then "" else (snd.head) f
                  where f = filter (\ (a,_) -> "cat" == a) fl 
      --
      pols  = (rcat, 1) : (zip scats negone)
              where negone = repeat (-1)
      --
      oldfm = tpolarities te
      addpol (p,c) fm = addToFM_C (+) fm p c 
  in if (ttype te == Initial) 
     then te { tpolarities = foldr addpol oldfm pols }
     else te
\end{code}

\subsection{Chart sharing}

Chart sharing is based on the idea that instead of performing a 
seperate generation task for each automaton path, we should do
single generation task, but annotate each tree with set of the
automata paths it appears on.  We then allow trees on the
same paths to be compared only if they are on the same path.
Note: chart sharing involves some mucking around with the generation
engine (see page \pageref{fn:lookupGenRep})

\paragraph{detectPolPaths} Given a list of paths 
(i.e. a list of list of trees), we return a list of trees such that each
tree is annotated with the paths it belongs to.

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

\subsection{Polarity signatures}
\label{sec:polarity_signatures}
 
Polarity signatures is an optimisation of the automaton construction.  We
pre-process the lexically selected trees, and group together trees with
identical semantics and polarity keys.  The groups are labeled with a tuple of
$\tuple{S,K}$ where $S$ is the tree semantics and $K$ is the set of polarity
keys that the tree holds.  These tuples, which we call \jargon{polarity
signatures}, could then be used instead of trees as the labels of the
polarity automaton, the reasoning being that though there maybe hundreds of
trees for a given lexical item, the number of distinct polarity signatures is
likely to be smaller. 

The following function takes a list of trees, and returns a tuple with a list
of signatures, and a function to map these back to trees.

\begin{code}
mapByPolsig :: [TagElem] -> ([TagLite], TagLite -> [TagElem])
mapByPolsig tes = 
  let sigmap   = groupByFM gfn tes
      gfn t    = (showLitePm $ tpolarities t)
                 ++ " " ++ (showSem $ tsemantics t) 
      -- creating a representative "tree" for each polarity signature
      createTl key id = TELite { tlIdname    = key, 
                                 tlIdnum     = id,
                                 tlSemantics = tsemantics rep,
                                 tlPolarities = tpolarities rep}
                        where rep = head $ lookupWithDefaultFM sigmap [] key 
      taglites = zipWith createTl (keysFM sigmap) [0..]
      -- creating a lookup function
      lookupfn t = lookupWithDefaultFM sigmap [] (tlIdname t)
  in (taglites, lookupfn)
\end{code}

\subsection{Semantic sorting}

To minimise the number of states in the polarity automaton, we could
also sort the literals in the target semantics by the number of
corresponding lexically selected items.  The idea is to delay branching
as much as possible so as to mimimise the number of states in the
automaton.

Let's take a hypothetical example with two semantic literals:
bar (having two trees with polarties 0 and +1).
foo (having one tree with polarity -1) and
If we arbitrarily explored bar before foo (no semantic sorting), the
resulting automaton could look like this:

\begin{verbatim}
     bar     foo
(0)--+---(0)------(-1)
     |               
     +---(1)------(0)
\end{verbatim}

With semantic sorting, we would explore foo before bar because foo has
fewer items and is less likely to branch.  The resulting automaton
would have fewer states.

\begin{verbatim}
     foo      bar
(0)-----(-1)--+---(-1)
              |        
              +---(0)
\end{verbatim}

The hope is that this would make the polarity automata a bit
faster to build, especially considering that we are working over
multiple polarity keys.  

Note: this implementation assumes you have already done some work
grouping the trees by their semantics.  We reuse that work in the
form of a SemMap.

\begin{code}
sortSemByFreq :: Sem -> SemMap -> Sem
sortSemByFreq tsem smap = 
  let lengths = map lenfn tsem 
      lenfn l = length $ lookupWithDefaultFM smap [] l 
      --
      sortfn a b = compare (snd a) (snd b) 
      sorted = sortBy sortfn $ zip tsem lengths 
  in (fst.unzip) sorted 
\end{code}

% ----------------------------------------------------------------------
\section{Types}
% ----------------------------------------------------------------------

\begin{code}
type SemMap = FiniteMap Pred [TagLite]
type PolMap = FiniteMap String Int
type SemWeightMap = FiniteMap String [Int]
\end{code}

\subsection{TagLite}

To avoid passing around entire TagElems during the construction
of polarity automata, we reduce the candidates to the lighter-
weight TagLite structure that contains just the information needed
to build automata.

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
    compare (fn t1) (fn t2)
    where fn t = (tlIdname t, tlIdnum t)

emptyTL :: TagLite
emptyTL = TELite { tlIdname = "",
                   tlIdnum  = -1,
                   tlSemantics = [],
                   tlPolarities = emptyFM }

toTagLite :: TagElem -> TagLite
toTagLite te = TELite { tlIdname     = idname te,
                        tlIdnum      = tidnum te,   
                        tlSemantics  = tsemantics te,
                        tlPolarities = tpolarities te
                      }
\end{code}

We provide two ways to do the TagLite reduction: If \texttt{polsig} is False,
we just perform a direct one-on-one mapping.  If it is True, we use the
polarity signatures optimisation in section \ref{sec:polarity_signatures}.  In
both cases, we return a conversion function that maps the returned TagLites to
their original TagElems.

\begin{code}
reduceTags :: Bool -> [TagElem] -> ([TagLite], TagLite -> [TagElem])
reduceTags polsig tes =
  let tls          = map toTagLite tes 
      fromTagLite  = listArray (1,length tes) tes 
      lookupfn t   = [ fromTagLite ! (fromInteger $ tlIdnum t) ]
  in if polsig then mapByPolsig tes else (tls, lookupfn)
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
  let semmap    = delFromFM fm emptyPred
                  where fm = mapBySem tlSemantics cands
      ambiguity = map length $ eltsFM semmap
  in foldr (*) 1 ambiguity     
\end{code}
