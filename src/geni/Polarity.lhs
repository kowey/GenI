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

\chapter{Polarity Optimisation}
\label{cha:Polarity}

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

The basic idea is to use the polarity keys to determine which subsets of
candidate trees are incompatible with each other and rule them out.  We
construct a finite state automaton which uses polarity keys to
pre-calculate the compatibility of sets of trees.  At the end of the
optimisation, we are left with an automaton, each path of which is a
potentially compatible set of trees.  We then preform surface
realisation seperately, treating each path as a set of candidate trees.

\emph{Important note}: one thing that may be confusing in this chapter
is that we refer to polarities (charges) as single integers, e.g, $-2n$.
In reality, to account for weird stuff like atomic disjunction, we do
not use simple integers, but polarities intervals, so more something 
like $(-2,-2)n$!  But for the most part, the intervals are zero length,
and you can just think of $-2n$ as shorthand for $(-2,-2)n$.

\begin{code}
module Polarity(PolAut,AutDebug,makePolAut,
                TagLite, reduceTags, lookupAndTweak,
                fixPronouns, detectPols, detectPolPaths, prefixRootCat,
                declareRestrictors, detectRestrictors,
                defaultPolPaths,
                showLite, showLitePm, showPolPaths, showPolPaths',
                toGvPolAut, calculateTreeCombos,
                NFA(states),
                )
where
\end{code}

\begin{code}
import Data.Array (listArray, (!)) 
import Data.Bits
import qualified Data.Map as Map
import Data.List
import Data.Maybe (isNothing)
--import Data.Set

import Automaton
import Graphviz(GraphvizShow(..))
import Tags(TagElem(..), TagItem(..), mapBySem, substTagElem)
import Btypes(Pred, Sem, Flist, AvPair, showAv,
              GeniVal, fromGConst,
              emptyPred, Ptype(Initial), 
              showSem, sortSem, 
              root, gup, 
              SemPols, unifyFeat, rootUpd)
import General(
    BitVector, groupByFM, isEmptyIntersect, fst3, snd3, thd3,
    Interval, ival, (!+!), showInterval)
\end{code}

\section{Overview}
\label{polarity:overview}

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

Note: 
\begin{itemize}
\item the \fnparam{extraPol} argument is a map containing any initial
  values for polarity keys.  This is useful to impose external filters
  like ``I only want sentences'' or ``I only want expressions where 
  the object is topicalised''.  
\item to recuperate something useful from these automaton, it might
  be helpful to call \fnref{automatonPaths} on it.
\end{itemize}

\begin{code}
type AutDebug = (String, PolAut, PolAut)

makePolAut :: [TagLite] -> Sem -> PolMap -> ([AutDebug], PolAut)
makePolAut cands tsem extraPol = let
    (ks',seed) = makePolAutHelper cands tsem extraPol 
    ks = sortBy (flip compare) ks'
    -- building and remembering the automata 
    build k xs = (k,aut,prune aut):xs
                 where aut   = buildPolAut k initK (thd3 $ head xs)
                       initK = Map.findWithDefault (ival 0) k extraPol
    res = foldr build [("(seed)",seed,prune seed)] ks
    in (reverse res, thd3 $ head res)

makePolAutHelper :: [TagLite] -> Sem -> PolMap -> ([String],PolAut)
makePolAutHelper candsRaw tsemRaw extraPol =
  let -- polarity items 
      ksCands = concatMap ((Map.keys).tlPolarities) cands
      ksExtra = Map.keys extraPol
      ks      = nub $ ksCands ++ ksExtra
      -- perform index counting
      (tsem, cands) = fixPronouns (tsemRaw,candsRaw)
      -- sorted semantics (for more efficient construction)
      sortedsem = sortSemByFreq tsem cands 
      -- the seed automaton
      smap = buildColumns cands sortedsem 
      seed = buildSeedAut smap  sortedsem
  in (ks, seed)
\end{code}

\paragraph{reduceTags}
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

\paragraph{lookupAndTweak} is the flipside to reduceTags.  You don't
want to directly call the conversion function returned by reduceTags,
because the some cases, the polarity code might be able to usefully
modify your lexical selection.  This function will call the 
conversion function for you and then perform any neccesary
modifications.

\begin{code}
lookupAndTweak :: (TagLite -> [TagElem]) -> TagLite -> [TagElem]
lookupAndTweak lookupfn tl = 
  let cands  = lookupfn tl
      --
      getIndex [x] = if isExtraCol x then Just (fst3 x) else Nothing
      getIndex _   = Nothing 
  in case (getIndex $ tlSemantics tl) of
       Nothing -> cands
       Just i  -> map (assignIndex i) cands
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
\subsection{Columns}
% ----------------------------------------------------------------------

We build the columns for the polarity automaton as follows.  Given a
input semantics \texttt{sem} and a list of trees \texttt{cands}, we
group the trees by the first literal of sem that is part of their tree
semantics.  

Note: this is not the same function as Tags.mapBySem! The fact that we
preserve the order of the input semantics is important for our handling
of multi-literal semantics and for semantic frequency sorting.

\begin{code}
buildColumns :: (TagItem t) => [t] -> Sem -> Map.Map Pred [t] 
buildColumns cands [] = 
  Map.singleton emptyPred e 
  where e = filter (null.tgSemantics) cands

buildColumns cands (l:ls) = 
  let matchfn t = l `elem` tgSemantics t
      (match, cands2) = partition matchfn cands
      next = buildColumns cands2 ls
  in Map.insert l match next
\end{code}

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
      hasZero (x,y) = x <= 0 && y >= 0
      initAut = NFA 
        { startSt = start
        , isFinalSt = \ (PolSt ctr _ pols) -> 
            ctr == length tsem && all hasZero pols
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
      next                 = (nub newStates):(states aut)
      -- recursive step to the next literal
  in buildSeedAut' cands ls (i+1) (newAut { states = next })

-- for each candidate corresponding to literal l...
buildSeedAutHelper :: [TagLite] -> Pred -> Int -> PolState -> (PolAut,[PolState]) -> (PolAut,[PolState]) 
buildSeedAutHelper cs l i st (aut,prev) =
  let -- get the extra semantics from the last state
      (PolSt _ ex1 _) = st
      -- candidates that match the target semantics and which
      -- do not overlap the extra baggage semantics
      tcand = [ Just t | t <- cs
              , isEmptyIntersect ex1 (tlSemantics t) ]
      -- add the transitions out of the current state 
      addT tr (a,n) = (addTrans a st tr st2, st2:n)
        where 
         st2 = PolSt i ex2 []
         ex2 = case tr of 
                 Nothing  -> []
                 Just tr_ -> delete l (ex1 ++ tlSemantics tr_)
  in if (l `elem` ex1) 
     then addT Nothing (aut,prev)
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
buildPolAut :: String -> Interval -> PolAut -> PolAut 
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
      skelStTrans = Map.findWithDefault Map.empty skelSt skeleton
      cands       = Map.keys skelStTrans 
      -- . for each state that a label transitions to...
      addT tr (a,n) = foldr (addTS tr) (a,n) (lookupTr tr)
      lookupTr tr   = Map.findWithDefault [] tr skelStTrans
      -- .. calculate a new state and add a transition to it
      addTS tr skel2 (a,n) = (addTrans a st tr st2, st2:n)
        where st2 = newSt tr skel2
      --
      newSt t skel2 = PolSt pr2 ex2 (po2:skelPo2)
        where 
         PolSt pr2 ex2 skelPo2 = skel2 
         po2 = po1 !+! (Map.findWithDefault (ival 0) fk pol)
         pol = case t of Nothing -> Map.empty 
                         Just t2 -> tlPolarities t2   
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
      sansTrans st = isNothing (Map.lookup st oldT)
      blacklist    = filter sansTrans sts
      -- removing all transitions to the blacklist
      subDelete  = Map.map (\e -> e \\ blacklist)
      -- removing all transitions labels that only go to the blacklist
      subCleanup = Map.filter (\e -> not $ null e)
      -- removing all states that only transition to the blacklist
      newT    = cleanup $ Map.map (\e -> subCleanup $ subDelete e) oldT
      cleanup = Map.filter (\e -> not $ Map.null e)
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

% ====================================================================
\section{Zero-literal semantics}
\label{sec:multiuse}
\label{semantic_weights}
\label{sec:nullsem}
\label{sec:co-anchors}
% ====================================================================

Lexical items with a \jargon{null semantics} typically correspond to
functions words: complementisers \natlang{(John likes \textbf{to}
read.)}, subcategorised prepositions \natlang{(Mary accuses John
\textbf{of} cheating.)}.  Such items need not be lexical items at all.
We can exploit TAG's support for trees with multiple anchors, by
treating them as co-anchors to some primary lexical item. The English
infinitival \natlang{to}, for example, can appear in the tree
\tautree{to~take} as \koweytree{s(comp(to),v(take),np$\downarrow$)}. 

On the other hand, pronouns have a \jargon{zero-literal} semantics, one
which is not null, but which consists only of a variable index.  For
example, the pronoun \natlang{she} in (\ref{ex:pronoun_pol_she}) has
semantics \semexpr{s} and in (\ref{ex:pronoun_pol_control}),
\natlang{he} has the semantics \semexpr{j}.  

{\footnotesize
\eenumsentence{\label{ex:pronoun_pol} 
\item \label{ex:pronoun_pol_sue} 
\semexpr{joe(j), sue(s), book(b), lend(l,j,b,s), boring(b) }  
\\ \natlang{Joe lends Sue a boring book.}

% Note for visually impaired readers: ignore anything with \color{white}. 
% It is used as a form of indentation to help sighted users. 
\item \label{ex:pronoun_pol_she} 
\semexpr{joe(j), {\color{white}sue(s),} book(b), lend(l,j,b,s), boring(b) }  
\\ \natlang{Joe lends her a boring book.}
}
\eenumsentence{\label{ex:pronoun_pol_control} 
\item[{\color{white}a.}] \label{ex:pronoun_pol_control_inf}
\semexpr{joe(j), sue(s), leave(l,j), promise(p,j,s,l)}
\\ \natlang{Joe promises Sue to leave.}
\\ or \natlang{Joe promises Sue that he would leave.} 
}}

In figure \ref{fig:polarity_automaton_zerolit_bad}, we compare the
construction of polarity automata for (\ref{ex:pronoun_pol_sue}, left)
and (\ref{ex:pronoun_pol_she}, right).  Building an automaton for
(\ref{ex:pronoun_pol_she}) fails because \tautree{sue} is not available
to cancel the negative polarities for \tautree{lends}; instead, a
pronoun must be used to take its place.  The problem is that the
selection of a lexical items is only triggered when the construction
algorithm visits one of its semantic literals.  Since pronoun semantics
have zero literals, they are \emph{never} selected.  Making pronouns
visible to the construction algorithm would require us to count the
indices from the input semantics.  Each index refers to an entity.  This
entity must be ``consumed'' by a syntactic functor (e.g. a verb) and
``provided'' by a syntactic argument (e.g. a noun).

%\footnote{This also holds true for sentences like \natlang{Joe sings
%badly and Sue sings well}, \semexpr{sing(s1,j) good(s1), sing(s2,m),
%bad(s2)} because each usage of \natlang{sings} actually corresponds to a
%different lexical item, \tautree{sings1} with the semantics
%\semexpr{sings(s1,j)} and \tautree{sings2} with \semexpr{sings(s2,m)}.}.  

\begin{figure}[htpb]
\begin{center}
\includegraphics[scale=0.25]{images/zeroaut-noun.pdf}
\includegraphics[scale=0.25]{images/zeroaut-sans.pdf}
\end{center}
\vspace{-0.4cm}
\caption{Difficulty with zero-literal semantics.}
\label{fig:polarity_automaton_zerolit_bad}
\end{figure}

We make this explicit by annotating the semantics of the lexical input
(that is the set of lexical items selected on the basis of the input
semantics) with a form of polarities.  Roughly, nouns provide
indices\footnote{except for predicative nouns, which like verbs, are
semantic functors} ($+$), modifiers leave them unaffected, and verbs
consume them ($-$).  Predicting pronouns is then a matter of counting
the indices.  If the positive and negative indices cancel each other
out, no pronouns are required.  If there are more negative indices than
positive ones, then as many pronouns are required as there are negative
excess indices.  In the table below, we show how the example semantics
above may be annotated and how many negative excess indices result:

\begin{center}
{\footnotesize
\begin{tabular}{|l|r|r|r|}
\hline
\multicolumn{1}{|c|}{\bf semantics} & 
{\bf \tt b} &
{\bf \tt j} & 
{\bf \tt s} \\ 
\hline
\semexpr{joe(+j)  sue(+s)  book(+b)  lend(l,-j,-b,-s)  boring(b)} & 
\semexpr{0} &
\semexpr{0} &
\semexpr{0} \\
\hline
\semexpr{joe(+j)  {\color{white}sue(+s)} book(+b)  lend(l,-j,-b,-s)  boring(b)} & 
\semexpr{0} &
\semexpr{0} &
\semexpr{1} \\
\hline
\semexpr{joe(+j) sue(+s) leave(l,-j,-s) promise(p,{\color{white}-}j,-s,l)} &
\semexpr{0} &
\semexpr{0} & 
\semexpr{0} \\ 
\hline
\semexpr{joe(+j) sue(+s) leave(l,-j,-s) promise(p,-j,-s,l)} &
\semexpr{0} &
\semexpr{1} & 
\semexpr{0} \\ 
\hline
\end{tabular}
}
\end{center}

Counting surplus indices allows us to establish the number of pronouns
used and thus gives us the information needed to build polarity
automata.  We implement this by introducing a virtual literal for
negative excess index, and having that literal be realised by pronouns.
Building the polarity automaton as normal yields lexical combinations
with the required number of pronouns, as in figure
\ref{fig:polarity_automaton_zerolit}.   

\begin{figure}[htpb]
\begin{center}
\includegraphics[scale=0.25]{images/zeroaut-pron.pdf}
\end{center}
\vspace{-0.4cm}
\caption{Constructing a polarity automaton with zero-literal semantics.}
\label{fig:polarity_automaton_zerolit}
\end{figure}

\label{different_sem_annotations}
The sitation is more complicated where the lexical input
contains lexical items with different annotations for the same
semantics.  For instance, the control verb \natlang{promise} has two
forms: one which solicits an infinitive as in \natlang{promise to
leave}, and one which solicits a declarative clause as in
\natlang{promise that he would leave}.  This means two different counts
of subject index \semexpr{j} in (\ref{ex:pronoun_pol_control}) : zero
for the form that subcategorises for the infinitive, or one for the
declarative.  But to build a single automaton, these counts must be
reconciled, i.e., how many virtual literals do we introduce for
\semexpr{j}, zero or one?  The answer is to introduce enough virtual
literals to satisfy the largest demand, and then use the multi-literal
extension to support alternate forms with a smaller demand.  To handle
example (\ref{ex:pronoun_pol_control}), we introduce one virtual literal
for \semexpr{j} so that the declarative form can be produced, and treat
the soliciting \natlang{promise} as though its semantics includes that
literal along with its regular semantics (figure
\ref{fig:polarity_automaton_zerolit_promise}).  In other words, the
infinitive-soliciting form is treated as if it already fulfils the role
of a pronoun, and does not need one in its lexical combination.

\begin{figure}[htpb]
\begin{center}
\includegraphics[scale=0.25]{images/zeroaut-promise.pdf}
\end{center}
\vspace{-0.4cm}
\caption{Constructing a polarity automaton with zero-literal semantics.}
\label{fig:polarity_automaton_zerolit_promise}
\end{figure}

paragraph{fixPronouns} returns a modified input semantics and lexical
selection in which pronouns are properly accounted for.

\begin{code}
type PredLite = (String,[GeniVal]) -- handle is head of arg list 
type SemWeightMap = Map.Map PredLite SemPols
fixPronouns :: (Sem,[TagLite]) -> (Sem,[TagLite])
fixPronouns (tsem,cands) = 
\end{code}

First, for each literal in the input semantics, we establish the
smallest charge for each of its semantic indices.

\begin{code}
  let getpols :: TagLite -> [ (PredLite,SemPols) ]
      getpols x = zip (map fn $ tlSemantics x) (tlSempols x)
        where fn :: Pred -> PredLite
              fn s = (snd3 s, fst3 s : thd3 s)
      sempols :: [ (PredLite,SemPols) ]
      sempols = concatMap getpols cands
      usagefn :: (PredLite,SemPols) -> SemWeightMap -> SemWeightMap 
      usagefn (lit,cnts) m = Map.insertWith (zipWith min) lit cnts m
      usagemap :: SemWeightMap 
      usagemap = foldr usagefn Map.empty sempols 
\end{code}

Second, we cancel out the polarities for every index in the input
semantics.  

\begin{code}
      usagelist :: [(GeniVal,Int)]
      usagelist = concatMap fn (Map.toList usagemap)
        where fn ((_,idxs),pols) = zip idxs pols
      chargemap :: Map.Map GeniVal Int -- index to charge 
      chargemap =  foldr addfn Map.empty usagelist 
        where addfn (p,c) m = Map.insertWith (+) p c m
\end{code}

Third, we compensate for any uncancelled negative polarities by an
adding an additional literal to the input semantics -- a pronoun --
for every negative charge.

\begin{code}
      indices = concatMap fn (Map.toList chargemap) 
        where fn (i,c) = take (0-c) (repeat i)
      -- the extra columns 
      extraSem = map indexPred indices
      tsem2    = sortSem (tsem ++ extraSem)
      -- zero-literal semantic items to realise the extra columns 
      zlit = filter (null.tlSemantics) cands    
      cands2 = (cands \\ zlit) ++ (concatMap fn indices)
        where fn i = map (tweak i) zlit
              tweak i x = x { tlSemantics = [indexPred i] }
\end{code}

Fourth, and finally, we deal with the problem of lexical items
who require fewer pronouns than predicted by inserting the 
excess pronouns in their extra literal semantics (see page
\pageref{different_sem_annotations})

\begin{code}
      -- fixPronouns  
      comparefn :: GeniVal -> Int -> Int -> [GeniVal]
      comparefn i c1 c2 = if (c2 < c1) then extra else []
        where extra = take (c1 - c2) $ repeat i 
      compare :: (PredLite,SemPols) -> [GeniVal]
      compare (lit,c1) = concat $ zipWith3 comparefn idxs c1 c2
        where idxs = snd lit
              c2   = Map.findWithDefault [] lit usagemap
      addextra :: TagLite -> TagLite
      addextra c = c { tlSemantics = sortSem (sem ++ extra) } 
        where sem   = tlSemantics c
              extra = map indexPred $ concatMap compare (getpols c)
      cands3 = map addextra cands2
  in (tsem2, cands3)
\end{code}

\paragraph{indexPred} builds a fake semantic predicate that the index
counting mechanism uses to represent extra columns.

\begin{code}
indexPred :: GeniVal -> Pred
indexPred x = (x, "", [])
\end{code}

\paragraph{isExtraCol} returns True if the given literal was introduced
by the index counting mechanism

\begin{code}
isExtraCol :: Pred -> Bool
isExtraCol (_,"",[]) = True
isExtraCol _         = False
\end{code}

\paragraph{assignIndex} is a useful way to restrict the behaviour of
null semantic items like pronouns using the information generated by
the index counting mechanism.  The problem with null semantic items 
is that their indices are not set, which means that they could
potentially combine with any other tree.  To make things more 
efficient, we can set the index of these items and thus reduce the
number of spurious combinations.  

Notes
\begin{itemize}
%\item These combinations could produce false results if the
%input has to use multiple pronouns.  For example, if you wanted to say
%something like \natlang{John promises Mary to convince Paul to give her
%  his book}, these combinations could instead produce \natlang{give him
%    \textbf{her} book}.
\item This function works by FS unification on the root node of the
  tree with the \fs{\it idx:i\\}.  If unification is not possible, 
  we simply return the tree as is.
\item This function renames the tree by appending the index to its name
\end{itemize}

\begin{code}
assignIndex :: GeniVal -> TagElem -> TagElem 
assignIndex i te =
  let idxfs = [ ("idx", i) ]
      oldt  = ttree te
      oldr  = root oldt
      tfup  = gup oldr
      --
      (success, newgup, subst) = unifyFeat tfup idxfs 
      newr = oldr { gup = newgup }
      newt = rootUpd oldt newr
      --
      newTe = substTagElem (te { ttree = newt }) subst
  in if success then newTe else te
\end{code}


% ====================================================================
\section{Further optimisations}
% ====================================================================

\subsection{Lexical filtering} \label{fn:detectRestrictors}

Lexical filtering allows the user to restricte the lexical selection
to only those items that contain a certain property, for example, the
realising an item as a cleft.

The idea is that the user provides an input like
\verb$restrictors:[cleft:j]$ which means that the lexical selection
must include exactly one tree with the property cleft:j in its
interface.  This mechanism works as pre-processing step after
lexical selection and before polarity automaton construction, in
conjuction with the ExtraPolarities mechanism.  What we do is

\begin{enumerate}
\item Preprocess the lexically selected trees; any tree which has a
      a desired property (e.g. cleft:j) in its interface is assigned
      a positive polarity for that property (+cleft:j)
\item Add all the restrictions as negative extra polarities (-cleft:j)
\end{enumerate}

Note: we assume the restrictors and interface are sorted; also, we prefix
the restrictor polarities with a ``.'' because they are likely to be very
powerful filters and we would like them to be used first

\begin{code}
detectRestrictors :: Flist -> Flist -> PolMap 
detectRestrictors restrict interface =
  let matches  = intersect restrict interface
      matchStr = map showRestrictor matches
  in Map.fromList $ zip matchStr ((repeat.ival) 1)

declareRestrictors :: Flist -> PolMap
declareRestrictors restrict =
  let restrictStr = map showRestrictor restrict
      minusone    = (repeat.ival) (-1)
  in Map.fromList $ zip restrictStr minusone

showRestrictor :: AvPair -> String
showRestrictor = ('.' :) . showAv
\end{code}

\subsection{Automatic detection}

Automatic detection is not an optimisation in itself, but a means to
make grammar development with polarities more convenient.  

First the simplified explanation: we assign every tree with a $-1$ charge for
every category for every substitution node it has.  Additionally, we assign
every initial tree with a $+1$ charge for the category of its root node.  So
for example, the tree s(n$\downarrow$, cl$\downarrow$, v(aime), n$\downarrow$)
should have the following polarities: s +1, cl -1, n -2. These charges are
added to any that previously been defined in the grammar.

Now what really happens: we treat automaton polarities as intervals, not 
as single integers!  For the most part, nothing changes from the simplified
explanation.  Where we added a $-1$ charge before, we now add a $(-1,-1)$
charge.  Similarly, we where added a $+1$ charge, we now add $(1,1)$.  So
what's the point of all this?  It helps us deal with atomic disjunction.

\subparagraph{Atomic disjunction} Say we encounter a substitution node 
whose category is either cl or n.  What we do is add the polarities
$cl (-1,0),  n (-1,0)$ which means that there are anywhere from -1 to 
0 cl, and for n.  
FIXME: What kind of sucks about all this though is that this slightly worsens
the filter because it allows for both cl and n to be $-1$ (or $0$) at the same
time.  It would be nice to have some kind of mutual exclusion working.

\begin{code}
detectPols :: [TagElem] -> [TagElem]
detectPols = map detectPols'

detectPols' :: TagElem -> TagElem
detectPols' te =
  let rootup   = (gup.root.ttree) te
      subup    = map snd3 (substnodes te)
      rstuff   :: [[String]]
      rstuff   = concat [getcat rootup]--,getidx rootup]
      substuff :: [[String]]
      substuff = concatMap getcat subup -- ++ (map getidx subup)
      --
      getcat = getval "cat"
      -- getidx = getval "idx"
      getval :: String -> Flist -> [[String]]
      getval att fl = [ (addPrefix.fromGConst) v | (a,v) <- fl, a == att ] 
        where addPrefix :: [String] -> [String]
              addPrefix = map (\x -> att ++ ('_' : x))
      -- 
      commonPols :: [ (String,Interval) ]
      commonPols = concatMap fn substuff 
        where interval = (-1, 0)
              fn []  = error "null GConst detected in detectPols':commonPols!"
              fn [x] = [ (x, (-1,-1)) ]
              fn amb = map (\x -> (x,interval)) amb
      ipols :: [ (String,Interval) ]
      ipols = commonPols ++ concatMap fn rstuff 
        where interval = (0, 1)
              fn []  = error "null GConst detected in detectPols':iPols!"
              fn [x] = [ (x, (1,1)) ]
              fn amb = map (\x -> (x,interval)) amb
      pols :: [ (String,Interval) ]
      pols  = if ttype te == Initial then ipols else commonPols 
      --
      oldfm = tpolarities te
  in te { tpolarities = foldr addPol oldfm pols }
\end{code}

\paragraph{prefixRootCat} converts a category like ``s'' into an negative
polarity like ``cat\_s''.  This is to offset the extra polarity that comes from
automatically assigning a $+$ polarity to every root node category.

\begin{code}
prefixRootCat :: String -> String
prefixRootCat cat = "cat_" ++ cat
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
  let pathFM     = detectPolPaths' Map.empty 0 paths
      lookupTr k = Map.findWithDefault 0 k pathFM
      setPath k  = k {tpolpaths = lookupTr k}
  in map setPath $ Map.keys pathFM

type PolPathMap = Map.Map TagElem BitVector
detectPolPaths' :: PolPathMap -> Int -> [[TagElem]] -> PolPathMap  

detectPolPaths' accFM _ [] = accFM
detectPolPaths' accFM counter (path:ps) = 
  let currentBits = shiftL 1 counter -- shift counter times the 1 bit
      fn f []     = f
      fn f (t:ts) = fn (Map.insertWith (.|.) t currentBits f) ts 
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
      gfn t    = (showSem $ tsemantics t) ++ " " ++
                 (showLitePm $ tpolarities t) ++
                 (show $ tsempols t)
      -- creating a representative "tree" for each polarity signature
      createTl key id = TELite { tlIdname    = key, 
                                 tlIdnum     = id,
                                 tlSemantics = tsemantics rep,
                                 tlSempols   = tsempols rep,
                                 tlPolarities = tpolarities rep}
                        where rep = case (Map.lookup key sigmap) of
                                      Just (x:_) -> x
                                      _          -> error "mapByPolSig error" 
      taglites = zipWith createTl (Map.keys sigmap) [0..]
      -- creating a lookup function
      lookupfn t = Map.findWithDefault [] (tlIdname t) sigmap
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

Note: we have to take care to count each literal for each lexical
entry's semantics or else the multi-literal semantic code will choke.

\begin{code}
sortSemByFreq :: Sem -> [TagLite] -> Sem
sortSemByFreq tsem cands = 
  let counts = map lenfn tsem 
      lenfn l = length $ filter fn cands 
                where fn x = l `elem` (tlSemantics x)
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
\end{code}

% ----------------------------------------------------------------------
\section{Types}
% ----------------------------------------------------------------------

\begin{code}
type SemMap = Map.Map Pred [TagLite]
type PolMap = Map.Map String Interval 
\end{code}

\paragraph{addToPol} adds a new polarity item to a PolMap.  If there
already is a polarity for that item, it is summed with the new polarity.

\begin{code}
addPol :: (String,Interval) -> PolMap -> PolMap
addPol (p,c) m = Map.insertWith (!+!) p c m
\end{code}

\subsection{TagLite}

To avoid passing around entire TagElems during the construction
of polarity automata, we reduce the candidates to the lighter-
weight TagLite structure that contains just the information needed
to build automata.  See section \ref{polarity:overview} for the
reduction code.

\begin{code}
data TagLite = TELite { tlIdname      :: String
                      , tlIdnum       :: Integer
                      , tlSemantics   :: Sem
                      , tlPolarities  :: Map.Map String Interval 
                      , tlSempols     :: [SemPols] }
        deriving (Show, Eq)

instance TagItem TagLite where
  tgIdName    = tlIdname
  tgIdNum     = tlIdnum
  tgSemantics = tlSemantics

instance Ord TagLite where
  compare t1 t2 = 
    compare (fn t1) (fn t2)
    where fn t = (tlIdname t, tlIdnum t)

toTagLite :: TagElem -> TagLite
toTagLite te = TELite { tlIdname     = idname te
                      , tlIdnum      = tidnum te   
                      , tlSemantics  = tsemantics te
                      , tlPolarities = tpolarities te
                      , tlSempols    = tsempols te }
\end{code}

\paragraph{nubAut} ensures that all states and transitions in the polarity automaton 
are unique.  This is a slight optimisation so that we don't have to repeatedly check
the automaton for state uniqueness during its construction, but it is essential that
this check be done after construction

\begin{code}
nubAut :: (Ord ab, Ord st) => NFA st ab -> NFA st ab 
nubAut aut = 
  aut {
      transitions = Map.map (\e -> Map.map nub e) (transitions aut)
  }
\end{code}

\subsection{Polarity NFA}

We can define the polarity automaton as a NFA, or a five-tuple 
$(Q, \Sigma, \delta, q_0, q_n)$ such that 

\begin{enumerate}
\item $Q$ is a set of states, each state being a tuple $(i,e,p)$ where $i$
is an integer (representing a single literal in the target semantics), 
$e$ is a list of extra literals
which are known by the state, and $p$ is a polarity.
\item $\Sigma$ is the union of the sets of candidate trees for all
propositions
\item $q_0$ is the start state $(0,[0,0])$ which does not correspond to any
propositions and is used strictly as a starting point.
\item $q_n$ is the final state $(n,[x,y])$ which corresponds to the last
proposition, with polarity $x \leq 0 \leq y$.
\item $\delta$ is the transition function between states, which we
define below.
\end{enumerate}

Note: 
\begin{itemize}
\item For convenience during automaton intersection, we actually define
      the states as being $(i, [(p_x,p_y)])$ where $[(p_x,p_y)]$ is a list of
      polarity intervals.  
\item We use integer $i$ for each state instead of literals directly,
      because it is possible for the target semantics to contain the 
      same literal twice (at least, with the index counting mechanism
      in place)
\end{itemize}

\begin{code}
data PolState = PolSt Int [Pred] [(Int,Int)] deriving (Eq)
type PolTrans = TagLite
type PolAut   = NFA PolState PolTrans
type PolTransFn = Map.Map PolState (Map.Map (Maybe PolTrans) [PolState])
\end{code}

\begin{code}
instance Show PolState
  where show (PolSt pr ex po) = show pr ++ " " ++ showSem ex ++ show po
-- showPred pr ++ " " ++ showSem ex ++ show po
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
fakestate :: Int -> [Interval] -> PolState
fakestate s pol = PolSt s [] pol --PolSt (0, s, [""]) [] pol
-- an initial state for polarity automata
polstart :: [Interval] -> PolState
polstart pol = fakestate 0 pol -- fakestate "START" pol
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
%    concatMap showTrans $ toList (transitions aut)
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
  concatMap showPair $ toList sm 
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
  let showPair (f, pol) = showInterval pol ++ f 
  in concat $ intersperse " " $ map showPair $ Map.toList pm
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
  graphvizShow _ (GvPolAut aut) = 
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
           stmap = Map.fromList $ zip st ids
\end{code}

\begin{code}
gvShowState :: String -> PolState -> String
gvShowState stId st = " " ++ stId ++ " [ label=\"" ++ showSt st ++ "\" ];\n"
  where showSt (PolSt pr ex po) = showPr pr ++ showEx ex ++ showPo po
        showPr _ = "" -- (_,pr,_) = pr ++ "\\n"
        showPo po = concat $ intersperse "," $ map showInterval po
        showEx ex = if (null ex) then "" else (showSem ex) ++ "\\n"
\end{code}

Specify that the final states are drawn with a double circle

\begin{code}
gvShowFinal :: PolAut -> Map.Map PolState String -> String
gvShowFinal aut stmap = 
  if isEmptyIntersect (concat $ states aut) fin 
  then ""
  else "node [ peripheries = 2 ]; " 
  ++ concatMap (\x -> " " ++ lookupId x) fin
  ++ "\n"
  where fin = finalSt aut
        lookupId x = Map.findWithDefault "error_final" x stmap 
\end{code}

Each transition is displayed with the name of the tree.  If there is more
than one transition to the same state, they are displayed on a single
label.

\begin{code}
gvShowTrans :: PolAut -> Map.Map PolState String
               -> String -> PolState -> String 
gvShowTrans aut stmap idFrom st = 
  let -- outgoing transition labels from st
      alpha = Map.keys $ Map.findWithDefault Map.empty st (transitions aut) 
      -- associate each st2 with a list of labels that transition to it
      inverter x fm = foldr fn fm (lookupTrans aut st x)
                      where fn    s f   = Map.insert s (xlist s f x) f
                            xlist s f x = x:(Map.findWithDefault [] s f)
      invFM = foldr inverter Map.empty alpha
      -- returns the graphviz dot command to draw a labeled transition
      drawTrans (stTo,x) = case Map.lookup stTo stmap of
                             Nothing   -> drawTrans' ("id_error_" ++ (showSem stTo)) x 
                             Just idTo -> drawTrans' idTo x
                           where showSem (PolSt i _ _) = show i 
                                 --showSem (PolSt (_,pred,_) _ _) = pred 
      drawTrans' idTo x = " " ++ idFrom ++ " -> " ++ idTo ++ 
                          " [ label=\"" ++ drawLabel x ++ "\"];\n"
      drawLabel labels = concat $ intersperse "\\n" $ labs 
        where 
          lablen = length labels
          max    = 6 
          excess = "...and " ++ (show $ lablen - max) ++ " more"
          --
          labstrs = map fn labels
          fn Nothing  = "EMPTY"
          fn (Just x) = tlIdname x
          --
          labs = if (lablen > max) 
                 then (take max labstrs) ++ [ excess ]
                 else labstrs 
  in concatMap drawTrans $ Map.toList invFM
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
%  in map showfv $ Map.keys p 

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
calculateTreeCombos :: (TagItem t) => [t] -> Int
calculateTreeCombos cands = 
  let smapRaw   = mapBySem cands
      ambiguity = map length $ Map.elems smapRaw 
  in foldr (*) 1 ambiguity     
\end{code}




