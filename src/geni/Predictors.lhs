\chapter{Predictor Optimisation}

One optimisation is to annotate the macros with a set of
\jargon{predictors}.  This allows macros to predict that they will
combine with certain (usually) null-semantic trees.  For example, a
common noun would predict that it needs a determiner.  

\begin{code}
module Predictors 
where
\end{code}

\begin{code}
import Debug.Trace
import FiniteMap
import Data.List (nub, sortBy, groupBy, intersect)
import Monad (when, ap, foldM)
import MonadState (get, put)

import Btypes (Sem, Flist, AvPair, showSem, showAv, isVar)
import Tags (TagElem(TE), emptyTE, idname, tsemantics, substnodes, 
             derivation, tpredictors, drawTagTrees)
import Configuration (defaultParams)
import Mstate (MS, Gstats, initGstats, addGstats, initMState,
               runState, genstats, 
               incrNumcompar, incrSzchart, incrGeniter,
               renameTagElem,
               addToInitRep, 
               getGenRep, lookupGenRep, genRepToList, addListToGenRep,
               iapplySubstNode,
               nullInitRep, getInitRep, genrep, getSem, selectGiven)

import Polarity (showLite)
\end{code}

% ----------------------------------------------------------------------
\section{Optimisation}
% ----------------------------------------------------------------------

We attempt substitution between macro and any predictors that it has.
Whenever we succeed, we can pass the combined tree as a candidate.
Whenever we fail, we have to pass both the macro and its predictors.
This is basically an indirect means of adding some kind of indexing to
the generator's chart.

Note: there are actually two cases here.  For those predictors that
we can substitute into the macro, we return the resulting tree and
discard the predictor.  

\begin{code}
optimisePredictors :: [[TagElem]] -> PredictorMap -> ([[TagElem]], Gstats)
optimisePredictors cands predictmap =
  let trees = nub $ concat cands  
      -- calculate predicted trees
      sumup = foldr addGstats initGstats 
      optTree t = optimisePredictors' predictmap t
      optPath p = (r, sumup s)
                  where (r,s) = unzip $ map optTree p       
      (res, stats) = optPath trees
      treemap = listToFM $ zip trees res
      -- replace trees with their predicted equivalents
      repTree t = lookupWithDefaultFM treemap [t] t  
      repPath p = concatMap repTree p
      {- repPath  p = trace ("\n==============\npath\n=============\n" ++ drawTagTrees l) l
                   where l = repPath' p -}
  in (map repPath cands, stats)
\end{code}

\paragraph{optimisePredictors'} is a helper function that tries to
fulfill as many of a tree's predictors as possible.  Any predictors
it cannot use are also returned so that they can be passed to the     
generator proper.

\begin{code}
optimisePredictors' :: PredictorMap -> TagElem -> ([TagElem], Gstats)
optimisePredictors' predictmap te =
  let -- grab the predictors (helper fns)
      isneg _ e    = e < 0 
      predictors t = keysFM $ filterFM isneg $ tpredictors t
      ptrees     t = concatMap fn (predictors t)
                     where fn = lookupWithDefaultFM predictmap []
      -- generate
      tePtrees    = ptrees te
      initSt      = initMState tePtrees [te] (tsemantics te) defaultParams
      (res', st)  = runState miniGenerate initSt
      -- pick the trees with the largest derivation history
      derSz = length.snd.derivation
      cmpDerSz  t1 t2 = compare (derSz t2) (derSz t1) -- note the inversion 
      sameDerSz t1 t2 = (derSz t2) == (derSz t1)              
      groupedres  = groupBy sameDerSz $ sortBy cmpDerSz res' 
      -- return the results
      result  = head groupedres -- trace ("\n==============\nresults for " ++ idname te ++ "\n=============\n" ++ drawTagTrees res) $ 
      rejects = concatMap ptrees result
      stats   = genstats st
      --
      debugstr = "\n===================" 
               ++ "\noptimising " ++ showLite te 
               ++ "\nptrees: " ++ showLite (tePtrees)
               ++ "\n==================== "
      errormsg = "Geni: Predictors.optimisePredictors' is broken"
  in case () of _ | null tePtrees   -> ([te], stats)
                  | null groupedres -> error errormsg 
                  | otherwise -> (result ++ rejects, stats)
\end{code}

% ----------------------------------------------------------------------
\subsection{miniGenerate}
% ----------------------------------------------------------------------

miniGenerate is a lightweight version of the generator which operates 
on the following principles: 

\begin{enumerate}
\item There is one primary tree (chart) and some secondary trees 
      (agenda), which should not be confused with auxiliary trees
\item All operations are performed between the primary
      tree and the secondary trees, that is, you won't
      have any interaction between secondary trees
\item The primary tree may substitute with or be 
      substituted any number of secondary trees
\item Secondary trees may only be used once
\end{enumerate}

It is used as a helper function for optimisePredictors.  

\begin{code}
miniGenerate :: MS [TagElem]
miniGenerate = do 
  nir <- nullInitRep
  gr  <- getGenRep
  if nir then return (concat $ eltsFM gr) else do 
    incrGeniter 1
    tsem <- getSem
    -- choose a secondary tree from the agenda
    given <- selectGiven
    -- perform any substitutions 
    chart <- lookupGenRep given 
    let (res', cost') = unzip $ map (timidSubstitution given) chart
        res  = concat res'
        cost = foldr (+) 0 cost' 
    incrSzchart (length res)
    incrNumcompar cost
    -- add any succesful results to the chart
    st <- get
    put st { genrep = addListToGenRep gr res }
    miniGenerate
\end{code}

\paragraph{timidSubstitution} attempts to perform substitution between
input trees $te_1$ and $te_2$.  This is meant strictly to be a helper
function for optimisePredictors, so we'll have a somewhat conservative
and quirky behaviour:
\begin{itemize}
\item If there are no ways to perform substitution, we return the empty
list
\item If there is exactly one way to perform substitution
(either $te_1$ into $te_2$ or vice versa), we
return that substitution.  
\item If there is more than one way to do it, we return the empty list.
This is because the situation is ambiguous and could lead to unpredictable
results (see section \ref{sec:optimisePredictors_tricky})
\end{itemize}

This is somewhat similar to MState's applySubstitution, except that we
rule out the case of multiple results, and that we do not require the
substitution nodes to be in any particular order.

\begin{code}
timidSubstitution :: TagElem -> TagElem -> ([TagElem],Int)
timidSubstitution te1 te2 = 
  let tesem = tsemantics te1
      -- we only substitute tags with no overlaping semantics
      notOverlap = null $ intersect (tsemantics te2) tesem
      -- we rename tags to do a proper substitution
      rte1 = renameTagElem 'A' te1
      rte2 = renameTagElem 'B' te2
      -- perform the substitution
      subst t1 t2 = concatMap (iapplySubstNode t1 t2) $ substnodes t2
      res' = (subst rte1 rte2) ++ (subst rte2 rte1)
      res  = if (length res' == 1) then res' else []
      -- measuring efficiency
      cost = fn te1 + fn te2 
             where fn t = length $ substnodes t 
  in if notOverlap then (res, cost) else ([], 0)
\end{code}

\subsection{Trickiness in optimisePredictors} 
\label{sec:optimisePredictors_tricky}

Rejecting ambiguous substitutions is crucial to the idea that
secondary trees may only be used once.

Consider the trees for \textit{the N, enemy of N, friend}.
The idea is that we eventually want to generate \textit {the enemy of the
friend}, so the result of optimisePredictors should ideally be something like:
\textit{the friend, the enemy of N} 

But this isn't so easy to achieve.  In fact, if we tried to achieve
the above result, we would instead get a highly undesirable result 
like this \textit{the friend, the enemy of the N} 

Do you see why the above result is bad?  It is because now there is
no way to substitute friend into that noun-substitution node.  To
avoid this sort of over-ambitiousness, we avoid ambiguous cases where a 
tree could both substitute into or be substituted into another.  So we
get a less optimal, but much safer result \textit{the friend, enemy of, 
the}:

% ----------------------------------------------------------------------
\section{Cleanup}
% ----------------------------------------------------------------------

\paragraph{fillPredictors} This is neccesary when either the
predictor optimisation is disabled or if there are some
predictor substitutions which do not succeed.  It takes a list of paths
and inserts all required predictors on the paths.

\begin{code}
fillPredictors :: [[TagElem]] -> PredictorMap -> [[TagElem]]
fillPredictors paths predictmap =
  let isneg _ pol   = pol < 0 
      getP          = lookupWithDefaultFM predictmap []
      predictors te = keysFM $ filterFM isneg $ tpredictors te
      addP te       = te : (concatMap getP $ predictors te)
  in map (\p -> nub $ concatMap addP p) paths
\end{code}

% --------------------------------------------------------------------
\section{Instatiation of predictors}
% --------------------------------------------------------------------

We combine the predictors from the lexicon and macros.  The idea is
to do this in a way which lets the grammar writer be lazy while having
as simple and predictable a behaviour as possible.  Any predictors that
the lexicon has must correspond to some variable predictor in the
macros, so if I say in the lexicon that a tree as predictor
$+vsup:avoir$ there had better be a $+vsup:X$ in the macros to back it
up.

\begin{code}
combinePredictors tt le = 
  let -- fn to add an item to the predictors map
      addP (fv,c) fm  = addToFM_C (+) fm fv c
      -- lexicon predictors 
      lpr             = sort $ ipredictors le
      -- tree predictors (variable vs constant predictors)
      tpr             = sort $ ptpredictors tt
      isVpr ((f,v),_) = (not $ null v) && isVar v
      (varPr,constPr) = partition isVpr tpr
      constPrFm       = foldr addP emptyFM constPr
      -- separating the charges from the fv
      (lfv, lc) = unzip lpr 
      (vfv, vc) = unzip varPr
      -- the unification
      unify [] [] = []
      unify ((tf,tv):tnext) ((lf,lv):lnext) 
        | tf /= lf  = error errmsg
        | isVar lv  = error errvlex
        | isVar tv  = (lf,lv):(unify (substFlist' tnext (tv,lv)) lnext)
        | lv == tv  = (lf,lv):(unify tnext lnext)
        | otherwise = error errmsg
      unification = unify vfv lfv 
      -- error messages in case things don't line up
      errmsg  = "Word '" ++ (iword le)    ++ "' does not correctly " 
             ++ " instantiate the variable predictors in tree " 
             ++  (itreename le) 
             ++ "\n Tree predictors: " ++ (show $ map fst varPr) 
             ++ "\n Word predictors:     " ++ (show $ map fst lpr)
             ++ "\n Hint: only the variable predictors should be instantiated" 
      errvlex = "Word '" ++ (iword le)    ++ "' contains variable " 
             ++ " predictors in " ++ (show $ map fst lpr)
  in if (lc == vc) -- note: this implies list equality
     then foldr addP constPrFm $ zip unification lc 
     else error errmsg
\end{code}


% ----------------------------------------------------------------------
\section{PredictorMap}
% ----------------------------------------------------------------------

We create a map between predictors and the trees that provide them.

\begin{code}
type PredictorMap = FiniteMap AvPair [TagElem]
\end{code}

\begin{code}
mapByPredictors :: [TagElem] -> PredictorMap 
mapByPredictors trees = foldr mapByPredictors' emptyFM trees 

mapByPredictors' :: TagElem -> PredictorMap -> PredictorMap 
mapByPredictors' tree fm = 
   let ispos _ pol = (pol > 0)
       predictors  = keysFM $ filterFM ispos $ tpredictors tree
       addp p f    = addToFM_C (++) f p [tree]
   in foldr addp fm predictors 
\end{code}
